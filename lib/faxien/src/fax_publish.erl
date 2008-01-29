%%%-------------------------------------------------------------------
%%% @doc Handles the publishing of applications from local to a 
%%%      versioned  code repository.
%%% 
%%%  @type repo() = string(). Contains address and repo designation. 
%%%   Example: http://www.erlware.org/stable   
%%%
%%%  @type repo_suffix() = string(). Contains ErtsVsn/Area/Application/Vsn/TarFile.
%%%
%%%  @type timeout() = integer() | infinity. Timeouts are specified in milliseconds.
%%%
%%% @author Martin Logan
%%% @copyright Erlware
%%% @end
%%%-------------------------------------------------------------------
-module(fax_publish).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 publish/3,
	 publish/4
	 ]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("faxien.hrl").
-include("epkg.hrl").

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 
%%  Publish a release or appliation to a repository. The PackageDirPath must be formatted in the following 
%%  way &lt;relname&gt;-&lt;rel version&gt; If you are publishing a tarball it must be compressed and have 
%%  the extention .epkg.
%%
%% <pre>
%% Example:
%%  publish(["http://www.erlware.org/stable"], "/home/jdoe/my_proj/lib/my_app", 40000).
%% </pre>
%%
%% @spec publish(Repos, PackageDirPath, Timeout::timeout()) -> ok | {error, Reason}
%% where
%%     IsGuarded = bool() 
%%     Repos = [repo()] 
%%     ErtsVsn = string()
%%     PackageDirPath = string() 
%% @end
%%--------------------------------------------------------------------
publish(Repos, RawPackageDirPath, Timeout) -> 
    PackageDirPath = epkg_util:unpack_to_tmp_if_archive(RawPackageDirPath),
    case epkg_validation:validate_type(PackageDirPath) of
	{ok, Type} ->
	    io:format("Publishing ~p package~n", [Type]), 
	    publish(Type, Repos, PackageDirPath, Timeout);
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc publish a local application to a repository. 
%%
%% <pre>
%% Example:
%%  publish(binary, ["http://www.erlware.org/stable"], "5.5.5", "/home/jdoe/my_proj/lib/my_app", 40000).
%% </pre>
%%
%% @spec publish(Type, Repos, PackageDirPath, Timeout::timeout()) -> ok | {error, Reason}
%% where
%%     Type = generic | binary | release | erts
%%     Repos = [repo()]
%%     ErtsVsn = string()
%%     PackageDirPath = string()
%% @end
%%--------------------------------------------------------------------
publish(Type, Repos, RawPackageDirPath, Timeout) when Type == generic; Type == binary; Type == release; Type == erts -> 
    PackageDirPath = epkg_util:unpack_to_tmp_if_archive(RawPackageDirPath),
    case catch publish2(Type, Repos, PackageDirPath, Timeout) of
	{error, _Reason} = Res ->
	    ?INFO_MSG("publish(~p, ~p, ~p, ~p) -> ~p~n", [Type, Repos, PackageDirPath, Timeout, Res]),
	    Res;
	{'EXIT', Reason} = Res ->
	    ?INFO_MSG("publish(~p, ~p, ~p, ~p) -> ~p~n", [Type, Repos, PackageDirPath, Timeout, Res]),
	    {error, Reason};
	{ok, URLS} ->
	    io:format("Publishing to ~p~n", [URLS]), 
	    ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================

publish2(erts, Repos, ErtsDirPath, Timeout) -> 
    {ok, {"erts", ErtsVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ErtsDirPath),
    fax_put:put_erts_package(Repos, ErtsVsn, pack(ErtsDirPath), Timeout); 

publish2(generic, Repos, AppDirPath, Timeout) -> 
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppDirPath),
    {ok, AppFileBinary}     = file:read_file(ewl_file:join_paths(AppDirPath, "ebin/" ++ AppName ++ ".app")),
    {ok, ErtsVsn}           = fax_util:get_erts_vsn(AppDirPath),
    %% @todo make this transactional - if .app file put fails run a delete.
    fax_put:put_dot_app_file(Repos, ErtsVsn, AppName, AppVsn, AppFileBinary, Timeout), 
    fax_put:put_generic_app_package(Repos, ErtsVsn, AppName, AppVsn, pack(AppDirPath), Timeout); 

publish2(binary, Repos, AppDirPath, Timeout) -> 
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppDirPath),
    {ok, AppFileBinary}     = file:read_file(ewl_file:join_paths(AppDirPath, "ebin/" ++ AppName ++ ".app")),
    {ok, ErtsVsn}           = fax_util:get_erts_vsn(AppDirPath),
    %% @todo make this transactional - if .app file put fails run a delete.
    fax_put:put_dot_app_file(Repos, ErtsVsn, AppName, AppVsn, AppFileBinary, Timeout), 
    fax_put:put_binary_app_package(Repos, ErtsVsn, AppName, AppVsn, pack(AppDirPath), Timeout); 

publish2(release, Repos, RelDirPath, Timeout) -> 
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(RelDirPath),
    RelFilePath             = epkg_package_paths:release_package_rel_file_path(RelDirPath, RelName, RelVsn),
    ErtsVsn                 = epkg_util:consult_rel_file(erts_vsn, RelFilePath),
    ok                      = handle_control(RelDirPath),
    fax_put:put_release_package(Repos, ErtsVsn, RelName, RelVsn, pack(handle_lib_in_release(RelDirPath)), Timeout).
	
    
%%--------------------------------------------------------------------
%% @private
%% @doc Packs up a package and returns a binary of the archive.
%% @spec pack(PackageDirPath::string()) -> DirPath::string()
%% @end
%%--------------------------------------------------------------------
pack(TarDirPath) ->
    {ok, {PackageName, _PackageVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(TarDirPath),
    TarDirName   = filename:basename(TarDirPath),
    {ok, TmpDirPath} = epkg_util:create_unique_tmp_dir(),
    ok               = ewl_file:copy_dir(TarDirPath, ewl_file:join_paths(TmpDirPath, TarDirName)),
    ?INFO_MSG("copy ~s to ~s~n", [TarDirPath, TmpDirPath]),
    TarName = PackageName ++ ".tar.gz",

    %% Add the tar file name to the end of each path suffix and the repo to the beginning. 
    io:format("Creating ~s from ~s~n", [TarName, TarDirName]),

    {ok, CWD}     = file:get_cwd(),
    ok            = file:set_cwd(TmpDirPath),
    ok            = ewl_file:compress(TarName, TarDirName),
    {ok, TarFile} = file:read_file("./" ++ TarName),
    ok            = file:set_cwd(CWD),
    ok            = ewl_file:delete_dir(TmpDirPath),
    TarFile.

%%--------------------------------------------------------------------
%% @private
%% @doc make sure the control file is valid before publishing.  If it is not create it.
%% @end
%%--------------------------------------------------------------------
handle_control(RelDirPath) ->
    ControlFilePath = epkg_package_paths:release_package_control_file_path(RelDirPath),
    case epkg_validation:is_valid_control_file(ControlFilePath) of
	true ->
	    ok;
	{error, Reason} when element(1, Reason) == bad_categories; Reason == no_categories ->
	    ?ERROR_MSG("Bad control file. Validation failed with ~p~n", [Reason]),
	    io:format("~nOne of more of the categories in the control file are invalid please re-enter them.~n"),
	    {ok, [{control, PackageName, ControlList}]} = file:consult(ControlFilePath),
	    ControlTerm = {control, PackageName, lists:keyreplace(categories, 1, ControlList, {categories, enter_categories()})},
	    write_out(ControlFilePath, ControlTerm);
	{error, Reason} ->
	    ?ERROR_MSG("Bad control file. Validation failed with ~p~n", [Reason]),
	    io:format("~nIt appears the package does not contain a valid control file. Lets create a basic one.~n"),
	    {ok, {PackageName, _PackageVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(RelDirPath),
	    ControlTerm                      = collect_control_info(PackageName),
	    io:format("~n~p.~n~nAbove is the control information collected about this package. This information~n", [ControlTerm]),
	    io:format("will be placed under the root directory of the package in a file named \"control\".~n"),
	    io:format("**If done manually for the next publish be sure to include the period after the term**~n~n"),
	    ControlFilePath = epkg_package_paths:release_package_control_file_path(RelDirPath),
	    write_out(ControlFilePath, ControlTerm)
    end.

write_out(ControlFilePath, ControlTerm) ->
    case file:open(ControlFilePath, [write]) of
	{ok, IOD} ->
	    io:fwrite(IOD, "~p.", [ControlTerm]);
	Error ->
	    Error
    end.
	    
%%--------------------------------------------------------------------
%% @private
%% @doc Collect control data from the user.
%% <pre>
%% Types:
%%  ControlInfo = [{Key, Value}]
%% </pre>
%% @spec collect_control_info(PackageName) -> ControlTerm
%% @end
%%--------------------------------------------------------------------
collect_control_info(PackageName) when is_atom(PackageName) ->
    collect_control_info(list_to_atom(PackageName));
collect_control_info(PackageName) ->
    {control, PackageName, lists:flatten([collect_manditory_control_info(),  collect_additional_control_info()])}. 

collect_manditory_control_info() ->
    [
     {package_owner, ewl_talk:ask("~nEnter the package owners full name > ")},
     {package_owner_email, ewl_talk:ask("~nEnter a contact email address > ")},
     {categories, enter_categories()},
     {description, ewl_talk:ask("~nEnter a short description of the package > ")}
    ].

enter_categories() ->
    Categories = [string:strip(E, both, $ ) || 
		     E <- string:tokens(
			    ewl_talk:ask(
			      lists:flatten(["~nEnter from the list below. Separate multiple categories with commas:\n", 
					     printable_categories(), 
					     " > "])), ",")],
    case epkg_util:find_bad_control_categories(Categories) of
	[] ->
	    Categories;
	BadCategories ->
	    io:format("The following categories are invalid ~p. Please enter only the categories suggested exactly.~n", 
		      [BadCategories]),
	    enter_categories()
    end.

printable_categories() ->
    string:strip(lists:foldl(fun(C, Acc) -> C ++ ", " ++ Acc end, [], ?CONTROL_CATEGORIES), right, $,).
			
collect_additional_control_info() ->
    case ewl_talk:ask("~nWould you like to specify additional control information? [yes|no] > ") of
	Yes when Yes == $y; Yes == $Y; Yes == "yes" ->
	    [
	     {author, ewl_talk:ask("~nEnter the authors full name > ")},
	     {authors_email, ewl_talk:ask("~nEnter the authors email address > ")},
	     {keywords, [string:strip(E, both, $ ) || 
			   E <- string:tokens(ewl_talk:ask("~nEnter comma separated keywords for the package > "), ",")]},
	     {project_page, ewl_talk:ask("~nEnter project page url > ")}
	    ];
	No when No == $n; No == $N; No == "no" ->
	    [];
	Error ->
	    ?INFO_MSG("user entered \"~p\"~n", [Error]),
	    io:format("Please enter \"yes\" or \"no\"~n"),
	    collect_additional_control_info()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc if a release contains a lib dir copy the release to a temp dir and get rid of lib.  Return a path to the new package dir.
%%      If the release contains no lib dir just return the path to the unaltered release.
%% @end
%%--------------------------------------------------------------------
handle_lib_in_release(RelDirPath) ->
    case filelib:is_dir(RelDirPath ++ "/lib") of
	true ->
	    ?INFO_MSG("ignoring the lib dir when publishing of ~p~n", [RelDirPath]),
	    TmpRelDirPath = fax_util:copy_dir_to_tmp_dir(RelDirPath),
	    ok = ewl_file:delete_dir(TmpRelDirPath ++ "/lib"),
	    TmpRelDirPath;
	false ->
	    RelDirPath
    end.
