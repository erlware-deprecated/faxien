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
publish([], RawPackageDirPath, Timeout) -> 
    {error, no_publish_repos};
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
    fax_put:put_erts_signature_file(Repos, ErtsVsn, create_signature(ErtsVsn), Timeout),
    fax_put:put_erts_package(Repos, ErtsVsn, pack(ErtsDirPath), Timeout); 

publish2(Type, Repos, AppDirPath, Timeout) when Type == binary; Type == generic -> 
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppDirPath),
    {ok, AppFileBinary}     = file:read_file(ewl_file:join_paths(AppDirPath, "ebin/" ++ AppName ++ ".app")),
    case fax_util:get_erts_vsn(AppDirPath) of
	{ok, ErtsVsn} ->
	    %% @todo make this transactional - if .app file put fails run a delete.
	    fax_put:put_dot_app_file(Repos, ErtsVsn, AppName, AppVsn, AppFileBinary, Timeout), 
	    fax_put:put_signature_file(Repos, ErtsVsn, "lib", AppName, AppVsn, create_signature(AppVsn), Timeout),
	    case Type of
		generic -> fax_put:put_generic_app_package(Repos, ErtsVsn, AppName, AppVsn, pack(AppDirPath), Timeout); 
		binary  -> fax_put:put_binary_app_package(Repos, ErtsVsn, AppName, AppVsn, pack(AppDirPath), Timeout)
	    end;
	Error ->
	    ?ERROR_MSG("beams compiled with an unsuppored erts vsn. Error ~p~n", [Error]),
	    Error
    end;
publish2(release, Repos, RelDirPath, Timeout) -> 
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(RelDirPath),
    RelFilePath             = epkg_package_paths:release_package_rel_file_path(RelDirPath, RelName, RelVsn),
    ErtsVsn                 = epkg_util:consult_rel_file(erts_vsn, RelFilePath),
    ok                      = handle_control(RelDirPath),
    {ok, ControlFileBinary} = file:read_file(epkg_package_paths:release_package_control_file_path(RelDirPath)),
    {ok, RelFileBinary}     = file:read_file(RelFilePath),
    fax_put:put_release_control_file(Repos, ErtsVsn, RelName, RelVsn, ControlFileBinary, Timeout),
    fax_put:put_dot_rel_file(Repos, ErtsVsn, RelName, RelVsn, RelFileBinary, Timeout),
    fax_put:put_signature_file(Repos, ErtsVsn, "releases", RelName, RelVsn, create_signature(RelVsn), Timeout),
    FilesToBeIgnored        = ["erts-" ++ ErtsVsn, "lib", "install.sh"],
    fax_put:put_release_package(Repos, ErtsVsn, RelName, RelVsn, 
				pack(ignore_files_in_release(RelDirPath, FilesToBeIgnored)), Timeout).
	
    
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
	    ControlTerm = {control, PackageName, lists:keyreplace(categories, 1, ControlList, fax_control:get_categories())},
	    write_out(ControlFilePath, ControlTerm);
	{error, Reason} ->
	    ?ERROR_MSG("Bad control file. Validation failed with ~p~n", [Reason]),
	    io:format("~nIt appears the package does not contain a valid control file. Lets create a basic one.~n"),
	    {ok, {PackageName, _PackageVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(RelDirPath),
	    ControlTerm                      = fax_control:collect_control_info(PackageName),
	    io:format("~n~p.~n~nAbove is the control information collected about this package. This information~n", [ControlTerm]),
	    io:format("will be placed under the root directory of the package in a file named \"control\".~n"),
	    io:format("**If done manually for the next publish be sure to include the period after the term**~n~n"),
	    ControlFilePath = epkg_package_paths:release_package_control_file_path(RelDirPath),
	    write_out(ControlFilePath, ControlTerm)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc create a signature to send over with a package. 
%% @end
%%--------------------------------------------------------------------
create_signature(PackageVsn) ->
    {ok, Vsn} = faxien:version(), 
    ConfigFilePath = epkg_installed_paths:find_config_file_path(faxien, Vsn),
    {ok, {{public_key, {Mod, ExpPub}}, {private_key, {Mod, ExpPriv}}}} = fax_manage:get_signature(ConfigFilePath),
    Signature = cg_rsa:encrypt(lists:foldl(fun(D, A) -> D + A end, 0, PackageVsn), Mod, ExpPriv),
    list_to_binary(io_lib:fwrite("~p.", [{signature, Signature, Mod, ExpPub}])).

%%--------------------------------------------------------------------
%% @private
%% @doc write a term file out
%% @end
%%--------------------------------------------------------------------
write_out(FilePath, Term) ->
    case file:open(FilePath, [write]) of
	{ok, IOD} ->
	    io:fwrite(IOD, "~p.", [Term]);
	Error ->
	    Error
    end.
	    

%%--------------------------------------------------------------------
%% @private
%% @doc if a release contains one of the directories or files to be ignored then
%%      dir copy the release to a temp dir and get rid the files.  Return a path to the new package dir.
%%      If the release contains no files to be ignored just return the path to the unaltered release.
%% @end
%%--------------------------------------------------------------------
ignore_files_in_release(RelDirPath, FilesToBeIgnored) ->
    Res = lists:any(fun(File) ->  filelib:is_file(ewl_file:join_paths(RelDirPath, File)) end, FilesToBeIgnored),
    case Res of
	true ->
	    ?INFO_MSG("ignoring ~p dir when publishing of ~p~n", [FilesToBeIgnored, RelDirPath]),
	    TmpRelDirPath = fax_util:copy_dir_to_tmp_dir(RelDirPath),
	    ok = lists:foreach(fun(File) -> ewl_file:delete_dir(ewl_file:join_paths(TmpRelDirPath, File)) end, FilesToBeIgnored),
	    TmpRelDirPath;
	false ->
	    RelDirPath
    end.
