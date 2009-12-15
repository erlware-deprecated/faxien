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
	 publish_sinan/3,
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
%% @doc Publish from within a Sinan project. This will publish all
%% compiled applications as well as any releases that have been
%% created via "dist"
%% @spec (Repos, CWD, Timeout) -> ok | {error, Reason}
%% where
%%  Repos = [string()]
%%  Timeout = integer()
%% @end
%%--------------------------------------------------------------------
publish_sinan(Repos, CWD, Timeout) ->
    ProjectRootDir = ewl_sinan_paths:find_project_root(CWD),
    BuildFlavor    = epkg_util:get_sinan_build_flavor(ProjectRootDir),
    ?INFO_MSG("Publishing contents within the ~s build flavor~n", [BuildFlavor]),
    publish_apps_from_sinan(Repos, ProjectRootDir, BuildFlavor, Timeout),
    publish_dist_tarball_from_sinan(Repos, ProjectRootDir, BuildFlavor, Timeout).
    
%%--------------------------------------------------------------------
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
%% @spec publish(Repos, PackageDirPaths, Timeout::timeout()) -> ok | {error, Reason}
%% where
%%     IsGuarded = bool() 
%%     Repos = [repo()] 
%%     ErtsVsn = string()
%%     PackageDirPaths = [PackageDirPath] | PackageDirPath 
%%      PackageDirPath = string()
%% @end
%%--------------------------------------------------------------------
publish([], _RawPackageDirPath, _Timeout) -> 
    {error, no_publish_repos};
publish(Repos, [H|_] = RawPackageDirPath, Timeout) when is_integer(H) -> 
    PackageDirPath = epkg_util:unpack_to_tmp_if_archive(RawPackageDirPath),
    case epkg_validation:validate_type(PackageDirPath) of
	{ok, Type} ->
	    io:format("Publishing ~p package~n", [Type]), 
	    publish(Type, Repos, PackageDirPath, Timeout);
	{error, Reason} ->
	    {error, Reason}
    end;
publish(Repos, RawPackageDirPaths, Timeout) -> 
    lists:foreach(fun(PackagePath) ->
			  io:format("Publishing ~p package~n", [PackagePath]), 
			  publish(Repos, PackagePath, Timeout)
		  end, RawPackageDirPaths).
			  

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
publish(Type, Repos, RawPackageDirPath, Timeout)
  when Type == unbuilt; Type == generic; Type == binary; Type == release; Type == erts -> 
    PackageDirPath = epkg_util:unpack_to_tmp_if_archive(RawPackageDirPath),
    case catch publish2(Type, Repos, PackageDirPath, Timeout) of
	{error, _Reason} = Res ->
	    io:format("publish error~n"), 
	    ?INFO_MSG("publish(~p, ~p, ~p, ~p) -> ~p~n", [Type, Repos, PackageDirPath, Timeout, Res]),
	    Res;
	{'EXIT', Reason} = Res ->
	    io:format("publish error~n"), 
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
    Binary = pack(ErtsDirPath),
    Res = fax_put:put_erts_package(Repos, ErtsVsn, Binary, Timeout), 
    fax_put:put_erts_checksum_file(Repos, ErtsVsn, Binary, Timeout),
    Res;
publish2(Type, Repos, RawAppDirPath, Timeout) when Type == unbuilt; Type == binary; Type == generic -> 
    AppDirPath = move_to_proper_dir_if_no_name_and_vsn(RawAppDirPath),
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppDirPath),
    {ok, AppFileBinary}     = file:read_file(ewl_file:join_paths(AppDirPath, "ebin/" ++ AppName ++ ".app")),

    F = fun({ok, ErtsVsn}, _) -> {ok, ErtsVsn};
	   ({error, no_beam_files}, unbuilt) -> {ok, "0.0"};
	   (Error, _)                        -> Error
	end,

    case F(epkg_util:discover_app_erts_vsns(AppDirPath), Type) of
	{ok, [ErtsVsn|_]} ->
	    %% @todo make this transactional - if .app file put fails run a delete.
	    fax_put:put_dot_app_file(Repos, ErtsVsn, AppName, AppVsn, AppFileBinary, Timeout), 
	    fax_put:put_signature_file(Repos, ErtsVsn, "lib", AppName, AppVsn, create_signature(AppVsn), Timeout),
	    Binary = pack(AppDirPath),
	    Result = 
		case Type of
		    unbuilt -> fax_put:put_unbuilt_app_package(Repos, ErtsVsn, AppName, AppVsn, Binary, Timeout); 
		    generic -> fax_put:put_generic_app_package(Repos, ErtsVsn, AppName, AppVsn, Binary, Timeout); 
		    binary  -> fax_put:put_binary_app_package(Repos, ErtsVsn, AppName, AppVsn, Binary, Timeout)
		end,
	    fax_put:put_checksum_file(Repos, ErtsVsn, "lib", AppName, AppVsn, Binary, Timeout),
	    Result;
	Error ->
	    ?ERROR_MSG("beams compiled with an unsuppored erts vsn. Error ~p~n", [Error]),
	    Error
    end;
publish2(release, Repos, RelDirPath, Timeout) -> 
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(RelDirPath),
    RelFilePath             = ewl_package_paths:release_package_rel_file_path(RelDirPath, RelName, RelVsn),
    ErtsVsn                 = epkg_util:consult_rel_file(erts_vsn, RelFilePath),
    ok                      = handle_control(RelDirPath),
    {ok, ControlFileBinary} = file:read_file(ewl_package_paths:release_package_control_file_path(RelDirPath)),
    {ok, RelFileBinary}     = file:read_file(RelFilePath),
    fax_put:put_release_control_file(Repos, ErtsVsn, RelName, RelVsn, ControlFileBinary, Timeout),
    fax_put:put_dot_rel_file(Repos, ErtsVsn, RelName, RelVsn, RelFileBinary, Timeout),
    fax_put:put_signature_file(Repos, ErtsVsn, "releases", RelName, RelVsn, create_signature(RelVsn), Timeout),
    FilesToBeIgnored = ["erts-" ++ ErtsVsn, "lib", "install.sh"],
    Binary = pack(ignore_files_in_release(RelDirPath, FilesToBeIgnored)), 
    Res = fax_put:put_release_package(Repos, ErtsVsn, RelName, RelVsn, Binary, Timeout),
    fax_put:put_checksum_file(Repos, ErtsVsn, "releases", RelName, RelVsn, Binary, Timeout),
    Res.
	
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
    ControlFilePath = ewl_package_paths:release_package_control_file_path(RelDirPath),
    case epkg_validation:is_valid_control_file(ControlFilePath) of
	true ->
	    ok;
	{error, Reason} when element(1, Reason) == bad_categories; Reason == no_categories ->
	    ?ERROR_MSG("Bad control file. Validation failed with ~p~n", [Reason]),
	    io:format("~nOne of more of the categories in the control file are invalid please re-enter them.~n"),
	    {ok, [{control, PackageName, ControlList}]} = file:consult(ControlFilePath),
	    ControlTerm = {control, PackageName, lists:keyreplace(categories, 1, ControlList, epkg_control:get_categories())},
	    write_out(ControlFilePath, ControlTerm);
	{error, Reason} ->
	    ?ERROR_MSG("Bad control file. Validation failed with ~p~n", [Reason]),
	    io:format("~nIt appears the package does not contain a valid control file. Lets create a basic one.~n"),
	    {ok, {PackageName, _PackageVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(RelDirPath),
	    ControlTerm                      = epkg_control:collect_control_info(PackageName),
	    io:format("~n~p.~n~nAbove is the control information collected about this package. This information~n", [ControlTerm]),
	    io:format("will be placed under the root directory of the package in a file named \"control\".~n"),
	    io:format("**If done manually for the next publish be sure to include the period after the term**~n~n"),
	    ControlFilePath = ewl_package_paths:release_package_control_file_path(RelDirPath),
	    write_out(ControlFilePath, ControlTerm)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc create a signature to send over with a package. 
%% @end
%%--------------------------------------------------------------------
create_signature(PackageVsn) ->
    ConfigFilePaths = epkg_util:multi_config_paths(),
    {ok, {{public_key, {Mod, ExpPub}}, {private_key, {Mod, ExpPriv}}}} = fax_manage:get_signature(ConfigFilePaths),
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
    
publish_apps_from_sinan(Repos, ProjectRootDir, BuildFlavor, Timeout) ->
    %% Publish the apps
    Apps = filelib:wildcard(filename:join([ProjectRootDir, "_build", BuildFlavor, "apps/*"])),
    ?INFO_MSG("Found the following apps ~p~n", [Apps]),
    AppPaths = 
	lists:map(fun({AppName, AppVsn}) ->
			  ewl_sinan_paths:built_app_path(ProjectRootDir, BuildFlavor, AppName, AppVsn)
		      end,
		      epkg_util:collect_name_and_high_vsn_pairs(Apps)),

    case epkg_util:ask_about_string_in_list(AppPaths, "Do you want to publish the app: ") of
	[] ->
	    io:format("~nNO apps will be published. To generate app packages run 'sinan'~nin your project dir~n~n");
	AlteredAppPaths ->
	    publish(Repos, AlteredAppPaths, Timeout)
    end.

publish_dist_tarball_from_sinan(Repos, ProjectRootDir, BuildFlavor, Timeout) ->
    %% Publish the release
    Tars = filelib:wildcard(filename:join([ProjectRootDir, "_build", BuildFlavor, "tar/*"])),
    ?INFO_MSG("Found the following dist tarballs ~p~n", [Tars]),
    RelPaths = 
	lists:map(fun({RelName, RelVsn}) ->
			  ewl_sinan_paths:dist_tarball_path(ProjectRootDir, BuildFlavor, RelName, RelVsn)
		  end,
		  epkg_util:collect_name_and_high_vsn_pairs(Tars)),
    
    case epkg_util:ask_about_string_in_list(RelPaths, "Do you want to publish the release: ") of
	[] ->
	    io:format("~nNO release tarball will be published. To generate a release~n" ++
		      "tarball run 'sinan dist' in your project dir~n~n");
	AlteredRelPaths -> 
	    publish(Repos, AlteredRelPaths, Timeout)
    end.


move_to_proper_dir_if_no_name_and_vsn(AppDirPath) ->
    try
	{ok, {_AppName, _AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppDirPath),
	AppDirPath
    catch
	_C:_E ->
	    AppBaseName = filename:basename(AppDirPath),
	    case re:run(AppBaseName, "[a-z][a-z0-9]*", []) of
		{match,[{0, Length}]} when Length == length(AppBaseName) ->
		    DotAppFilePath = AppDirPath ++ "/ebin/" ++ AppBaseName ++ ".app",
		    {ok, [{_, _, List}]} = file:consult(DotAppFilePath),
		    {value,{vsn,VsnString}} = lists:keysearch(vsn, 1, List),
		    ProperAppDirName = AppBaseName ++ "-" ++ VsnString,
		    {ok, TmpDirPath}   = epkg_util:create_unique_tmp_dir(),
		    TmpArtifactFilePath = ewl_file:join_paths(TmpDirPath, ProperAppDirName),
		    ?INFO_MSG("Moving dir with no version ~p to ~p~n", [AppDirPath, TmpArtifactFilePath]),
		    ok = ewl_file:copy_dir(AppDirPath, TmpArtifactFilePath),
		    TmpArtifactFilePath
	    end
    end.
		    
		    
		    
