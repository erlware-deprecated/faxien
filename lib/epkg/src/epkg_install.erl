%%%-------------------------------------------------------------------
%%% @author Martin Logan 
%%% @doc Understands how to install local packages.
%%% @copyright (C) 2007, Martin Logan, Eric Merritt, Erlware
%%% @end
%%% Created : 14 Dec 2007 by Martin Logan 
%%%-------------------------------------------------------------------
-module(epkg_install).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("epkg.hrl").
-include("macros.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 install_erts/2,
	 install_application/2,
	 install_application/3,
	 install_release/3,
	 install_sinan_release/3,
	 create_script_and_boot/4
	]).

%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% @doc If run from within a sinan project this will publish the
%%      latest dist tarball it finds within the Sinan project.
%%
%% @spec install_sinan_release(CWD, InstallationPath, IsLocalBoot) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
install_sinan_release(CWD, InstallationPath, IsLocalBoot) ->
    ProjectRootDir = ewl_sinan_paths:find_project_root(CWD),
    BuildFlavor    = epkg_util:get_sinan_build_flavor(ProjectRootDir),
    ?INFO_MSG("Installing release within the ~s build flavor~n", [BuildFlavor]),
    Tars = filelib:wildcard(filename:join([ProjectRootDir, "_build", BuildFlavor, "tar/*"])),
    RelPaths = 
	lists:map(fun({RelName, RelVsn}) ->
			  ewl_sinan_paths:dist_tarball_path(ProjectRootDir, BuildFlavor, RelName, RelVsn)
		  end,
		  epkg_util:collect_name_and_high_vsn_pairs(Tars)),
    
    case epkg_util:ask_about_string_in_list(RelPaths, "Do you want to install the release: ") of
	[] ->
	    io:format("~nNO release tarball will be installed. To generate a release~n" ++
		      "tarball run 'sinan dist' in your project dir~n~n");
	AlteredRelPaths -> 
	   lists:foreach(fun(RelPath) -> install_release(RelPath, InstallationPath, IsLocalBoot) end, AlteredRelPaths)
    end.
    
%%--------------------------------------------------------------------
%% @doc Install an application from a complte local application package. 
%% @spec install_application(AppPackageDirOrArchive, InstallationPath) -> {ok, ErtsVsn} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
install_application(AppPackageDirOrArchive, InstallationPath) ->
    AppPackageDirPath        = epkg_util:unpack_to_tmp_if_archive(AppPackageDirOrArchive), 
    {ok, {AppName, _AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppPackageDirPath),
    case epkg_util:discover_app_erts_vsns(AppPackageDirPath) of
	{ok, [ErtsVsn]} ->
	    install_application(AppPackageDirOrArchive, ErtsVsn, InstallationPath);
	{ok, [ErtsVsn|_] = ErtsVsns} ->
	    io:format("Warning: this application is compiled with more than one erts vsn ~p~n", [ErtsVsns]),
	    install_application(AppPackageDirOrArchive, ErtsVsn, InstallationPath);
	Error ->
	    ?ERROR_MSG("bad app ~p beams compiled with an unsuppored erts vsn. Error ~p~n", [AppName, Error]),
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Install an application from a complete local application package. 
%% @spec install_application(AppPackageDirOrArchive, ErtsVsn, InstallationPath) -> {ok, ErtsVsn} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
install_application(AppPackageDirOrArchive, ErtsVsn, InstallationPath) ->
    AppPackageDirPath       = epkg_util:unpack_to_tmp_if_archive(AppPackageDirOrArchive), 
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppPackageDirPath),
    ?INFO_MSG("Installing Application ~s-~s to ~p~n", [AppName, AppVsn, AppPackageDirPath]),

    Res = 
	case epkg_validation:is_package_an_app(AppPackageDirPath) of
	    false -> 
		{error, badly_formatted_or_missing_app_package};
	    true  -> 
		AppInstallationPath = ewl_installed_paths:application_container_path(InstallationPath, ErtsVsn),
		InstalledPackageDir = ewl_installed_paths:installed_app_dir_path(InstallationPath, ErtsVsn, AppName, AppVsn),
		install_non_release_package(AppPackageDirPath, InstalledPackageDir, AppInstallationPath)
	end,

    case Res of
	ok ->
	    {ok, [ActualErtsVsn|_]} = epkg_util:discover_app_erts_vsns(AppPackageDirPath),
	    print_erts_warning(AppName, ActualErtsVsn, ErtsVsn),
	    {ok, ActualErtsVsn};
	Error ->
	    Error
    end.

print_erts_warning(_AppName, ErtsVsn, ErtsVsn) ->
    ok;
print_erts_warning(AppName, ActualErtsVsn, ErtsVsn) ->
    ?INFO_MSG("Forcing install of ~p with actual erts vsn of ~p under erts vsn ~p~n", [AppName, ActualErtsVsn, ErtsVsn]).
    

%%--------------------------------------------------------------------
%% @doc Install an erts package. 
%% @spec install_erts(ErtsPackageDirOrArchive, InstallationPath) -> ok | {error, Reason}
%% where
%%  Reason = badly_formatted_or_missing_erts_package
%% @end
%%--------------------------------------------------------------------
install_erts(ErtsPackageDirOrArchive, InstallationPath) ->
    ?INFO_MSG("with args ~p and ~p~n", [ErtsPackageDirOrArchive, InstallationPath]),
    ErtsPackageDirPath      = epkg_util:unpack_to_tmp_if_archive(ErtsPackageDirOrArchive),
    {ok, {"erts", ErtsVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ErtsPackageDirOrArchive),
    InstalledErtsPath       = ewl_installed_paths:installed_erts_path(InstallationPath, ErtsVsn),
    case {epkg_validation:is_package_erts(InstalledErtsPath),
	  epkg_validation:is_package_erts(ErtsPackageDirPath)} of
	{true, _} ->
	    ok;
	{false, false} -> 
	    {error, badly_formatted_or_missing_erts_package};
	{false, true}  -> 
	    {ok, {_, ErtsVsn}}   = epkg_installed_paths:package_dir_to_name_and_vsn(ErtsPackageDirPath),
	    InstalledPackageDir  = ewl_installed_paths:installed_erts_path(InstallationPath, ErtsVsn),
	    ErtsInstallationPath = ewl_installed_paths:erts_container_path(InstallationPath, ErtsVsn),
	    install_non_release_package(ErtsPackageDirPath, InstalledPackageDir, ErtsInstallationPath),
	    epkg_util:set_all_executable_perms(ewl_installed_paths:executable_container_path(InstalledPackageDir))
    end.

%%--------------------------------------------------------------------
%% @doc Install a release from a complete local release package. If the lib dir does not contain the apps required this function
%% will go out and check repos in its lib directory. 
%% @spec install_release(ReleasePackagePath, InstallationPath, IsLocalBoot::bool()) -> ok | {error, Reason}
%% where
%%  Reason = badly_formatted_or_missing_release | {failed_to_install, [{AppName, AppVsn}]}
%% @end
%%--------------------------------------------------------------------
install_release(ReleasePackageArchiveOrDirPath, InstallationPath, IsLocalBoot) ->
    ?INFO_MSG("installing to ~s from ~s~n", [InstallationPath, ReleasePackageArchiveOrDirPath]),
    ReleasePackageDirPath   = epkg_util:unpack_to_tmp_if_archive(ReleasePackageArchiveOrDirPath),
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ReleasePackageDirPath),
    InstalledRelPath        = ewl_installed_paths:installed_release_dir_path(InstallationPath, RelName, RelVsn),
    case {epkg_validation:is_package_a_release(InstalledRelPath), 
	  epkg_validation:is_package_a_release(ReleasePackageDirPath)} of
	{_, false} -> 
	    {error, badly_formatted_or_missing_release};
	{false, true}  -> 
	    ewl_file:delete_dir(InstalledRelPath),
	    execute_release_installation_steps(ReleasePackageDirPath, InstallationPath, IsLocalBoot);
	{true, true}  -> 
	    ?INFO_MSG("The release is already installed at ~p~n", [InstalledRelPath]),
	    execute_release_installation_steps(ReleasePackageDirPath, InstallationPath, IsLocalBoot)
    end.

%%--------------------------------------------------------------------
%% @doc Create script and boot files for a release. 
%% <pre>
%% Example:
%%  create_script_and_boot("/usr/local/erlware/releases/my_rel/3.3.4/my_rel.rel", 
%%                         "/usr/local/erlware/lib/", false). 
%%  The RelFilePath points to the .rel file that is to be used while the LibDir function points to the location of the library
%%  applications to be included in the release. 
%% </pre>
%% @spec create_script_and_boot(InstallationPath, RelName, RelVsn, IsLocalBoot::boolean()) -> ok | exit()
%% @end
%%--------------------------------------------------------------------
create_script_and_boot(InstallationPath, RelName, RelVsn, IsLocalBoot) ->
    InstalledRelFilePath    = ewl_installed_paths:installed_release_rel_file_path(InstallationPath, RelName, RelVsn),
    [ErtsVsn, AppSpecs]     = epkg_util:consult_rel_file([erts_vsn, app_specs], InstalledRelFilePath),
    LoadedPaths             = code:get_path(),

    lists:foreach(fun(AppSpec) ->
			  AppName       = atom_to_list(element(1, AppSpec)),
			  AppVsn        = element(2, AppSpec),
			  AppNameAndVsn = AppName ++ "-" ++ AppVsn, 
			  AppPath       = ewl_installed_paths:installed_app_dir_path(InstallationPath, ErtsVsn,
										     AppName, AppVsn),
			  remove_redundant_paths(LoadedPaths, AppName, AppNameAndVsn),
			  ?INFO_MSG("Adding code path: ~p~n", [AppPath]),
			  code:add_pathz(ewl_file:join_paths(AppPath, "ebin"))
		  end, AppSpecs),

    Opts = 
	case IsLocalBoot of
	    true  -> [local, no_module_tests];
	    false -> [no_module_tests]
	end,
		      
    case systools:make_script(string:substr(InstalledRelFilePath, 1, length(InstalledRelFilePath) - 4), Opts) of
	ok -> 
	    ok;
	Error -> 
	    io:format("ERROR - Make script and boot failed ~p~n", [Error]),
	    exit({error, script_and_boot_failed})
    end.
    
%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Compose the steps needed to install a release. This is the only function that should understand pre installed 
%%      package release paths.
%% @todo trap exceptions here and format them into nice errors
%% @spec execute_release_installation_steps(ReleasePackageDirPath, InstallationPath, IsLocalBoot) -> ok | {error, Reason} 
%% where
%%  Reason = term() | {failed_to_install, [{AppName, AppVsn}]} | badly_formatted_or_missing_erts_package
%% @end
%%--------------------------------------------------------------------
execute_release_installation_steps(ReleasePackageDirPath, InstallationPath, IsLocalBoot) ->
    ?INFO_MSG("execute_release_installation_steps(~p, ~p, ~p)~n", [ReleasePackageDirPath, InstallationPath, IsLocalBoot]),
    case install_apps_for_release(ReleasePackageDirPath, InstallationPath) of
	{error, _} = Error ->
	    ?INFO_MSG("failed to install all apps for the local release package: ~p~n", [Error]),
	    Error;
	ok ->
	    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ReleasePackageDirPath),
	    PackageRelFilePath      = epkg_util:rel_file_path(ReleasePackageDirPath, RelName, RelVsn),
	    ErtsVsn                 = epkg_util:consult_rel_file(erts_vsn, PackageRelFilePath),
	    PackageErtsPackagePath  = ewl_package_paths:release_package_erts_package_path(ReleasePackageDirPath, ErtsVsn),
	    try 
	    lists:foreach(fun(Fun) -> Fun() end, 
			  [
			   fun() -> ok = install_erts(PackageErtsPackagePath, InstallationPath) end,
			   fun() -> ok = install_release_package(ReleasePackageDirPath, InstallationPath) end,
			   fun() -> ok = create_script_and_boot(InstallationPath, RelName, RelVsn, IsLocalBoot) end,
			   fun() -> ok = create_executable_script(InstallationPath, RelName, RelVsn, ErtsVsn) end
			  ])
	    catch
		_C:{badmatch, Error} -> Error;
		_C:Error             -> Error
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc install a release package assuming that all applications are present
%% @spec install_release_package(PackagePath, InstallationPath) -> ok | exit()
%% @end
%%--------------------------------------------------------------------
install_release_package(PackagePath, InstallationPath) ->
    % XXX need fully stage a release package before moving it into the releases directory
    ?INFO_MSG("with args ~p and ~p~n", [PackagePath, InstallationPath]),
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackagePath),
    InstalledRelPath        = ewl_installed_paths:installed_release_dir_path(InstallationPath, RelName, RelVsn),
    ok                      = ewl_file:delete_dir(InstalledRelPath), 
    PackageRelFilePath      = epkg_util:rel_file_path(PackagePath, RelName, RelVsn),

    
    ok = ewl_file:mkdir_p(InstalledRelPath),
    ok = ewl_file:mkdir_p(ewl_installed_paths:executable_container_path(InstallationPath)),
    ok = ewl_file:mkdir_p(ewl_installed_paths:release_container_path(InstallationPath)),

    ewl_file:delete_dir(InstalledRelPath),
    run_pre_install_hook(PackagePath),
    ok = ewl_file:copy_dir(PackagePath, InstalledRelPath),
    ok = ewl_file:copy_dir(filename:dirname(PackageRelFilePath), InstalledRelPath),
    run_post_install_hook(InstalledRelPath),
    ok = ewl_file:delete_dir(InstalledRelPath ++ "/lib"),
    ok = ewl_file:delete_dir(InstalledRelPath ++ "/releases").
    

%%--------------------------------------------------------------------
%% @doc Ensure that all apps specified by the release file are installed
%% @spec install_apps_for_release(ReleasePackagePath, InstallationPath) -> ok | {error, Reason}
%% where 
%%  Reason = term() | {failed_to_install, [{AppName, AppVsn}]}
%% @private
%% @end
%%--------------------------------------------------------------------
install_apps_for_release(ReleasePackagePath, InstallationPath) ->
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ReleasePackagePath),
    PackageRelFilePath      = epkg_util:rel_file_path(ReleasePackagePath, RelName, RelVsn),
    ErtsVsn                 = epkg_util:consult_rel_file(erts_vsn, PackageRelFilePath),

    ok = ewl_file:mkdir_p(ewl_installed_paths:application_container_path(InstallationPath, ErtsVsn)),
    [ErtsVsn, RawAppSpecs] = epkg_util:consult_rel_file([erts_vsn, app_specs], PackageRelFilePath),
    AppSpecs = [{element(1, AppSpec), element(2, AppSpec)} || AppSpec <- RawAppSpecs],
    case install_each_app_from_appspecs(AppSpecs, ReleasePackagePath, InstallationPath, ErtsVsn) of
	[] ->
	    ok;
        FailedApps ->
	    {error, {failed_to_install, FailedApps}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Given a list of OTP release app specs try to install each from the lib of a release pacakge.
%% @spec install_each_app_from_appspecs(AppSpecs, ReleasePackagePath, InstallationPath, ErtsVsn) -> FailedApps
%% where
%%  FailedApps = [{Name, Vsn}]
%% @end
%%--------------------------------------------------------------------
install_each_app_from_appspecs(AppSpecs, ReleasePackagePath, InstallationPath, ErtsVsn) ->
    lists:foldl(fun(AppSpec, Acc) ->
			AppName = atom_to_list(element(1, AppSpec)),
			AppVsn  = element(2, AppSpec),
			AppPackagePath =
			    ewl_package_paths:release_package_app_package_path(ReleasePackagePath, AppName, AppVsn),
			InstalledAppPath =
			    ewl_installed_paths:installed_app_dir_path(InstallationPath, ErtsVsn, AppName, AppVsn),
			Validations = {epkg_validation:is_package_an_app(InstalledAppPath),
				       epkg_validation:is_package_an_app(AppPackagePath)},
			case Validations of
			    {_, true} ->
				case install_application(AppPackagePath, ErtsVsn, InstallationPath) of
				    {ok, _} -> Acc;
				    _Error  -> [{AppName, AppVsn}|Acc]
				end;
			    {true, false} ->
				Acc;
			    {false, false} ->
				[{AppName, AppVsn}|Acc]
			end
		end, [], AppSpecs).

%%--------------------------------------------------------------------
%% @private
%% @doc Remove any additional paths from the path list. 
%% @end
%%--------------------------------------------------------------------
remove_redundant_paths([LoadedPath|T], AppName, AppNameAndVsn) ->
    case filename:basename(filename:dirname(LoadedPath)) of
	AppNameAndVsn -> 
	    ?INFO_MSG("Removing the path: ~p~n", [LoadedPath]),
	    code:del_path(LoadedPath);
	_ -> 
	    ok
    end,
    remove_redundant_paths(T, AppName, AppNameAndVsn);
remove_redundant_paths([], _, _) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Take a versioned directory from a local package and install it making 
%%      sure that you check for a previously installed version
%% @spec install_non_release_package(PackagePath, InstalledPackageDir, PackageInstallationPath) -> ok | exit()
%% @end
%%--------------------------------------------------------------------
install_non_release_package(PackagePath, InstalledPackageDir, PackageInstallationPath) ->
    ewl_file:mkdir_p(PackageInstallationPath),
    ?INFO_MSG("copying ~s over to ~s~n", [PackagePath, InstalledPackageDir]),
    ewl_file:delete_dir(InstalledPackageDir),
    ?INFO_MSG("deleted previous installed version, if present, at ~p~n", [InstalledPackageDir]),
    run_pre_install_hook(PackagePath),
    ewl_file:copy_dir(PackagePath, InstalledPackageDir),
    run_post_install_hook(InstalledPackageDir).

%%-------------------------------------------------------------------
%% @private
%% @doc if the package contains a hook script then run it. 
%% @end
%%-------------------------------------------------------------------
run_pre_install_hook(PackagePath) ->
    run_hook(PackagePath, "fax_pre_install").

run_post_install_hook(PackagePath) ->
    run_hook(PackagePath, "fax_post_install").

run_hook(PackagePath, FileName) ->
    HooksDir     = filename:join(PackagePath, "_hooks"),
    HookFilePath = filename:join(HooksDir, FileName),
    case filelib:is_file(HookFilePath) of
	true ->
	    io:format("running hook file: ~s~n", [FileName]),
	    ?INFO_MSG("running hook file: ~s~n", [FileName]),
	    {ok, CWD} = file:get_cwd(),
	    ok        = file:set_cwd(HooksDir), % run script from _hooks dir
	    ?INFO_MSG("setting perms on the build script ~p with result ~p~n", 
		      [HookFilePath, epkg_util:set_executable_perms("./" ++ FileName)]),
	    FileOutput = os:cmd("./" ++ FileName),
	    ?INFO_MSG("~s resulted in ~p~n", [FileName, FileOutput]),
	    ok = file:set_cwd(CWD);
	false ->
	    ?INFO_MSG("no fax_post_install script ~p present - skipping ~n", [HookFilePath]),
	    ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc This is the entry point to creating an excutable script for a particular release 
%% @todo This needs Windows support. 
%% @spec create_executable_script(InstallationPath, RelName, RelVsn, ErtsVsn) -> ok | exit()
%% @end
%%--------------------------------------------------------------------
create_executable_script(InstallationPath, RelName, RelVsn, ErtsVsn) ->
    BinDirPath   = ewl_installed_paths:installed_release_bin_dir_path(InstallationPath, RelName, RelVsn),
    BinFilePaths = lists:filter(fun(BinFilePath_) ->
					not filelib:is_dir(BinFilePath_) end,
				filelib:wildcard(BinDirPath ++ "/*")),
    ?INFO_MSG("bindir path ~p~nbin file paths ~p~n", [BinDirPath, BinFilePaths]),
    lists:foreach(fun(BinFilePath) -> 
			  BinFileName                      = filename:basename(BinFilePath),
			  ExecutableContainerPath          = ewl_installed_paths:executable_container_path(InstallationPath),
			  ExecutableContainerPlusErtsPath  = ewl_file:join_paths(ExecutableContainerPath, ErtsVsn),
			  InstalledBinFilePath             = ewl_file:join_paths(ExecutableContainerPath, BinFileName),
			  InstalledBinPlusErtsFilePath     = ewl_file:join_paths(ExecutableContainerPlusErtsPath, BinFileName),

			  ok = remove_existing_executable_script(InstalledBinFilePath), 
			  ok = remove_existing_executable_script(InstalledBinPlusErtsFilePath), 

			  file:copy(BinFilePath, InstalledBinFilePath),
			  ewl_file:gsub_file(InstalledBinFilePath, "%BACKUP%", ".."),
			  epkg_util:set_executable_perms(InstalledBinFilePath),

			  ok = ewl_file:mkdir_p(ExecutableContainerPlusErtsPath),
			  file:copy(BinFilePath, InstalledBinPlusErtsFilePath),
			  ewl_file:gsub_file(InstalledBinPlusErtsFilePath, "%BACKUP%", "../.."),
			  epkg_util:set_executable_perms(InstalledBinPlusErtsFilePath)
		  end, BinFilePaths). 

%%--------------------------------------------------------------------
%% @private
%% @doc Deal with existing executable scripts.
%% @end
%%--------------------------------------------------------------------
remove_existing_executable_script(ExecutableFile) ->
    case filelib:is_file(ExecutableFile) of
	true ->
	    io:format("Replacing existing executable file at: ~s~n", [ExecutableFile]),
	    file:delete(ExecutableFile),
	    ok;
	false  ->
	    ok
    end.

