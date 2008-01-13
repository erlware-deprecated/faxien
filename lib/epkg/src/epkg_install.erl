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

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 install_erts/2,
	 install_application/2,
	 install_release/3,
	 create_script_and_boot/4
	]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Install an application from a complte local application package. 
%% @spec install_application(AppPackageDirOrArchive, InstallationPath) -> ok | {error, Reason}
%% where
%%  Reason = badly_formatted_or_missing_app_package
%% @end
%%--------------------------------------------------------------------
install_application(AppPackageDirOrArchive, InstallationPath) ->
    AppPackageDirPath       = epkg_util:unpack_to_tmp_if_archive(AppPackageDirOrArchive), 
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppPackageDirPath),
    ?INFO_MSG("Installing Application ~s-~s to ~p~n", [AppName, AppVsn, AppPackageDirPath]),
    Res = 
	case epkg_validation:is_package_an_app(AppPackageDirPath) of
	    false -> 
		{error, badly_formatted_or_missing_app_package};
	    true  -> 
		AppInstallationPath     = epkg_installed_paths:application_container_path(InstallationPath),
		InstalledPackageDir     = epkg_installed_paths:installed_app_dir_path(InstallationPath, AppName, AppVsn),
		install_non_release_package(AppPackageDirPath, InstalledPackageDir, AppInstallationPath)
	end,
    ?INFO_MSG("returned ~p~n", [Res]), 
    Res.

%%--------------------------------------------------------------------
%% @doc Install an erts package. 
%% @spec install_erts(ErtsPackageDirOrArchive, InstallationPath) -> ok | {error, Reason}
%% where
%%  Reason = badly_formatted_or_missing_erts_package
%% @end
%%--------------------------------------------------------------------
install_erts(ErtsPackageDirOrArchive, InstallationPath) ->
    ?INFO_MSG("with args ~p and ~p~n", [ErtsPackageDirOrArchive, InstallationPath]),
    ErtsPackageDirPath      = ensure_correct_erts_dir(epkg_util:unpack_to_tmp_if_archive(ErtsPackageDirOrArchive)),
    {ok, {"erts", ErtsVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ErtsPackageDirOrArchive),
    InstalledErtsPath       = epkg_installed_paths:installed_erts_path(InstallationPath, ErtsVsn),
    case {epkg_validation:is_package_erts(InstalledErtsPath),
	  epkg_validation:is_package_erts(ErtsPackageDirPath)} of
	{true, _} ->
	    ok;
	{false, false} -> 
	    {error, badly_formatted_or_missing_erts_package};
	{false, true}  -> 
	    {ok, {_, ErtsVsn}}   = epkg_installed_paths:package_dir_to_name_and_vsn(ErtsPackageDirPath),
	    InstalledPackageDir  = epkg_installed_paths:installed_erts_path(InstallationPath, ErtsVsn),
	    ErtsInstallationPath = epkg_installed_paths:erts_container_path(InstallationPath),
	    install_non_release_package(ErtsPackageDirPath, InstalledPackageDir, ErtsInstallationPath),
	    epkg_util:set_all_executable_perms(epkg_installed_paths:executable_container_path(InstalledPackageDir))
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
    ReleasePackageDirPath   = epkg_util:unpack_to_tmp_if_archive(ReleasePackageArchiveOrDirPath),
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ReleasePackageDirPath),
    InstalledRelPath        = epkg_installed_paths:installed_release_dir_path(InstallationPath, RelName, RelVsn),
    case {epkg_validation:is_package_a_release(InstalledRelPath), 
	  epkg_validation:is_package_a_release(ReleasePackageDirPath)} of
	{_, false} -> 
	    {error, badly_formatted_or_missing_release};
	{false, true}  -> 
	    ewl_file:delete_dir(InstalledRelPath),
	    execute_release_installation_steps(ReleasePackageDirPath, InstallationPath, IsLocalBoot);
	{true, true}  -> 
	    {error, release_already_instaled}
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
    InstalledRelFilePath    = epkg_installed_paths:installed_release_rel_file_path(InstallationPath, RelName, RelVsn),
    AppSpecs                = epkg_util:consult_rel_file(app_specs, InstalledRelFilePath),
    LoadedPaths             = code:get_path(),

    lists:foreach(fun(AppSpec) ->
			  AppName       = atom_to_list(element(1, AppSpec)),
			  AppVsn        = element(2, AppSpec),
			  AppNameAndVsn = AppName ++ "-" ++ AppVsn, 
			  AppPath       = epkg_installed_paths:installed_app_dir_path(InstallationPath, AppName, AppVsn),
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
    try 
	case install_apps_for_release(ReleasePackageDirPath, InstallationPath) of
	    {error, _} = Error ->
		?INFO_MSG("failed to install all apps for the local release package: ~p~n", [Error]),
		Error;
	    ok ->
		{ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ReleasePackageDirPath),
		PackageRelFilePath      = epkg_package_paths:release_package_rel_file_path(ReleasePackageDirPath, RelName, RelVsn),
		ErtsVsn                 = epkg_util:consult_rel_file(erts_vsn, PackageRelFilePath),
		PackageErtsPackagePath  = epkg_package_paths:release_package_erts_package_path(ReleasePackageDirPath, ErtsVsn),
		lists:foreach(fun(Fun) -> Fun() end, 
			      [
			       fun() -> ok = install_erts(PackageErtsPackagePath, InstallationPath) end,
			       fun() -> ok = install_release_package(ReleasePackageDirPath, InstallationPath) end,
			       fun() -> ok = create_script_and_boot(InstallationPath, RelName, RelVsn, IsLocalBoot) end,
			       fun() -> ok = create_executable_script(InstallationPath, RelName, RelVsn, ErtsVsn) end
			      ])
	end
    catch
	Class:Exception = {badmatch, ActualError} ->
	    ?ERROR_MSG("Caught exception ~p of class ~p~n", [Exception, Class]), 
	    ActualError;
	Class:Exception ->
	    ?ERROR_MSG("Caught exception ~p of class ~p~n", [Exception, Class]), 
	    {error, Exception}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc install a release package assuming that all applications are present
%% @spec install_release_package(PackagePath, InstallationPath) -> ok | exit()
%% @end
%%--------------------------------------------------------------------
install_release_package(PackagePath, InstallationPath) ->
    ?INFO_MSG("with args ~p and ~p~n", [PackagePath, InstallationPath]),
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackagePath),
    InstalledRelPath        = epkg_installed_paths:installed_release_dir_path(InstallationPath, RelName, RelVsn),
    ok                      = ewl_file:delete_dir(InstalledRelPath), 

    build_if_build_file(PackagePath),
    
    ok = ewl_file:mkdir_p(InstalledRelPath),
    ok = ewl_file:mkdir_p(epkg_installed_paths:executable_container_path(InstallationPath)),
    ok = ewl_file:mkdir_p(epkg_installed_paths:log_container_path(InstallationPath)),
    ok = ewl_file:mkdir_p(epkg_installed_paths:release_container_path(InstallationPath)),

    ok = ewl_file:copy_dir(PackagePath, InstalledRelPath),
    ok = ewl_file:copy_dir(InstalledRelPath ++ "/releases/" ++ RelVsn, InstalledRelPath ++ "/release"),
    ok = ewl_file:delete_dir(InstalledRelPath ++ "/releases").
    

%%--------------------------------------------------------------------
%% @private
%% @doc Ensure that all apps specified by the release file are installed
%% @spec install_apps_for_release(ReleasePackagePath, InstallationPath) -> ok | {error, Reason}
%% where 
%%  Reason = term() | {failed_to_install, [{AppName, AppVsn}]}
%% @end
%%--------------------------------------------------------------------
install_apps_for_release(ReleasePackagePath, InstallationPath) ->
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ReleasePackagePath),
    PackageRelFilePath      = epkg_package_paths:release_package_rel_file_path(ReleasePackagePath, RelName, RelVsn),

    ok = ewl_file:mkdir_p(epkg_installed_paths:application_container_path(InstallationPath)),
    MissingAppSpecs = missing_apps_for_release(InstallationPath, PackageRelFilePath),
    case install_each_app_from_appspecs(MissingAppSpecs, ReleasePackagePath, InstallationPath) of
	[] ->
	    ok;
        FailedApps ->
	    {error, {failed_to_install, FailedApps}}
    end.
		   
	    

%%--------------------------------------------------------------------
%% @private
%% @doc Given the installation path and a release file this function determines which apps perscribed by the release file
%%      are not yet installed.
%% @spec missing_apps_for_release(InstallationPath, PackageRelFilePath) -> [AppSpecs]
%% @end
%%--------------------------------------------------------------------
missing_apps_for_release(InstallationPath, PackageRelFilePath) ->
    AppSpecs = epkg_util:consult_rel_file(app_specs, PackageRelFilePath),
    lists:foldl(fun(AppSpec, Acc) ->
			AppName = atom_to_list(element(1, AppSpec)),
			AppVsn  = element(2, AppSpec),
			InstalledAppPath = epkg_installed_paths:installed_app_dir_path(InstallationPath, AppName, AppVsn),
			case epkg_validation:is_package_an_app(InstalledAppPath) of
			    false -> 
				[AppSpec|Acc];
			    true  -> 
				Acc
			end
		end, [], AppSpecs).
			
    
%%--------------------------------------------------------------------
%% @private
%% @doc Given a list of OTP release app specs try to install each from the lib of a release pacakge.
%% @spec install_each_app_from_appspecs(AppSpecs, ReleasePackagePath, InstallationPath) -> FailedApps
%% where
%%  FailedApps = [{Name, Vsn}]
%% @end
%%--------------------------------------------------------------------
install_each_app_from_appspecs(AppSpecs, ReleasePackagePath, InstallationPath) ->
    lists:foldl(fun(AppSpec, Acc) ->
			AppName = atom_to_list(element(1, AppSpec)),
			AppVsn  = element(2, AppSpec),
			AppPackagePath = epkg_package_paths:release_package_app_package_path(ReleasePackagePath, AppName, AppVsn),
			case install_application(AppPackagePath, InstallationPath) of
			    ok     -> Acc;
			    _Error -> [{AppName, AppVsn}|Acc]
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
    ewl_file:copy_dir(PackagePath, InstalledPackageDir),
    build_if_build_file(InstalledPackageDir).

%%-------------------------------------------------------------------
%% @private
%% @doc if the package contains a executable build script run it. 
%% Note: for now the build file must exist right beneith the package dir. 
%% @spec build_if_build_file(InstalledAppPath) -> ok | exit()
%% @end
%%-------------------------------------------------------------------
build_if_build_file(InstalledPackagePath) ->
    BuildFile = InstalledPackagePath ++ "/" ++ "build",
    case filelib:is_file(BuildFile) of
	true ->
	    {ok, CWD} = file:get_cwd(),
	    ok        = file:set_cwd(InstalledPackagePath),
	    ?INFO_MSG("setting perms on the build script ~p with result ~p~n", 
				  [BuildFile, epkg_util:set_executable_perms(BuildFile)]),
	    ?INFO_MSG("running build file ~p with result ~p~n", [BuildFile, os:cmd(BuildFile)]),
	    ok = file:set_cwd(CWD);
	false ->
	    ?INFO_MSG("no build script ~p present - skipping ~n", [BuildFile]),
	    ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc make sure that if someone passes a package path like /home/martinjlogan/erts instead of the full path like 
%%      /home/martinjlogan/erts/5.5.5 that we return that latter path. At the same time if the path passed is a traditional
%%      erts path like /home/martinjlogan/erts-5.5.5 do nothing. 
%% @spec ensure_correct_erts_dir(RawErtsPackagePath) -> ErtsDir | exit()
%% @end
%%--------------------------------------------------------------------
ensure_correct_erts_dir(RawErtsPackagePath) ->
    PackageDir = filename:basename(filename:absname(RawErtsPackagePath)),
    ?INFO_MSG("package dir is ~p~n", [PackageDir]),
    case regexp:match(PackageDir, "(^erts-[0-9\.]+$|^[0-9\.]+$)") of
	{match, 1, _} -> 
	    RawErtsPackagePath;
	_ ->
	    case PackageDir of
		[$e,$r,$t,$s] -> just_erts(RawErtsPackagePath);
		Error         -> {error,{bad_erts_directory, Error}}
	    end
    end.

just_erts(RawErtsPackagePath) ->
    case filelib:wildcard(RawErtsPackagePath ++ "/*") of
	[ErtsVsn] ->
	    case regexp:match(ErtsVsn, ".*\/[0-9\.]+\/?$") of
		{match, 1, _} -> ErtsVsn;
		_             -> {error,{bad_erts_directory, ErtsVsn}}
	    end;
        List when length(List) > 1 ->
	    {error,{bad_erts_directory, List}};
	Error -> 
	    {error,{bad_erts_directory, Error}}
    end.
		 
    

%%--------------------------------------------------------------------
%% @private
%% @doc This is the entry point to creating an excutable script for a particular release 
%% @todo This needs Windows support. 
%% @spec create_executable_script(InstallationPath, RelName, RelVsn, ErtsVsn) -> ok | exit()
%% @end
%%--------------------------------------------------------------------
create_executable_script(InstallationPath, RelName, RelVsn, ErtsVsn) ->
    create_executable_script_use_cmdr(InstallationPath, RelName, RelVsn, ErtsVsn),
    create_executable_script_use_bin(InstallationPath, RelName, RelVsn).
    
create_executable_script_use_cmdr(InstallationPath, RelName, RelVsn, ErtsVsn) ->
    CmdsDirPath = epkg_installed_paths:installed_release_cmds_dir_path(InstallationPath, RelName, RelVsn),
    LauncherTemplateFiles = filelib:wildcard(CmdsDirPath ++ "/*"),
    lists:foreach(fun(TemplateFile) -> 
			  TemplateName = filename:basename(filename:absname(TemplateFile)),
			  case regexp:match(TemplateFile, ".*\.tmpl") of
			      {match, _, _} ->
				  ExecutableFile = lists:flatten([InstallationPath,
								  "/bin/",
								  string:substr(TemplateName, 1, length(TemplateName) - 5)]),
				  ok = remove_existing_executable_script(ExecutableFile), 
				  ok = write_executable_file(ExecutableFile, 
							     InstallationPath, 
							     RelName, 
							     RelVsn, 
							     ErtsVsn, 
							     TemplateName);
			      _Error ->
				  ?INFO_MSG("A non template file ~p found - skipping~n", [TemplateFile])
			  end
		  end, LauncherTemplateFiles). 

create_executable_script_use_bin(InstallationPath, RelName, RelVsn) ->
    BinDirPath   = epkg_installed_paths:installed_release_bin_dir_path(InstallationPath, RelName, RelVsn),
    BinFilePaths = filelib:wildcard(BinDirPath ++ "/*"),
    ?INFO_MSG("bindir path ~p~nbin file paths ~p~n", [BinDirPath, BinFilePaths]),
    lists:foreach(fun(BinFilePath) -> 
			  ExecutableContainerPath = epkg_installed_paths:executable_container_path(InstallationPath),
			  InstalledBinFilePath    = ewl_file:join_paths(ExecutableContainerPath, filename:basename(BinFilePath)),
			  ok                      = remove_existing_executable_script(InstalledBinFilePath), 
			  file:copy(BinFilePath, InstalledBinFilePath),
			  epkg_util:set_executable_perms(InstalledBinFilePath)
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

%% 
%%--------------------------------------------------------------------
%% @private
%% @doc Handle different os's when writing the executable file.
%% @todo This needs Windows support. 
%% @end
%%--------------------------------------------------------------------
write_executable_file(ExecutableFile, InstallationPath, RelName, RelVsn, ErtsVsn, TemplateName) ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    ?INFO_MSG("SysArch was win32~n", []),
	    io:format("Win 32 is not yet supported.~n");
	SysArch ->
	    ?INFO_MSG("SysArch was ~p~n", [SysArch]),
	    Contents = lists:flatten(["#!/bin/sh\n",
                                      "PROG_PATH=`dirname $0`\n",
                                      "$PROG_PATH/faxien_launcher -t ", TemplateName, " -p ", InstallationPath, " -x ", RelName, " -v ", RelVsn, " -e ", 
				      ErtsVsn, " -- $@"]),
	    ?INFO_MSG("executable script contents ~p~n", [Contents]),
	    Res = file:write_file(ExecutableFile, Contents),
	    ?INFO_MSG("Writing executable file to ~s is ~p~n", [ExecutableFile, Res]),
	    epkg_util:set_executable_perms(ExecutableFile),
	    Res
    end.


