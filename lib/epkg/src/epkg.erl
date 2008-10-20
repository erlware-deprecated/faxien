%%%-------------------------------------------------------------------
%%% @author Martin Logan 
%%% @doc The interface module for all user level epkg functions. Application programmers may want to use the lower level 
%%%      libraries in order to avoid output to stdout and other such UI concerns. 
%%% 
%%% @end
%%% @copyright (C) 2007, Martin Logan, Eric Merritt, Erlware
%%%-------------------------------------------------------------------
-module(epkg).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([
	 cmdln_apply/1,
	 install_release/0,
	 install_release/1,
	 install_release/2,
	 install_erts/1,
	 install_erts/2,
	 install_app/1,
	 install_app/2,
	 install_app/3,
	 install/1,
	 install/2,
	 list/0,
	 list_lib/0,
	 list_releases/0,
	 diff_config/3,
	 config_file_path/2,
	 config_file_path/1,
	 remove_all_apps/1,
	 remove_app/2,
	 remove_all/1,
	 remove/2,
	 version/0,
	 help/0,
	 help/1
	]).

-export([
	 install_release_help/0,
	 install_app_help/0,
	 install_help/0,
	 install_erts_help/0,
	 list_help/0,
	 remove_app_help/0,
	 remove_all_apps_help/0,
	 remove_help/0,
	 remove_all_help/0,
	 diff_config_help/0,
	 config_file_path_help/0,
	 examples_help/0,
	 commands_help/0
	]).

-include("macros.hrl").
-include("epkg.hrl").
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc apply functions from the commandline. 
%% @spec (MFA) -> void()
%% @end
%%--------------------------------------------------------------------
cmdln_apply([_Mod]) ->
    help();
cmdln_apply([Mod, RawFunc|Args]) ->
    ?INFO_MSG("mod:func ~p:~p with raw args from commandline: ~w~n", [Mod, RawFunc, Args]),
    Func = list_to_atom(
	     epkg_cmdln:resolve_alias(
	       epkg_cmdln:translate_dash_to_underscore(epkg_cmdln:resolve_alias(atom_to_list(RawFunc), ?EPKG_ALIAS_LIST)),
	       ?EPKG_ALIAS_LIST)),
    epkg_cmdln:cmdln_apply([Mod, Func|Args]).

%%--------------------------------------------------------------------
%% @doc 
%%  Determine the type of a package and then install it appropriately. You must specify the erts vsn the package was created
%%  with or for. 
%% @spec install(RelPackagePath, InstallationPath) -> ok | {ok, ErtsVsn} | {error, Reason}
%% where
%%  ErtsVsn = string()
%%  Reason = badly_formatted_or_missing_package | {failed_to_install, [{AppName, AppVsn}]}
%% @end
%%--------------------------------------------------------------------
install(PackageDirOrArchive, RawInstallationPath) -> 
    InstallationPath = epkg_util:if_atom_or_integer_to_string(RawInstallationPath),
    PackageDirPath   = epkg_util:unpack_to_tmp_if_archive(PackageDirOrArchive), 
    ?INFO_MSG("with ~p ~p ~p~n", [PackageDirOrArchive, InstallationPath]),
    case epkg_validation:validate_type(PackageDirPath) of
	{ok, binary}  -> epkg_install:install_application(PackageDirPath, InstallationPath);
	{ok, generic} -> epkg_install:install_application(PackageDirPath, InstallationPath);
	{ok, release} -> epkg_install:install_release(PackageDirPath, InstallationPath, false);
	{ok, erts}    -> epkg_install:install_erts(PackageDirPath, InstallationPath);
	Error   -> Error
    end.

%% @spec install(RelPackagePath) -> ok | {error, Reason}
%% @equiv install(RelPackagePath, InstallationPath) 
install(PackageDirOrArchive) -> 
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    install(PackageDirOrArchive, InstallationPath).

%% @private
install_help() ->
    ["\nHelp for install\n",
     "Usage: install <package_path> [installation-path]: Install a release from a local package\n"]. 

%%--------------------------------------------------------------------
%% @doc 
%%  Install a release package or package archive.
%% @spec install_release(RelPackagePath, InstallationPath) -> ok | {error, Reason}
%% where
%%  Reason = badly_formatted_or_missing_release_package | {failed_to_install, [{AppName, AppVsn}]}
%% @end
%%--------------------------------------------------------------------
install_release(RelPackagePath, InstallationPath) -> 
    ?INFO_MSG("~p into ~p~n", [RelPackagePath, InstallationPath]),
    epkg_install:install_release(RelPackagePath, epkg_util:if_atom_or_integer_to_string(InstallationPath), false).

%% @spec install_release(RelPackagePath) -> ok | {error, Reason}
%% @equiv install_release(RelPackagePath, InstallationPath) 
install_release(RelPackagePath) -> 
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    install_release(RelPackagePath, InstallationPath).

%%--------------------------------------------------------------------
%% @doc Install release with no arguments from within a Sinan project
%%      will publish the latest dist tarball it finds within the Sinan project.
%%
%% @spec install_release() -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
install_release() ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    {ok, CWD} = file:get_cwd(),
    epkg_install:install_sinan_release(CWD, InstallationPath, false).

%% @private
install_release_help() ->
    ["\nHelp for install-release\n",
     "Usage: install-release [package_path] [installation-path]: Install an release from a local package",
    "If no arguments are supplied and this is run from within a sinan project this will publish the",
    "latest release tarball found within the project~n"]. 

%%--------------------------------------------------------------------
%% @doc 
%%  Install an application package or package archive. Including the erts vsn means that this should be installed for use
%%  with erts vsn X no matter the erts vsn this was compiled with. 
%% @spec install_app(AppPackagePath, ErtsVsn, InstallationPath) -> {ok, ErtsVsn} | {error, Reason}
%% where
%%  Reason = badly_formatted_or_missing_app_package
%% @end
%%--------------------------------------------------------------------
install_app(AppPackagePath, ErtsVsn, InstallationPath) -> 
    epkg_install:install_application(AppPackagePath, ErtsVsn, epkg_util:if_atom_or_integer_to_string(InstallationPath)).

%%--------------------------------------------------------------------
%% @doc 
%%  Install an application package or package archive. The erts version will be determined automatically.
%% @spec install_app(AppPackagePath, InstallationPath) -> {ok, ErtsVsn} | {error, Reason}
%% where
%%  Reason = badly_formatted_or_missing_app_package
%% @end
%%--------------------------------------------------------------------
install_app(AppPackagePath, InstallationPath) -> 
    epkg_install:install_application(AppPackagePath, epkg_util:if_atom_or_integer_to_string(InstallationPath)).

%% @spec install_app(AppPackagePath) -> {ok, ErtsVsn} | {error, Reason}
%% @equiv install_app(AppPackagePath, InstallationPath) 
install_app(AppPackagePath) -> 
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    install_app(AppPackagePath, InstallationPath). 

%% @private
install_app_help() ->
    ["\nHelp for install-app\n",
     "Usage: install-app <package_path> [installation-path]: Install an application from a local package\n"]. 

%%--------------------------------------------------------------------
%% @doc 
%%  Install an erts package or archive. 
%% @spec install_erts(ErtsPackagePath, InstallationPath) -> ok | {error, Reason}
%% where
%%  Reason = badly_formatted_or_missing_erts_package | {failed_to_install, [{AppName, AppVsn}]}
%% @end
%%--------------------------------------------------------------------
install_erts(ErtsPackagePath, InstallationPath) -> 
    epkg_install:install_erts(ErtsPackagePath, epkg_util:if_atom_or_integer_to_string(InstallationPath)).

%% @spec install_erts(ErtsPackagePath) -> ok | {error, Reason}
%% @equiv install_erts(ErtsPackagePath, InstallationPath) 
install_erts(ErtsPackagePath) -> 
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    install_erts(ErtsPackagePath, InstallationPath).
%% @private
install_erts_help() ->
    ["\nHelp for install-erts\n",
     "Usage: install-erts <package_path> [installation-path]: Install an erts from a local package\n"]. 

%%--------------------------------------------------------------------
%% @doc 
%%  Returns a list of all OTP packages currently installed.
%% @spec list() -> string()
%% @end
%%--------------------------------------------------------------------
list() ->
    list_lib(),
    list_releases().

%% @private
list_help() ->
    ["\nHelp for list\n",
     "Usage: list: list all installed packages\n"]. 

%%--------------------------------------------------------------------
%% @doc 
%%  Returns a list of all applications currently installed.
%% @spec list_lib() -> string()
%% @end
%%--------------------------------------------------------------------
list_lib() ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    {ok, TargetErtsVsn}    = gas:get_env(epkg, target_erts_vsn, ewr_util:erts_version()),
    Series                 = epkg_util:all_erts_vsns(TargetErtsVsn),
    NameVsnsPairs          = collect_dups(epkg_manage:list_lib(InstallationPath, Series)),
    case Series of
	[TargetErtsVsn, HighErtsVsn|_] ->
	    io:format("~nInstalled Applications for ERTS versions between ~s and ~s:~n", [TargetErtsVsn, HighErtsVsn]);
	_ ->
	    io:format("~nInstalled Applications:~n")
    end,
    print_installed(NameVsnsPairs).

%%--------------------------------------------------------------------
%% @doc 
%%  Returns a list of all releases currently installed.
%% @spec list_releases() -> string()
%% @end
%%--------------------------------------------------------------------
list_releases() ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    {ok, TargetErtsVsn}    = gas:get_env(epkg, target_erts_vsn, ewr_util:erts_version()),
    Series                 = epkg_util:all_erts_vsns(TargetErtsVsn),
    NameVsnsPairs          = collect_dups(epkg_manage:list_releases(InstallationPath, Series)),
    case Series of
	[TargetErtsVsn, HighErtsVsn|_] ->
	    io:format("~nInstalled Releases (Erlang standalone services) for ERTS versions between ~s and ~s:~n",
		      [TargetErtsVsn, HighErtsVsn]);
	_ ->
	    io:format("~nInstalled Releases (Erlang standalone services):~n")
    end,
    print_installed(NameVsnsPairs).

%%--------------------------------------------------------------------
%% @doc 
%%  Remove an installed application.
%% @spec remove_app(AppName, AppVsn) -> ok
%%  where
%%   AppName = string()
%%   AppVsn = string()
%% @end
%%--------------------------------------------------------------------
remove_app(AppName, AppVsn) ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    {ok, TargetErtsVsn}    = gas:get_env(epkg, target_erts_vsn, ewr_util:erts_version()),
    [A, B]                 = epkg_util:if_atom_or_integer_to_string([AppName, AppVsn]),
    epkg_manage:remove_app(InstallationPath, TargetErtsVsn, A, B).

%% @private
remove_app_help() ->
    ["\nHelp for remove_app\n",
     "Usage: remove-app <app name> <app version>: remove a particular for a particular version.\n",
     "Example: remove-app tools 2.4.5 - removes version 2.4.5 of the tools application."].

%%--------------------------------------------------------------------
%% @doc 
%%  Remove all versions of an installed application.
%% @spec remove_all_apps(AppName) -> ok
%%  where
%%   AppName = string()
%% @end
%%--------------------------------------------------------------------
remove_all_apps(AppName) ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    {ok, TargetErtsVsn}    = gas:get_env(epkg, target_erts_vsn, ewr_util:erts_version()),
    epkg_manage:remove_all_apps(InstallationPath, TargetErtsVsn, epkg_util:if_atom_or_integer_to_string(AppName)).
			  
%% @private
remove_all_apps_help() ->
    ["\nHelp for remove_all_apps\n",
     "Usage: remove-all-app <app name>: remove a particular application for all versions installed.\n",
     "Example: remove-all-app tools - removes all versions of the tools application currently installed."].
   

%%--------------------------------------------------------------------
%% @doc 
%%  Remove an installed release.
%% @spec remove(RelName, RelVsn) -> ok 
%%  where
%%   RelName = string()
%%   RelVsn = string()
%% @end
%%--------------------------------------------------------------------
remove(RelName, RelVsn) ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    [A, B]                 = epkg_util:if_atom_or_integer_to_string([RelName, RelVsn]),
    epkg_manage:remove_release(InstallationPath, A, B, false).
    
%% @private
remove_help() ->
    ["\nHelp for remove\n",
     "Usage: remove <release-name> <release-version>: remove a particular for a particular version.\n",
     "Example: remove faxien 0.8.6 - removes version 0.8.6 of the sinan release."].

%%--------------------------------------------------------------------
%% @doc 
%%  Remove all versions of an installed release.
%% @spec remove_all(RelName) -> ok
%%  where
%%   RelName = string()
%% @end
%%--------------------------------------------------------------------
remove_all(RelName) ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    epkg_manage:remove_all_releases(InstallationPath, epkg_util:if_atom_or_integer_to_string(RelName), false).

%% @private
remove_all_help() ->
    ["\nHelp for remove_all\n",
     "Usage: remove-all <release-name>: remove a particular rlease for all versions installed.\n",
     "Example: remove-all sinan - removes all versions of the sinan release that are currently installed."].

%%--------------------------------------------------------------------
%% @doc Diff all the config files shared by two releases. 
%% @spec diff_config(RelName, RelVsn1, RelVsn2) -> {ok, Diffs}
%% where
%%  Diffs = [{ConfigFilePath1, ConfigFilePath2, DiffTerms}]
%% @end
%%--------------------------------------------------------------------
diff_config(RelName, RelVsn1, RelVsn2) -> 
    {ok, epkg_manage:diff_config(RelName, RelVsn1, RelVsn2)}.

%% @private
diff_config_help() ->
    ["\nHelp for diff-config\n",
     "Usage: diff-config <release-name> <rel-vsn1> <rel-vsn2>: diff config files for two versions of a release\n",
     "Example: diff-config sinan 0.8.8 0.8.10 - Diff config file for installed versions of sinan 0.8.8 and 0.8.10."].

%%--------------------------------------------------------------------
%% @doc Returns the path to a release config file for a particular version.
%% @spec config_file_path(RelName, RelVsn) -> string()
%% @end
%%--------------------------------------------------------------------
config_file_path(RelName, RelVsn) ->
    {ok, epkg_installed_paths:find_config_file_path(RelName, RelVsn)}.

%%--------------------------------------------------------------------
%% @doc Returns the path to a release config file for the highest version of the release found for the current erts vsn.
%% @spec config_file_path(RelName) -> string()
%% @end
%%--------------------------------------------------------------------
config_file_path(RelName) ->
    config_file_path(RelName, epkg_manage:find_highest_local_release_vsn(RelName)).

%% @private
config_file_path_help() ->
    ["\nHelp for config-file-path\n",
     "Usage: config-file-path <release-name> [rel-vsn]: return the path to a release config file for a particular version or the highest version if no version is specified\n",
     "Example: config-file-path sinan 0.8.8",
     "Example: config-file-path sinan"].
    
%%--------------------------------------------------------------------
%% @doc 
%%  Return the version of the current Epkg release.
%% @spec version() -> string()
%% @end
%%--------------------------------------------------------------------
version() -> 
    {value, {epkg, _, Vsn}} = lists:keysearch(epkg, 1, application:which_applications()),
    {ok, Vsn}.

%%--------------------------------------------------------------------
%% @doc 
%%  Print the help screen
%% @spec help() -> ok
%% @end
%%--------------------------------------------------------------------
help() -> 
    print_help_list(
      [
       "\nEpkg is a powerful package manager and the backbone of Faxien. This message is the gateway into further Epkg help.",
       "\nUsage:",
       "epkg help",
       "epkg version",
       "epkg <command> [options|arguments...]",
       "\nMore Help:",
       "epkg help commands: Lists all epkg commands",
       "epkg help <command>: Gives help on an individual command",
       "epkg help examples: Lists example usages of epkg",
       "\nShort Examples:",
       "epkg install sinan",
       "epkg list",
       "epkg help install"
      ]).  

%% @private
examples_help() ->
    [
     "\nExamples:",
     "\nInstall the tools application from the local filesystem", 
     "  epkg install /usr/local/erlang/lib/tools-2.5.4 5.5.5",
     "\nInstall a new version of epkg from a release tarball", 
     "  epkg install epkg-0.19.3.epkg 5.5.5"
    ].

%% @private
commands_help() ->
    [
     "\nCommands:",
     "help                    print help information.",
     "list                    list the packages installed on the local system.",
     "install                 install a package.",
     "remove-app              uninstall a particular version of an application",
     "                        package.",
     "remove-all-apps         uninstall all versions of an application package.",
     "remove                  uninstall a particular version of a release package.",
     "remove-all              uninstall all versions of a release package.",
     "version                 display the current Faxien version installed",
     "                        on the local system.",
     "config-file-path        display the path to a release config file."
    ].

%%--------------------------------------------------------------------
%% @doc 
%%  Print the help screen for a specific command.
%% @spec help(Command::atom()) -> ok
%% @end
%%--------------------------------------------------------------------
help(Command) when is_atom(Command) ->
    help_for_command(atom_to_list(Command)).

%%====================================================================
%% Internal functions
%%====================================================================

help_for_command(RawCommand) ->
    Command = epkg_cmdln:resolve_alias(
		epkg_cmdln:translate_dash_to_underscore(
		  epkg_cmdln:resolve_alias(RawCommand, ?EPKG_ALIAS_LIST)), ?EPKG_ALIAS_LIST),
    Func             = list_to_atom(Command ++ "_help"),
    case catch ?MODULE:Func() of
	{'EXIT', _Reason} ->
	    io:format("The command ~s does not have detailed help associated with it~n", [Command]);
	HelpList -> 
	    print_help_list(HelpList) 
    end.

print_help_list(HelpList) ->	   
    lists:foreach(fun(HelpString) -> io:format("~s~n", [HelpString]) end, HelpList).
    

%%--------------------------------------------------------------------
%% @private
%% @doc used to format the output of the list functions
%% @end
%%--------------------------------------------------------------------
print_installed(NameVsnsPairs) ->
    L1 = [{Name,format_vsns(lists:reverse(Vsns))} || {Name, Vsns} <- NameVsnsPairs],
    Col = lists:foldr(fun ({A,_B},Max) when length(A)>Max ->
			      length(A);
			  (_, Max) ->
			      Max
		      end, 0, L1),
    Fmt = lists:flatten(io_lib:format("~s-~ps~s~n",["~",Col+3,"~s"])),
    lists:foreach(fun(T) ->
			  io:format(Fmt, tuple_to_list(T))
		  end, L1).

format_vsns(Vsns) when length(Vsns) > 5 ->
    SortedVsns = lists:sort(fun(V1, V2) -> ewr_util:is_version_greater(V1, V2) end, Vsns),
    lists:flatten([ewr_util:join(lists:reverse(lists:nthtail(length(Vsns) - 5, lists:reverse(SortedVsns))), " | "), " | ..."]);
format_vsns(Vsns) ->
    SortedVsns = lists:sort(fun(V1, V2) -> ewr_util:is_version_greater(V1, V2) end, Vsns),
    ewr_util:join(SortedVsns, " | ").

collect_dups([]) -> 
    [];
collect_dups([{Name, Vsn}|NameAndVsnPairs]) -> 
    collect_dups(NameAndVsnPairs, [{Name, [Vsn]}]).

collect_dups([{Name, Vsn}|T], [{Name, Vsns}|Acc]) ->
    collect_dups(T, [{Name, [Vsn|Vsns]}|Acc]);
collect_dups([{Name, Vsn}|T], Acc) ->
    collect_dups(T, [{Name, [Vsn]}|Acc]);
collect_dups([], Acc) ->
    Acc.
    
