%%%-------------------------------------------------------------------
%%% @author Martin Logan 
%%% @doc Contains functions that help manage the packages installed on the local disk.
%%% 
%%% @end
%%% @copyright (C) 2007, Martin Logan, Eric Merritt, Erlware
%%% Created : 14 Dec 2007
%%%-------------------------------------------------------------------
-module(epkg_manage).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([
	 remove_app/4,
	 remove_all_apps/3,
	 remove_release/4,
	 remove_all_releases/3,
	 list_lib/2,
	 list_releases/1
	]).

%%--------------------------------------------------------------------
%% Include Files
%%--------------------------------------------------------------------
-include("epkg.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%%  Returns a list of all applications currently installed.
%% @spec list_lib(InstallationPath, ErtsVsn) -> [{Name, Vsn}]
%% @end
%%--------------------------------------------------------------------
list_lib(InstallationPath, ErtsVsn) ->
    LibDir       = epkg_installed_paths:application_container_path(InstallationPath, ErtsVsn),
    Paths        = filelib:wildcard(LibDir ++ "/*"),
    name_and_vsn(Paths).

%%--------------------------------------------------------------------
%% @doc 
%%  Returns a list of all releases currently installed.
%% @spec list_releases(InstallationPath) -> [{Name, Vsn}]
%% @end
%%--------------------------------------------------------------------
list_releases(InstallationPath) ->
    RelDir       = epkg_installed_paths:release_container_path(InstallationPath),
    Paths        = filelib:wildcard(RelDir ++ "/*"),
    name_and_vsn(Paths).

%%--------------------------------------------------------------------
%% @doc 
%%  Remove an installed application.
%% @spec remove_app(InstallationPath, ErtsVsn, AppName, AppVsn) -> ok
%%  where
%%   AppName = string()
%%   AppVsn = string()
%% @end
%%--------------------------------------------------------------------
remove_app(InstallationPath, ErtsVsn, AppName, AppVsn) ->
    AppPath = epkg_installed_paths:installed_app_dir_path(InstallationPath, ErtsVsn, AppName, AppVsn),
    ewl_file:delete_dir(AppPath).

%%--------------------------------------------------------------------
%% @doc 
%%  Remove all versions of an installed application.
%% @spec remove_all_apps(InstallationPath, ErtsVsn, AppName) -> ok
%%  where
%%   AppName = string()
%% @end
%%--------------------------------------------------------------------
remove_all_apps(InstallationPath, ErtsVsn, AppName) ->
    AppVsns = epkg_installed_paths:list_app_vsns(InstallationPath, ErtsVsn, AppName),
    lists:foreach(fun(AppVsn) -> remove_app(InstallationPath, ErtsVsn, AppName, AppVsn) end, AppVsns).
			  
%%--------------------------------------------------------------------
%% @doc 
%%  Remove an installed release. If force is set to false this function will be interactive.
%% @spec remove_release(InstallationPath, RelName, RelVsn, Force) -> ok 
%%  where
%%   RelName = string()
%%   RelVsn = string()
%%   Force = bool()
%% @end
%%--------------------------------------------------------------------
remove_release(InstallationPath, RelName, RelVsn, Force) ->
%% @todo executable files must be found to be non-unique when blasting an app as well
    UniqueSpecs = find_non_shared_app_specs(InstallationPath, RelName, RelVsn),
    case Force of
	true ->
	    %blast_executable_files(InstallationPath, RelName, RelVsn),
	    blast_app_and_release_packages(UniqueSpecs, InstallationPath, RelName, RelVsn);
	false ->
	    case question_removal(UniqueSpecs, RelName, RelVsn) of
		true ->
		    %blast_executable_files(InstallationPath, RelName, RelVsn),
		    blast_app_and_release_packages(UniqueSpecs, InstallationPath, RelName, RelVsn);
		false ->
		    ok
	    end
    end.

question_removal(UniqueSpecs, PackageName, PackageVsn) ->
    case ewl_talk:ask([lists:flatten(
			 io_lib:fwrite("To remove ~s-~s you must delete the following apps:~n~p?~n~n Please answer [yes|no]", 
				       [PackageName, PackageVsn, UniqueSpecs]))]) of
	Yes when Yes == $y; Yes == $Y; Yes == "yes" ->
	    true;
	No when No == $n; No == $N; No == "no" ->
	    false;
	Error ->
	    ?INFO_MSG("user entered \"~p\"~n", [Error]),
	    io:format("Please enter \"yes\" or \"no\"~n"),
	    question_removal(UniqueSpecs, PackageName, PackageVsn)
    end.
    

%%--------------------------------------------------------------------
%% @doc 
%%  Remove all versions of an installed release. If force is set to false this function will be interactive.
%% @spec remove_all_releases(InstallationPath, RelName, Force) -> ok
%%  where
%%   RelName = string()
%% @end
%%--------------------------------------------------------------------
remove_all_releases(InstallationPath, RelName, Force) ->
    RelVsns = epkg_installed_paths:list_release_vsns(InstallationPath, RelName),
    lists:foreach(fun(RelVsn) -> remove_release(InstallationPath, RelName, RelVsn, Force) end, RelVsns).

%%====================================================================
%% Internal functions
%%====================================================================
    
%%--------------------------------------------------------------------
%% @private
%% @doc return a list of {Name, Vsn} tuples for all supplied package paths.
%% @end
%%--------------------------------------------------------------------
name_and_vsn(Paths) ->
    lists:foldl(fun(Path, Acc) ->
			case epkg_installed_paths:package_dir_to_name_and_vsn(Path) of
			    {ok, {Name, Vsn}} -> [{Name, Vsn}|Acc];
			    _                 -> Acc
			end
		end, [], Paths).




%%--------------------------------------------------------------------
%% @private
%% @doc return a list of specs for apps from a particular release that are not shared with any other release.
%% @end
%%--------------------------------------------------------------------
find_non_shared_app_specs(InstallationPath, RelName, RelVsn) ->
    {ok, TargetSpecs} = fetch_app_specs(InstallationPath, RelName, RelVsn),
    RelVsns           = lists:delete(RelVsn, epkg_installed_paths:list_release_vsns(InstallationPath, RelName)),
    ReleaseNames      = epkg_installed_paths:list_releases(InstallationPath),
    ReleaseTuples = 
	[{RelName, RelVsns}|
	 lists:keydelete(RelName, 1, 
			 lists:map(fun(ReleaseName) ->
					   ReleaseVersions = epkg_installed_paths:list_release_vsns(InstallationPath, ReleaseName),
					   {ReleaseName, ReleaseVersions}
				   end, ReleaseNames))],
    FlatSpecs = fetch_flat_list_app_specs(InstallationPath, ReleaseTuples),
    TargetSpecs -- FlatSpecs.
    
fetch_flat_list_app_specs(InstallationPath, ReleaseTuples) ->
    lists:flatten(fetch_flat_list_app_specs2(InstallationPath, ReleaseTuples)).

fetch_flat_list_app_specs2(InstallationPath, [{RelName, RelVsns}|ReleaseTuples]) ->
    lists:map(fun(RelVsn) ->
		      case fetch_app_specs(InstallationPath, RelName, RelVsn) of
			  {ok, AppSpecs} -> AppSpecs;
			  _              -> []
		      end
	      end, RelVsns) ++ fetch_flat_list_app_specs2(InstallationPath, ReleaseTuples);
fetch_flat_list_app_specs2(_, []) ->
    [].

fetch_app_specs(InstallationPath, RelName, RelVsn) when is_atom(RelName) ->
    fetch_app_specs(InstallationPath, atom_to_list(RelName), RelVsn);
fetch_app_specs(InstallationPath, RelName, RelVsn) ->
    RelFilePath = epkg_installed_paths:installed_release_rel_file_path(InstallationPath, RelName, RelVsn),
    case catch epkg_util:consult_rel_file(app_specs, RelFilePath) of
	{'EXIT', Reason} -> {error, {"could not find app specs for the release specified", Reason}};
	AppSpecs         -> {ok, AppSpecs}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc get rid of all the executable files for a particular release.
%% @end
%%--------------------------------------------------------------------
last_executable_files(InstallationPath, RelName, RelVsn) ->
    CmdsDirPath           = epkg_installed_paths:installed_release_cmds_dir_path(InstallationPath, RelName, RelVsn),
    LauncherTemplateFiles = filelib:wildcard(CmdsDirPath ++ "/*"),
    BinDirPath            = epkg_installed_paths:executable_container_path(InstallationPath),
    lists:foreach(fun(FilePath) -> 
			  FileName    = filename:basename(FilePath),
			  BinFilePath = ewl_file:join_paths(BinDirPath, string:substr(FileName, 1, length(FileName) - 5)),
			  ?INFO_MSG("deleting ~s~n", [BinFilePath]),
			  file:delete(BinFilePath) 
		  end, 
		  LauncherTemplateFiles).

%%--------------------------------------------------------------------
%% @private
%% @doc get rid of all the applications for a release as well as the release itself.
%% @end
%%--------------------------------------------------------------------
blast_app_and_release_packages(UniqueSpecs, InstallationPath, RelName, RelVsn) ->
    RelFilePath = epkg_installed_paths:installed_release_rel_file_path(InstallationPath, RelName, RelVsn),
    case catch epkg_util:consult_rel_file(erts_vsn, RelFilePath) of
	{'EXIT', Reason} -> 
	    {error, {"could not find erts for the release specified", Reason}};
	ErtsVsn -> 
	    lists:foreach(fun({AppName, AppVsn}) -> 
				  remove_app(InstallationPath, ErtsVsn, atom_to_list(AppName), AppVsn) 
			  end, UniqueSpecs),
	    RelPath = epkg_installed_paths:installed_release_dir_path(InstallationPath, RelName, RelVsn),
	    ewl_file:delete_dir(RelPath)
    end.
    
