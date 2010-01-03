%%%-------------------------------------------------------------------
%%% @author Martin Logan 
%%% @doc Contains functions that help manage the packages installed on the local disk.
%%% 
%%% @type target_erts_vsns() = [TargetErtsVsn] | TargetErtsVsn
%%%  where
%%%   TargetErtsVsn = string()
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
	 diff_config/3,
	 list_lib/2,
	 list_releases/2,
	 list_all_releases/1,
	 find_highest_local_release_vsn/2,
	 find_highest_local_release_vsn/1,
	 find_highest_local_app_vsn/2,
	 find_highest_local_app_vsn/1
	]).

%%--------------------------------------------------------------------
%% Include Files
%%--------------------------------------------------------------------
-include("epkg.hrl").
-include("macros.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%%  Returns a list of all applications currently installed.
%% @spec list_lib(InstallationPath, TargetErtsVsns) -> [{Name, Vsn}]
%% where
%%  TargetErtsVsns = [TargetErtsVsn] | TargetErtsVsn
%%   TargetErtsVsn = string()
%% @end
%%--------------------------------------------------------------------
list_lib(InstallationPath, [A|_] = TargetErtsVsn) when is_integer(A) ->
    list_lib(InstallationPath, [TargetErtsVsn]);
list_lib(InstallationPath, TargetErtsVsns) ->
    ?INFO_MSG("listing lib dirs for erts vsns ~p~n", [TargetErtsVsns]),
    lists:reverse(
      ordsets:to_list(
	ordsets:from_list(
	  epkg_util:remove_tuple_dups(
	    2, 
	    lists:sort(
	      fun({N, V}, {N, V1}) -> ewr_util:is_version_greater(V, V1);
		 ({N, _}, {N1, _}) -> N > N1 end,
	      lists:flatten([
			     lists:map(
			       fun(ErtsVsn) -> list_lib_for_erts_vsn(InstallationPath, ErtsVsn) end,
			       TargetErtsVsns)
			    ])))))).

list_lib_for_erts_vsn(InstallationPath, ErtsVsn) ->
    LibDir       = ewl_installed_paths:application_container_path(InstallationPath, ErtsVsn),
    Paths        = filelib:wildcard(LibDir ++ "/*"),
    name_and_vsn(Paths).

%%--------------------------------------------------------------------
%% @doc 
%%  Returns a list of all releases currently installed.
%% @spec list_all_releases(InstallationPath) -> [{Name, Vsn}]
%% where
%%  TargetErtsVsns = [TargetErtsVsn] | TargetErtsVsns
%%  TargetErtsVsn = string()
%% @end
%%--------------------------------------------------------------------
list_all_releases(InstallationPath) ->
    Series = epkg_installed_paths:list_erts_vsns(InstallationPath),
    ?INFO_MSG("listing dirs for erts vsns ~p~n", [Series]),
    lists:sort(fun({N, _}, {N1, _}) -> N > N1 end,
	       lists:flatten([lists:map(fun(ErtsVsn) -> list_releases_for_erts_vsn(InstallationPath, ErtsVsn) end, Series)])).

%%--------------------------------------------------------------------
%% @doc 
%%  Returns a list of all releases currently installed.
%% @spec list_releases(InstallationPath, TargetErtsVsns) -> [{Name, Vsn}]
%% where
%%  TargetErtsVsns = [TargetErtsVsn] | TargetErtsVsns
%%  TargetErtsVsn = string()
%% @end
%%--------------------------------------------------------------------
list_releases(InstallationPath, [A|_] = TargetErtsVsn) when is_integer(A) ->
    list_releases(InstallationPath, [TargetErtsVsn]);
list_releases(InstallationPath, TargetErtsVsns) ->
    ?INFO_MSG("listing lib dirs for erts vsns ~p~n", [TargetErtsVsns]),

    ReleaseList = lists:flatten([
				 lists:map(
				   fun(ErtsVsn) -> list_releases_for_erts_vsn(InstallationPath, ErtsVsn) end,
				   TargetErtsVsns)
				]),

    ?INFO_MSG("release list is ~p~n", [ReleaseList]),

    SortedList = lists:sort(
		   fun({N, V}, {N, V1}) -> ewr_util:is_version_greater(V, V1);
		      ({N, _}, {N1, _}) -> N > N1 end,
		   ReleaseList),

    ?INFO_MSG("sorted list is ~p~n", [SortedList]),

    lists:reverse(ordsets:to_list(ordsets:from_list(epkg_util:remove_tuple_dups(2, SortedList)))).

list_releases_for_erts_vsn(InstallationPath, ErtsVsn) ->
    RelDir = ewl_installed_paths:release_container_path(InstallationPath),
    Paths  = filelib:wildcard(RelDir ++ "/*"),
    lists:filter(fun({RelName, RelVsn}) ->
			 try
			     RelFilePath = ewl_installed_paths:installed_release_rel_file_path(InstallationPath,
											       RelName, RelVsn),
			     ErtsVsn_    = epkg_util:consult_rel_file(erts_vsn, RelFilePath),
			     ErtsVsn_ == ErtsVsn
			 catch
			     _C:_E ->
				 false
			 end;
		    (NoNameVersionPattern) ->
			 ?ERROR_MSG("non standard erlware release naming ~p~n", [NoNameVersionPattern]),
			 false
		 end,
		 name_and_vsn(Paths)).


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
    AppPath = ewl_installed_paths:installed_app_dir_path(InstallationPath, ErtsVsn, AppName, AppVsn),
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
		    io:format("removing ~s-~s~n", [RelName, RelVsn]),
		    blast_app_and_release_packages(UniqueSpecs, InstallationPath, RelName, RelVsn);
		false ->
		    ok
	    end
    end.

question_removal([], _PackageName, _PackageVsn) ->
    true;
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

%%--------------------------------------------------------------------
%% @doc Find the highest version of a particular release for a particular erts vsn that is installed locally.
%% @spec find_highest_local_release_vsn(ReleaseName, TargetErtsVsn) -> HighestVsn
%% where
%%  TargetErtsVsns = target_erts_vsns()
%% @end
%%--------------------------------------------------------------------
find_highest_local_release_vsn(ReleaseName, [_H|_] = TargetErtsVsn) when is_integer(_H) ->
    find_highest_local_release_vsn(ReleaseName, [TargetErtsVsn]); 
find_highest_local_release_vsn(ReleaseName, TargetErtsVsns) when is_atom(ReleaseName) ->
    find_highest_local_release_vsn(atom_to_list(ReleaseName), TargetErtsVsns);
find_highest_local_release_vsn(ReleaseName, TargetErtsVsns) ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    NameAndVsns =
	lists:foldl(
	  fun(TargetErtsVsn, Acc) ->
		  lists:filter(fun({Name, _}) ->
				       ReleaseName == Name
			       end,
			       list_releases(InstallationPath, TargetErtsVsn)) ++ Acc
	  end,
	  [],
	  TargetErtsVsns),
    epkg_util:highest_vsn([Vsn || {_Name, Vsn} <- NameAndVsns]).

%%--------------------------------------------------------------------
%% @doc Find the highest version of a particular release that is installed locally.
%% @spec find_highest_local_release_vsn(ReleaseName) -> HighestVsn
%% @end
%%--------------------------------------------------------------------
find_highest_local_release_vsn(ReleaseName) when is_atom(ReleaseName) ->
    find_highest_local_release_vsn(atom_to_list(ReleaseName));
find_highest_local_release_vsn(ReleaseName) ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    NameAndVsns = lists:filter(fun({Name, _}) -> ReleaseName == Name end,
			      list_all_releases(InstallationPath)),
    epkg_util:highest_vsn([Vsn || {_Name, Vsn} <- NameAndVsns]).

%%--------------------------------------------------------------------
%% @doc Find the highest version of a particular application that is installed locally.
%% @spec find_highest_local_app_vsn(AppName) -> HighestVsn::string()
%% @end
%%--------------------------------------------------------------------
find_highest_local_app_vsn(AppName) ->
    ErtsVsns = [ErtsVsn || {_, ErtsVsn, _} <- ?COMPILER_VSN_TO_ERTS_VSN_TO_ERLANG_VSN],
    epkg_util:highest_vsn([find_highest_local_app_vsn(AppName, TargetErtsVsn) || TargetErtsVsn <- ErtsVsns]).
			
%%--------------------------------------------------------------------
%% @doc Find the highest version of a particular application that is installed locally.
%% @spec find_highest_local_app_vsn(AppName, TargetErtsVsn) -> HighestVsn
%% @end
%%--------------------------------------------------------------------
find_highest_local_app_vsn(AppName, TargetErtsVsn) when is_atom(AppName) ->
    find_highest_local_app_vsn(atom_to_list(AppName), TargetErtsVsn);
find_highest_local_app_vsn(AppName, TargetErtsVsn) ->
    {ok, InstallationPath} = epkg_installed_paths:get_installation_path(),
    NameAndVsns = lists:filter(fun({Name, _}) -> AppName == Name end,
			      list_lib(InstallationPath, TargetErtsVsn)),
    epkg_util:highest_vsn([Vsn || {_Name, Vsn} <- NameAndVsns]).


%%--------------------------------------------------------------------
%% @doc Diff all the config files shared by two releases.
%% @spec diff_config(RelName, RelVsn1, RelVsn2) -> Diffs
%% where
%%  Diffs = [{ConfigFilePath1, ConfigFilePath2, DiffTerms}]
%%   DiffTerms = [term()]
%% @end
%%--------------------------------------------------------------------
diff_config(RelName, RelVsn1, RelVsn2) -> 
    try
	Rel1ConfigFilePaths = fs_lists:make_list_if_not(epkg_installed_paths:find_config_file_path(RelName, RelVsn1)),
	Rel2ConfigFilePaths = fs_lists:make_list_if_not(epkg_installed_paths:find_config_file_path(RelName, RelVsn2)),
	lists:foldl(fun(Rel1Path, Acc) ->
			    case basename_member(Rel1Path, Rel2ConfigFilePaths) of
				false ->
				    Acc;
				Rel2Path ->
				    case ewl_config_diff:config_files(Rel1Path, Rel2Path) of
					[]   -> Acc;
					Diff -> [{Rel1Path, Rel2Path, Diff}|Acc]
				    end
			    end
		    end,
		    [],
		    Rel1ConfigFilePaths)
    catch
	_Type:Ex ->
	    ?ERROR_MSG("error on config diff ~p~n", [Ex]),
	    []
    end.
    
basename_member(Target, [Path|T]) ->
    Basename = filename:basename(Path),
    case filename:basename(Target) of
	Basename -> Path;
	_        -> basename_member(Target, T)
    end;
basename_member(_Target, []) ->
    false.
		   
	
    
    
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
    ReleaseNames  = epkg_installed_paths:list_releases(InstallationPath),
    ReleaseTuples = lists:map(fun(ReleaseName) when ReleaseName == RelName ->
				      ReleaseVersions = epkg_installed_paths:list_release_vsns(InstallationPath, ReleaseName),
				      {ReleaseName, lists:delete(RelVsn, ReleaseVersions)};
				 (ReleaseName) ->
				      ReleaseVersions = epkg_installed_paths:list_release_vsns(InstallationPath, ReleaseName),
				      {ReleaseName, ReleaseVersions}
			      end, ReleaseNames),
    FlatSpecs = fetch_flat_list_app_specs(InstallationPath, ReleaseTuples),
    ?INFO_MSG("flat app ~p target ~p~n", [FlatSpecs, TargetSpecs]),
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
    RelFilePath = ewl_installed_paths:installed_release_rel_file_path(InstallationPath, RelName, RelVsn),
    case catch epkg_util:consult_rel_file(app_specs, RelFilePath) of
	{'EXIT', Reason} -> {error, {"could not find app specs for the release specified", Reason}};
	AppSpecs         -> {ok, [{element(1, AS), element(2, AS)} || AS <- AppSpecs]}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc get rid of all the executable files for a particular release.
%% @end
%%--------------------------------------------------------------------
blast_executable_files(InstallationPath, RelName, RelVsn) ->
    CmdsDirPath           = ewl_installed_paths:installed_release_cmds_dir_path(InstallationPath, RelName, RelVsn),
    LauncherTemplateFiles = filelib:wildcard(CmdsDirPath ++ "/*"),
    BinDirPath            = ewl_installed_paths:executable_container_path(InstallationPath),
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
    RelFilePath = ewl_installed_paths:installed_release_rel_file_path(InstallationPath, RelName, RelVsn),
    case catch epkg_util:consult_rel_file(erts_vsn, RelFilePath) of
	{'EXIT', Reason} -> 
	    {error, {"could not find erts for the release specified", Reason}};
	ErtsVsn -> 
	    lists:foreach(fun(AppSpec) -> 
				  AppName = element(1, AppSpec),
				  AppVsn  = element(2, AppSpec),
				  remove_app(InstallationPath, ErtsVsn, atom_to_list(AppName), AppVsn) 
			  end, UniqueSpecs),
	    RelPath = ewl_installed_paths:installed_release_dir_path(InstallationPath, RelName, RelVsn),
	    ewl_file:delete_dir(RelPath)
    end.
    
