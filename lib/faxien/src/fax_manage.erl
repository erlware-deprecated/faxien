%%%-----------------------------------------------------------------
%%% @doc Handles fetching packages from the remote repository and 
%%%      placing them in the erlware repo.
%%% 
%%% @type repo() = string(). Contains address and repo designation. Example: http://www.erlware.org/stable.
%%% @type force() = bool(). Indicates whether an existing app is to be overwritten with or without user conscent.  
%%% @type erts_prompt() = bool(). indicate whether or not to prompt upon finding a package outside of the target erts vsn.
%%% @type options() = [Option]
%%% where
%%%  Options = {force, force()} | {erts_prompt, erts_prompt()}
%%% @type target_erts_vsns() = [TargetErtsVsn] | TargetErtsVsn
%%%  where
%%%   TargetErtsVsn = string()
%%%
%%%
%%% @type timeout() = integer() | infinity. Timeouts are specified in milliseconds.
%%%
%%% @copyright 2007 Erlware
%%% @author Martin Logan
%%% @end
%%%-------------------------------------------------------------------
-module(fax_manage).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%-include("eunit.hrl").
-include("faxien.hrl").


%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 outdated_applications/3,
	 upgrade_applications/4,
	 upgrade_application/5,
	 outdated_releases/3,
	 upgrade_releases/5,
	 upgrade_release/6,
	 get_signature/1,
	 add_repo_to_publish_to/2,
	 remove_repo_to_publish_to/2,
	 add_repo_to_fetch_from/2,
	 remove_repo_to_fetch_from/2,
	 set_request_timeout/2,
	 set_preferred_erts_vsn/2,
	 search/5,
	 describe_release/5,
	 describe_latest_release/4,
	 describe_app/5,
	 describe_latest_app/4
	]).

-export([raw_list/3]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%%  Fetch the description for the latest version of a particular release from a remote repository.
%% @spec describe_latest_release(Repos, TargetErtsVsns, RelName, Timeout) -> ok | {error, Reason} | exit()
%%  where
%%   Repos = list()
%%   TargetErtsVsns = target_erts_vsns()
%%   RelName = string()
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%--------------------------------------------------------------------
describe_latest_release(Repos, [_H|_] = TargetErtsVsn, RelName, Timeout) when is_integer(_H) ->
    describe_latest_release(Repos, [TargetErtsVsn], RelName, Timeout);
describe_latest_release(Repos, TargetErtsVsns, RelName, Timeout) ->
    Fun = fun(Repo, RelVsn, ErtsVsn) ->
		  describe_release([Repo], ErtsVsn, RelName, RelVsn, Timeout)
	  end,
    fax_util:execute_on_latest_package_version(Repos, TargetErtsVsns, RelName, Fun, lib, false). 

%%--------------------------------------------------------------------
%% @doc 
%%  Fetch the description for a particular release from a remote repository.
%% @spec describe_release(Repos, TargetErtsVsns, RelName, RelVsn, Timeout) -> ok | {error, Reason} | exit()
%%  where
%%   Repos = list()
%%   TargetErtsVsns = target_erts_vsns()
%%   RelName = string()
%%   RelVsn = string()
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%--------------------------------------------------------------------
describe_release(Repos, [_H|_] = TargetErtsVsn, RelName, RelVsn, Timeout) when is_integer(_H) ->
    describe_release(Repos, [TargetErtsVsn], RelName, RelVsn, Timeout);
describe_release(Repos, TargetErtsVsns, RelName, RelVsn, Timeout) ->
    Fun = fun(ErtsVsn) -> 
		  ControlSuffix = ewr_repo_paths:release_control_file_suffix(ErtsVsn, RelName, RelVsn),
		  fs_lists:do_until(
		    fun(Repo) ->
			    case ewr_util:repo_consult(Repo, ControlSuffix, Timeout) of
				{ok, {control, RelName, Terms}} -> 
				    io:format("~nDescribing Release: ~s ~s~n~n~p~n", [RelName, RelVsn, Terms]);
				Error ->
				    ?ERROR_MSG("consulting ~s with suffix ~s returns ~p~n", [Repo, ControlSuffix, Error]),
				    Error
			    end
		    end, ok, Repos)
	  end,
    ok = epkg_util:foreach_erts_vsn(TargetErtsVsns, Fun).

%%--------------------------------------------------------------------
%% @doc 
%%  Fetch the description for the latest version of a particular application from a remote repository.
%% @spec describe_latest_app(Repos, TargetErtsVsns, AppName, Timeout) -> ok | {error, Reason} | exit()
%%  where
%%   Repos = list()
%%   TargetErtsVsns = target_erts_vsns()
%%   AppName = string()
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%--------------------------------------------------------------------
describe_latest_app(Repos, [_H|_] = TargetErtsVsn, AppName, Timeout) when is_integer(_H) ->
    describe_latest_app(Repos, [TargetErtsVsn], AppName, Timeout);
describe_latest_app(Repos, TargetErtsVsns, AppName, Timeout) ->
    Fun = fun(Repo, AppVsn, ErtsVsn) ->
		  describe_app([Repo], ErtsVsn, AppName, AppVsn, Timeout)
	  end,
    fax_util:execute_on_latest_package_version(Repos, TargetErtsVsns, AppName, Fun, lib, false). 

%%--------------------------------------------------------------------
%% @doc 
%%  Fetch the description for a particular application from a remote repository.
%% @spec describe_app(Repos, TargetErtsVsns, AppName, AppVsn, Timeout) -> ok | {error, Reason} | exit()
%%  where
%%   Repos = list()
%%   TargetErtsVsns = target_erts_vsns()
%%   AppName = string()
%%   AppVsn = string()
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%--------------------------------------------------------------------
describe_app(Repos, [_H|_] = TargetErtsVsn, AppName, AppVsn, Timeout) when is_integer(_H) ->
    describe_app(Repos, [TargetErtsVsn], AppName, AppVsn, Timeout);
describe_app(Repos, TargetErtsVsns, AppName, AppVsn, Timeout) ->
    Fun = fun(ErtsVsn) -> 
		  Suffix = ewr_repo_paths:dot_app_file_suffix(ErtsVsn, AppName, AppVsn),
		  fs_lists:do_until(
		    fun(Repo) ->
			    case ewr_util:repo_consult(Repo, Suffix, Timeout) of
				{ok, {application, _, Terms}} -> 
				    io:format("~nDescribing Application: ~s~n~n~s~n", [AppName, format_app_terms(Terms)]);
				Error ->
				    ?ERROR_MSG("consulting ~s with suffix ~s returns ~p~n", [Repo, Suffix, Error]),
				    Error
			    end
		    end, ok, Repos)
	  end,
    fs_lists:do_until(Fun, ok, TargetErtsVsns).

%%--------------------------------------------------------------------
%% @doc Add a repository to fetch from. 
%% @spec add_repo_to_fetch_from(Repo, ConfigFilePaths) -> ok | {error, Reason}
%%  where
%%   Repos = string()
%%   ConfigFilePaths = [string()]
%% @end
%%--------------------------------------------------------------------
add_repo_to_fetch_from(Repo, ConfigFilePaths) ->
    add_to_config_list(repos_to_fetch_from, Repo, ConfigFilePaths).

%%--------------------------------------------------------------------
%% @doc fetch and or create a signature for this instance of Faxien.
%% @spec get_signature(ConfigFilePaths) -> {ok, Sig}
%%  where
%%   ConfigFilePaths = [string()]
%%   Sig = {{public_key, {Mod, ExpPub}}, {private_key, {Mod, ExpPriv}}}
%% @end
%%--------------------------------------------------------------------
get_signature(ConfigFilePaths) ->
    ?INFO_MSG("fetch signature", []),
    case gas:get_env(faxien, signature) of
	undefined -> 
	    ?INFO_MSG("no signature found, creating and writing one to ~p~n", [ConfigFilePaths]),
	    {ok, {{public_key, {N, E}}, {private_key, {N, D}}, {max_message_size, _Bytes}}} = cg_rsa:keygen(), 
	    Sig = {{public_key, {N, E}}, {private_key, {N, D}}},
	    gas:modify_config_file(ConfigFilePaths, faxien, signature, Sig),
	    {ok, Sig};
	{ok, Sig} ->
	    {ok, Sig}
    end.

%%--------------------------------------------------------------------
%% @doc Remove a repository to fetch from. 
%% @spec remove_repo_to_fetch_from(Repo, ConfigFilePaths) -> ok
%%  where
%%   Repos = string()
%%   ConfigFilePaths = [string()]
%% @end
%%--------------------------------------------------------------------
remove_repo_to_fetch_from(Repo, ConfigFilePaths) ->
    remove_from_config_list(repos_to_fetch_from, Repo, ConfigFilePaths).

%%--------------------------------------------------------------------
%% @doc Add a repository to publish to. 
%% @spec add_repo_to_publish_to(Repo, ConfigFilePaths) -> ok | {error, Reason}
%%  where
%%   Repos = string()
%%   ConfigFilePaths = [string()]
%% @end
%%--------------------------------------------------------------------
add_repo_to_publish_to(Repo, ConfigFilePaths) ->
    add_to_config_list(repos_to_publish_to, Repo, ConfigFilePaths).

%%--------------------------------------------------------------------
%% @doc Remove a repository to publish to. 
%% @spec remove_repo_to_publish_to(Repo, ConfigFilePaths) -> ok
%%  where
%%   Repos = string()
%%   ConfigFilePaths = [string()]
%% @end
%%--------------------------------------------------------------------
remove_repo_to_publish_to(Repo, ConfigFilePaths) ->
    remove_from_config_list(repos_to_publish_to, Repo, ConfigFilePaths).

%%--------------------------------------------------------------------
%% @doc Set the request timeout.
%% @spec set_request_timeout(Timeout, ConfigFilePaths) -> ok | {error, Reason}
%%  where
%%   Timeout = timeout()
%%   ConfigFilePaths = [string()]
%% @end
%%--------------------------------------------------------------------
%% @todo set up the gas functions so that they will insert a config entry if none exists.
set_request_timeout(Timeout, ConfigFilePaths) ->
    gas:modify_config_file(ConfigFilePaths, faxien, request_timeout, Timeout).

%%--------------------------------------------------------------------
%% @doc Set the preferred erts vsn for Faxien to use. 
%% @spec set_preferred_erts_vsn(PreferredErtsVsn, ConfigFilePaths) -> ok | {error, Reason}
%%  where
%%   Timeout = timeout()
%%   ConfigFilePaths = [string()]
%% @end
%%--------------------------------------------------------------------
set_preferred_erts_vsn(PreferredErtsVsn, ConfigFilePaths) ->
    %% @todo - this should not modify the epkg config - when less tired make clean
    gas:modify_config_file(ConfigFilePaths, epkg, preferred_erts_vsn, PreferredErtsVsn).

%%--------------------------------------------------------------------
%% @doc Display all currently installed releases that have available updates.
%% @spec outdated_releases(Repos, TargetErtsVsns, Timeout) -> OutdatedReleases
%%  where
%%   Repos = [string()]
%%   TargetErtsVsns = target_erts_vsns()
%%   OutdatedReleases = [{ReleaseName, HighestLocalVsn, HigherVersion}]
%% @end
%%--------------------------------------------------------------------
outdated_releases(Repos, [_H|_] = TargetErtsVsn, Timeout) when is_integer(_H) ->
    outdated_releases(Repos, [TargetErtsVsn], Timeout);
outdated_releases(Repos, TargetErtsVsns, Timeout) ->
    Releases      = epkg_installed_paths:list_releases(),
    lists:foldl(fun(ReleaseName, Acc) -> 
			try
			    {ok, {lower, HighestLocalVsn, HighestRemoteVsn, _RemoteErtsVsn}} =
			    is_outdated_release(Repos, TargetErtsVsns, ReleaseName, Timeout),
			    [{ReleaseName, HighestLocalVsn, HighestRemoteVsn}|Acc]
			catch
			    _Class:_Exception -> 
				Acc
			end
		end, [], Releases).

%%--------------------------------------------------------------------
%% @doc upgrade all applications on the install path.
%% @type erts_prompt() = bool(). indicate whether or not to prompt upon finding a package outside of the target erts vsn.
%% @spec upgrade_releases(Repos, TargetErtsVsns, IsLocalBoot, Options, Timeout) -> ok | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsns = target_erts_vsns()
%%   Options = options()
%% @end
%%--------------------------------------------------------------------
upgrade_releases(Repos, [_H|_] = TargetErtsVsn, IsLocalBoot, Options, Timeout) when is_integer(_H) ->
    upgrade_releases(Repos, [TargetErtsVsn], IsLocalBoot, Options, Timeout);
upgrade_releases(Repos, TargetErtsVsns, IsLocalBoot, Options, Timeout) ->
    Releases = epkg_installed_paths:list_releases(),
    lists:foreach(fun(ReleaseName) -> 
			  (catch upgrade_release(Repos, TargetErtsVsns, ReleaseName, IsLocalBoot, Options, Timeout))
		  end, Releases).

%%--------------------------------------------------------------------
%% @doc upgrade a single release.
%%  IsLocalBoot indicates whether a local specific boot file is to be created or not. See the systools docs for more information.
%% @type erts_prompt() = bool(). indicate whether or not to prompt upon finding a package outside of the target erts vsn.
%% @spec upgrade_release(Repos, TargetErtsVsns, ReleaseName, IsLocalBoot, Options, Timeout) -> ok | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsns = target_erts_vsns()
%%   ReleaseName = string()
%%   Options = options()
%% @end
%%--------------------------------------------------------------------
upgrade_release(Repos, [_H|_] = TargetErtsVsn, ReleaseName, IsLocalBoot, Options, Timeout) when is_integer(_H) -> 
    upgrade_release(Repos, [TargetErtsVsn], ReleaseName, IsLocalBoot, Options, Timeout);
upgrade_release(Repos, [TargetErtsVsn|_] = TargetErtsVsns, ReleaseName, IsLocalBoot, Options, Timeout) -> 
    ErtsPrompt = fs_lists:get_val(erts_prompt, Options),
    ?INFO_MSG("fax_manage:upgrade_release(~p, ~p)~n", [Repos, ReleaseName]),
    case catch is_outdated_release(Repos, TargetErtsVsns, ReleaseName, Timeout) of
	{ok, {lower, HighestLocalVsn, HighestRemoteVsn, TargetErtsVsn}} ->
	    handle_upgrade_release(Repos, TargetErtsVsn, ReleaseName, HighestLocalVsn,
				   HighestRemoteVsn, IsLocalBoot, Options,Timeout);
	{ok, {lower, HighestLocalVsn, HighestRemoteVsn, RemoteErtsVsn}} when ErtsPrompt == true ->
	    case fax_util:ask_about_switching_target_erts_vsns(ReleaseName, HighestRemoteVsn, TargetErtsVsn, RemoteErtsVsn) of
		true  ->
		    handle_upgrade_release(Repos, RemoteErtsVsn, ReleaseName, HighestLocalVsn,
					   HighestRemoteVsn, IsLocalBoot, Options, Timeout);
		false -> 
		    ok
	    end;
	{ok, {lower, HighestLocalVsn, HighestRemoteVsn, RemoteErtsVsn}} ->
	    handle_upgrade_release(Repos, RemoteErtsVsn, ReleaseName, HighestLocalVsn,
				   HighestRemoteVsn, IsLocalBoot, Options, Timeout);
	{ok, {_, HighestLocalVsn}} ->
	    io:format("~s at version ~s is up to date~n", [ReleaseName, HighestLocalVsn]);
	{error, {package_not_found, ReleaseName, _Exp}} ->
	    io:format("~s was not found in the repos. It is considered up to date~n", [ReleaseName]);
	{'EXIT', Reason} ->
	    io:format("~p~n", [Reason]),
	    {error, Reason}
    end.


handle_upgrade_release(Repos, TargetErtsVsns, ReleaseName, HighestLocalVsn, HighestRemoteVsn, IsLocalBoot, Options, Timeout) ->
    io:format("Upgrading from version ~s of ~s to version ~s~n", [HighestLocalVsn, ReleaseName, HighestRemoteVsn]),
    fax_install:install_remote_release(Repos,TargetErtsVsns,ReleaseName,HighestRemoteVsn,IsLocalBoot,Options,Timeout), 
    Force = fs_lists:get_val(force, Options),
    case Force of
	true -> ok;
	_    -> handle_config_on_upgrade(ReleaseName, HighestRemoteVsn, HighestLocalVsn)
    end.
    

handle_config_on_upgrade(ReleaseName, HighestRemoteVsn, HighestLocalVsn) ->
    case epkg:diff_config(ReleaseName, HighestRemoteVsn, HighestLocalVsn) of
	{ok, []} ->
	    ok;
	{ok, Diffs} ->
	    lists:foreach(fun({Rel1ConfigFilePath, Rel2ConfigFilePath, Diff}) ->
				  io:format("~nFaxien has found the following differences in config " ++
					    "files when upgrading\nfrom ~s to ~s:~n~n~p~n~n",
					    [HighestLocalVsn, HighestRemoteVsn, Diff]),
				  prompt_for_config_policy(ReleaseName, Rel1ConfigFilePath, Rel2ConfigFilePath)
			  end, Diffs)
    end.

prompt_for_config_policy(RelName, Rel1ConfigFilePath, Rel2ConfigFilePath) ->
    Prompt = "Differences have been found between the past and the latest version of config in\n" ++ Rel1ConfigFilePath ++
	"\nDo you want to use the (o)ld or (n)ew file [o|n]",
    case ewl_talk:ask([Prompt]) of 
	"o" ->
	    ?INFO_MSG("replacing ~p with ~p~n", [Rel2ConfigFilePath, Rel1ConfigFilePath]),
	    file:copy(Rel2ConfigFilePath, Rel1ConfigFilePath),
	    ok;
	"n" ->
	    ok;
	Error ->
	    ?INFO_MSG("user entered \"~p\"~n", [Error]),
	    io:format("Please enter \"o\" or \"p\"~n"),
	    prompt_for_config_policy(RelName, Rel1ConfigFilePath, Rel2ConfigFilePath)
    end.

%%--------------------------------------------------------------------
%% @doc Display all currently installed applications that have available updates.
%% @spec outdated_applications(Repos, TargetErtsVsns, Timeout) -> OutdatedApps
%%  where
%%   Repos = [string()]
%%   TargetErtsVsns = target_erts_vsns()
%%   OutdatedApps = [{AppName, HighestLocalVsn, HigherVersion}]
%% @end
%%--------------------------------------------------------------------
outdated_applications(Repos, [_H|_] = TargetErtsVsn, Timeout) when is_integer(_H) ->
    outdated_applications(Repos, [TargetErtsVsn], Timeout);
outdated_applications(Repos, TargetErtsVsns, Timeout) ->
    Apps = epkg_installed_paths:list_apps(TargetErtsVsns),
    lists:foldl(fun(AppName, Acc) -> 
			try 
			    {ok, {lower, HighestLocalVsn, HighestRemoteVsn, _RemoteErtsVsn}} =
			    is_outdated_app(Repos, TargetErtsVsns, AppName, Timeout),
			    [{AppName, HighestLocalVsn, HighestRemoteVsn}|Acc]
			catch
			    _Class:_Other -> 
				Acc
			end
		end, [], Apps).


%%--------------------------------------------------------------------
%% @doc upgrade all applications on the install path.
%% @type erts_prompt() = bool(). indicate whether or not to prompt upon finding a package outside of the target erts vsn.
%% @spec upgrade_applications(Repos, TargetErtsVsns, Options, Timeout) -> ok | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsns = target_erts_vsns()
%%   Options = options()
%% @end
%%--------------------------------------------------------------------
upgrade_applications(Repos, [_H|_] = TargetErtsVsn, Options, Timeout) when is_integer(_H) -> 
    upgrade_applications(Repos, [TargetErtsVsn], Options, Timeout);
upgrade_applications(Repos, TargetErtsVsns, Options, Timeout) -> 
    AppNames = epkg_installed_paths:list_apps(TargetErtsVsns),
    lists:foreach(fun(AppName) -> (catch upgrade_application(Repos, TargetErtsVsns, AppName, Options, Timeout)) end, AppNames).

%%--------------------------------------------------------------------
%% @doc upgrade a single application.
%% @type erts_prompt() = bool(). indicate whether or not to prompt upon finding a package outside of the target erts vsn.
%% @spec upgrade_application(Repos, TargetErtsVsns, AppName, Options, Timeout) -> ok | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsns = target_erts_vsns()
%%   AppName = string()
%%   Options = options()
%% @end
%%--------------------------------------------------------------------
upgrade_application(Repos, [_H|_] = TargetErtsVsn, AppName, Options, Timeout) when is_integer(_H) ->
    upgrade_application(Repos, [TargetErtsVsn], AppName, Options, Timeout);
upgrade_application(Repos, TargetErtsVsns, AppName, Options, Timeout) ->
    case epkg_manage:find_highest_local_app_vsn(AppName) of
	"" ->
	    io:format("No version of ~s exists locally, initiating install~n", [AppName]),
	    fax_install:install_latest_remote_application(Repos, TargetErtsVsns, AppName, Options, Timeout);
	HighestLocalVsn ->
	    upgrade_application(Repos, TargetErtsVsns, AppName, HighestLocalVsn, Options, Timeout)
    end.

upgrade_application(Repos, TargetErtsVsns, AppName, HighestLocalVsn, Options, Timeout) -> 
    ?INFO_MSG("fax_manage:upgrade_application(~p, ~p, ~p)~n", [Repos, AppName]),
    Force      = fs_lists:get_val(force, Options),
    ErtsPrompt = fs_lists:get_val(erts_prompt, Options),

    Fun = fun(Repo, HighestRemoteVsn, ErtsVsn) ->
		  case ewr_util:is_version_greater(HighestRemoteVsn, HighestLocalVsn) of
		      false ->
			  io:format("~s at version ~s is up to date~n", [AppName, HighestLocalVsn]);
		      true ->
			  io:format("Upgrading from version ~s of ~s to version ~s~n",
				    [HighestLocalVsn, AppName, HighestRemoteVsn]),
			  fax_install:install_remote_application([Repo], ErtsVsn, AppName,
								 HighestRemoteVsn, Force, Timeout)
		  end
	  end,

    fax_util:execute_on_latest_package_version(Repos, TargetErtsVsns, AppName, Fun, lib, ErtsPrompt).
	    
%%--------------------------------------------------------------------
%% @doc 
%%  Search through and list packages in remote repositories.
%% @spec search(Repos, Side, SearchType, SearchString, TargetErtsVsns) -> string()
%%  where
%%   Repos = list()
%%   Side = lib | releases | both
%%   SearchType = regexp | normal
%%   SearchString = string()
%%   TargetErtsVsns = target_erts_vsns()
%%    TargetErtsVsn = string()
%% @end
%%--------------------------------------------------------------------
search(Repos, Side, SearchType, SearchString, [H|_] = TargetErtsVsn) when is_integer(H) -> 
    search(Repos, Side, SearchType, SearchString, [TargetErtsVsn]);
search(Repos, Side, SearchType, SearchString, TargetErtsVsns) -> 
    FilterFun = case SearchType of
		    regexp ->
			fun(E) -> case re:run(E, SearchString) of {match, _} -> true; _ -> false end end;
		    normal ->
			fun(E) -> case re:run(E, ".*" ++ SearchString ++ ".*") of {match, _} -> true; _ -> false end end;
		    Invalid ->
			exit({"Not a valid search type, try normal or regexp", Invalid})
		end,

    case Side of
	both ->
	    Lib      = filter(FilterFun, lists:foldl(fun({_, A}, Acc) -> A ++ Acc end,
						     [],
						     raw_list(Repos, "lib", TargetErtsVsns))),
	    Releases = filter(FilterFun, lists:foldl(fun({_, A}, Acc) -> A ++ Acc end,
						     [],
						     raw_list(Repos, "releases", TargetErtsVsns))),
	    print_list(lib, Lib),
	    print_list(releases, Releases);
	Side ->
	    List = filter(FilterFun, lists:foldl(fun({_, A}, Acc) -> A ++ Acc end,
						 [],
						 raw_list(Repos, atom_to_list(Side), TargetErtsVsns))),
	    print_list(Side, List)
    end.


%%====================================================================
%% Internal functions
%%====================================================================

print_list(_Side, []) ->
    ok;
print_list(lib, List) ->
    print_list2("Applications (install with: faxien install-app)", List);
print_list(releases, List) ->
    print_list2("Releases (install with: faxien install-release)", List).

print_list2(Header, List) ->
    io:format("~s~n", [Header]),
    lists:foreach(fun(E) -> io:format("    ~s~n", [E]) end, List).
			  


filter(FilterFun, List) -> 
    SortedList = lists:sort(List),
    element(2, lists:foldr(fun(E, {Cur, IAcc} = Acc) -> 
				   case E == Cur of
				       true  -> 
					   Acc;
				       false -> 
					   case FilterFun(E) of
					       true  -> {E, [E|IAcc]};
					       false -> Acc
					   end
				   end
			   end, {undefined, []}, SortedList)).

raw_list(Repos, Side, TargetErtsVsns) ->
    lists:foldl(fun(Repo, Acc) -> 
			SysInfo  = ewr_util:system_info(),
			Suffixes = lists:foldl(fun(ErtsVsn, SufAcc) ->
						       [ewr_repo_paths:side_suffix(ErtsVsn, SysInfo, Side),
							ewr_repo_paths:side_suffix(ErtsVsn, "Generic", Side)|SufAcc]
					       end, [], TargetErtsVsns),
			try
			    lists:foldl(fun(Suf, Acc2) -> 
						?INFO_MSG("pulling data for list from ~s~n", [Repo ++ "/" ++ Suf]),
						case fax_util:repo_list(Repo ++ "/" ++ Suf ++ "/") of
						    {ok, Vsns}           -> [{Repo, lists:reverse(Vsns)}|Acc2]; 
						    {error, conn_failed} -> throw(conn_failed);
						    {error, _Reason}     -> Acc2
						end
					end, Acc, Suffixes)
			catch
				conn_failed ->
				       Acc
			end
		end, [], Repos).

%%--------------------------------------------------------------------
%% @private
%% @doc Add an element to a config tuple whose value is a list.
%% @spec add_to_config_list(Key, ValueToAdd, ConfigFilePath) -> ok | {error, Reason}
%% where
%%  Reason = no_such_config_entry
%% @end
%%--------------------------------------------------------------------
add_to_config_list(Key, ValueToAdd, ConfigFilePaths) ->
    gas:modify_config_value(ConfigFilePaths, faxien, Key, fun(Value) -> [ValueToAdd|Value] end).

%%--------------------------------------------------------------------
%% @private
%% @doc Remove an element to a config tuple whose value is a list.
%% @spec remove_from_config_list(Key, ValueToRemove, ConfigFilePaths) -> ok | {error, Reason}
%% where
%%  Reason = no_such_config_entry
%% @end
%%--------------------------------------------------------------------
remove_from_config_list(Key, ValueToRemove, ConfigFilePaths) ->
    gas:modify_config_value(ConfigFilePaths, faxien, Key, fun(Value) -> lists:delete(ValueToRemove, Value) end).

%%--------------------------------------------------------------------
%% @private
%% @doc A determine if a release has a lower version than what is available in the remote repositories.
%% @spec is_outdated_release(Repos, TargetErtsVsns, ReleaseName, Timeout) -> {ok, Compare}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsns = target_erts_vsns()
%%   Compare = {higher, HighestLocalVsn} | {same, HighestLocalVsn} | {lower, {HighestLocalVsn, HigherRemoteVsn, RemoteErtsVsn}}
%% @end
%%--------------------------------------------------------------------
is_outdated_release(Repos, TargetErtsVsns, ReleaseName, _Timeout) ->
    case fax_util:find_highest_vsn(Repos, TargetErtsVsns, ReleaseName, releases) of
	{ok, {_Repo, HighestRemoteVsn, ErtsVsn}} ->
	    case epkg_manage:find_highest_local_release_vsn(ReleaseName) of
		"" ->
		    throw("release not found locally");
		HighestLocalVsn ->
		    case ewr_util:is_version_greater(HighestLocalVsn, HighestRemoteVsn) of
			true ->
			    {ok, {higher, HighestLocalVsn}};
			false when HighestRemoteVsn == HighestLocalVsn ->
			    {ok, {same, HighestLocalVsn}};
			false ->
			    {ok, {lower, HighestLocalVsn, HighestRemoteVsn, ErtsVsn}}
		    end
	    end;
	{error, {package_not_found, _PackageName, _Exp}} = Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc A determine if an application has a lower version than what is available in the remote repositories.
%% @spec is_outdated_app(Repos, TargetErtsVsns, AppName, Timeout) -> {ok, Compare}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsns = target_erts_vsns()
%%   Compare = {higher, HighestLocalVsn} | {same, HighestLocalVsn} | {lower, {HighestLocalVsn, HigherRemoteVsn, RemoteErtsVsn}}
%% @end
%%--------------------------------------------------------------------
is_outdated_app(Repos, TargetErtsVsns, AppName, _Timeout) ->
    {ok, {_Repo, HighestRemoteVsn, RemoteErtsVsn}} = fax_util:find_highest_vsn(Repos, TargetErtsVsns, AppName, lib),
    case epkg_manage:find_highest_local_app_vsn(AppName, TargetErtsVsns) of
	"" ->
	    throw("app not found on the local system");
	HighestLocalVsn ->
	    case ewr_util:is_version_greater(HighestLocalVsn, HighestRemoteVsn) of
		true ->
		    {ok, {higher, HighestLocalVsn}};
		false when HighestRemoteVsn == HighestLocalVsn ->
		    {ok, {same, HighestLocalVsn}};
		false ->
		    {ok, {lower, HighestLocalVsn, HighestRemoteVsn, RemoteErtsVsn}}
	    end
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc Return a pretty-print string version of application configuration terms.
%% @spec format_app_terms(Terms) -> string()
%% @end
%%--------------------------------------------------------------------
format_app_terms(Terms) ->
    lists:concat([format_app_term(T) || T <- Terms]).

format_app_term({_Key, []}) ->
    "";
format_app_term({Key, Val}) ->
    lists:concat([Key, ":\n", format_app_val(Key, Val), "\n"]).

format_app_val(applications, Val) ->
    format_atom_list(Val);
format_app_val(description, Val) ->
    format_app_string(Val);
format_app_val(modules, Val) ->
    format_atom_list(Val);
format_app_val(registered, Val) ->
    format_atom_list(Val);
format_app_val(vsn, Val) ->
    format_app_string(Val);
format_app_val(versioned_dependencies, Val) ->
    join([format_versioned_dependency(Dep) || Dep <- Val], "\n");
format_app_val(_Key, Val) ->
    io_lib:format("    ~p", [Val]).


format_app_string(Str) ->
    string:concat("    ", Str).

format_atom_list(Atoms) ->
    Words = join([erlang:atom_to_list(A) || A <- Atoms], ","),
    wrap(Words, 60, "    ").

format_versioned_dependency({Pkg, Ver}) ->
    io_lib:format("    ~p == ~s", [Pkg, Ver]);
format_versioned_dependency({Pkg, Ver, gte}) ->
    io_lib:format("    ~p >= ~s", [Pkg, Ver]);
format_versioned_dependency(Dep) ->
    io_lib:format("    ~p", [Dep]).

%%--------------------------------------------------------------------
%% @private
%% @doc Join list terms together with a separator.
%% @spec join(Items::list(), Sep) -> list()
%% @end
%%--------------------------------------------------------------------
join(Items, Sep) ->
    lists:reverse(join(Items, Sep, [])).

join([], _Sep, Acc) ->
    Acc;
join([Head | []], _Sep, Acc) ->
    [Head | Acc];
join([Head | Tail], Sep, Acc) ->
    join(Tail, Sep, [Sep, Head | Acc]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Text wrap a list of words with the given column width and leading
%% indentation string.
%% @spec wrap(Words::list(), Width::integer(), Indent::string()) -> string()
%% @end
%%--------------------------------------------------------------------
wrap(Words, Width, Indent) ->
    RRLines = wrap(Words, Width, Indent, [[]]), % reversed, reversed line list
    RLines = lists:map(fun lists:reverse/1, RRLines), % reversed line list
    Lines = lists:reverse(RLines),
    lists:flatten(join(Lines, "\n")).

wrap([], _Width, _Indent, Acc) ->
    Acc;
wrap([Word | Words], Width, Indent, [[] | Lines]) ->
    wrap(Words, Width, Indent, [[Word, Indent] | Lines]);
wrap(["," | Words], Width, Indent, [Line | Lines]) ->
    wrap(Words, Width, Indent, [["," | Line] | Lines]);
wrap([Word | Words], Width, Indent, [Line | Lines]) ->
  case lists:flatlength(Line) + length(Word) + 1 < Width of
      true -> wrap(Words, Width, Indent, [[Word, " " | Line] | Lines]);
      false -> wrap([Word | Words], Width, Indent, [[], Line | Lines])
  end.

