
%%%-----------------------------------------------------------------
%%% @doc Handles fetching packages from the remote repository and 
%%%      placing them in the erlware repo.
%%% 
%%% @type force() = bool(). Indicates whether an existing app is to be overwritten with or without user conscent.  
%%%
%%% @type repo() = string(). Contains address and repo designation. 
%%%   Example: http://www.erlware.org/stable   
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
	 add_repo_to_publish_to/2,
	 remove_repo_to_publish_to/2,
	 add_repo_to_fetch_from/2,
	 remove_repo_to_fetch_from/2,
	 set_request_timeout/2,
	 set_target_erts_vsn/2,
	 search/4,
	 describe_release/5,
	 describe_latest_release/4,
	 describe_app/5,
	 describe_latest_app/4
	]).

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
%% @spec describe_latest_release(Repos, TargetErtsVsn, RelName, Timeout) -> ok | {error, Reason} | exit()
%%  where
%%   Repos = list()
%%   TargetErtsVsn = string()
%%   RelName = string()
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%--------------------------------------------------------------------
describe_latest_release(Repos, TargetErtsVsn, RelName, Timeout) ->
    Fun = fun(ManagedRepos, RelVsn) ->
		  describe_release(ManagedRepos, TargetErtsVsn, RelName, RelVsn, Timeout)
	  end,
    fax_util:execute_on_latest_package_version(Repos, TargetErtsVsn, RelName, Fun, lib). 

%%--------------------------------------------------------------------
%% @doc 
%%  Fetch the description for a particular release from a remote repository.
%% @spec describe_release(Repos, TargetErtsVsn, RelName, RelVsn, Timeout) -> ok | {error, Reason} | exit()
%%  where
%%   Repos = list()
%%   TargetErtsVsn = string()
%%   RelName = string()
%%   RelVsn = string()
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%--------------------------------------------------------------------
describe_release(Repos, TargetErtsVsn, RelName, RelVsn, Timeout) ->
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
    ok = epkg_util:foreach_erts_vsn(TargetErtsVsn, Fun).

%%--------------------------------------------------------------------
%% @doc 
%%  Fetch the description for the latest version of a particular application from a remote repository.
%% @spec describe_latest_app(Repos, TargetErtsVsn, AppName, Timeout) -> ok | {error, Reason} | exit()
%%  where
%%   Repos = list()
%%   TargetErtsVsn = string()
%%   AppName = string()
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%--------------------------------------------------------------------
describe_latest_app(Repos, TargetErtsVsn, AppName, Timeout) ->
    Fun = fun(ManagedRepos, AppVsn) ->
		  describe_app(ManagedRepos, TargetErtsVsn, AppName, AppVsn, Timeout)
	  end,
    fax_util:execute_on_latest_package_version(Repos, TargetErtsVsn, AppName, Fun, lib). 

%%--------------------------------------------------------------------
%% @doc 
%%  Fetch the description for a particular application from a remote repository.
%% @spec describe_app(Repos, TargetErtsVsn, AppName, AppVsn, Timeout) -> ok | {error, Reason} | exit()
%%  where
%%   Repos = list()
%%   TargetErtsVsn = string()
%%   AppName = string()
%%   AppVsn = string()
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%--------------------------------------------------------------------
describe_app(Repos, TargetErtsVsn, AppName, AppVsn, Timeout) ->
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
    ok = epkg_util:foreach_erts_vsn(TargetErtsVsn, Fun).

%%--------------------------------------------------------------------
%% @doc Add a repository to fetch from. 
%% @spec add_repo_to_fetch_from(Repo, ConfigFilePath) -> ok | {error, Reason}
%%  where
%%   Repos = string()
%%   ConfigFilePath = string()
%% @end
%%--------------------------------------------------------------------
add_repo_to_fetch_from(Repo, ConfigFilePath) ->
    add_to_config_list(repos_to_fetch_from, Repo, ConfigFilePath).

%%--------------------------------------------------------------------
%% @doc Remove a repository to fetch from. 
%% @spec remove_repo_to_fetch_from(Repo, ConfigFilePath) -> ok
%%  where
%%   Repos = string()
%%   ConfigFilePath = string()
%% @end
%%--------------------------------------------------------------------
remove_repo_to_fetch_from(Repo, ConfigFilePath) ->
    remove_from_config_list(repos_to_fetch_from, Repo, ConfigFilePath).

%%--------------------------------------------------------------------
%% @doc Add a repository to publish to. 
%% @spec add_repo_to_publish_to(Repo, ConfigFilePath) -> ok | {error, Reason}
%%  where
%%   Repos = string()
%%   ConfigFilePath = string()
%% @end
%%--------------------------------------------------------------------
add_repo_to_publish_to(Repo, ConfigFilePath) ->
    add_to_config_list(repos_to_publish_to, Repo, ConfigFilePath).

%%--------------------------------------------------------------------
%% @doc Remove a repository to publish to. 
%% @spec remove_repo_to_publish_to(Repo, ConfigFilePath) -> ok
%%  where
%%   Repos = string()
%%   ConfigFilePath = string()
%% @end
%%--------------------------------------------------------------------
remove_repo_to_publish_to(Repo, ConfigFilePath) ->
    remove_from_config_list(repos_to_publish_to, Repo, ConfigFilePath).

%%--------------------------------------------------------------------
%% @doc Set the request timeout.
%% @spec set_request_timeout(Timeout, ConfigFilePath) -> ok | {error, Reason}
%%  where
%%   Timeout = timeout()
%%   ConfigFilePath = string()
%% @end
%%--------------------------------------------------------------------
%% @todo set up the gas functions so that they will insert a config entry if none exists.
set_request_timeout(Timeout, ConfigFilePath) ->
    gas:modify_config_file(ConfigFilePath, faxien, request_timeout, Timeout).

%%--------------------------------------------------------------------
%% @doc Set the target erts vsn for Faxien to use. Basically this is the highest erts vsn it will try to pull for.
%% @spec set_target_erts_vsn(TargetErtsVsn, ConfigFilePath) -> ok | {error, Reason}
%%  where
%%   Timeout = timeout()
%%   ConfigFilePath = string()
%% @end
%%--------------------------------------------------------------------
set_target_erts_vsn(TargetErtsVsn, ConfigFilePath) ->
    %% @todo - this should not modify the epkg config - when less tired make clean
    gas:modify_config_file(ConfigFilePath, epkg, target_erts_vsn, TargetErtsVsn).

%%--------------------------------------------------------------------
%% @doc Display all currently installed releases that have available updates.
%% @spec outdated_releases(Repos, TargetErtsVsn, Timeout) -> OutdatedReleases
%%  where
%%   Repos = [string()]
%%   TargetErtsVsn = string()
%%   OutdatedReleases = [{ReleaseName, HighestLocalVsn, HigherVersion}]
%% @end
%%--------------------------------------------------------------------
outdated_releases(Repos, TargetErtsVsn, Timeout) ->
    Releases      = epkg_installed_paths:list_releases(),
    lists:foldl(fun(ReleaseName, Acc) -> 
			case catch is_outdated_release(Repos, TargetErtsVsn, ReleaseName, Timeout) of
			    {ok, {lower, HighestLocalVsn, HighestRemoteVsn}} -> 
				[{ReleaseName, HighestLocalVsn, HighestRemoteVsn}|Acc];
			    _Other -> 
				Acc
			end
		end, [], Releases).

%%--------------------------------------------------------------------
%% @doc upgrade all applications on the install path.
%% @spec upgrade_releases(Repos, TargetErtsVsn, IsLocalBoot, Force, Timeout) -> ok | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsn = string()
%%   Force = force()
%% @end
%%--------------------------------------------------------------------
upgrade_releases(Repos, TargetErtsVsn, IsLocalBoot, Force, Timeout) ->
    Releases = epkg_installed_paths:list_releases(),
    lists:foreach(fun(ReleaseName) -> 
			  upgrade_release(Repos, TargetErtsVsn, ReleaseName, IsLocalBoot, Force, Timeout)
		  end, Releases).

%%--------------------------------------------------------------------
%% @doc upgrade a single release.
%%  IsLocalBoot indicates whether a local specific boot file is to be created or not. See the systools docs for more information.
%% @spec upgrade_release(Repos, TargetErtsVsn, ReleaseName, IsLocalBoot, Force, Timeout) -> ok | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsn = string()
%%   ReleaseName = string()
%%   Force = force()
%% @end
%%--------------------------------------------------------------------
upgrade_release(Repos, TargetErtsVsn, ReleaseName, IsLocalBoot, Force, Timeout) -> 
    ?INFO_MSG("fax_manage:upgrade_release(~p, ~p, ~p)~n", [Repos, ReleaseName]),
    case is_outdated_release(Repos, TargetErtsVsn, ReleaseName, Timeout) of
	{ok, {lower, HighestLocalVsn, HighestRemoteVsn}} ->
	    io:format("Upgrading from version ~s of ~s to version ~s~n", [HighestLocalVsn, ReleaseName, HighestRemoteVsn]),
	    fax_install:install_remote_release(Repos,TargetErtsVsn,ReleaseName,HighestRemoteVsn,IsLocalBoot,Force,Timeout), 
	    handle_config_on_upgrade(ReleaseName, HighestRemoteVsn, HighestLocalVsn);
	{ok, {_, HighestLocalVsn}} ->
	    io:format("~s at version ~s is up to date~n", [ReleaseName, HighestLocalVsn]);
	Error ->
	    io:format("~p~n", [Error]),
	    Error
    end.

handle_config_on_upgrade(ReleaseName, HighestRemoteVsn, HighestLocalVsn) ->
    case epkg:diff_config(ReleaseName, HighestRemoteVsn, HighestLocalVsn) of
	{ok, []} ->
	    ok;
	{ok, Diff} ->
	    io:format("Faxien has found the following differences in config " ++
		      "files when upgrading from ~s to ~s:~n~n~p~n",
		      [HighestLocalVsn, HighestRemoteVsn, Diff]),
	    prompt_for_config_policy(ReleaseName, HighestRemoteVsn, HighestLocalVsn)
    end.
	    
prompt_for_config_policy(RelName, HighestRemoteVsn, HighestLocalVsn) ->
    case ewl_talk:ask(["Would you like to (k)eep the latest config or (o)verwrite it with the past config? [k|o]"]) of
	"o" ->
	    Rel1ConfigFilePath = epkg_installed_paths:find_config_file_path(RelName, HighestRemoteVsn),
	    Rel2ConfigFilePath = epkg_installed_paths:find_config_file_path(RelName, HighestLocalVsn),
	    ?INFO_MSG("replacing ~p with ~p~n", [Rel2ConfigFilePath, Rel1ConfigFilePath]),
	    file:copy(Rel2ConfigFilePath, Rel1ConfigFilePath),
	    ok;
	"k" ->
	    ok;
	Error ->
	    ?INFO_MSG("user entered \"~p\"~n", [Error]),
	    io:format("Please enter \"k\" or \"o\"~n"),
	    prompt_for_config_policy(RelName, HighestRemoteVsn, HighestLocalVsn)
    end.

%%--------------------------------------------------------------------
%% @doc Display all currently installed applications that have available updates.
%% @spec outdated_applications(Repos, TargetErtsVsn, Timeout) -> OutdatedApps
%%  where
%%   Repos = [string()]
%%   TargetErtsVsn = string()
%%   OutdatedApps = [{AppName, HighestLocalVsn, HigherVersion}]
%% @end
%%--------------------------------------------------------------------
outdated_applications(Repos, TargetErtsVsn, Timeout) ->
    Apps          = epkg_installed_paths:list_apps(TargetErtsVsn),
    lists:foldl(fun(AppName, Acc) -> 
			case catch is_outdated_app(Repos, TargetErtsVsn, AppName, Timeout) of
			    {ok, {lower, HighestLocalVsn, HighestRemoteVsn}} -> 
				[{AppName, HighestLocalVsn, HighestRemoteVsn}|Acc];
			    _Other -> 
				Acc
			end
		end, [], Apps).


%%--------------------------------------------------------------------
%% @doc upgrade all applications on the install path.
%% @spec upgrade_applications(Repos, TargetErtsVsn, Force, Timeout) -> ok | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsn = string()
%%   Force = force()
%% @end
%%--------------------------------------------------------------------
upgrade_applications(Repos, TargetErtsVsn, Force, Timeout) -> 
    AppNames      = epkg_installed_paths:list_apps(TargetErtsVsn),
    lists:foreach(fun(AppName) -> upgrade_application(Repos, TargetErtsVsn, AppName, Force, Timeout) end, AppNames).

%%--------------------------------------------------------------------
%% @doc upgrade a single application.
%% @spec upgrade_application(Repos, TargetErtsVsn, AppName, Force, Timeout) -> ok | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsn = string()
%%   AppName = string()
%%   Force = force()
%% @end
%%--------------------------------------------------------------------
upgrade_application(Repos, TargetErtsVsn, AppName, Force, Timeout) -> 
    ?INFO_MSG("fax_manage:upgrade_application(~p, ~p, ~p)~n", [Repos, AppName]),
    case is_outdated_app(Repos, TargetErtsVsn, AppName, Timeout) of
	{ok, {lower, HighestLocalVsn, HighestRemoteVsn}} ->
	    io:format("Upgrading from version ~s of ~s to version ~s~n", [HighestLocalVsn, AppName, HighestRemoteVsn]),
	    fax_install:install_remote_application(Repos, TargetErtsVsn, AppName, HighestRemoteVsn, Force, Timeout); 
	{ok, {_, HighestLocalVsn}} ->
	    io:format("~s at version ~s is up to date~n", [AppName, HighestLocalVsn]);
	Error ->
	    io:format("~p~n", [Error]),
	    Error
    end.
    
%%--------------------------------------------------------------------
%% @doc 
%%  Search through and list packages in remote repositories.
%% @spec search(Repos, Side, SearchType, SearchString) -> string()
%%  where
%%   Repos = list()
%%   Side = lib | releases | both
%%   SearchType = regexp | normal
%%   SearchString = string()
%% @end
%%--------------------------------------------------------------------
search(Repos, Side, SearchType, SearchString) -> 
    FilterFun = case SearchType of
		    regexp ->
			fun(E) -> case regexp:match(E, SearchString) of {match, _, _} -> true; _ -> false end end;
		    normal ->
			fun(E) -> case regexp:match(E, ".*" ++ SearchString ++ ".*") of {match, _, _} -> true; _ -> false end end;
		    Invalid ->
			exit({"Not a valid search type, try normal or regexp", Invalid})
		end,

    case Side of
	both ->
	    Lib      = filter(FilterFun, lists:foldl(fun({_, A}, Acc) -> A ++ Acc end, [], raw_list(Repos, lib))),
	    Releases = filter(FilterFun, lists:foldl(fun({_, A}, Acc) -> A ++ Acc end, [], raw_list(Repos, releases))),
	    print_list(lib, Lib),
	    print_list(releases, Releases);
	Side ->
	    List = filter(FilterFun, lists:foldl(fun({_, A}, Acc) -> A ++ Acc end, [], raw_list(Repos, Side))),
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
    print_list2("Releases (install with: faxien install)", List).

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

raw_list(Repos, Side) ->
    lists:foldl(fun(Repo, Acc) -> 
			SysInfo  = ewr_util:system_info(),
			Suffixes = ewr_util:gen_multi_erts_repo_stub_suffix("", [SysInfo, "Generic"], Side),
			try
			    lists:foldl(fun(Suf, Acc2) -> 
						?INFO_MSG("pulling data for list from ~s~n", [Repo ++ "/" ++ Suf]),
						case fax_util:repo_list(Repo ++ "/" ++ Suf) of
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
add_to_config_list(Key, ValueToAdd, ConfigFilePath) ->
    gas:modify_config_value(ConfigFilePath, faxien, Key, fun(Value) -> [ValueToAdd|Value] end).

%%--------------------------------------------------------------------
%% @private
%% @doc Remove an element to a config tuple whose value is a list.
%% @spec remove_from_config_list(Key, ValueToRemove, ConfigFilePath) -> ok | {error, Reason}
%% where
%%  Reason = no_such_config_entry
%% @end
%%--------------------------------------------------------------------
remove_from_config_list(Key, ValueToRemove, ConfigFilePath) ->
    gas:modify_config_value(ConfigFilePath, faxien, Key, fun(Value) -> lists:delete(ValueToRemove, Value) end).

%%--------------------------------------------------------------------
%% @private
%% @doc A determine if a release has a lower version than what is available in the remote repositories.
%% @spec is_outdated_release(Repos, TargetErtsVsn, ReleaseName, Timeout) -> {ok, Compare} | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsn = string()
%%   Compare = {higher, HighestLocalVsn} | {same, HighestLocalVsn} | {lower, {HighestLocalVsn, HigherRemoteVsn}}
%% @end
%%--------------------------------------------------------------------
is_outdated_release(Repos, TargetErtsVsn, ReleaseName, _Timeout) ->
    {ok, {_Repo, HighestRemoteVsn}} = fax_util:find_highest_vsn(Repos, TargetErtsVsn, ReleaseName, releases),
    case epkg_manage:find_highest_local_release_vsn(ReleaseName, TargetErtsVsn) of
	{ok, HighestLocalVsn} ->
	    case ewr_util:is_version_greater(HighestLocalVsn, HighestRemoteVsn) of
		true ->
		    {ok, {higher, HighestLocalVsn}};
		false when HighestRemoteVsn == HighestLocalVsn ->
		    {ok, {same, HighestLocalVsn}};
		false ->
		    {ok, {lower, HighestLocalVsn, HighestRemoteVsn}}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc A determine if an application has a lower version than what is available in the remote repositories.
%% @spec is_outdated_app(Repos, TargetErtsVsn, AppName, Timeout) -> {ok, Compare} | {error, Reason}
%%  where
%%   Repos = [string()]
%%   TargetErtsVsn = string()
%%   Compare = {higher, HighestLocalVsn} | {same, HighestLocalVsn} | {lower, {HighestLocalVsn, HigherRemoteVsn}}
%% @end
%%--------------------------------------------------------------------
is_outdated_app(Repos, TargetErtsVsn, AppName, _Timeout) ->
    {ok, {_Repo, HighestRemoteVsn}} = fax_util:find_highest_vsn(Repos, TargetErtsVsn, AppName, lib),
    case epkg_manage:find_highest_local_app_vsn(AppName, TargetErtsVsn) of
	{ok, HighestLocalVsn} ->
	    case ewr_util:is_version_greater(HighestLocalVsn, HighestRemoteVsn) of
		true ->
		    {ok, {higher, HighestLocalVsn}};
		false when HighestRemoteVsn == HighestLocalVsn ->
		    {ok, {same, HighestLocalVsn}};
		false ->
		    {ok, {lower, HighestLocalVsn, HighestRemoteVsn}}
	    end;
	{error, Reason} ->
	    {error, Reason}
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
format_app_val(_Key, Val) ->
    io_lib:format("    ~p", [Val]).


format_app_string(Str) ->
    string:concat("    ", Str).

format_atom_list(Atoms) ->
    Words = join([erlang:atom_to_list(A) || A <- Atoms], ","),
    wrap(Words, 60, "    ").

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
