%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@erlware.org>
%%% @copyright 2007 Erlware
%%% @end
%%%-------------------------------------------------------------------
-module(fax_util).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%-include("eunit.hrl").
-include("faxien.hrl").
-include("epkg.hrl").


%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 execute_on_latest_package_version/6,
	 execute_on_latest_package_version/7,
	 find_highest_vsn/4,
	 find_highest_vsn/5,
	 copy_dir_to_tmp_dir/1,
	 to_app_dirs/1,
	 flatten_term/1,
	 repo_list/1,
	 add_pzs/1,
	 ask_about_switching_target_ertsVsns/4,
	 get_erts_vsns_gte_than/2,
	 get_erts_vsn/1
        ]).

%%====================================================================
%% External functions
%%====================================================================

%%-------------------------------------------------------------------
%% @doc
%%  Manage finding the highest version of a particular package and executing on it. Terminate execution when the fun returns ok.
%%
%% <pre>
%% Variables:
%%  TargetErtsVsn - the erts version to start with
%%  Fun - This fun takes three arguments Repos PackageVsn, and ErtsVsn. This function terminates when the fun returns ok.
%%  VsnThreshold - is used to indicate that the highest version possible is to be installed but no higher than VsnThreshold.
%%  ErtsPrompt - indicates whether or not the user should be prompted if a package is to be installed for an erts vsn other than the target.
%% </pre>
%%
%% @spec execute_on_latest_package_version(Repos, TargetErtsVsn, PackageName, Fun, Side, VsnThreshold, ErtsPrompt) -> ok | exit
%% where
%%  Side = lib | releases
%%  VsnThreshold = string() | infinity
%%  ErtsPrompt = bool()
%% @end
%%-------------------------------------------------------------------
execute_on_latest_package_version([], _TargetErtsVsn, _PackageName, _Fun, _Side, _VsnThreshold, _ErtsPrompt) ->
    {error, package_not_found};
execute_on_latest_package_version(Repos, TargetErtsVsn, PackageName, Fun, Side, VsnThreshold, ErtsPrompt) 
  when Side == lib; Side == releases ->
    execute_on_latest_package_version(Repos, TargetErtsVsn, infinity, PackageName, Fun, Side, VsnThreshold, ErtsPrompt).
    

	     
%% @spec execute_on_latest_package_version(Repos, TargetErtsVsn, PackageName, Fun, Side, ErtsPrompt) -> ok | exit()
%% @equiv execute_on_latest_package_version(Repos, TargetErtsVsn, PackageName, Fun, Side, infinity, ErtsPrompt) 
execute_on_latest_package_version(Repos, TargetErtsVsn, PackageName, Fun, Side, ErtsPrompt) ->
    execute_on_latest_package_version(Repos, TargetErtsVsn, PackageName, Fun, Side, infinity, ErtsPrompt).

%%-------------------------------------------------------------------
%% @doc return all the erts vsns that are greater than or equal to the target erts versions.
%% @spec get_erts_vsns_gte_than(TargetErtsVsn, VsnThreshold) -> ErtsVsns::list()
%% @end
%%-------------------------------------------------------------------
get_erts_vsns_gte_than(TargetErtsVsn, infinity) ->
    ErtsVsns = [E || {_, E, _} <- ?COMPILER_VSN_TO_ERTS_VSN_TO_ERLANG_VSN, ewr_util:is_version_greater(E, TargetErtsVsn)],
    [TargetErtsVsn|ErtsVsns];
get_erts_vsns_gte_than(TargetErtsVsn, VsnThreshold) ->
    ErtsVsns = [E || {_, E, _} <- ?COMPILER_VSN_TO_ERTS_VSN_TO_ERLANG_VSN,
		     ewr_util:is_version_greater(E, TargetErtsVsn),
		     not ewr_util:is_version_greater(E, VsnThreshold)],
    [TargetErtsVsn|ErtsVsns].

%%-------------------------------------------------------------------
%% @doc
%% Return the latest version lower than the VsnThreshold and repo that contains it for a given package and repos.
%%
%% <pre>
%% Variables:
%%  VsnThreshold - is used to indicate that the highest version possible is to be found but no higher than VsnThreshold.
%% </pre>
%%
%% @spec find_highest_vsn(Repos, TargetErtsVsns, PackageName, Side, VsnThreshold) ->
%%       {ok, {Repo, Vsn, ErtsVsn}} | {error, Reason}
%%  where 
%%   Side = lib | releases
%%   VsnThreshold = string() | infinity
%% @end
%%-------------------------------------------------------------------
find_highest_vsn(Repos, TargetErtsVsns, PackageName, Side, VsnThreshold) ->
    case catch find_highest_vsn2(Repos, TargetErtsVsns,  PackageName, Side, VsnThreshold) of
	{ok, {_Repo, _HighVsn, _ErtsVsn}} = Reply -> Reply;
	Exp                                       -> {error, {package_not_found, PackageName, Exp}}
    end.

%% @equiv find_highest_vsn(Repos, TargetErtsVsns, PackageName, Side, infinity)
%% @spec find_highest_vsn(Repos, TargetErtsVsns, PackageName, Side) -> {ok, {Repo, Vsn, ErtsVsn}} | {error, Reason}
find_highest_vsn(Repos, TargetErtsVsns, PackageName, Side) ->
    find_highest_vsn(Repos, TargetErtsVsns, PackageName, Side, infinity).

				 
find_highest_vsn2(Repos, TargetErtsVsns, PackageName, Side, VsnThreshold) ->
    ?INFO_MSG("Target erts versions to search are ~p~n", [TargetErtsVsns]),
    VsnLists =
	lists:flatten(
	  lists:foldl(fun(Repo, Acc) -> 
			      SysInfo  = ewr_util:system_info(),
			      Suffixes = suffixes(TargetErtsVsns, PackageName, [SysInfo, "Generic"], Side),
			      ?INFO_MSG("suffixes to check are ~p~n", [Suffixes]),
			      lists:foldl(fun(Suf, Acc2) -> 
						  ?INFO_MSG("Checking for highest version of ~p in ~s/~s~n", 
							    [PackageName, Repo, Suf]),
						  case repo_list(Repo ++ "/" ++ Suf) of
						      {ok, Vsns} -> 
							  ?INFO_MSG("found vsns ~p~n", [Vsns]),
							  Elements = ewr_repo_paths:decompose_suffix(Suf),
							  ErtsVsn  = fs_lists:get_val(erts_vsn, Elements),
							  [[{Repo, Vsn, ErtsVsn} || Vsn <- lists:reverse(Vsns)]|Acc2]; 
						      {error, _Reason} -> 
							  Acc2
						  end
					  end, Acc, Suffixes)
		      end, [], Repos)),
    find_highest_remote_vsn_under_threshold(VsnThreshold, VsnLists).

suffixes(ErtsVsns, PackageName, Areas, Side) ->
    lists:foldr(fun(ErtsVsn, Acc) ->
			Acc ++ ewr_util:gen_repo_stub_suffix(ErtsVsn, PackageName, Areas, Side)
		end, [], ErtsVsns).

find_highest_remote_vsn_under_threshold(VsnThreshold, VsnLists) ->
    ?INFO_MSG("find_highest_vsn list of remote versions ~p with threshold of ~p~n", [VsnLists, VsnThreshold]),
    case VsnThreshold of
	infinity ->
	    {ok, hd(lists:sort(fun({_, V1, _}, {_, V2, _}) -> ewr_util:is_version_greater(V1, V2) end, VsnLists))};
	VsnThreshold ->
	    case lop_off_top(VsnLists, VsnThreshold) of
		{ok, {Repo, HighVsn, _HighErtsVsn}} = Res ->
		    ?INFO_MSG("find_highest_vsn list of remote versions ~p with threshold of ~p found highest ~p at vsn ~s~n", 
			      [VsnLists, VsnThreshold, Repo, HighVsn]),
		    Res;
		{error, _Reason} = Error ->
		    ?INFO_MSG("Failed to find a version lower than ~p~n", [VsnThreshold]),
		    Error
	    end
    end.

lop_off_top(VsnLists, VsnThreshold) ->
    SortedVsnLists = lists:sort(fun({_, V1, _}, {_, V2, _}) -> ewr_util:is_version_greater(V1, V2) end, VsnLists),
    lop_off_top2(SortedVsnLists, VsnThreshold).

lop_off_top2([{Repo, Vsn, ErtsVsn}|T], VsnThreshold) ->
    case ewr_util:is_version_greater(VsnThreshold, Vsn) of
	true  -> {ok, {Repo, Vsn, ErtsVsn}};
	false -> lop_off_top2(T, VsnThreshold) 
    end;
lop_off_top2([], VsnThreshold) ->
    {error, {"no packages with version less than ", VsnThreshold}}.
	    

%%--------------------------------------------------------------------
%% @doc take a directory and copy it into a temporary directory.
%% @spec copy_dir_to_tmp_dir(TargetDirPath::string()) -> {ok, TmpDirPath} | exit()
%% @end
%%--------------------------------------------------------------------
copy_dir_to_tmp_dir(TargetDirPath) ->
    {ok, TmpDirPath} = epkg_util:create_unique_tmp_dir(),
    TargetTmpDirPath = ewl_file:join_paths(TmpDirPath, filename:basename(TargetDirPath)),
    ewl_file:copy_dir(TargetDirPath, TargetTmpDirPath),
    TargetTmpDirPath.
    

%%--------------------------------------------------------------------
%% @doc 
%%  Transform a mix of local paths and local app dirs into a list of 
%%  fully qualified ebin paths and add them to the code loader load path.
%% @spec add_pzs(LibDirs) -> list()
%% @end
%%--------------------------------------------------------------------
add_pzs(LibDirs) -> 
    lists:foreach(fun(LibDir) -> 
			  code:add_pathz(LibDir ++ "/ebin") 
		  end, to_app_dirs(LibDirs)).

%%--------------------------------------------------------------------
%% @doc 
%%  Transform a mix of local paths and local app dirs into a list of 
%%  fully qualified local app dirs.
%%  i.e ["../*"] -> ["/home/martin/otp/lib/resource_discovery", ... ]
%%  @spec to_app_dirs(LibDirs) -> list()
%% @end
%%--------------------------------------------------------------------
to_app_dirs(LibDirs) ->
    lists:foldl(fun(P, Acc) -> 
			case lists:reverse(P) of
			    [$*|_] -> [abs_path(X) || X <- filelib:wildcard(P)] ++ Acc;
			    _      -> [abs_path(P)|Acc]
			end
		end,
		[], LibDirs).
    
%%-------------------------------------------------------------------
%% @doc
%% Return a list of versions. 
%% @todo add ability to specifiy a timeout instead of relying on the default. 
%% @spec repo_list(Url) -> {ok, DirContents} | {error, Reason}
%%  where
%%   DirContents = list()
%% @end
%%-------------------------------------------------------------------
repo_list(Url) ->
    Opts = [{"Connection", "TE"},
	    {"TE", "trailers"},
	    {"Depth", "1"},
	    {"Content-Type", "application/xml"}],
    case ibrowse:send_req(Url, Opts, propfind, "") of
        {ok, "207", _, Body} -> 
	    ?ERROR_MSG("repo_list(~p) -> ~p~n", [Url, "success:207"]),
	    {ok, parse_out_package_versions(Body)};
	{ok, Code, _, _} = Res -> 
	    ?ERROR_MSG("repo_list(~p) -> ~p~n", [Url, Res]),
	    {error, {"No list found. http code: ", Code}};
        {error, _Reason} = Res -> 
	    ?ERROR_MSG("repo_list(~p) -> ~p~n", [Url, Res]),
	    Res;
        {'EXIT', Reason} = Exit -> 
	    ?ERROR_MSG("repo_list(~p) -> ~p~n", [Url, Exit]),
            {error, Reason}
    end.
    
parse_out_package_versions(Body) ->
    {Elem, _} = xmerl_scan:string(Body),
    [filename:basename(E) || E <- tl(lists:sort(xmerl_xs:value_of(xmerl_xs:select("//D:href", Elem))))].

%%--------------------------------------------------------------------
%% @private
%% @doc Fetch the erts version that matches the compiler version of the modules in the application supplied. 
%% @spec get_erts_vsn(AppDirPath) -> {ok, ErtsVsn} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_erts_vsn(AppDirPath) ->
    case get_compiler_vsn(AppDirPath) of
	{ok, CompilerVsn} -> search_static_vsns(CompilerVsn);
	Error             -> Error
    end.

search_static_vsns(CompilerVsn) ->
    search_static_vsns(CompilerVsn, ?COMPILER_VSN_TO_ERTS_VSN_TO_ERLANG_VSN).

search_static_vsns(CompilerVsn, [{CompilerVsn, ErtsVsn, _ErlangVsn}|_]) ->
    {ok, ErtsVsn};
search_static_vsns(CompilerVsn, [_|T]) ->
    search_static_vsns(CompilerVsn, T);
search_static_vsns(CompilerVsn, []) ->
    search_dynamic_vsns(CompilerVsn).


search_dynamic_vsns(CompilerVsn) ->
    %% @todo this function will find the version being looked for in a repo and then return the erts vsn it is found for.
    {error, {no_erts_vsn_found, {compiler_vsn, CompilerVsn}}}.
				 
%%--------------------------------------------------------------------
%% @doc Fetch the compiler version that all modules in the application were compiled with.
%% @spec get_compiler_vsn(AppDirPath) -> {ok, CompilerVsn} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_compiler_vsn(AppDirPath) ->
    {ok, [{modules, Modules}]} = ewr_util:fetch_local_appfile_key_values(AppDirPath, [modules]),
    try
	case Modules of
	    [] ->
		{error, {empty_module_list_for_app, AppDirPath}};
	    Modules ->
		{ok, _CompilerVsn} = Resp = get_compiler_vsn(AppDirPath, Modules, undefined),
		Resp
	end
    catch
	_C:Error ->
	    ?ERROR_MSG("error ~p ~n", [Error]),
	    {error, {bad_module, "found a module compiled with unsuppored version", Error, Modules}}
    end.

get_compiler_vsn(AppDirPath, [Module|Modules], undefined) ->
    CompilerVsn = fetch_vsn(AppDirPath, Module),
    get_compiler_vsn(AppDirPath, Modules, CompilerVsn);
get_compiler_vsn(AppDirPath, [Module|Modules], CompilerVsn) ->
    case catch fetch_vsn(AppDirPath, Module) of
	CompilerVsn ->
	    get_compiler_vsn(AppDirPath, Modules, CompilerVsn);
	Error ->
	    throw(Error)
    end;
get_compiler_vsn(_AppDirPath, [], CompilerVsn) ->
    {ok, CompilerVsn}.
	
fetch_vsn(AppDirPath, Module) ->
    BeamPath  = AppDirPath ++ "/ebin/" ++ atom_to_list(Module),
    {ok, {Module, [{compile_info, CompileInfo}]}} = beam_lib:chunks(BeamPath, [compile_info]),
    case fs_lists:get_val(version, CompileInfo) of
	undefined ->
	    {error, {no_compiler_vsn_found, BeamPath}};
	Vsn ->
	    Vsn
    end.

%%--------------------------------------------------------------------
%% @doc Prompt the user when he is switching erts vsns due to an install. 
%% @spec ask_about_switching_target_ertsVsns(PackageName, PackageVsn, TargetErtsVsn, RemoteErtsVsn) -> bool()
%% @end
%%--------------------------------------------------------------------
ask_about_switching_target_ertsVsns(PackageName, PackageVsn, TargetErtsVsn, RemoteErtsVsn) ->
    {ok, TargetErlangVsn} = faxien:translate_version(erts, erlang, TargetErtsVsn),
    {ok, RemoteErlangVsn} = faxien:translate_version(erts, erlang, RemoteErtsVsn),
    Prompt = lists:flatten(["~nYou are attempting to install ", PackageName, "-", PackageVsn, " which is compiled for~n",
			    RemoteErlangVsn, " (erts: ", RemoteErtsVsn, ").~n",
			    "This version is different from your target version of ",TargetErlangVsn,
			    " (erts: ",TargetErtsVsn,")~n",
			    "*Note* This is generally not a concern unless you manage your versions~n",
			    "closely for special reasons.~n"
			    "Would you like to (p)roceed or (s)top and look for something matching~n",
			    "your current vsn [p|s]"]),
			    
    case ewl_talk:ask(Prompt) of
	"p" ->
	    true;
	"s" ->
	    false;
	Error ->
	    ?INFO_MSG("user entered \"~p\"~n", [Error]),
	    io:format("Please enter \"p\" or \"s\"~n"),
	    ask_about_switching_target_ertsVsns(PackageName, PackageVsn, TargetErtsVsn, RemoteErtsVsn) 
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Resolve a relative path turning it into an absolute path.
%% @end
%%--------------------------------------------------------------------
abs_path(RelativePath) ->
    {ok, InitialPath} = file:get_cwd(),
    file:set_cwd(RelativePath),
    {ok, AbsPath} = file:get_cwd(),
    file:set_cwd(InitialPath),
    AbsPath.


%%----------------------------------------------------------------------------
%% @private
%% @doc Flattens a complex term into a single string.
%% Note* References are not handled they will appear as reference
%% @spec flatten_term(term()) -> string()
%% @end
%%----------------------------------------------------------------------------
flatten_term(Terms) ->
    lists:concat(flattening(Terms)).
                                                                                   
flattening(Atom) when is_atom(Atom) ->
    [atom_to_list(Atom)];
flattening(Float) when is_float(Float) ->
    [float_to_list(Float)];
flattening(Integer) when is_integer(Integer) ->
    [integer_to_list(Integer)];
flattening(Binary) when binary(Binary) ->
    [binary_to_list(Binary)];
flattening(Pid) when pid(Pid) ->
    [pid_to_list(Pid)];
flattening(Ref) when is_reference(Ref) ->
    ["<<reference>>"];
flattening(Tuple) when tuple(Tuple) ->
    Terms = tuple_to_list(Tuple),
    Fun   = fun(Term, Acc) -> acc_check(Term, Acc) end,
    ["{"] ++ lists:foldl(Fun, [], Terms) ++ ["}"];
flattening(Terms) when list(Terms) ->
    case epkg_util:is_string(Terms) of
        true  -> 
            ["\"" ++ Terms ++ "\""];
        false ->
            Fun = fun(Term, Acc) -> acc_check(Term, Acc) end,
            ["["] ++ lists:foldl(Fun, [], Terms) ++ ["]"]
    end.
                                                                                   
acc_check(Term, [])  -> flattening(Term);
acc_check(Term, Acc) -> Acc ++ [", "] ++ flattening(Term).


execute_on_latest_package_version(Repos, TargetErtsVsn, ErtsVsnThreshold, PackageName, Fun, Side, VsnThreshold, ErtsPrompt) ->
    case find_highest_vsn(Repos, get_erts_vsns_gte_than(TargetErtsVsn, ErtsVsnThreshold), PackageName, Side, VsnThreshold) of
	{ok, {Repo, HighVsn, RemoteErtsVsn}} -> 
	    ShortenedRepos = lists:delete(Repo, Repos),
	    case catch execute_fun(Fun, [Repo|ShortenedRepos], PackageName, HighVsn,
				   TargetErtsVsn, RemoteErtsVsn, ErtsPrompt) of
		ok ->
		    ok;
		{ok, NewErtsVsn} -> 
		    execute_on_latest_package_version(Repos, NewErtsVsn, NewErtsVsn, PackageName, Fun, Side, VsnThreshold, ErtsPrompt);
		Error -> 
		    ?ERROR_MSG("failed with ~p for vsn ~p in repo ~p moving on to next repo and setting vsn threshold to ~p~n", 
			       [Error, HighVsn, Repo, HighVsn]), 
		    io:format("Failed operation on ~s at vsn ~s.  Trying for a lower version~n", [PackageName, HighVsn]),
		    execute_on_latest_package_version(ShortenedRepos, TargetErtsVsn, PackageName, Fun, Side, HighVsn, ErtsPrompt)
	    end;
	{error, Reason} ->
	    ?ERROR_MSG("failed to find package: ~p~n", [Reason]),
	    exit("Did not succeed in finding any version of the package")
    end.

execute_fun(Fun, Repos, _PackageName, HighVsn, ErtsVsn, ErtsVsn, _ErtsPrompt) ->
    Fun(Repos, HighVsn, ErtsVsn);
execute_fun(Fun, Repos, _PackageName, HighVsn, _TargetErtsVsn, RemoteErtsVsn, false) ->
    Fun(Repos, HighVsn, RemoteErtsVsn);
execute_fun(Fun, Repos, PackageName, HighVsn, TargetErtsVsn, RemoteErtsVsn, true) ->
    case ask_about_switching_target_ertsVsns(PackageName, HighVsn, TargetErtsVsn, RemoteErtsVsn) of
	true  -> Fun(Repos, HighVsn, RemoteErtsVsn);
	false -> {ok, TargetErtsVsn}
    end.

%%%===================================================================
%%% Testing Functions
%%%===================================================================

