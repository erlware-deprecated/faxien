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


%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 execute_on_latest_package_version/7,
	 execute_on_latest_package_version/5,
	 foreach_erts_vsn/3,
	 foreach_erts_vsn/2,
	 find_highest_vsn/4,
	 find_highest_vsn/5,
	 copy_dir_to_tmp_dir/1,
	 to_app_dirs/1,
	 flatten_term/1,
	 repo_list/1,
	 add_pzs/1,
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
%%  ErtsLowerBound - supply erts vsns from the target to the lowerbound. For example 5.5.5 down to 5.5.3.
%%  Fun - This fun takes two arguments Repos and PackageVsn. This function terminates when the fun returns ok.
%%  VsnThreshold - is used to indicate that the highest version possible is to be installed but no higher than VsnThreshold.
%% </pre>
%%
%% @spec execute_on_latest_package_version(Repos, TargetErtsVsn, ErtsLowerBound, PackageName, 
%%                                                              Fun, Side, VsnThreshold) -> ok | exit
%% where
%%  Side = lib | releases
%%  VsnThreshold = string() | infinity
%% @end
%%-------------------------------------------------------------------
execute_on_latest_package_version([], _TargetErtsVsn, _ErtsLowerBound, _PackageName, _Fun, _Side, _VsnThreshold) ->
    {error, package_not_found};
execute_on_latest_package_version(Repos, TargetErtsVsn, ErtsLowerBound, PackageName, Fun, Side, VsnThreshold) 
  when Side == lib; Side == releases ->
    {ok, {Repo, HighVsn}} = find_highest_vsn(Repos, TargetErtsVsn, PackageName, Side, VsnThreshold),
    ShortenedRepos        = lists:delete(Repo, Repos),
    case catch Fun([Repo|ShortenedRepos], HighVsn) of
	ok -> 
	    ok;
	Error -> 
	    ?ERROR_MSG("failed with ~p for vsn ~p in repo ~p moving on to next repo and setting vsn threshold to ~p~n", 
		       [Error, HighVsn, Repo, HighVsn]), 
	    io:format("Failed operation on ~s at vsn ~s.  Trying for a lower version~n", [PackageName, HighVsn]),
	    execute_on_latest_package_version(ShortenedRepos, TargetErtsVsn, ErtsLowerBound, PackageName, Fun, Side, HighVsn)
    end.

%% @spec execute_on_latest_package_version(Repos, TargetErtsVsn, PackageName, Fun, Side) -> ok | exit()
%% @equiv execute_on_latest_package_version(Repos, TargetErtsVsn, DefaultErtsLowerBound, PackageName, Fun, Side, infinity) 
execute_on_latest_package_version(Repos, TargetErtsVsn, PackageName, Fun, Side) ->
    ErtsLowerBound = erts_lower_bound_from_target(TargetErtsVsn),
    execute_on_latest_package_version(Repos, TargetErtsVsn, ErtsLowerBound, PackageName, Fun, Side, infinity).

%%-------------------------------------------------------------------
%% @doc
%% Return the latest version lower than the VsnThreshold and repo that contains it for a given package and repos.
%%
%% <pre>
%% Variables:
%%  VsnThreshold - is used to indicate that the highest version possible is to be found but no higher than VsnThreshold.
%% </pre>
%%
%% @spec find_highest_vsn(Repos, TargetErtsVsn, PackageName, Side, VsnThreshold) -> {ok, {Repo, Vsn}} | {error, Reason}
%%  where 
%%   Side = lib | releases
%%   VsnThreshold = string() | infinity
%% @end
%%-------------------------------------------------------------------
find_highest_vsn(Repos, TargetErtsVsn, PackageName, Side, VsnThreshold) ->
    case catch find_highest_vsn2(Repos, TargetErtsVsn,  PackageName, Side, VsnThreshold) of
	{ok, {_Repo, _HighVsn}} = Reply -> Reply;
	Exp                             -> {error, {package_not_found, PackageName, Exp}}
    end.
				 
find_highest_vsn2(Repos, TargetErtsVsn, PackageName, Side, VsnThreshold) ->
    VsnLists = lists:flatten(lists:foldl(fun(Repo, Acc) -> 
						 SysInfo  = ewr_util:system_info(),
						 Suffixes = ewr_util:gen_multi_erts_repo_stub_suffix(TargetErtsVsn, 
												     PackageName,
												     [SysInfo, "Generic"], 
												     Side),
				   lists:foldl(fun(Suf, Acc2) -> 
						       ?INFO_MSG("Checking for highest version of ~p in ~s/~s~n", 
									     [PackageName, Repo, Suf]),
						       case repo_list(Repo ++ "/" ++ Suf) of
							   {ok, Vsns} -> 
							       ?INFO_MSG("found vsns ~p~n", [Vsns]),
							       [[{Repo, Vsn} || Vsn <- lists:reverse(Vsns)]|Acc2]; 
							   {error, _Reason} -> 
							       Acc2
						       end
					       end, Acc, Suffixes)
			   end, [], Repos)),
    ?INFO_MSG("find_highest_vsn list of remote versions ~p~n", [VsnLists]),
    {Repo, HighVsn} =
	case VsnThreshold of
	    infinity ->
		hd(lists:sort(fun({_, V1}, {_, V2}) -> ewr_util:is_version_greater(V1, V2) end, VsnLists));
	    VsnThreshold ->
		lop_off_top(VsnLists, VsnThreshold)
	end,
    ?INFO_MSG("find_highest_vsn list of remote versions ~p with threshold of ~p found highest in ~p~n", 
	      [VsnLists, VsnThreshold, {Repo, HighVsn}]),
    {ok, {Repo, HighVsn}}.


%% @equiv find_highest_vsn(Repos, TargetErtsVsn, PackageName, Side, infinity)
%% @spec find_highest_vsn(Repos, TargetErtsVsn, PackageName, Side) -> {ok, {Repo, Vsn}} | {error, Reason}
find_highest_vsn(Repos, TargetErtsVsn, PackageName, Side) ->
    find_highest_vsn(Repos, TargetErtsVsn, PackageName, Side, infinity).

lop_off_top(VsnLists, VsnThreshold) ->
    SortedVsnLists = lists:sort(fun({_, V1}, {_, V2}) -> ewr_util:is_version_greater(V1, V2) end, VsnLists),
    lop_off_top2(SortedVsnLists, VsnThreshold).

lop_off_top2([{Repo, Vsn}|T], VsnThreshold) ->
    case ewr_util:is_version_greater(VsnThreshold, Vsn) of
	true  -> {Repo, Vsn};
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



	    
%%-------------------------------------------------------------------
%% @doc For each patch compatible erts vsn less than or equal to the TargetErtsVSn call the fun 
%% with a single erts vsn in the series. The fun takes a single arg - ErtsVsn::string(). 
%% @spec foreach_erts_vsn(TargetErtsVsn, ErtsLowerBound, Fun) -> ok | Error 
%% @end
%%-------------------------------------------------------------------
foreach_erts_vsn(TargetErtsVsn, ErtsLowerBound, Fun) ->
    ?INFO_MSG("erts versions from ~s to ~s~n", [TargetErtsVsn, ErtsLowerBound]),
    [MajorErtsVsn, MinorErtsVsn, HighPatchErtsVsn] = string:tokens(TargetErtsVsn, "."),
    case catch string:tokens(ErtsLowerBound, ".") of
	[MajorErtsVsn, MinorErtsVsn, LowPatchErtsVsn] ->
	    ErtsVsns = [lists:flatten([MajorErtsVsn, ".", MinorErtsVsn, ".", integer_to_list(E)]) || 
			   E <- lists:seq(list_to_integer(LowPatchErtsVsn), list_to_integer(HighPatchErtsVsn))], 
	    fs_lists:do_until(fun(ErtsVsn) -> (catch Fun(ErtsVsn)) end, ok, lists:reverse(ErtsVsns));
	_Error ->
	    {error, {bad_erts_lower_bound, ErtsLowerBound}}
    end.

%% @equiv foreach_erts_vsn(TargetErtsVsn, DEFAULT_LOWEST_VSN, Fun) 
%% @spec foreach_erts_vsn(TargetErtsVsn, Fun) -> ok | Error
foreach_erts_vsn(TargetErtsVsn, Fun) ->
    ErtsLowerBound = erts_lower_bound_from_target(TargetErtsVsn),
    foreach_erts_vsn(TargetErtsVsn, ErtsLowerBound, Fun).


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
    search_static_vsns(CompilerVsn, ?COMPILER_VSN_TO_ERTS_VSN).

search_static_vsns(CompilerVsn, [{CompilerVsn, ErtsVsn}|_]) ->
    {ok, ErtsVsn};
search_static_vsns(CompilerVsn, [_|T]) ->
    search_static_vsns(CompilerVsn, T);
search_static_vsns(CompilerVsn, []) ->
    search_dynamic_vsns(CompilerVsn).


search_dynamic_vsns(_CompilerVsn) ->
    %% @todo this function will find the version being looked for in a repo and then return the erts vsn it is found for.
    {error, no_erts_vsn_found}.
				 

%%--------------------------------------------------------------------
%% @private
%% @doc Fetch the compiler version that all modules in the application were compiled with.
%% @spec get_compiler_vsn(AppDirPath) -> {ok, CompilerVsn} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_compiler_vsn(AppDirPath) ->
    {ok, [{modules, Modules}]} = ewr_util:fetch_local_appfile_key_values(AppDirPath, [modules]),
    try
	{ok, _CompilerVsn} = Resp = get_compiler_vsn(AppDirPath, Modules, undefined),
	Resp
    catch
	_C:Error ->
	    ?ERROR_MSG("error ~p ~n", [Error]),
	    {error, {bad_module, "found a module compiled with unsuppored version", Modules}}
    end.

get_compiler_vsn(AppDirPath, [Module|Modules], undefined) ->
    CompilerVsn = fetch_vsn(AppDirPath, Module),
    get_compiler_vsn(AppDirPath, Modules, CompilerVsn);
get_compiler_vsn(AppDirPath, [Module|Modules], CompilerVsn) ->
    case catch fetch_vsn(AppDirPath, Module) of
	CompilerVsn ->
	    get_compiler_vsn(AppDirPath, Modules, CompilerVsn);
	_ ->
	    throw({bad_module, Module})
    end;
get_compiler_vsn(_AppDirPath, [], CompilerVsn) ->
    {ok, CompilerVsn}.
	
fetch_vsn(AppDirPath, Module) ->
    BeamPath  = AppDirPath ++ "/ebin/" ++ atom_to_list(Module),
    {ok, {Module, [{compile_info, CompileInfo}]}} = beam_lib:chunks(BeamPath, [compile_info]),
    fs_lists:get_val(version, CompileInfo).
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


%%--------------------------------------------------------------------
%% @private
%% @doc return the lowest erts version acceptable based on the TargetErtsVsn
%%--------------------------------------------------------------------
erts_lower_bound_from_target(TargetErtsVsn) ->
    [MajorErtsVsn, MinorErtsVsn, _HighPatchErtsVsn] = string:tokens(TargetErtsVsn, "."),
    lists:flatten([MajorErtsVsn,".", MinorErtsVsn,".0"]).
    


%%%===================================================================
%%% Testing Functions
%%%===================================================================

