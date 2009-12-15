%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @doc Misc shared functions used across epkg
%%% 
%%% @end
%%% @copyright (C) 2007, Martin Logan, Eric Merritt, Erlware
%%% Created : 14 Dec 2007 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epkg_util).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([
	 multi_config_paths/0,
	 md5/1,
	 remove_tuple_dups/2,
	 highest_vsn/1,
	 get_current_release_version/1,
	 is_string/1,
	 overwrite_yes_no/4,
	 overwrite_yes_no/3,
	 create_unique_tmp_dir/1,
	 create_unique_tmp_dir/0,
	 set_executable_perms/1,
	 set_all_executable_perms/1,
	 unpack_to_tmp/1,
	 unpack_to_tmp_if_archive/1,
	 foreach_erts_vsn/3,
	 foreach_erts_vsn/2,
	 all_erts_vsns/0,
	 all_erts_vsns/1,
	 all_erts_vsns/3,
	 erts_series/2,
	 erts_series/1,
	 erts_lower_bound_from_target/1,
	 find_bad_control_categories/1,
	 if_atom_or_integer_to_string/1,
	 consult_rel_file/2,
	 consult_control_file/2,
	 get_sinan_build_flavor/1,
	 ask_about_string_in_list/2,
	 discover_app_erts_vsns/1,
	 collect_name_and_high_vsn_pairs/1
	]).

-include("eunit.hrl").

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("epkg.hrl").
-include("ewrepo.hrl").
-include("eunit.hrl").
-include("macros.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc return a list of faxiens config files in overide order
%% @spec () -> string() | [string()]
%% @end
%%--------------------------------------------------------------------
multi_config_paths() ->
    case gas_override_config:override_file_path() of
	undefined          -> epkg_installed_paths:installed_config_file_path();
	{ok, HomeFilePath} -> [HomeFilePath, epkg_installed_paths:installed_config_file_path()]
    end.

%%--------------------------------------------------------------------
%% @TODO move this into fs_lists
%% @doc Remove the duplicates in a list of sorted tuples based on a
%% particular element in each tuple. 
%% @spec (Element, TupleList) -> NewList
%% @end
%%--------------------------------------------------------------------
remove_tuple_dups(Element, [E1|T]) ->
    case remove_tuple_dups(Element, T) of
	[]                                                           -> [E1];
	[E2|_] = L when element(Element, E1) == element(Element, E2) -> L;
	L          when is_list(L)                                   -> [E1|L];
	E2                                                           -> [E1, E2]
    end;
remove_tuple_dups(_Element, []) ->
    [].
    
%%--------------------------------------------------------------------
%% @doc given a list of versioned apps return a list of each app
%%      with its highest found version.
%% <pre>
%% Example [{"faxien", "2.2"}, {"sinan", "5.6"}] = collect_name_and_high_vsn_pairs(["faxien-1.4", "faxien-2.2", "sinan-5.6"]).
%% </pre>
%% @spec collect_name_and_high_vsn_pairs(Packages::list()) -> list() 
%% @end
%%--------------------------------------------------------------------
collect_name_and_high_vsn_pairs(Packages) ->
    PackageAndVsns = lists:foldl(fun(Package, Acc) ->
					 try
					     {ok, {Name, Vsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(Package),
					     [{Name, Vsn}|Acc]
					 catch
					     _C:E ->
						 ?ERROR_MSG("Skipping ~p because ~p~n", [Package, E]),
						 Acc
					 end
			       end, [], Packages),
    lists:map(fun({Package, Vsns}) -> {Package, epkg_util:highest_vsn(Vsns)} end, collect_vsns(PackageAndVsns)).
    
collect_vsns(PackageAndVsns) ->
    Sorted = lists:sort(fun({Name, _}, {Name2, _}) -> Name < Name2 end, PackageAndVsns),
    collect_vsns(Sorted, []).

collect_vsns([{Package, Vsn}|T], [{Package, Vsns}|Acc]) ->
    collect_vsns(T, [{Package, [Vsn|Vsns]}|Acc]);
collect_vsns([{Package, Vsn}|T], Acc) ->
    collect_vsns(T, [{Package, [Vsn]}|Acc]);
collect_vsns([], Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc Select one build flavor among potentially many.
%% @spec get_sinan_build_flavor(ProjectRoot) -> string()
%% @end
%%--------------------------------------------------------------------
get_sinan_build_flavor(ProjectRoot) ->
    case ewl_sinan_paths:get_build_flavors(ProjectRoot) of
	[BuildFlavor] ->
	    BuildFlavor;
	[] ->
	    throw(project_not_built);
	BuildFlavors ->
	    ask_about_build_flavor(BuildFlavors)
    end.

ask_about_build_flavor(BuildFlavors) ->
    FlavorsString = string:join(BuildFlavors, ", "),
    Prompt = lists:flatten(["The following build flavors have been found ", FlavorsString, 
			    " please type the name of the one you would like to publish from >"]),
    Flavor = ewl_talk:ask(Prompt),
    case lists:member(Flavor, BuildFlavors) of
	true ->
	    Flavor;
	false ->
	    ?INFO_MSG("user entered \"~p\"~n", [Flavor]),
	    io:format("That is not a valid build flavor please retry~n"),
	    ask_about_build_flavor(BuildFlavors)
    end.

%%--------------------------------------------------------------------
%% @doc Given a list of strings ask the user one by one if each string
%%      should remain in the final list. The prompt prefix is appended
%%      to the begining of the prompt the users sees.
%% @spec ask_about_string_in_list(List::list(), PromptPrefix::string()) -> NewList::list()
%% @end
%%--------------------------------------------------------------------
ask_about_string_in_list(Paths, PromptPrefix) ->
    ask_about_string_in_list(Paths, Paths, PromptPrefix, []).
	    
ask_about_string_in_list([], _Paths, _PromptPrefix, Acc) ->
    Acc;
ask_about_string_in_list([Path|T] = CurrentPaths, Paths, PromptPrefix, Acc) ->
    Prompt = lists:flatten([PromptPrefix, Path, "~nEnter (y)es, (n)o, or yes to (a)ll? > "]),
    case ewl_talk:ask(Prompt) of
	"y" ->
	    ask_about_string_in_list(T, Paths, PromptPrefix, [Path|Acc]);
	"n" ->
	    ask_about_string_in_list(T, Paths, PromptPrefix, Acc);
	"a" ->
	    Paths;
	Error ->
	    ?INFO_MSG("user entered \"~p\"~n", [Error]),
	    io:format("Please enter 'y', 'n', or 'a'~n"),
	    ask_about_string_in_list(CurrentPaths, Paths, PromptPrefix, Acc)
    end.

%%--------------------------------------------------------------------
%% @doc find the highest version in a list of version strings.
%% @spec (Vsns::list()) -> Vsn::string()
%% @end
%%--------------------------------------------------------------------
highest_vsn(Vsns) when length(Vsns) > 0 ->
    hd(lists:sort(fun(A, B) -> ewr_util:is_version_greater(A, B) end, Vsns));
highest_vsn([]) ->
    [].

%%--------------------------------------------------------------------
%% @doc 
%%  Return the version of the current specified release.
%% @spec get_current_release_version(RelName) -> string()
%% @end
%%--------------------------------------------------------------------
get_current_release_version(RelName) -> 
    {value, {RelName, _, Vsn}} = lists:keysearch(RelName, 1, application:which_applications()),
    Vsn.

%%-------------------------------------------------------------------
%% @doc For each patch compatible erts vsn less than or equal to the TargetErtsVSn call the fun 
%% with a single erts vsn in the series. The fun takes a single arg - ErtsVsn::string(). 
%% @spec foreach_erts_vsn(TargetErtsVsn, ErtsLowerBound, Fun) -> ok | Error 
%% @end
%%-------------------------------------------------------------------
foreach_erts_vsn(TargetErtsVsn, ErtsLowerBound, Fun) ->
    ?INFO_MSG("erts versions from ~s to ~s~n", [TargetErtsVsn, ErtsLowerBound]),
    ErtsVsns = erts_series(TargetErtsVsn, ErtsLowerBound),
    fs_lists:do_until(fun(ErtsVsn) -> (catch Fun(ErtsVsn)) end, ok, lists:reverse(ErtsVsns)).

%% @equiv foreach_erts_vsn(TargetErtsVsn, DEFAULT_LOWEST_VSN, Fun) 
%% @spec foreach_erts_vsn(TargetErtsVsn, Fun) -> ok | Error
foreach_erts_vsn(TargetErtsVsn, Fun) ->
    ErtsLowerBound = erts_lower_bound_from_target(TargetErtsVsn),
    foreach_erts_vsn(TargetErtsVsn, ErtsLowerBound, Fun).

%%-------------------------------------------------------------------
%% @doc Return a list of erts vsns from the target vsn to the lower bound For example if 
%%      the target version is 5.6.2 then the series would be ["5.6.2", "5.6.1", "5.6"]
%% @spec erts_series(TargetErtsVsn, ErtsLowerBound) -> ok | Error 
%% @end
%%-------------------------------------------------------------------
erts_series(TargetErtsVsn, ErtsLowerBound) ->
    ?INFO_MSG("erts versions from ~s to ~s~n", [TargetErtsVsn, ErtsLowerBound]),
    [MajorErtsVsn, MinorErtsVsn, HighPatchErtsVsn] = string:tokens(TargetErtsVsn, "."),
    case catch string:tokens(ErtsLowerBound, ".") of
	[MajorErtsVsn, MinorErtsVsn, LowPatchErtsVsn] ->
	    create_series(MajorErtsVsn, MinorErtsVsn, HighPatchErtsVsn, LowPatchErtsVsn);
	[MajorErtsVsn, MinorErtsVsn] ->
	    create_series(MajorErtsVsn, MinorErtsVsn, HighPatchErtsVsn, "0"); 
	_Error ->
	    {error, {bad_erts_lower_bound, ErtsLowerBound}}
    end.

create_series(MajorErtsVsn, MinorErtsVsn, HighPatchErtsVsn, LowPatchErtsVsn) ->
    lists:reverse(
      lists:map(fun(PatchVsn) when PatchVsn > 0 ->
		      lists:flatten([MajorErtsVsn, ".", MinorErtsVsn, ".", integer_to_list(PatchVsn)]);
		 (0) ->
		      lists:flatten([MajorErtsVsn, ".", MinorErtsVsn])
	      end,
	      lists:seq(list_to_integer(LowPatchErtsVsn), list_to_integer(HighPatchErtsVsn)))).

%%-------------------------------------------------------------------
%% @doc Return a list of erts vsns from the target vsn to the lowest patch compatible version. For example if 
%%      the target version is 5.6.2 then the series would be ["5.6.2", "5.6.1", "5.6"]
%% @equiv erts_series(TargetErtsVsn, DEFAULT_LOWEST_VSN) 
%% @spec erts_series(TargetErtsVsn) -> ok | Error
%% @end
%%-------------------------------------------------------------------
erts_series(TargetErtsVsn) ->
    ErtsLowerBound = erts_lower_bound_from_target(TargetErtsVsn),
    erts_series(TargetErtsVsn, ErtsLowerBound).

%%--------------------------------------------------------------------
%% @doc return the lowest erts version acceptable based on the TargetErtsVsn
%% `example: erts_lower_bound_from_target("5.5.5") -> "5.5"'
%% @spec erts_lower_bound_from_target(TargetErtsVsn) -> string()
%% @end
%%--------------------------------------------------------------------
erts_lower_bound_from_target(TargetErtsVsn) ->
    [MajorErtsVsn, MinorErtsVsn|_] = string:tokens(TargetErtsVsn, "."),
    lists:flatten([MajorErtsVsn,".", MinorErtsVsn]).

%%--------------------------------------------------------------------
%% @doc return all known erts vsns
%% @spec () -> string()
%% @end
%%--------------------------------------------------------------------
all_erts_vsns() ->
    [ErtsVsn || {_, ErtsVsn, _} <- ?COMPILER_VSN_TO_ERTS_VSN_TO_ERLANG_VSN].

%%--------------------------------------------------------------------
%% @doc return all known erts vsns where the versions are greater or
%%      equal to the LowBound
%% @spec (LowBound::string()) -> string()
%% @end
%%--------------------------------------------------------------------
all_erts_vsns(LowBound) ->
    A = [ErtsVsn || ErtsVsn <- all_erts_vsns(), ewr_util:is_version_greater(ErtsVsn, LowBound) orelse LowBound == ErtsVsn],
    [H|T] = lists:reverse(A),
    [H|lists:reverse(T)].

%%--------------------------------------------------------------------
%% @doc return all known erts vsns in decending order where all
%%      versions are are greater or equal to the LowBound less than or
%%      equal to the HighBound but where the preferred version is
%%      listed first.
%% Example: ["5.6.1", "5.6.3", "5.6.2", "5.6"] =  all_erts_vsns("5.6", "5.6.3", "5.6.1").
%% @spec (LowBound::string(), HighBound::string(), PreferredBound::string()) -> string()
%% @end
%%--------------------------------------------------------------------
all_erts_vsns(LowBound, HighBound, PreferredVsn) ->
    [PreferredVsn|
     [ErtsVsn ||
	 ErtsVsn <- all_erts_vsns(),
	 (ewr_util:is_version_greater(ErtsVsn, LowBound) orelse LowBound == ErtsVsn),
	 ((not ewr_util:is_version_greater(ErtsVsn, HighBound)) orelse HighBound == ErtsVsn),
	 not (ErtsVsn == PreferredVsn)
     ]
    ].

%%----------------------------------------------------------------------------
%% @doc Checks to see if a list is a string.
%% @spec is_string(List) -> bool()
%% @end
%%----------------------------------------------------------------------------
is_string([])                          -> true;
is_string(Term) when not is_list(Term) -> false;
is_string(Term)                        -> not lists:any(fun(Element) -> not_string_p1(Element) end, Term).
                                                                                   
not_string_p1(H) when is_integer(H), H >= $\s, H < 255 -> false;
not_string_p1($\n)                                  -> false;
not_string_p1($\r)                                  -> false;
not_string_p1($\t)                                  -> false;
not_string_p1($\v)                                  -> false;
not_string_p1($\b)                                  -> false;
not_string_p1($\f)                                  -> false;
not_string_p1($\e)                                  -> false;
not_string_p1(_)                                    -> true.
    
%%--------------------------------------------------------------------
%% @doc helper for overwriting previously instlaled applications. If force is set to true then the overwrite takes place, 
%%      FunOnYes is executed, without prompting the user.  If false is set then the user is prompted. This function 
%%      returns the results of whicever fun is executed.
%%     
%% @spec overwrite_yes_no(FunOnYes, FunOnNo, Target, Force::bool()) -> term()
%% @end
%%--------------------------------------------------------------------
overwrite_yes_no(FunOnYes, _FunOnNo, Target, true) when is_list(Target) ->
    overwrite(FunOnYes, Target);
overwrite_yes_no(FunOnYes, FunOnNo, Target, false) when is_list(Target) ->
    case ewl_talk:ask([Target, " is already installed. Overwrite [yes|no]"]) of
	Yes when Yes == $y; Yes == $Y; Yes == "yes" ->
	    overwrite(FunOnYes, Target);
	No when No == $n; No == $N; No == "no" ->
	    FunOnNo();
	Error ->
	    ?INFO_MSG("user entered \"~p\"~n", [Error]),
	    io:format("Please enter \"yes\" or \"no\"~n"),
	    overwrite_yes_no(FunOnYes, FunOnNo, Target)
    end.

overwrite_yes_no(FunOnYes, FunOnNo, Target) ->
    overwrite_yes_no(FunOnYes, FunOnNo, Target, false).

overwrite(FunOnYes, Target) ->    
    ewl_file:delete_dir(Target),
    FunOnYes().

%%--------------------------------------------------------------------
%% @doc just a funny little helper to convert elements of a list that
%%      are atoms or integers into strings, and leaves them alone if they are not.
%% @end
%%--------------------------------------------------------------------
if_atom_or_integer_to_string(Arg) when is_integer(Arg) ->
    integer_to_list(Arg);
if_atom_or_integer_to_string(Arg) when is_atom(Arg) ->
    atom_to_list(Arg);
if_atom_or_integer_to_string(List) ->
    case is_string(List) of
	true ->
	    List;
	false ->
	    lists:map(fun(V) when is_atom(V)    -> atom_to_list(V);
			 (V) when is_integer(V) -> integer_to_list(V);
			 (V)                    -> V 
		      end, List)
    end.    
%%----------------------------------------------------------------------------
%% @doc Applies executable permissions to the file provided. 
%% @spec set_executable_perms(Filename) -> ok | {error, Reason}
%% @end
%%----------------------------------------------------------------------------
set_executable_perms(Filename) ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    ?INFO_MSG("epkg_util:set_executable_perms(~p) SysArch was win32~n", [Filename]),
	    io:format("Win 32 is not yet supported.~n");
	SysArch ->
	    ?INFO_MSG("epkg_util:set_executable_perms(~p) SysArch was ~p~n", [Filename, SysArch]),
	    case os:cmd("chmod a+x " ++ Filename) of
		[]    ->
		    ok;
		Error ->
		    ?ERROR_MSG("got the following result when setting executable perms ~p~n", [Error]),
		    {error, Error}
	    end
    end.

%%----------------------------------------------------------------------------
%% @doc Applies executable permissions recursively to all files in the directory.
%% @spec set_all_executable_perms(Dir) -> ok | {error, Reason}
%% @end
%%----------------------------------------------------------------------------
set_all_executable_perms(Dir) ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    ?INFO_MSG("epkg_util:set_executable_perms(~p) SysArch was win32~n", [Dir]),
	    io:format("Win 32 is not yet supported.~n");
	SysArch ->
	    ?INFO_MSG("epkg_util:set_executable_perms(~p) SysArch was ~p~n", [Dir, SysArch]),
	    case os:cmd("chmod -R a+x " ++ Dir) of
		[]    ->
		    ok;
		Error ->
		    ?ERROR_MSG("got the following result when setting executable perms ~p~n", [Error]),
		    {error, Error}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc Check a list of categories from a control file and return any bad ones.
%% @spec find_bad_control_categories(Categories) -> list()
%% @end
%%--------------------------------------------------------------------
find_bad_control_categories(Categories) ->
    lists:filter(fun(Category) -> 
			 not lists:member(Category, ?CONTROL_CATEGORIES) 
		 end, 
		 Categories).

%%--------------------------------------------------------------------
%% @doc take an compressed artifact and unpack it into a unique temporary directory and 
%%      return a path or paths to the resulting artifacts.
%% @spec unpack_to_tmp(ArtifactFilePath) -> Path | Paths
%% where
%%  Paths = [Path]
%%  Path = string()
%% @end
%%--------------------------------------------------------------------
unpack_to_tmp(ArtifactFilePath) ->
    try
	{ok, TmpDirPath}   = create_unique_tmp_dir(),
	ArtifactFileName    = filename:basename(filename:absname(ArtifactFilePath)),
	TmpArtifactFilePath = ewl_file:join_paths(TmpDirPath, ArtifactFileName),
	{ok, _}            = file:copy(ArtifactFilePath, TmpArtifactFilePath),
	{ok, CWD}          = file:get_cwd(),
	ok                 = file:set_cwd(TmpDirPath),
	ok                 = ewl_file:uncompress(ArtifactFileName),
	ok                 = file:delete(ArtifactFileName),
	ok                 = file:set_cwd(CWD),
	case filelib:wildcard(TmpDirPath ++ "/*") of
	    [TmpArtifactPath]                                  -> TmpArtifactPath;
	    TmpArtifactPaths when length(TmpArtifactPaths) > 1 -> TmpArtifactPaths
	end
    catch
	_Class:Exception ->
	    throw({"could not create tmp directory", Exception})
    end.

%%--------------------------------------------------------------------
%% @doc if a package is a tarball then untar it into a tmp dir and hand back the path(s) to the unpacked contents of the temp dir.
%% @spec unpack_to_tmp_if_archive(PackageDirPath::string()) -> DirPath::string()
%% @end
%%--------------------------------------------------------------------
unpack_to_tmp_if_archive(ArchiveFilePath) ->
    case re:run(ArchiveFilePath, ".*" ++ ?REPO_FILE_EXT_REGEXP ++ "$") of
	{match, _} ->
	    epkg_util:unpack_to_tmp(ArchiveFilePath);
	_NoMatch ->
	    ArchiveFilePath
    end.

%%--------------------------------------------------------------------
%% @doc create a unique temorory directory for staging packages.
%% @spec create_unique_tmp_dir(Prefix::string()) -> {ok, TmpDirPath} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
create_unique_tmp_dir(Prefix) ->
    TmpDirPath = lists:flatten([Prefix, "/ewl_tmp-", integer_to_list(element(3, now())), "/"]) ,
    case ewl_file:mkdir_p(TmpDirPath) of
	ok    -> {ok, TmpDirPath};
	Error -> Error
    end.
	     
%% @spec create_unique_tmp_dir() -> {ok, TmpDirPath} | {error, Reason}
%% @equiv create_unique_tmp_dir(TmpDir)
create_unique_tmp_dir() ->
    create_unique_tmp_dir("/tmp").

%%--------------------------------------------------------------------
%% @doc Returns an element or list of elements from a control file. 
%% @spec consult_control_file(Keys, ControlFilePath) -> [Value] | Value | {error, Reason}
%% where
%%  Keys = Key | [Key]
%%  Reason = badly_formatted_control_file | enoent 
%% @end
%%--------------------------------------------------------------------
consult_control_file(Key, ControlFilePath) when is_atom(Key) ->
    case consult_control_file([Key], ControlFilePath) of
	[Value] -> Value;
	Error   -> Error
    end;
consult_control_file(Keys, ControlFilePath) ->
    ?INFO_MSG("consulting ~s~n", [ControlFilePath]),
    case file:consult(ControlFilePath) of
	{ok, [{control, _PackageName, ControlList}]} ->
	    lists:foldr(fun(Key, Acc) -> 
				case fs_lists:get_val(Key, ControlList) of
				    undefined -> Acc;
				    Value     -> [Value|Acc]
				end
			end, [], Keys);
	Error = {error, _} ->
	    Error;
	{ok, _BadTerm} ->
	    {error, badly_formatted_control_file}
    end.


%%--------------------------------------------------------------------
%% @doc Returns a list of the elements that correspond to the keys that were supplied.
%% @spec consult_rel_file(Keys, RelFilePath) -> [Value] | Value | {error, Reason}
%% where
%%  Keys = Key | [Key]
%%   Key = release_name | release_vsn | erts_vsn | app_specs
%%  Reason = badly_formatted_rel_file | enoent 
%% @end
%%--------------------------------------------------------------------
consult_rel_file(Key, RelFilePath) when is_atom(Key) ->
    [Value] = consult_rel_file([Key], RelFilePath),
    Value;
consult_rel_file(Keys, RelFilePath) ->
    ?INFO_MSG("consulting ~s for ~p~n", [RelFilePath, Keys]),
    case file:consult(RelFilePath) of
	{ok, [RelTerm]}    -> lists:map(fun(Key) -> extract_rel_value(Key, RelTerm) end, Keys);
	Error = {error, _} -> Error
    end.
    
extract_rel_value(release_name, {release, {Name, _}, _, _}) ->
    Name;
extract_rel_value(release_vsn, {release, {_, Vsn}, _, _}) ->
    Vsn;
extract_rel_value(erts_vsn, {release, _, {erts, ErtsVsn}, _}) ->
    ErtsVsn;
extract_rel_value(app_specs, {release, _, _, AppSpecs}) ->
    AppSpecs;
extract_rel_value(_, _Junk) ->
    {error, badly_formatted_rel_file}.
    
%%-------------------------------------------------------------------
%% @doc return the hex encoded md5 string for a binary
%% @spec (List) -> string()
%% @end
%%-------------------------------------------------------------------
md5(List) -> hex(binary_to_list(erlang:md5(List))).

hex(L) when is_list (L) -> lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f -> [hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I)               -> [$0, hex0(I)].

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 + I.


%%--------------------------------------------------------------------
%% @doc Discover what erts versions all beams in an app were compiled with.
%% @spec discovery_app_erts_vsns(AppDirPath) -> {ok, [ErtsVsn]} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
discover_app_erts_vsns(AppDirPath) ->
    case length(filelib:wildcard(AppDirPath ++ "/ebin/*beam")) of
	0 ->
	    {error, no_beam_files};
	_NumBeams ->
	    case get_compiler_vsn(AppDirPath) of
		{ok, CompilerVsns}  -> {ok, lists:map(fun(CV) -> search_static_vsns(CV) end, CompilerVsns)};
		Error               -> Error
	    end
    end.

%%====================================================================
%% Internal functions
%%====================================================================
search_static_vsns(CompilerVsn) ->
    search_static_vsns(CompilerVsn, ?COMPILER_VSN_TO_ERTS_VSN_TO_ERLANG_VSN).

search_static_vsns(CompilerVsn, [{CompilerVsn, ErtsVsn, _ErlangVsn}|_]) ->
    ErtsVsn;
search_static_vsns(CompilerVsn, [_|T]) ->
    search_static_vsns(CompilerVsn, T);
search_static_vsns(CompilerVsn, []) ->
    search_dynamic_vsns(CompilerVsn).


search_dynamic_vsns(CompilerVsn) ->
    %% @todo this function will find the version being looked for in a repo and then return the erts vsn it is found for.
    {error, {no_erts_vsn_found, {compiler_vsn, CompilerVsn}}}.
				 
%%--------------------------------------------------------------------
%% @private
%% @doc Fetch the compiler version that all modules in the application were compiled with.
%% @spec get_compiler_vsn(AppDirPath) -> {ok, [CompilerVersion]} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_compiler_vsn(AppDirPath) ->
    {ok, [{modules, Modules}]} = ewr_util:fetch_local_appfile_key_values(AppDirPath, [modules]),
    try
	case Modules of
	    [] ->
		{error, {empty_module_list_for_app, AppDirPath}};
	    Modules ->
		{ok, CompilerVsns} = get_compiler_vsn(AppDirPath, Modules, []),
		{ok, sort_vsn_list(CompilerVsns)}
	end
    catch
	_C:Error ->
	    {error, {bad_module, "application compiled improperly or with unsupported version", Error, Modules}}
    end.

get_compiler_vsn(AppDirPath, [Module|Modules], CompilerVsns) ->
    case catch fetch_vsn(AppDirPath, Module) of
        missing_module ->
	    % Module was missing, no compiler info available, but since the file doesn't
	    % exist, the compiler info is irrelevant; log a warning but continue on
            ?INFO_MSG("WARNING: ~p beam file listed in .app, but doesn't actually exist!",
		      [Module]),
            get_compiler_vsn(AppDirPath, Modules, CompilerVsns);
	CompilerVsn ->
	    case lists:member(CompilerVsn, CompilerVsns) of
		true ->
		    get_compiler_vsn(AppDirPath, Modules, CompilerVsns);
		false ->
		    get_compiler_vsn(AppDirPath, Modules, [CompilerVsn|CompilerVsns])
	    end
    end;
get_compiler_vsn(_AppDirPath, [], CompilerVsns) ->
    {ok, CompilerVsns}.
	
fetch_vsn(AppDirPath, Module) ->
    BeamPath  = AppDirPath ++ "/ebin/" ++ atom_to_list(Module),
    case beam_lib:chunks(BeamPath, [compile_info]) of
        {ok, {Module, [{compile_info, CompileInfo}]}} ->
            case fs_lists:get_val(version, CompileInfo) of
                undefined ->
                    {error, {no_compiler_vsn_found, BeamPath}};
                Vsn ->
                    Vsn
            end;
        {error, beam_lib, {file_error, _, enoent}} ->
            %% Arguably, if a .beam is listed in a .app, it shouldn't cause the 
            %% entire publish to fail. We know of at least one case in the core Erlang
            %% distribution (hipe) where modules are listed that actually live within
            %% the VM. Therefore, notify the caller that the module doesn't exist
            %% but don't make everything blow up.
            missing_module;
        Error ->
            Error
    end.

sort_vsn_list(VsnList) ->
    lists:sort(fun(V1, V2) -> ewr_util:is_version_greater(V1, V2) end, VsnList).

%%%===================================================================
%%% Testing Functions
%%%===================================================================
is_string_test() ->
    ?assertMatch(false, is_string([hello])),
    ?assertMatch(false, is_string({})),
    ?assertMatch(true, is_string("hell]o")),
    ?assertMatch(true, is_string("hello")).

md5_test() ->
    ?assertMatch("b1946ac92492d2347c6235b4d2611184", md5("hello\n")).

    
all_erts_vsns_test() ->
    ?assertMatch(["5.6.1", "5.6.3", "5.6.2", "5.6", "5.5.5"], all_erts_vsns("5.5.5", "5.6.3", "5.6.1")).
			       
