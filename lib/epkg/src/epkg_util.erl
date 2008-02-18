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
	 is_string/1,
	 overwrite_yes_no/4,
	 overwrite_yes_no/3,
	 create_unique_tmp_dir/1,
	 create_unique_tmp_dir/0,
	 set_executable_perms/1,
	 set_all_executable_perms/1,
	 unpack_to_tmp/1,
	 unpack_to_tmp_if_archive/1,
	 find_bad_control_categories/1,
	 if_atom_or_integer_to_string/1,
	 consult_rel_file/2,
	 consult_control_file/2
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("epkg.hrl").
-include("ewrepo.hrl").
%-include("eunit.hrl").
-include("macros.hrl").

%%====================================================================
%% API
%%====================================================================

%%----------------------------------------------------------------------------
%% @doc Checks to see if a list is a string.
%% @spec is_string(List) -> bool()
%% @end
%%----------------------------------------------------------------------------
is_string([])                          -> true;
is_string(Term) when not is_list(Term) -> false;
is_string(Term)                        -> not lists:any(fun(Element) -> not_string_p1(Element) end, Term).
                                                                                   
not_string_p1(H) when integer(H), H >= $\s, H < 255 -> false;
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
if_atom_or_integer_to_string(Arg) when is_atom(Arg); is_integer(Arg) ->
    [Fixed] = if_atom_or_integer_to_string([Arg]),
    Fixed;
if_atom_or_integer_to_string(List) ->
    lists:map(fun(V) when is_atom(V)    -> atom_to_list(V);
		 (V) when is_integer(V) -> integer_to_list(V);
		 (V)                    -> V 
	      end, List).
    
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
		[]    -> ok;
		Error -> {error, Error}
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
		[]    -> ok;
		Error -> {error, Error}
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
    end.

%%--------------------------------------------------------------------
%% @doc if a package is a tarball then untar it into a tmp dir and hand back the path(s) to the unpacked contents of the temp dir.
%% @spec unpack_to_tmp_if_archive(PackageDirPath::string()) -> DirPath::string()
%% @end
%%--------------------------------------------------------------------
unpack_to_tmp_if_archive(ArchiveFilePath) ->
    case regexp:match(ArchiveFilePath, ".*" ++ ?REPO_FILE_EXT_REGEXP ++ "$") of
	{match, _, _} ->
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
    
%%====================================================================
%% Internal functions
%%====================================================================


%%%===================================================================
%%% Testing Functions
%%%===================================================================
%is_string_test() ->
    %?assertMatch(false, is_string([hello])),
    %?assertMatch(false, is_string({})),
    %?assertMatch(true, is_string("hell]o")),
    %?assertMatch(true, is_string("hello")).

    
			       
