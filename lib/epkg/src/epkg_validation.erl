%%%-------------------------------------------------------------------
%%% @doc Functions used to validate that a package is well-formatted OTP. 
%%% @end
%%%-------------------------------------------------------------------
-module(epkg_validation).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 validate_type/1,
	 verify_presence_of_erl_files/1,
	 verify_app_erts_vsn/1,
	 is_package_erts/1,
	 is_package_an_app/1,
	 is_package_a_binary_app/1,
	 is_package_a_release/1,
	 is_valid_control_file/1,
	 is_valid_signature_file/1
	]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(BINARY_FILE_EXTENSIONS, ["cmx","py","c","bat","exe","so"]).

%% List of regexs that are compared against the output of the "file" command to determine if a
%% given file is a "binary" file or not
-define(BINARY_FILE_REGEX, [ "executable",
                             "shell script",
                             "shared object",
                             "dynamically linked" ]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("epkg.hrl").
-include("macros.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Determine the type of the package and make sure it is a valid instance of that type.
%% @spec validate_type(PackageDir) -> {ok, Type} | {error, Reason}
%% where
%%  Type = binary | generic | release | erts
%% @end
%%--------------------------------------------------------------------
validate_type(PackageDir) ->
    case is_package_erts(PackageDir) of
	true ->
	    {ok, erts};
	false -> 
	    case is_package_an_app(PackageDir) of
		true ->
		    case is_package_a_binary_app(PackageDir) of
			true  -> {ok, binary};
			false -> {ok, generic}
		    end;
		false ->
		    case is_package_a_release(PackageDir) of
			true  -> {ok, release};
			false -> {error, badly_formatted_or_missing_package}
		    end
	    end
    end.

is_package_erts(PackageDir) ->
    lists:all(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if all of them return true then we have a well formed application.
	
	fun(PackageDir_) ->  
            case filelib:wildcard(PackageDir_ ++ "/include/driver_int.h") of
		[_|_] -> 
		    true;
		[] -> 
		    false
	    end
	end, 

	fun(PackageDir_) ->  
            case filelib:wildcard(PackageDir_ ++ "/include/erl_fixed_size_int_types.h") of
		[_|_] -> 
		    true;
		[] -> 
		    false
	    end
	end 
    ]).

is_package_an_app(PackageDir) ->
    lists:all(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if all of them return true then we have a well formed application.
	
	fun(PackageDir_) ->  
            case filelib:wildcard(PackageDir_ ++ "/ebin/*.app") of
		[_|_] -> 
		    true;
		[] -> 
		    false
	    end
	end, 
	
	fun(PackageDir_) ->
            case filelib:wildcard(PackageDir_ ++ "/ebin/*.beam") of
		[_|_] -> 
                    true;
		[] -> 
                    false
	    end
	end, 

	fun(PackageDir_) ->
            case verify_presence_of_erl_files(PackageDir_) of
		ok -> 
                    true;
		{error, _} -> 
                    false
	    end
	end
    ]).

is_package_a_binary_app(PackageDir) ->
    lists:any(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if any of them return true the package dir is a binary app and the function
	%% will return true.
	
	fun(PackageDir_) ->  
	    lists:any(fun(Dir) -> 
		case regexp:match(Dir, ".*_src") of
			{match, _, _} -> true;
			_             -> false
		end
	    end, filelib:wildcard(PackageDir_ ++ "/*"))
	end, 
	
	fun(PackageDir_) ->
		RegexpBody = string:strip(lists:flatten([".*\\." ++ Ext ++ "$|" || Ext <- ?BINARY_FILE_EXTENSIONS]), right, $|), 
		Exts = lists:flatten(["(", RegexpBody, ")"]),
		case ewl_file:find(PackageDir_, Exts) of
		    []    -> false;
		    [_|_] -> true
		end
	end,

        fun(PackageDir_) ->
                Files = filelib:wildcard(PackageDir_ ++ "/priv/*/*"),
                lists:any(fun is_binary_file/1, Files)
        end
    ]).
		
is_package_a_release(PackageDir) ->
    ?INFO_MSG("~p~n", [PackageDir]),
    lists:any(fun(F) -> F(PackageDir) end, 
	      [
	       
	       %% Run all the following lambda's and if all of them return true then we have a well formed release.
	       
	       fun(PackageDir_) ->
		       case filelib:wildcard(PackageDir_ ++ "/releases/*/*.rel") of
			   []  -> false;
			   [_] -> true
		       end
	       end,
	       
	       fun(PackageDir_) ->
		       case filelib:wildcard(PackageDir_ ++ "/release/*.rel") of
			   []  -> false;
			   [_] -> true
		       end
	       end
	      ]).

%%--------------------------------------------------------------------
%% @doc determine if a signature file supplied is a valid one.
%% @spec is_valid_signature_file(SignatureFilePath) -> bool()
%% @end
%%--------------------------------------------------------------------
is_valid_signature_file(SignatureFilePath) ->
    ?INFO_MSG("~p~n", [SignatureFilePath]),
    case file:consult(SignatureFilePath) of
	{ok, [Signature]} ->
	    is_valid_signature_term(Signature);
	_Error ->
	    false
    end.

is_valid_signature_term({signature, _Signature, _Modulus, _Exponent}) ->
    true;
is_valid_signature_term(_BadSigature) ->
    false.


%%--------------------------------------------------------------------
%% @doc determine if a control file supplied is a valid one.
%% @spec is_valid_control_file(ControlFilePath) -> true | {error, Reason}
%% where
%%  Reason = {missing_control_keys, {need, list()}, {found, list()}} | {bad_categories, list()} | no_categories | term()
%% @end
%%--------------------------------------------------------------------
is_valid_control_file(ControlFilePath) ->
    ?INFO_MSG("~p~n", [ControlFilePath]),
    case epkg_util:consult_control_file(?MANDITORY_CONTROL_KEYS, ControlFilePath) of
	{error, Reason} -> 
	    {error, Reason};
	Keys when length(Keys) == length(?MANDITORY_CONTROL_KEYS) -> 
	    has_bad_control_categories(ControlFilePath);
	Keys ->
	    {error, {missing_control_keys, {need, ?MANDITORY_CONTROL_KEYS}, {found, Keys}}}
    end.

has_bad_control_categories(ControlFilePath) ->
    case epkg_util:consult_control_file(categories, ControlFilePath) of
	Categories when is_list(Categories), Categories /= [] ->
	    case epkg_util:find_bad_control_categories(Categories) of
		[] -> 
		    true;
		BadCategories -> 
		    ?ERROR_MSG("bad control categories ~p~n", [BadCategories]),
		    {error, {bad_categories, BadCategories}}
	    end;
	_Error ->
	    {error, no_categories}
    end.
	    

%%--------------------------------------------------------------------
%% @doc Make sure an application contains all the source files that the .app files suggests it does.
%% @spec verify_presence_of_erl_files(AppDirPath::string()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
verify_presence_of_erl_files(AppDirPath) ->
    {ok, [{modules, Modules}]} = 
	ewr_util:fetch_local_appfile_key_values(AppDirPath, [modules]),
    F = fun(Mod) -> 
		case filelib:is_file(AppDirPath ++ "/src/" ++ atom_to_list(Mod) ++ ".erl") of
		    true -> 
			true;
		    false ->
			?INFO_MSG("~p is missing~n", [Mod]),
			false
		end
	end, 
    case catch lists:foreach(F, Modules) of
	ok     -> ok;
	Error -> {error, {"missing source files", Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Verify that all beams within an application were compiled with for the same erts vsn. 
%% @spec verify_app_erts_vsn(AppDirPath) -> {ok, ErtsVsn} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
verify_app_erts_vsn(AppDirPath) ->
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

%%====================================================================
%% Internal functions
%%====================================================================

%% Predicate that uses the O/S supplied "file" command to determine if a given filename is an
%% executable
is_binary_file(Filename) ->
    FileType = os:cmd(io_lib:format("file -b ~s", [Filename])),
    lists:any(fun(Regex) ->
                      case regexp:match(FileType, Regex) of
                          nomatch -> false;
                          {match, _, _} -> true
                      end
              end, ?BINARY_FILE_REGEX).
                              


                           
