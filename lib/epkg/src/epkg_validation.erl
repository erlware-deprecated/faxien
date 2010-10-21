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
	 analyze_app/1,
	 verify_app_erts_vsn/1,
	 is_package_erts/1,
	 is_package_an_app/1,
	 is_package_a_binary_app/1,
	 is_package_an_unbuilt_app/1,
	 is_package_a_release/1,
	 is_valid_control_file/1,
	 is_valid_signature_file/1
	]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(BINARY_FILE_EXTENSIONS, ["cmx","py","bat","exe","so"]).

%% List of regexs that are compared against the output of the "file" command to determine if a
%% given file is a "binary" file or not
-define(BINARY_FILE_REGEX, [ "ELF .* executable",
                             "shared object",
                             "dynamically linked",
                             "ar archive"]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("epkg.hrl").
-include("macros.hrl").
-include("eunit.hrl").

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
    ?INFO_MSG("is it erts, binary, generic, unbuilt, or edoc ~p~n", [PackageDir]),
    val_edoc(PackageDir).

%%--------------------------------------------------------------------
%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> {ok, Type} | {error, Reason}
%% where
%%  Type = binary | generic | unbuilt | release | erts | docs
%% @end
%%--------------------------------------------------------------------
is_package_edoc(PackageDir) ->
    lists:all(fun(F) -> F(PackageDir) end,
	      [
	       fun(PackageDir_) ->  
		       filelib:is_file(PackageDir_ ++ "/edoc-info")
	       end 
	      ]).

%%--------------------------------------------------------------------
%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> {ok, Type} | {error, Reason}
%% where
%%  Type = binary | generic | unbuilt | release | erts | docs
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (AppDir) -> {ok, Type} | {error, Reason}
%% where
%%  Type = binary | generic | unbuilt | release | erts | docs
%% @end
%%--------------------------------------------------------------------
is_package_an_unbuilt_app(AppDir) ->
    0 == length(filelib:wildcard(AppDir ++ "/ebin/*beam")).

%%--------------------------------------------------------------------
%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> {ok, Type} | {error, Reason}
%% where
%%  Type = binary | generic | unbuilt | release | erts | docs
%% @end
%%--------------------------------------------------------------------
is_package_a_binary_app(PackageDir) ->
    lists:any(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if any of them return true the package dir is a binary app and the function
	%% will return true.
	
	fun(PackageDir_) ->  
	    lists:any(fun(Dir) -> 
		case re:run(Dir, ".*_src") of
			{match, _} -> true;
			_          -> false
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
                Files = ewl_file:find(PackageDir_, ".*"),
                lists:any(fun is_binary_file/1, Files)
        end,

        fun(PackageDir_) ->
                has_binary_override_entry(PackageDir_)
        end
    ]).
		
%%--------------------------------------------------------------------
%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> {ok, Type} | {error, Reason}
%% where
%%  Type = binary | generic | unbuilt | release | erts | docs
%% @end
%%--------------------------------------------------------------------
is_package_a_release(PackageDir) ->
    {well_formed_release_structure, true} == well_formed_release_structure(PackageDir).

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
	    ?ERROR_MSG("bad signature file~n", []),
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
%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> {ok, Type} | {error, Reason}
%% where
%%  Type = binary | generic | unbuilt | release | erts | docs
%% @end
%%--------------------------------------------------------------------
is_package_an_app(AppDir) ->
    {contains_dot_app_file, true} == contains_dot_app_file(AppDir).

%%--------------------------------------------------------------------
%% @doc Analyse an application package and indicate if there are any
%%      defects.
%% @spec (AppDir) -> [{Property, bool()}]
%% where
%%  Type = binary | generic | unbuilt | release | erts | docs
%% @end
%%--------------------------------------------------------------------
analyze_app(AppDir) ->
    [contains_dot_app_file(AppDir), contains_all_erl_files(AppDir)].

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
%% @doc Verify that all beams within an application were compiled 
%%      for the same erts vsn and return that version. 
%% @spec verify_app_erts_vsn(AppDirPath) -> bool()
%% @end
%%--------------------------------------------------------------------
verify_app_erts_vsn(AppDirPath) ->
    case epkg_util:discover_app_erts_vsns(AppDirPath) of
	{ok, [_ErtsVsns]} -> true;
	{ok, _ErtsVsns}   -> false;
	Error             -> throw(Error)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% Predicate that uses the O/S supplied "file" command to determine if a given filename is an
%% executable
is_binary_file(Filename) ->
    FileType = os:cmd(io_lib:format("file -b ~s", [Filename])),
    lists:any(fun(Regex) ->
                      case re:run(FileType, Regex) of
                          {match, _} -> true;
                          _NoMatch   -> false
                      end
              end, ?BINARY_FILE_REGEX).
                              

%% Predicate that checks the application file for an override flag which will force the
%% app to be published as a "binary" app
has_binary_override_entry(PackageDir) ->
    case filelib:wildcard(PackageDir ++ "/ebin/*.app") of
        [File] ->
            case file:consult(File) of
                {ok, [{application, _, Keys}]} ->
                    proplists:get_bool(force_binary_app, Keys);
                _Other ->
                    false
            end;
        _ ->
            false
    end.
                        


val_edoc(PackageDir) ->
    case is_package_edoc(PackageDir) of
	true ->
	    {ok, edoc};
	false ->
	    val_erts(PackageDir)
    end.

val_erts(PackageDir) ->
    case is_package_erts(PackageDir) of
	true ->
	    {ok, erts};
	false -> 
	    val_app(PackageDir)
    end.

val_app(PackageDir) ->
    case is_package_an_app(PackageDir) of
	true ->
	    case is_package_an_unbuilt_app(PackageDir) of
		true ->
		    {ok, unbuilt};
		false ->
		    case is_package_a_binary_app(PackageDir) of
			true  -> {ok, binary};
			false -> {ok, generic}
		    end
	    end;
	false ->
	    val_release(PackageDir)
    end.

val_release(PackageDir) ->
    case is_package_a_release(PackageDir) of
	true  -> {ok, release};
	false -> {error, badly_formatted_or_missing_package}
    end.

contains_dot_app_file(AppDir) ->
    case filelib:wildcard(AppDir ++ "/ebin/*.app") of
	[_|_] -> 
	    {contains_dot_app_file, true};
	[] -> 
	    {contains_dot_app_file, false}
    end.
	

contains_all_erl_files(AppDir) ->
  case verify_presence_of_erl_files(AppDir) of
      ok -> 
	  {contains_all_erl_files, true};
      {error, _} -> 
	  {contains_all_erl_files, false}
  end.

well_formed_release_structure(ReleaseDir) ->
    case filelib:wildcard(ReleaseDir ++ "/releases/*/*.rel") of
	[]  -> {well_formed_release_structure, false};
	[_] -> {well_formed_release_structure, true}
    end.

