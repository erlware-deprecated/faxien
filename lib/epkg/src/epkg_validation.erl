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
	 is_package_erts/1,
	 is_package_an_app/1,
	 is_package_a_binary_app/1,
	 is_package_a_release/1,
	 is_valid_control_file/1
	]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(BINARY_FILE_EXTENSIONS, ["cmx","py","c","bat","exe","so"]).

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
    {ok, [{control, _PackageName, ControlList}]} = file:consult(ControlFilePath),
    case fs_lists:get_val(categories, ControlList, []) of
	[] -> 
	    {error, no_categories};
	Categories ->
	    case epkg_util:find_bad_control_categories(Categories) of
		[] -> 
		    true;
		BadCategories -> 
		    ?ERROR_MSG("bad control categories ~p~n", [BadCategories]),
		    {error, {bad_categories, BadCategories}}
	    end
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


%%====================================================================
%% Internal functions
%%====================================================================

