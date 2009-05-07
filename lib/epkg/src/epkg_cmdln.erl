%%%-------------------------------------------------------------------
%%% @doc Commandline handling
%%%
%%% @author Martin Logan
%%% @copyright 2007 Erlware
%%% @end
%%%-------------------------------------------------------------------
-module(epkg_cmdln).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("epkg.hrl").
-include("macros.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 translate_dash_to_underscore/1,
	 resolve_alias/2,
	 cmdln_apply/1
        ]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Allows any function to be called from the command line via erl -s
%% all arguments should be passed in the following manner:
%% erl -s mod func "arg1. " "arg2. "...  Be sure that proper escaping
%% of special characters has been done. Particularly, strings and spaces, 
%% if what you are sending as an arg is an erlang string make sure you escape
%% the quote " characters or they will be intepreted by the shell and
%% not passed to this function. \"hello\" makes it to this function
%% as '"hello"' which will be interpreted as the string "hello" whereas
%% "hello" makes it to this function as 'hello'.  All args to -s
%% are supplied to the function specified as atoms. 
%% <pre>
%%
%% Example Command Line Invocation:
%%  erl -s fax_util faxien_apply file list_dir \"/home/jdoe\" -s init stop  
%%
%% Variables:
%%  MFSList - a list containing the mod func and a list of args to apply comming via erl -s
%%
%% Types:
%%  MFAList = [Mod, Func|Args] Example [file, list_dir, "/var/log"]
%% </pre>
%% @spec cmdln_apply(MFAList) -> void()
%% @end
%%--------------------------------------------------------------------
cmdln_apply([Mod, Func|Args]) ->
    TokedArgs = lists:map(fun(ArgAtom) -> convert_string_to_terms(atom_to_list(ArgAtom)) end, no_space(Args)),
    Result    = apply_from_commandline(Mod, Func, TokedArgs),
    ?INFO_MSG("apply the following: apply(~w, ~w, ~p) -> ~p~n", [Mod, Func, TokedArgs, Result]),
    handle_apply_result(Result).

%%--------------------------------------------------------------------
%% @doc change any -'s into _'s 
%% @spec translate_dash_to_underscore(FuncAliasList) -> ResolvedFunc
%% where
%%  Func = atom() | string()
%%  AliasList = {string(), string()}
%%  ResolvedFunc = atom() | string()
%% @end
%%--------------------------------------------------------------------
translate_dash_to_underscore(Func) when is_atom(Func) ->
    list_to_atom(translate_dash_to_underscore(atom_to_list(Func)));
translate_dash_to_underscore(Func) ->
    re:replace(Func, "-", "_", [{return, list}, global]).

%%--------------------------------------------------------------------
%% @doc translate one string to another if there is a translation in the alias list.
%% @spec (FuncName, AliasList) -> NewFuncName
%% where
%%  FuncName = string()
%%  NewFuncName = string()
%%  AliasList = [{FuncName, NewFuncName}]
%% @end
%%--------------------------------------------------------------------
resolve_alias(FuncName, [{FuncName, Func}|_]) ->
    Func;
resolve_alias(FuncName, [_|T]) ->
    resolve_alias(FuncName, T);
resolve_alias(Func, []) ->
    Func.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc the place where all functions are applied from. The final catch all for exceptions.
%% @end
%%--------------------------------------------------------------------
apply_from_commandline(Mod, Func, Args) ->
    try 
	case apply(Mod, Func, Args) of 
	    ok -> 
		ok;
	    {ok, Value} -> 
		{ok, Value};
	    {error, Reason} -> 
		?ERROR_MSG("full error is ~p~n", [{error, Reason}]),
		sanitize_error({error, Reason})
	end
   catch 
	_Class:exit ->
	    ?INFO_MSG("fatal exit exception sent - halting now~n", []),
           init:stop(1);
	_Class:Exception ->
	    ?ERROR_MSG("full exception is ~p with stack trace ~p~n", [Exception,erlang:get_stacktrace()]),
	    sanitize_exception(Exception)
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc Take the sanitized result of an apply from the commandline and determine how to message the user.
%% @end
%%--------------------------------------------------------------------
handle_apply_result(ok) ->
    io:format("ok~n"),
    init:stop(0);
handle_apply_result({ok, Result}) ->
    case epkg_util:is_string(Result) of
	true  -> io:format("~s~n", [Result]);
	false -> io:format("~p~n", [Result])
    end,
    init:stop(0);
handle_apply_result({error, PrintableError}) ->
    io:format("~p~n", [PrintableError]),
    io:format("~nSuggestions:~n"),
    print_error_specific_error_msg(PrintableError),
    print_logfile_info(),
    init:stop(1).


%%----------------------------------------------------------------------------
%% @private
%% @doc take a nasty exception term and clean it up so it's almost fit for human consumption.
%% @end
%%----------------------------------------------------------------------------
sanitize_exception({'EXIT', {case_clause, Error}}) ->
    sanitize_error(Error);
sanitize_exception({'EXIT', {badmatch, Error}}) ->
    sanitize_error(Error);
sanitize_exception({case_clause, Error}) ->
    sanitize_error(Error);
sanitize_exception({badmatch, Error}) ->
    sanitize_error(Error);
sanitize_exception(undef) ->
    {error, {undef, "The function you are trying to execute does not exist."}};
sanitize_exception(Error) ->
    sanitize_error(Error).

%%----------------------------------------------------------------------------
%% @private
%% @doc take a nasty error or exception term and clean it up so it's almost fit for human consumption.
%% @end
%%----------------------------------------------------------------------------
sanitize_error({'EXIT', {{badmatch, {error, WrappedError}}, _Trace}}) when is_list(_Trace) ->
    sanitize_error(WrappedError);
sanitize_error({error, {badmatch, {error, _Reason} = WrappedError}}) ->
    sanitize_error(WrappedError);
sanitize_error({error, {publish_failure, Failed}}) ->
    {error, {publish_failure, "Failed for", [{Repo, because, sanitize_error(Ex)} || {Repo, Ex} <- Failed]}};
sanitize_error({error, {publish_partial_failure, {publish_success, Success}, {publish_failure, Failed}}}) ->
    {error, {publish_partial_failure, "Succeded for:", Success, 
	     "Failed for:", [Repo || {Repo, _Exception} <- Failed]}};
sanitize_error({unable_to_pull_from_repos, Error}) ->
    {error, {unable_to_pull_from_repos, Error}};
sanitize_error({error, req_timedout}) ->
    {error, request_timeout};
sanitize_error({error, {package_not_found, PackageName, _EXIT}}) when is_tuple(_EXIT) ->
    {error, {package_not_found, PackageName}};
sanitize_error({error, {Msg, {'EXIT', {_Reason, _Trace}}}}) when is_list(_Trace) ->
    {error, {package_not_found, Msg}};
sanitize_error({error, {{_Type, {error, Reason}}, _Trace}}) when is_list(_Trace) ->
    {error, Reason};
sanitize_error({Reason, _Trace}) when is_tuple(Reason), is_list(_Trace) ->
    {error, Reason};
sanitize_error(function_clause) ->
    {error, function_clause};
sanitize_error({error, Error}) ->
    {error, Error};
sanitize_error(Error) ->
    {error, {unhandled, Error}}.

%%----------------------------------------------------------------------------
%% @private
%% @doc Print out a custom error message to the user based on a sanitized error term.
%% @end
%%----------------------------------------------------------------------------
print_error_specific_error_msg(enoent) ->
    io:format(" - There may be a permissions problem. Ensure that you have perms to write to the location you~n" ++
	      "   have Erlware currently installed. To find out where you are installed run 'faxien env'~n");
print_error_specific_error_msg(eacces) ->
    io:format(" - There is a permissions problem. Ensure that you have perms to write to the location you~n" ++
	      "   have Erlware currently installed. To find out where you are installed run 'faxien env'~n");
print_error_specific_error_msg(no_publish_repos) ->
    io:format(" - No publish repos have been configured. Add publish repos with 'faxien add-publish-repo <repo-name>'~n" ++
	      "   Suggested repos are http://repo.erlware.org/writable and http://repo.martinjlogan.com/writable~n");
print_error_specific_error_msg({unable_to_pull_from_repos, Msg}) ->
    io:format(" - " ++ Msg ++ "~n   Please request that the package compiled for your local~n" ++
	      "   architecture be published to an accessible repository.~n");
print_error_specific_error_msg({publish_partial_failure, _, _, _, _}) ->
    io:format(" - Some of the repositories you are publishing to are failing. Please report broken repos to their maintainers.~n");
print_error_specific_error_msg({publish_failure, _, _}) ->
    io:format(" - All of the repositories you are publishing to are failing. Please report broken repos to their maintainers.~n");
print_error_specific_error_msg({unhandled, _Error}) ->
    io:format(" - Please report this unhandled error to Erlware at erlware-questions@googlegroups.com or contact@erlware.org.~n");
print_error_specific_error_msg(bad_package) ->
    io:format(" - The package specified is not valid.  Make sure OTP standards have been followed.~n"),
    io:format(" - You may have tried to run publish incorrectly try 'faxien help publish' for more information.~n");
print_error_specific_error_msg(function_clause) ->
    io:format(" - The function you are calling exists but you are most likely calling it with a bad value.  Check the docs.~n");
print_error_specific_error_msg({error,request_timeout}) ->
    io:format(" - The request has timed out.  Specify a larger timeout value.~n");
print_error_specific_error_msg({package_not_found, PackageName}) ->
    io:format(" - Please request that a " ++ PackageName ++ " package be compiled and published for your local architecture.~n" ++
	      "   If the package is available locally, publish it with 'faxien publish <package name>'~n");
print_error_specific_error_msg({undef, _}) ->
    [{Mod, Func, Args}|_] = erlang:get_stacktrace(),
    ArgStr = case length(Args) of
                0 -> "with no arguments";
                1 -> "with one argument";
                N -> "with " ++ integer_to_list(N) ++ " arguments"
              end,
     Error =
       io_lib:format("The function ~p ~s you are trying to execute "
                       "does not exist.",
                     [Func, ArgStr]),
     PropList =
       fun([], Acc, _) ->
               [" But the function ", atom_to_list(Func), " with ", Acc, " arguments exists."];
          ([A], Acc, _) ->
               ArityStr = io_lib:format(" and ~p", [A]),
               [" But the function ", atom_to_list(Func), " with ",
                lists:reverse([ArityStr | Acc]),
                " arguments exists."];
          ([A | As], Acc, Y) ->
               Y(As, [io_lib:format(", ~p", [A]) | Acc], Y)
       end,
     Exists =
       case lists:sort([N || {Name, N} <- Mod:module_info(exports), Name == Func]) of
           [] -> "";
           [Arity | Arities] ->
               PropList(Arities, [integer_to_list(Arity)], PropList)
       end,
    io:format(" - ~s~n", [lists:flatten(Error ++ Exists)]),
    io:format(" - Type 'faxien help' for information on the allowed commands.  Or visit www.erlware.org for more info.~n");
print_error_specific_error_msg(_PrintableError) ->
    io:format(" - Make sure you have the correct permissions to run Faxien~n").

%%-------------------------------------------------------------------
%% @private
%% @doc
%%  Prints information about how to find the faxien logfiles.
%%
%% @spec print_logfile_info() -> void()
%% @end
%%-------------------------------------------------------------------
print_logfile_info() ->
    case gas:get_env(gas, err_log, undefined) of
	{ok, undefined} ->
	    ok;
	{ok, LogFile} ->
	    io:format(" - For error_logger information look at ~p~n", [LogFile])
    end,
    case gas:get_env(sasl, sasl_error_logger, undefined) of
	{ok, {file, SaslFile}} ->
	    io:format(" - For sasl log information look at ~p~n", [SaslFile]);
	_NoLog ->
	    ok
    end.

%%----------------------------------------------------------------------------
%% @private
%% @doc Windows sometimes uses spaces in its directory names, if a \ is found at the end of a string connect 
%%      it with the next string
%% @end
%%----------------------------------------------------------------------------
no_space(Args) ->
    no_space(Args, []).

no_space([], Acc) ->
    lists:reverse(Acc);
no_space([E], Acc) ->
    lists:reverse([E|Acc]);
no_space([A1, A2|T], Acc) ->
    A1S = atom_to_list(A1),
    A2S = atom_to_list(A2),
    case hd(lists:reverse(A1S)) of
	$\\ -> no_space(T,[list_to_atom(A1S ++ " " ++ A2S)|Acc]);
	_   -> no_space([A2|T], [A1|Acc])
    end.
	    

%%----------------------------------------------------------------------------
%% @private
%% @doc Take a commandline string and convert it into a term taking into account some of the nuances of faxien.  This really comes down
%%      to two cases: 
%%       the fact that 2.1 gets turned into 2.111111 or somesuch by parse_term.  This needs to be a vsn string not a float but the user
%%       should not have to enter "2.1" from the commandline.
%%      and
%%       the fact that http://repo.blah.com/pub needs to be converted to a string off the commandline where it appears bare. The user
%%       should not have to type quotes as in "http://repo.blah.com/pub" but without them parse_term will fail. 
%% @end
%%----------------------------------------------------------------------------
convert_string_to_terms(ArgString) ->
    ?INFO_MSG("arg string ~p~n", [ArgString]),
    case re:run(ArgString, "^[0-9]+\.[0-9]+$") of
	{match, _} -> 
	    ArgString;
	_  ->
	    ScanableArg = ArgString ++ ". ", 
	    ?INFO_MSG("scanning and parsing ~p~n", [ScanableArg]),
	    {ok, Toks, _Line} = erl_scan:string(ScanableArg, 1),
	    case catch erl_parse:parse_term(Toks) of
		{ok, Term} -> 
		    Term;
		Error ->
                    convert_string_to_terms(special_case(Error, ArgString))
	    end
    end.

special_case(Msg, ArgString) ->
    ?INFO_MSG("Special case discovered for ~p converting input arg to string and reprocessing~n", [Msg]),
    "\"" ++ ArgString ++ "\"".
