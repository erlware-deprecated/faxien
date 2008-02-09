%% Contains shared macros


-import (error_logger).


-define (CURRENT_FUNCTION, tuple_to_list (hd (tl (tuple_to_list (process_info (self (), current_function)))))).

%% Usage: ?INFO_MSG("info message ~p~n", [Reason]),
%% INFO_MSG(Msg, Args) -> ok

-define (INFO_MSG (Msg, Args), error_logger:info_msg ("~p:~p/~p (line ~p) " ++ Msg, ?CURRENT_FUNCTION ++ [?LINE | Args])).


%% Usage: ?ERROR_MSG("error message ~p~n", [Reason]),
%% ERROR_MSG(Msg, Args) -> ok

-define (ERROR_MSG (Msg, Args), error_logger:error_msg ("~p:~p/~p (line ~p) " ++ Msg, ?CURRENT_FUNCTION ++ [?LINE | Args])).


%% A static list of compiler versions associate with the erts vsns they are packaged with.  This list should be in descending
%% order of erts version. 
-define(COMPILER_VSN_TO_ERTS_VSN, [
				   {"4.5", "5.6"},
				   {"4.4.5", "5.5.5"}
				  ]).

%% Must be a list of tuples of strings. Commands can use either the _ or the - separator notation.
-define(ALIAS_LIST, [
		     {"install",  "install-release"},
		     {"i",        "install-release"},
		     {"p",        "publish"},
		     {"outdated", "outdated-releases"},
		     {"remove",   "remove-release"}
		    ]).

		   
		     
