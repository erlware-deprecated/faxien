%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  Handle control files
%%% @end
%%% Created :  7 Jun 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epkg_control).

%% API
-export([
	 collect_control_info/1,
	 get_categories/0
	]).

-include("macros.hrl").
-include("epkg.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Collect control data from the user.
%% <pre>
%% Types:
%%  ControlInfo = [{Key, Value}]
%% </pre>
%% @spec collect_control_info(PackageName) -> ControlTerm
%% @end
%%--------------------------------------------------------------------
collect_control_info(PackageName) when is_atom(PackageName) ->
    collect_control_info(list_to_atom(PackageName));
collect_control_info(PackageName) ->
    {control, PackageName, lists:flatten([collect_manditory_control_info(),  collect_additional_control_info()])}. 

%%--------------------------------------------------------------------
%% @doc Fetch category data.
%% @spec get_categories() -> {categories, list()}
%% @end
%%--------------------------------------------------------------------
get_categories() ->
    {categories, enter_categories()}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

collect_manditory_control_info() ->
    [
     {package_owner, ewl_talk:ask("~nEnter the package owners full name > ")},
     {package_owner_email, ewl_talk:ask("~nEnter a contact email address > ")},
     {categories, enter_categories()},
     {description, ewl_talk:ask("~nEnter a short description of the package > ")}
    ].

enter_categories() ->
    Categories = [string:strip(E, both, $ ) || 
		     E <- string:tokens(
			    ewl_talk:ask(
			      lists:flatten(["~nEnter from the list below. Separate multiple categories with commas:\n", 
					     printable_categories(), 
					     " > "])), ",")],
    case epkg_util:find_bad_control_categories(Categories) of
	[] ->
	    Categories;
	BadCategories ->
	    io:format("The following categories are invalid ~p. Please enter only the categories suggested exactly.~n", 
		      [BadCategories]),
	    enter_categories()
    end.

printable_categories() ->
    string:strip(lists:foldl(fun(C, Acc) -> C ++ ", " ++ Acc end, [], ?CONTROL_CATEGORIES), right, $,).
			
collect_additional_control_info() ->
    case ewl_talk:ask("~nWould you like to specify additional control information? [yes|no] > ") of
	Yes when Yes == $y; Yes == $Y; Yes == "yes" ->
	    [
	     {author, ewl_talk:ask("~nEnter the authors full name > ")},
	     {authors_email, ewl_talk:ask("~nEnter the authors email address > ")},
	     {keywords, [string:strip(E, both, $ ) || 
			   E <- string:tokens(ewl_talk:ask("~nEnter comma separated keywords for the package > "), ",")]},
	     {project_page, ewl_talk:ask("~nEnter project page url > ")}
	    ];
	No when No == $n; No == $N; No == "no" ->
	    [];
	Error ->
	    ?INFO_MSG("user entered \"~p\"~n", [Error]),
	    io:format("Please enter \"yes\" or \"no\"~n"),
	    collect_additional_control_info()
    end.
