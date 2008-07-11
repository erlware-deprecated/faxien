%% Contains shared macros


%% Default faxien installtion path otherwise known as the prefix. 
-define(INSTALLATION_PATH, "/usr/local/erlware").

%% A static list of compiler versions associate with the erts vsns they are packaged with.  This list should be in descending
%% order of erts version. 
-define(COMPILER_VSN_TO_ERTS_VSN_TO_ERLANG_VSN, [
				   {"4.5.3", "5.6.3", "R12B-3"},
				   {"4.5.2", "5.6.2", "R12B-2"},
				   {"4.5.1", "5.6.1", "R12B-1"},
				   {"4.5", "5.6", "R12B-0"},
				   {"4.4.5", "5.5.5", "R11B-5"}
				  ]).

%% Valid categories for the 'category' key in a control file.
-define(CONTROL_CATEGORIES, [
			     "database",
			     "driver",
			     "game",
			     "graphic",
			     "math",
			     "misc",
			     "net",
			     "server",
			     "test",
			     "tool",
			     "web"
			    ]).

%% Keys that must be present in a control file for it to be considered valid.
-define(MANDITORY_CONTROL_KEYS, [package_owner, description, package_owner_email, categories]).
