%% Contains shared macros


%% Default faxien installtion path otherwise known as the prefix. 
-define(INSTALLATION_PATH, "/usr/local/erlware").

%% A static list of compiler versions associate with the erts vsns they are packaged with.  This list should be in descending
%% order of erts version. 
-define(COMPILER_VSN_TO_ERTS_VSN_TO_ERLANG_VSN, [
				   {"4.7",   "5.8",   "R14A"},
				   {"4.6.5", "5.7.5", "R13B04"},
				   {"4.6.4", "5.7.4", "R13B03"},
				   {"4.6.3", "5.7.3", "R13B02"},
				   {"4.6.2", "5.7.2", "R13B01"},
				   {"4.6.1", "5.7.1", "R13B"},
				   {"4.6",   "5.7",   "R13A"},
				   {"4.5.5", "5.6.5", "R12B-5"},
				   {"4.5.4", "5.6.4", "R12B-4"},
				   {"4.5.3", "5.6.3", "R12B-3"},
				   {"4.5.2", "5.6.2", "R12B-2"},
				   {"4.5.1", "5.6.1", "R12B-1"},
				   {"4.5",   "5.6",   "R12B-0"},
				   {"4.4.5", "5.5.5", "R11B-5"},
				   {"4.4.4", "5.5.4", "R11B-4"},
				   {"4.4.3", "5.5.3", "R11B-3"},
				   {"4.4.2", "5.5.2", "R11B-2"},
				   {"4.4.1", "5.5.1", "R11B-1"},
				   {"4.4",   "5.5",   "R11B-0"}
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

%% Must be a list of tuples of strings. Commands can use either the _ or the - separator notation.
-define(EPKG_ALIAS_LIST, [
		     {"cfp",      "config-file-path"},
		     {"raa",      "remove-all-apps"},
		     {"ra",       "remove-app"}
		    ]).
