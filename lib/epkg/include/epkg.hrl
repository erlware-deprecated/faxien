%% Contains shared macros


%% Default faxien installtion path otherwise known as the prefix. 
-define(INSTALLATION_PATH, "/usr/local/erlware").

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
