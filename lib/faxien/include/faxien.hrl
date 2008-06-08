%% Contains shared macros
%% A static list of compiler versions associate with the erts vsns they are packaged with.  This list should be in descending
%% order of erts version. 
-define(COMPILER_VSN_TO_ERTS_VSN_TO_ERLANG_VSN, [
				   {"4.5.2", "5.6.2", "R12B-2"},
				   {"4.5.1", "5.6.1", "R12B-1"},
				   {"4.5", "5.6", "R12B-0"},
				   {"4.4.5", "5.5.5", "R11B-5"}
				  ]).

%% Must be a list of tuples of strings. Commands can use either the _ or the - separator notation.
-define(ALIAS_LIST, [
		     {"upgrade",  "upgrade-release"},
		     {"uar",      "upgrade-all-releases"},
		     {"uaa",      "upgrade-all-apps"},
		     {"ur",       "upgrade-release"},
		     {"ua",       "upgrade-app"},
		     {"tv",       "translate-version"},
		     {"ir",       "install-release"},
		     {"ia",       "install-app"},
		     {"fr",       "fetch-release"},
		     {"fa",       "fetch-app"},
		     {"p",        "publish"},
		     {"ar",       "add-repo"},
		     {"sr",       "show-repos"},
		     {"apr",      "add-publish-repo"},
		     {"spr",      "show-publish-repos"},
		     {"rpr",      "remove-publish-repo"},
		     {"stev",     "set-target-erts-vsn"},
		     {"or",       "outdated-releases"},
		     {"rr",       "remove-release"},
		     {"ra",       "remove-app"}
		    ]).

		   
		     
