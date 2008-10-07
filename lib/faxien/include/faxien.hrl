%% Contains shared macros
-include("macros.hrl").

%% Must be a list of tuples of strings. Commands can use either the _ or the - separator notation.
-define(ALIAS_LIST, [
		     {"upgrade",  "upgrade-release"},
		     {"env",      "environment"},
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
		     {"rrp",      "remove-repo"},
		     {"apr",      "add-publish-repo"},
		     {"spr",      "show-publish-repos"},
		     {"rpr",      "remove-publish-repo"},
		     {"spev",     "set-preferred-erts-vsn"},
		     {"shpev",    "show-preferred-erts-vsn"},
		     {"or",       "outdated-releases"},
		     {"rr",       "remove-release"},
		     {"ra",       "remove-app"}
		    ]).

		   
		     
