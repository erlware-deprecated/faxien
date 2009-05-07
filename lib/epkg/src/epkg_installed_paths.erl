%%%-------------------------------------------------------------------
%%% @doc Understands the path structure of various packages types and states.  In order to keep that information encapsulated
%%% here there are a few path dependent operations in this module as well. 
%%% 
%%% Types:
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(epkg_installed_paths).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%-include("eunit.hrl").
-include("epkg.hrl").
-include("ewrepo.hrl").
-include("macros.hrl").


%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 installed_release_dir_path/2,
	 installed_app_dir_path/3,
	 installed_erts_path/1
	]).

-export([
	 list_releases/1,
	 list_releases/0,
	 list_release_vsns/2,
	 list_release_vsns/1,
	 list_apps/1,
	 list_apps/2,
	 list_app_vsns/2,
	 list_app_vsns/3,
	 list_erts_vsns/1,
	 list_erts_vsns_lower_than/2,
	 list_erts_vsns_lower_than/1,
	 package_dir_to_name_and_vsn/1,
	 get_installation_path/0,
	 find_config_file_path/2,
	 installed_config_file_path/0
	]).

%%====================================================================
%% Package Paths
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns a full installaed erts path.
%% @spec installed_erts_path(ErtsVsn) -> string()
%% @end
%%--------------------------------------------------------------------
installed_erts_path(ErtsVsn) -> 
    {ok, InstallationPath} = get_installation_path(),
    ewl_installed_paths:installed_erts_path(InstallationPath, ErtsVsn).

%%--------------------------------------------------------------------
%% @doc Returns a full installaed application path i.e underneith this directory lies the src and ebin dirs.
%% @spec installed_app_dir_path(ErtsVsn, AppName, AppVsn) -> string()
%% @end
%%--------------------------------------------------------------------
installed_app_dir_path(ErtsVsn, AppName, AppVsn) ->
    {ok, InstallationPath} = get_installation_path(),
    ewl_installed_paths:installed_app_dir_path(InstallationPath, ErtsVsn, AppName, AppVsn).

%%--------------------------------------------------------------------
%% @doc Returns a full installaed release path. Under this directory the releases directory would sit and perhaps the bin dir.
%% @spec installed_release_dir_path(RelName, RelVsn) -> string()
%% @end
%%--------------------------------------------------------------------
installed_release_dir_path(RelName, RelVsn) ->
    {ok, InstallationPath} = get_installation_path(),
    ewl_installed_paths:installed_release_dir_path(InstallationPath, RelName, RelVsn).

%%--------------------------------------------------------------------
%% @doc return the path to config.
%% @spec installed_config_file_path() -> string()
%% @end
%%--------------------------------------------------------------------
installed_config_file_path() ->
    %% @todo this is not compatible with composable apps - this must run with faxien.  Fix this.
    {ok, InstallationPath} = get_installation_path(),
    {ok, Version}          = faxien:version(),
    ewl_installed_paths:installed_config_file_path(InstallationPath, "faxien", Version, "sys.config").

%%====================================================================
%% Other External Functions 
%%====================================================================

%%--------------------------------------------------------------------
%% @doc return a list of release names that are currently installed.
%% @spec list_releases(InstallationPath) -> [string()]
%% @end
%%--------------------------------------------------------------------
list_releases(InstallationPath) -> 
    Packages = filelib:wildcard(lists:flatten([ewl_installed_paths:release_container_path(InstallationPath), "/*"])),
    RelNames = lists:map(fun(PackageName) -> 
			     {ok, {RelName, _}} = package_dir_to_name_and_vsn(PackageName),
			     RelName
		     end, Packages),
    ordsets:to_list(ordsets:from_list(RelNames)).

%% @spec list_releases() -> [string()]
%% @equiv list_releases(InstallationPath)
list_releases() -> 
    {ok, InstallationPath} = get_installation_path(),
    list_releases(InstallationPath).


%%--------------------------------------------------------------------
%% @doc return a list of installed apps.
%% @spec list_apps(InstallationPath, ErtsVsns) -> [string()]
%% @end
%%--------------------------------------------------------------------
list_apps(InstallationPath, [_H|_] = ErtsVsn) when is_integer(_H) -> 
    list_apps(InstallationPath, [ErtsVsn]);
list_apps(InstallationPath, ErtsVsns) -> 
    Packages = lists:foldl(fun(ErtsVsn, Acc) ->
				   filelib:wildcard(
				     lists:flatten([
						    ewl_installed_paths:application_container_path(InstallationPath, ErtsVsn),
						    "/*"
						   ])) ++ Acc
			   end,
			   [],
			   ErtsVsns),
    AppNames = lists:map(fun(PackageName) -> 
			     {ok, {AppName, _}} = package_dir_to_name_and_vsn(PackageName),
			     AppName
		     end, Packages),
    ordsets:to_list(ordsets:from_list(AppNames)).

%% @spec list_apps(ErtsVsns) -> [string()]
%% @equiv list_apps(InstallationPath, ErtsVsns)
list_apps([_H|_] = ErtsVsn) when is_integer(_H) ->
    list_apps([ErtsVsn]);
list_apps(ErtsVsns) ->
    {ok, InstallationPath} = get_installation_path(),
    list_apps(InstallationPath, ErtsVsns).

%%--------------------------------------------------------------------
%% @doc return a list of versions installed for a particular application.
%% @spec list_app_vsns(InstallationPath, ErtsVsn, AppName) -> [string()]
%% @end
%%--------------------------------------------------------------------
list_app_vsns(InstallationPath, ErtsVsn, AppName) -> 
    Packages = filelib:wildcard(
		 lists:flatten([ewl_installed_paths:application_container_path(InstallationPath, ErtsVsn),
				"/", AppName, "-*"])),
    lists:map(fun(PackageName) -> 
			     {ok, {_, Vsn}} = package_dir_to_name_and_vsn(PackageName),
			     Vsn
		     end, Packages).

%% @spec list_app_vsns(ErtsVsn, AppName) -> [string()]
%% @equiv list_app_vsns(InstallationPath, ErtsVsn, AppName) 
list_app_vsns(ErtsVsn, AppName) -> 
    {ok, InstallationPath} = get_installation_path(),
    list_app_vsns(InstallationPath, ErtsVsn, AppName) .

%%--------------------------------------------------------------------
%% @doc return a list of erts versions currently installed. 
%% @spec list_erts_vsns(InstallationPath) -> [string()]
%% @end
%%--------------------------------------------------------------------
list_erts_vsns(InstallationPath) ->
    lists:map(fun(Path) ->
		      Erts = filename:basename(Path),
		      string:substr(Erts, 6, length(Erts))
	      end, filelib:wildcard(InstallationPath ++ "/erts-*")).

%%--------------------------------------------------------------------
%% @doc return a list of erts versions currently installed that are lower than the supplied version. 
%% @spec list_erts_vsns_lower_than(InstallationPath, TargetErtsVsn) -> [string()]
%% @end
%%--------------------------------------------------------------------
list_erts_vsns_lower_than(InstallationPath, TargetErtsVsn) ->
    AllErtsVsns = list_erts_vsns(InstallationPath),
    lists:foldl(fun(Vsn, Acc) ->
			case ewr_util:is_version_greater(Vsn, TargetErtsVsn) of
			    true -> Acc;
			    false -> [Vsn|Acc]
			end
		end,
		AllErtsVsns).

%% @spec list_erts_vsns_lower_than(TargetErtsVsn) -> [string()]
%% @equiv list_erts_vsns_lower_than(InstallationPath, TargetErtsVsn) 
list_erts_vsns_lower_than(TargetErtsVsn) ->
    {ok, InstallationPath} = get_installation_path(),
    list_erts_vsns_lower_than(InstallationPath, TargetErtsVsn).
    
%%--------------------------------------------------------------------
%% @doc return a list of versions installed for a particular release.
%% @spec list_release_vsns(InstallationPath, RelName) -> [string()]
%% @end
%%--------------------------------------------------------------------
list_release_vsns(InstallationPath, RelName) when is_atom(RelName) -> 
    list_release_vsns(InstallationPath, atom_to_list(RelName)); 
list_release_vsns(InstallationPath, RelName) -> 
    Packages = filelib:wildcard(
		 lists:flatten([ewl_installed_paths:release_container_path(InstallationPath), "/", RelName, "-*"])),
    lists:map(fun(PackageName) -> 
			     {ok, {_, Vsn}} = package_dir_to_name_and_vsn(PackageName),
			     Vsn
		     end, Packages).

%% @spec list_release_vsns(RelName) -> [string()]
%% @equiv list_release_vsns(InstallationPath, RelName) 
list_release_vsns(RelName) -> 
    {ok, InstallationPath} = get_installation_path(),
    list_release_vsns(InstallationPath, RelName).


%%-------------------------------------------------------------------
%% @doc
%% returns the name and version of a package for a package directory:
%% Example:
%%  package_dir_to_name_and_vsn("/usr/local/erlware/releases/my_rel/1.2.4") -> {ok, {"my_rel". "1.2.4"}}
%%  package_dir_to_name_and_vsn("/home/martin/my_rel-1.2.4") -> {ok, {"my_rel". "1.2.4"}}
%%
%% @spec package_dir_to_name_and_vsn(RawPackageDir::string()) -> {ok, {Name, Version}} | {error, Reason}
%% @end
%%-------------------------------------------------------------------
package_dir_to_name_and_vsn(RawPackageDir) ->
    PackageDir = filename:basename(filename:absname(RawPackageDir)),
    case re:run(PackageDir, ?PACKAGE_NAME_AND_VSN_REGEXP) of
	{match, [{0, _}|_]} ->
	    {ok, {[PackageName], PackageVsn}} = ewl_string_manip:n_tokens(PackageDir, 1, "-"),
	    {ok, {PackageName, lop_off_end(PackageVsn)}};
	_Error -> 
	    ?ERROR_MSG("~p is not of the form <name>-<version>", [PackageDir]),
	    {error, "The package directory provided is not of the form <name>-<version>"}
    end.

lop_off_end(Vsn) ->
    lop_off_end2(lists:reverse(Vsn)).

%% Lop of suffixes like .epkg and .tar.gz
lop_off_end2([$g,$k,$p,$e,$.|Vsn]) ->
    lists:reverse(Vsn);
lop_off_end2([$z,$g,$.,$r,$a,$t,$.|Vsn]) ->
    lists:reverse(Vsn);
lop_off_end2(Vsn) ->
    lists:reverse(Vsn).

%%--------------------------------------------------------------------
%% @doc The installation_path variable is no longer needed in the config file except for the case of a non launcher installation.
%% In the case of usage via launcher we must rely on the -prefix argument it passes in. Config is rewritten based on that arg.
%% @spec get_installation_path() -> {ok, InstallationPath} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_installation_path() ->
    case init:get_argument(prefix) of
	{ok, [[Prefix]]} ->
	    {ok, Prefix};
	error ->
	    ?INFO_MSG("epkg:get_installation_path init:get_argument(prefix) returned error~n", []),
	    {error, no_prefix_supplied_with_startup}
    end.

%%--------------------------------------------------------------------
%% @doc Return the path to a config file within an installed release.
%% @spec find_config_file_path(RelName, RelVsn) -> ConfigFilePath
%% @end
%%--------------------------------------------------------------------
find_config_file_path(RelName, RelVsn) -> 
    {ok, InstallationPath} = get_installation_path(),
    RelDirPath             = ewl_installed_paths:release_file_container_path(InstallationPath, RelName, RelVsn),
    case ewl_file:find(RelDirPath, ".*config$") of
	[]                  -> throw({error, no_erlang_config_file_found});
	[RelConfigFilePath] -> RelConfigFilePath;
	RelConfigFilePaths  -> RelConfigFilePaths
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================



    
%%%====================================================================
%%% Test Functions
%%%====================================================================

%package_dir_to_name_and_vsn_test() ->
    %?assertMatch({ok, {"epkg", "12.34-alpha"}}, package_dir_to_name_and_vsn("/usr/local/erlware/lib/epkg-12.34-alpha")),
    %?assertMatch({ok, {"epkg", "12.34-alpha"}}, package_dir_to_name_and_vsn("/usr/local/erlware/lib/epkg-12.34-alpha.tar.gz")),
    %?assertMatch({ok, {"epkg", "12.34-alpha"}}, package_dir_to_name_and_vsn("/usr/local/erlware/lib/epkg-12.34-alpha.epkg")),
    %?assertMatch({ok, {"epkg", "12.34-alpha"}}, package_dir_to_name_and_vsn("/usr/local/erlware/lib/epkg-12.34-alpha/")).
