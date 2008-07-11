%%%-------------------------------------------------------------------
%%% @doc Handles fetching packages from the remote repository and 
%%%      placing them in the erlware repo.
%%%
%%% @type options() = [{force, force()}, {erts_prompt, erts_prompt()}]
%%% @type force() = bool(). Indicates whether an existing app is to be overwritten with or without user conscent.  
%%% @type erts_prompt() = bool(). indicate whether or not to prompt upon finding a package outside of the target erts vsn.
%%%
%%% @todo add the force option to local installs in epkg
%%% @todo add explicit timeouts to every interface function depricate the macro or use it as a default in the faxien module. 
%%%
%%% @author Martin Logan
%%% @copyright Erlware
%%% @end
%%%-------------------------------------------------------------------
-module(fax_install).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("faxien.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 install_latest_remote_application/5,
	 install_remote_application/6,
	 install_latest_remote_release/6,
	 install_remote_release/7,
	 install_remote_erts/3,
	 install_erts/3,
	 install_release/6,
	 fetch_latest_remote_release/6,
	 fetch_remote_release/6,
	 fetch_latest_remote_application/6,
	 fetch_remote_application/6
	]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%%  Install a the highest version found of an application package from a repository. 
%%
%% <pre>
%% Examples:
%%  install_latest_remote_application(["http"//repo.erlware.org/pub"], "5.5.5", gas, [], 10000)
%% </pre>
%%
%% @spec install_latest_remote_application(Repos, TargetErtsVsn, AppName, Options, Timeout) -> ok | {error, Reason} | exit()
%% where
%%     Repos = string()
%%     TargetErtsVsn = string()
%%     AppName = string()
%%     Options = options()
%% @end
%%--------------------------------------------------------------------
install_latest_remote_application(Repos, TargetErtsVsn, AppName, Options, Timeout) ->
    Force      = fs_lists:get_val(force, Options),
    ErtsPrompt = fs_lists:get_val(erts_prompt, Options),

    Fun = fun(ManagedRepos, AppVsn, ErtsVsn) ->
		  install_remote_application(ManagedRepos, ErtsVsn, AppName, AppVsn, Force, Timeout)
	  end,
    fax_util:execute_on_latest_package_version(Repos, TargetErtsVsn, AppName, Fun, lib, ErtsPrompt). 

%%--------------------------------------------------------------------
%% @doc 
%%  Install an application package from a repository. Versions can be the string "LATEST". Calling this function will install 
%%  a remote application at IntallationPath/lib/Appname-Appvsn.
%%
%% <pre>
%% Examples:
%%  install_remote_application(["http"//repo.erlware.org/pub"], "5.5.5", gas, "4.6.0", false)
%% </pre>
%%
%% @spec install_remote_application(Repos, TargetErtsVsn, AppName, AppVsn, Force, Timeout) -> ok | {error, Reason} | exit()
%% where
%%     Repos = string()
%%     TargetErtsVsn = string()
%%     AppName = string()
%%     AppVsn = string() 
%%     Force = bool()
%% @end
%%--------------------------------------------------------------------
install_remote_application(Repos, TargetErtsVsn, AppName, AppVsn, Force, Timeout) ->
    ?INFO_MSG("install_remote_application(~p, ~p, ~p, ~p)~n", [Repos, TargetErtsVsn, AppName, AppVsn]),
    AppDir = epkg_installed_paths:installed_app_dir_path(TargetErtsVsn, AppName, AppVsn),
    case epkg_validation:is_package_an_app(AppDir) of
	false -> 
	    io:format("Pulling down ~s-~s -> ", [AppName, AppVsn]),
	    {ok, AppPackageDirPath} = fetch_app_to_tmp(Repos, TargetErtsVsn, AppName, AppVsn, Timeout),
	    Res                     = epkg:install_app(AppPackageDirPath, TargetErtsVsn),
	    ok                      = ewl_file:delete_dir(AppPackageDirPath),
	    io:format("~p~n", [Res]),
	    Res;
	true -> 
	    epkg_util:overwrite_yes_no(
	      fun() -> install_remote_application(Repos, TargetErtsVsn, AppName, AppVsn, Force, Timeout) end,  
	      fun() -> ok end, 
	      AppDir, 
	      Force)
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  Install an erts package. 
%% @spec install_erts(Repos, ErtsVsnOrPath, Timeout) -> ok | {error, Reason} | exit()
%% where
%%     Type = application | release
%%     AppNameOrPath = string()
%% @end
%%--------------------------------------------------------------------
install_erts(Repos, ErtsVsnOrPath, Timeout) ->
    case filelib:is_file(ErtsVsnOrPath) of
	true  -> epkg:install_erts(ErtsVsnOrPath);
	false -> install_remote_erts(Repos, ErtsVsnOrPath, Timeout)
    end.
	    
%%--------------------------------------------------------------------
%% @doc 
%%  Install an erts package from a repository. 
%% <pre>
%% Examples:
%%  install_remote_erts(["http"//repo.erlware.org/pub"], "5.5.5", 100000)
%% </pre>
%% @spec install_remote_erts(Repos, ErtsVsn, Timeout) -> ok | {error, Reason} | exit()
%% where
%%     Repos = string()
%%     TargetErtsVsn = string()
%%     ErtsName = string()
%%     ErtsVsn = string() 
%% @end
%%--------------------------------------------------------------------
install_remote_erts(Repos, ErtsVsn, Timeout) ->
    ?INFO_MSG("install_remote_erts(~p, ~p)~n", [Repos, ErtsVsn]),
    ErtsDir = epkg_installed_paths:installed_erts_path(ErtsVsn),
    case epkg_validation:is_package_erts(ErtsDir) of
	false -> 
	    io:format("Pulling down erts-~s -> ", [ErtsVsn]),
	    {ok, ErtsPackageDirPath} = fetch_erts(Repos, ErtsVsn, Timeout),
	    Res                      = epkg:install_erts(ErtsPackageDirPath),
	    ok                       = ewl_file:delete_dir(ErtsPackageDirPath),
	    io:format("~p~n", [Res]),
	    Res;
	true -> 
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  Install a release package.  This function will determine whether the target (AppNameOrPath) is a request to install
%%  an application from a remote repository or to install a release package (.epkg) or an untarred package directory.
%%  IsLocalBoot indicates whether a local specific boot file is to be created or not. See the systools docs for more information.
%% @spec install_release(Repos, TargetErtsVsn, ReleasePackageArchiveOrDirPath, IsLocalBoot, Force, Timeout) -> ok | {error, Reason} | exit()
%% where
%%     Type = application | release
%%     AppNameOrPath = string()
%%     ReleasePackageArchiveOrDirPath = string()
%%     IsLocalBoot = bool()
%%     Force = force()
%% @end
%%--------------------------------------------------------------------
install_release(Repos, TargetErtsVsn, ReleasePackageArchiveOrDirPath, IsLocalBoot, Force, Timeout) ->
    case filelib:is_file(ReleasePackageArchiveOrDirPath) of
	true  -> install_from_local_release_package(Repos, ReleasePackageArchiveOrDirPath, IsLocalBoot, Force, Timeout);
	false -> install_latest_remote_release(Repos, TargetErtsVsn, ReleasePackageArchiveOrDirPath, IsLocalBoot, Force, Timeout)
    end.
				  
%%--------------------------------------------------------------------
%% @doc 
%%  Install the latest version found of a release package from a repository. 
%%  IsLocalBoot indicates whether a local specific boot file is to be created or not. See the systools docs for more information.
%% @spec install_latest_remote_release(Repos, TargetErtsVsn, RelName, IsLocalBoot, Options, Timeout) -> 
%%               ok | {error, Reason} | exit()
%% where
%%     Repos = string()
%%     RelName = string()
%%     RelVsn = string() 
%%     IsLocalBoot = bool()
%%     Options = options()
%% @end
%%--------------------------------------------------------------------
install_latest_remote_release(Repos, TargetErtsVsn, RelName, IsLocalBoot, Options, Timeout) ->
    Force      = fs_lists:get_val(force, Options),
    ErtsPrompt = fs_lists:get_val(erts_prompt, Options),

    Fun = fun(ManagedRepos, RelVsn, ErtsVsn) ->
		  install_remote_release(ManagedRepos, ErtsVsn, RelName, RelVsn, IsLocalBoot, Force, Timeout)
	  end,
    fax_util:execute_on_latest_package_version(Repos, TargetErtsVsn, RelName, Fun, releases, ErtsPrompt). 

%%--------------------------------------------------------------------
%% @doc 
%%  Install a release package from a repository. 
%%  IsLocalBoot indicates whether a local specific boot file is to be created or not. See the systools docs for more information.
%% @spec install_remote_release(Repos, TargetErtsVsn, RelName, RelVsn, IsLocalBoot, Options, Timeout) -> ok | {error, Reason} | exit()
%% where
%%     Repos = string()
%%     RelName = string()
%%     RelVsn = string() 
%%     IsLocalBoot = bool()
%%     Options = [{force, force()}, {erts_prompt, erts_prompt()}]
%% @end
%%--------------------------------------------------------------------
install_remote_release(Repos, TargetErtsVsn, RelName, RelVsn, IsLocalBoot, Force, Timeout) ->
    ?INFO_MSG("(~p, ~p, ~p, ~p, ~p)~n", [Repos, TargetErtsVsn, RelName, RelVsn, IsLocalBoot]),
    ReleaseDir = epkg_installed_paths:installed_release_dir_path(RelName, RelVsn),
    case epkg_validation:is_package_a_release(ReleaseDir) of
	false -> 
	    io:format("~nInitiating Install for Remote Release ~s-~s~n", [RelName, RelVsn]),
	    {ok, ReleasePackageDirPath} = fetch_release(Repos, TargetErtsVsn, RelName, RelVsn, Timeout),
	    Res = install_from_local_release_package(Repos, ReleasePackageDirPath, IsLocalBoot, Force, Timeout),
	    io:format("Installation of ~s-~s resulted in ~p~n", [RelName, RelVsn, Res]),
	    Res;
	true -> 
	    epkg_util:overwrite_yes_no(
	      fun() -> install_remote_release(Repos, TargetErtsVsn, RelName, RelVsn, IsLocalBoot, Force, Timeout) end,  
	      fun() -> ok end, 
	      ReleaseDir, 
	      Force)
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  Fetch the the highest version found of an application package from a repository. 
%%
%% <pre>
%% Examples:
%%  fetch_latest_remote_application(["http"//repo.erlware.org/pub"], "5.5.5", gas)
%% </pre>
%%
%% @spec fetch_latest_remote_application(Repos, TargetErtsVsn, AppName, ToDir, Options, Timeout) -> ok | {error, Reason} | exit()
%% where
%%     Repos = string()
%%     TargetErtsVsn = string()
%%     AppName = string()
%%     Options = options()
%% @end
%%--------------------------------------------------------------------
fetch_latest_remote_application(Repos, TargetErtsVsn, AppName, ToDir, Options, Timeout) ->
    ErtsPrompt = fs_lists:get_val(erts_prompt, Options),

    Fun = fun(ManagedRepos, AppVsn, ErtsVsn) ->
		  fetch_remote_application(ManagedRepos, ErtsVsn, AppName, AppVsn, ToDir, Timeout)
	  end,
    fax_util:execute_on_latest_package_version(Repos, TargetErtsVsn, AppName, Fun, lib, ErtsPrompt). 

%%--------------------------------------------------------------------
%% @doc pull down an application from a repo into the ToDir
%% @spec fetch_remote_application(Repos, TargetErtsVsn, AppName, AppVsn, ToDir, Timeout) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
fetch_remote_application(Repos, TargetErtsVsn, AppName, AppVsn, ToDir, Timeout) ->
    try
	ok = epkg_util:foreach_erts_vsn(TargetErtsVsn, 
				       fun(ErtsVsn_) -> 
					       ewr_fetch:fetch_binary_package(Repos, ErtsVsn_, AppName, AppVsn, ToDir, Timeout)
				       end)
    catch
	_Class:_Exception = {badmatch, {error, _} = Error} ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  Fetch the latest version found of a release package from a repository and place it in the specified directory. 
%% @spec fetch_latest_remote_release(Repos, TargetErtsVsn, RelName, ToDir, Options, Timeout) -> 
%%               ok | {error, Reason} | exit()
%% where
%%     Repos = string()
%%     RelName = string()
%%     RelVsn = string() 
%%     ToDir = string()
%% @end
%%--------------------------------------------------------------------
fetch_latest_remote_release(Repos, TargetErtsVsn, RelName, ToDir, Options, Timeout) ->
    ErtsPrompt = fs_lists:get_val(erts_prompt, Options),

    Fun = fun(ManagedRepos, RelVsn, ErtsVsn) ->
		  fetch_remote_release(ManagedRepos, ErtsVsn, RelName, RelVsn, ToDir, Timeout)
	  end,
    fax_util:execute_on_latest_package_version(Repos, TargetErtsVsn, RelName, Fun, releases, ErtsPrompt). 

%%--------------------------------------------------------------------
%% @doc 
%%  Install a release package from a repository. 
%%  IsLocalBoot indicates whether a local specific boot file is to be created or not. See the systools docs for more information.
%% @spec fetch_remote_release(Repos, TargetErtsVsn, RelName, RelVsn, ToDir, Timeout) -> ok | {error, Reason} | exit()
%% where
%%     Repos = string()
%%     RelName = string()
%%     RelVsn = string() 
%%     ToDir = string()
%% @end
%%--------------------------------------------------------------------
fetch_remote_release(Repos, TargetErtsVsn, RelName, RelVsn, ToDir, Timeout) ->
    ?INFO_MSG("(~p, ~p, ~p, ~p)~n", [Repos, TargetErtsVsn, RelName, RelVsn]),
    io:format("~nFetching for Remote Release Package ~s-~s~n", [RelName, RelVsn]),
    Res           = fetch_release(Repos, TargetErtsVsn, RelName, RelVsn, ToDir, Timeout),
    RelDirPath    = epkg_package_paths:package_dir_path(ToDir, RelName, RelVsn),
    RelLibDirPath = epkg_package_paths:release_package_library_path(RelDirPath),
    io:format("Fetching remote erts package (this may take a while) -> "),
    case catch ewr_fetch:fetch_erts_package(Repos, TargetErtsVsn, RelDirPath, Timeout) of
	ok     -> io:format("ok~n");
	_Error -> io:format("can't pull down erts - skipping~n")
    end,
    RelFilePath   = epkg_package_paths:release_package_rel_file_path(RelDirPath, RelName, RelVsn),
    AppAndVsns    = get_app_and_vsns(RelFilePath),
    lists:foreach(fun({AppName, AppVsn}) ->
			  io:format("Pulling down ~s-~s -> ", [AppName, AppVsn]),
			  Res = fetch_remote_application(Repos, TargetErtsVsn, AppName, AppVsn, RelLibDirPath, Timeout),
			  io:format("~p~n", [Res])
		  end, AppAndVsns),
    %Res = fetch_from_local_release_package(Repos, ReleasePackageDirPath, ToDir, Timeout),
    io:format("Fetch on ~s-~s resulted in ~p~n Note* You may install the fetched package with 'faxien install ~s/~s-~s'~n", 
	      [RelName, RelVsn, Res, ToDir, RelName, RelVsn]),
    Res.

get_app_and_vsns(RelFilePath) ->
    [{atom_to_list(element(1, AppSpec)), element(2, AppSpec)} || AppSpec <- epkg_util:consult_rel_file(app_specs, RelFilePath)].
%%====================================================================
%% Internal functions Containing Business Logic
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Install a release from a local package.  If all required app files are not present go out and fetch then and then 
%%      try again.
%% @end
%%--------------------------------------------------------------------
install_from_local_release_package(Repos, ReleasePackageArchiveOrDirPath, IsLocalBoot, Force, Timeout) ->
    ReleasePackageDirPath   = epkg_util:unpack_to_tmp_if_archive(ReleasePackageArchiveOrDirPath),
    case epkg_validation:is_package_a_release(ReleasePackageDirPath) of
	false ->
	    {error, bad_package};
	true ->
	    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(ReleasePackageDirPath),
	    RelFilePath             = epkg_package_paths:release_package_rel_file_path(ReleasePackageDirPath, RelName, RelVsn),
	    TargetErtsVsn           = epkg_util:consult_rel_file(erts_vsn, RelFilePath),
    
	    case catch epkg:install_release(ReleasePackageDirPath) of
		{error, {failed_to_install, AppAndVsns}} ->
		    %% The release does not contain all the applications required.  Pull them down, install them, and try again.
		    lists:foreach(fun({AppName, AppVsn}) ->
					  install_remote_application(Repos, TargetErtsVsn, AppName, AppVsn, Force, Timeout)
				  end, AppAndVsns),
		    install_from_local_release_package(Repos, ReleasePackageDirPath, IsLocalBoot, Force, Timeout);
		
		{error, badly_formatted_or_missing_erts_package} ->
		    %% The release package does not contain the appropriate erts package, and it is 
		    %% not already installed, pull it down install it and try again.
		    ok = install_remote_erts(Repos, TargetErtsVsn, Timeout),
		    install_from_local_release_package(Repos, ReleasePackageDirPath, IsLocalBoot, Force, Timeout);
		
		Other ->
		    ?INFO_MSG("exited release install on a local package with ~p~n", [Other]),
		    Other
	    end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc pull down an application from a repo and return the path to the temp directory where the package was put locally.
%% @spec fetch_app_to_tmp(Repos, TargetErtsVsn, AppName, AppVsn, Timeout) -> {ok, AppPackageDirPath} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
fetch_app_to_tmp(Repos, TargetErtsVsn, AppName, AppVsn, Timeout) ->
    AppDir              = epkg_installed_paths:installed_app_dir_path(TargetErtsVsn, AppName, AppVsn),
    ok                  = ewl_file:delete_dir(AppDir),
    {ok, TmpPackageDir} = epkg_util:create_unique_tmp_dir(),
    case fetch_remote_application(Repos, TargetErtsVsn, AppName, AppVsn, TmpPackageDir, Timeout) of
	ok ->
	    AppPackageDirPath = epkg_package_paths:package_dir_path(TmpPackageDir, AppName, AppVsn),
	    {ok, AppPackageDirPath};
	Error ->
	    Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc pull down an erts package from a repo and return the path to the temp directory where the package was put locally.
%% @spec fetch_erts(Repos, TargetErtsVsn, Timeout) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
fetch_erts(Repos, ErtsVsn, Timeout) ->
    try
	ErtsDir             = epkg_installed_paths:installed_erts_path(ErtsVsn),
	ok                  = ewl_file:delete_dir(ErtsDir),
	{ok, TmpPackageDir} = epkg_util:create_unique_tmp_dir(),
	ok                  = ewr_fetch:fetch_erts_package(Repos, ErtsVsn, TmpPackageDir, Timeout),
	ErtsPackageDirPath  = epkg_package_paths:package_dir_path(TmpPackageDir, "erts", ErtsVsn),
	{ok, ErtsPackageDirPath}
    catch
	_Class:_Exception = {badmatch, {error, _} = Error} ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc pull down a release from a repo.
%% @spec fetch_release(Repos, TargetErtsVsn, RelName, RelVsn, Timeout) -> {ok, ReleasePackageDirPath} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
fetch_release(Repos, TargetErtsVsn, RelName, RelVsn, Timeout) ->
    ReleaseDirPath      = epkg_installed_paths:installed_release_dir_path(RelName, RelVsn),
    ok                  = ewl_file:delete_dir(ReleaseDirPath),
    {ok, TmpPackageDir} = epkg_util:create_unique_tmp_dir(),
    case fetch_release(Repos, TargetErtsVsn, RelName, RelVsn, TmpPackageDir, Timeout) of
	ok ->
	    ReleasePackageDirPath = epkg_package_paths:package_dir_path(TmpPackageDir, RelName, RelVsn),
	    {ok, ReleasePackageDirPath};
	Error ->
	    Error
    end.

fetch_release(Repos, TargetErtsVsn, RelName, RelVsn, ToDir, Timeout) ->
    try
	ok = epkg_util:foreach_erts_vsn(TargetErtsVsn, 
				       fun(ErtsVsn) -> 
					       ewr_fetch:fetch_release_package(Repos, ErtsVsn, RelName, RelVsn, ToDir, Timeout)
				       end)
    catch
	_Class:_Exception = {badmatch, {error, _} = Error} ->
	    Error
    end.
