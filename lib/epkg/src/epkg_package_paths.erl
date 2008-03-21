%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @doc 
%%% 
%%% @end
%%% @copyright (C) 2007, Martin Logan, Erlware
%%% Created : 14 Dec 2007 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epkg_package_paths).

%% API
-export([
	 package_dir_path/3,
	 release_package_library_path/1,
	 release_package_control_file_path/1,
	 release_package_rel_file_path/3,
	 release_package_app_package_path/3,
	 release_package_erts_package_path/2
	]).

-include("macros.hrl").
%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Return the path to a newly downloaded package directory.
%% @spec package_dir_path(ContainerDirPath, PackageName, PackageVsn) -> string()
%% @end 
%%--------------------------------------------------------------------
package_dir_path(ContainerDirPath, PackageName, PackageVsn) -> 
    ewl_file:join_paths(ContainerDirPath, PackageName ++ "-" ++ PackageVsn).

%%--------------------------------------------------------------------
%% @doc Return the path to a .rel file in a package.
%% @spec release_package_rel_file_path(ReleasePackagePath::string(), RelName::string(), RelVsn::string()) -> string()
%% @end 
%%--------------------------------------------------------------------
release_package_rel_file_path(ReleasePackagePath, RelName, RelVsn) ->
    ewl_file:join_paths(rel_file_base_dir(ReleasePackagePath, RelVsn), RelName ++ ".rel").

%%--------------------------------------------------------------------
%% @doc Return the path to an app package within a release package.
%% @spec release_package_app_package_path(ReleasePackagePath, AppName, AppVsn) -> string()
%% @end 
%%--------------------------------------------------------------------
release_package_app_package_path(ReleasePackagePath, AppName, AppVsn) ->
    filename:join([ReleasePackagePath, "lib", AppName ++ "-" ++ AppVsn]).

%%--------------------------------------------------------------------
%% @doc Return the path to the directory that contains the releases libraries.
%% @spec release_package_library_path(ReleasePackagePath) -> string()
%% @end 
%%--------------------------------------------------------------------
release_package_library_path(ReleasePackagePath) ->
    filename:join([ReleasePackagePath, "lib"]).

%%--------------------------------------------------------------------
%% @doc Return the path to an app package within a release package.
%% @spec release_package_erts_package_path(ReleasePackagePath, ErtsVsn) -> string()
%% @end 
%%--------------------------------------------------------------------
release_package_erts_package_path(ReleasePackagePath, ErtsVsn) ->
    ewl_file:join_paths(ReleasePackagePath, "erts-" ++ ErtsVsn).

%%--------------------------------------------------------------------
%% @doc Return the path to the control file within a release package.
%% @spec release_package_control_file_path(ReleasePackagePath) -> string()
%% @end 
%%--------------------------------------------------------------------
release_package_control_file_path(ReleasePackagePath) ->
    ewl_file:join_paths(ReleasePackagePath, "control").

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Return the directory that contains the .rel, .boot, and .script files. 
%% @end 
%%--------------------------------------------------------------------
rel_file_base_dir(ReleasePackagePath, RelVsn) ->
    filename:join([ReleasePackagePath, "releases", RelVsn]).
    
