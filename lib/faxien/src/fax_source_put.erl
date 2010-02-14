%%%-------------------------------------------------------------------
%%% @author Martin Logan 
%%% @doc The functions in this file place packages and artifacts into a remote repository. 
%%% 
%%% @copyright (C) 2007, Martin Logan, Eric Merritt, Erlware
%%% @end
%%%-------------------------------------------------------------------
-module(fax_source_put).

-export([
	 put_app_package/5,
	 put_dot_app_file/5,
	 put_signature_file/6,
	 put_checksum_file/6
	]).

-macros("macros.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc put an unbuilt application src package onto a remote repository.
%% @spec (Repos, AppName, AppVsn, Payload, Timeout) -> {ok, Urls} | {error, Reason}
%% where
%%  Timeout = Milliseonds::integer()
%% @end 
%%--------------------------------------------------------------------
put_app_package(Repos, AppName, AppVsn, Payload, Timeout) when is_binary(Payload) -> 
    Suffix = ewr_source_repo_paths:package_suffix("lib", AppName, AppVsn),
    fax_put:repos_put(Repos, Suffix, Payload, Timeout).

%%--------------------------------------------------------------------
%% @doc put a .app onto a remote repository.
%% @spec put_dot_app_file(Repos, AppName, AppVsn, Payload, Timeout) -> {ok, Urls} | {error, Reason}
%% where
%%  Timeout = Milliseonds::integer()
%% @end 
%%--------------------------------------------------------------------
put_dot_app_file(Repos, AppName, AppVsn, Payload, Timeout) when is_binary(Payload) -> 
    Suffix = ewr_source_repo_paths:dot_app_file_suffix(AppName, AppVsn),
    fax_put:repos_put(Repos, Suffix, Payload, Timeout).

%%--------------------------------------------------------------------
%% @doc put a signature file onto a remote repository.
%% @spec put_signature_file(Repos, Side, PackageName, PackageVsn, Payload, Timeout) -> {ok, Urls} | {error, Reason}
%% where
%%  Timeout = Milliseonds::integer()
%% @end 
%%--------------------------------------------------------------------
put_signature_file(Repos, Side, PackageName, PackageVsn, Payload, Timeout) when is_binary(Payload) -> 
    Suffix = ewr_source_repo_paths:signature_file_suffix(Side, PackageName, PackageVsn),
    fax_put:repos_put(Repos, Suffix, Payload, Timeout).

%%--------------------------------------------------------------------
%% @doc put a checksum file onto a remote repository.
%% @spec put_checksum_file(Repos, Side, PackageName, PackageVsn, Payload, Timeout) -> {ok, Urls} | {error, Reason}
%% where
%%  Timeout = Milliseonds::integer()
%% @end 
%%--------------------------------------------------------------------
put_checksum_file(Repos, Side, PackageName, PackageVsn, Payload, Timeout) when is_binary(Payload) -> 
    Suffix   = ewr_source_repo_paths:checksum_file_suffix(Side, PackageName, PackageVsn),
    MD5      = epkg_util:md5(Payload), 
    Checksum = list_to_binary(io_lib:fwrite("~s", [MD5])),
    fax_put:repos_put(Repos, Suffix, Checksum, Timeout).

%%====================================================================
%% Internal functions
%%====================================================================
