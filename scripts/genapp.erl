%% Create .app and .beam files for erlang built-in libs
%% that do not have them, so they can be published with
%% faxien. This should be run with escript with the path
%% to the installed erlang lib directory as the only
%% argument.

%%
%% Template function which returns a skeleton .app file contents
%%
app_template(AppName, AppVsn, Beams) ->
    {application, list_to_atom(AppName),
     [{description, AppName},
      {vsn, AppVsn},
      {modules, Beams},
      {registered, []},
      {applications, [kernel, stdlib]}]}.


%%
%% Make certain that a given app directory has at least one .beam and that a .app file exists
%%
ensure_app(Base, AppDir) ->
    [AppName, AppVsn] = string:tokens(AppDir, "-"),
    Dir = filename:join([Base, AppDir, "ebin"]),

    %% First check for one or more .beam files -- if none exist, create one
    case filelib:wildcard(filename:join([Dir, "*.beam"])) of
        [] ->
            %% No .beam files found -- create a .erl file and them compile to .beam
            ErlName = filename:join([Dir, AppName ++ ".erl"]),
            ok = filelib:ensure_dir(ErlName),
            ErlSource = lists:concat(["-module(", AppName, ")."]),
            ok = file:write_file(ErlName, list_to_binary(ErlSource)),
            {ok, _} = compile:file(ErlName, [{outdir, Dir}]),
            BeamFiles = [filename:join([Dir, AppName ++ ".beam"])],
            ok;
        BeamFiles ->
            ok
    end,

    %% Now that we have a list of .beam files, check for and create (if necessary) a .app file
    DotAppFile = filename:join([Dir, AppName ++ ".app"]),
    case filelib:is_file(DotAppFile) of
        true ->
            %% App file exists; move along, nothing to do here
            ok;
        false ->
            %% Convert list of beam files into list of atoms, suitable for use in a .app file
            Beams = [list_to_atom(filename:basename(F, ".beam")) || F <- BeamFiles],
    
            %% Now that we have at least one  beam file, check for a .app file; if it doesn't exist,
            %% create it
            AppData = app_template(AppName, AppVsn, Beams),
            ok = file:write_file(DotAppFile, list_to_binary(io_lib:format("~p.\n", [AppData])))
    end.


%%
%% Given a filename, validate that it is a directory and that it conforms to the <app>-<vsn>
%% specification
%%
is_app_dir(Base, Name) ->
    FullName = filename:join([Base, Name]),
    case filelib:is_dir(FullName) of
        true ->
            L = length(Name),
            case regexp:matches(Name, "[a-zA-Z][a-zA-Z0-9_]*-[0-9.]+") of
                {match, [{1, L}]} ->
                    true;
                _ ->
                    false
            end;
        false ->
            false
    end.


main([Dir]) ->
    %% Using the provided directory, scan for sub-directories with a name that matches
    %% <app>-<vsn>
    {ok, Files} = file:list_dir(Dir),
    AppDirs = [F || F <- Files, is_app_dir(Dir, F)],

    %% Each app directory needs to have at least one .beam file and a .app file. Traverse all
    %% the directories and create the appropriate files as necessary
    [ok = ensure_app(Dir, AppDir) || AppDir <- AppDirs].
