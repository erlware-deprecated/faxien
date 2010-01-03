%%% -*- mode:erlang -*-
{application, epkg,
 [
  % A quick description of the application.
  {description, "Manages local erlang/OTP packages. Faxien depends on this for local operations."},

  % The version of the applicaton
  {vsn, "0.15.3.1"},

  % All modules used by the application.
  {modules,
   [
    epkg_cmdln,
    epkg,
    epkg_installed_paths,
    epkg_control,
    epkg_install,
    epkg_manage,
    epkg_util,
    epkg_validation
   ]},

  % A list of the registered processes in your application.  Used to prevent collisions. 
  {registered, []},

  % The applications this app depends on in order to run. 
  {applications, [eunit, kernel, stdlib, ewrepo, ewlib, gas, fslib]},

  % configuration parameters similar to those in the config file specified
  % on the command line. can be fetched with gas:get_env
  {env, []}
 ]
}.

