%%% -*- mode:erlang -*-
{application, 'epkg',
 [
  % A quick description of the application.
  {description, "Manages local erlang/OTP packages. Faxien depends on this for local operations."},

  % The version of the applicaton
  {vsn, "0.3.2.1"},

  % All modules used by the application.
  {modules,
   [
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

