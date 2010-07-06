%%% -*- mode:erlang -*-
{application, faxien,
 [
  % A quick description of the application.
  {description, "The erlang package management tool."},

  % The version of the applicaton
  {vsn, "0.43.1.23"},

  % All modules used by the application. 
  {modules,
   [
    faxien,
    fax_install,
    fax_manage,
    fax_publish,
    fax_source_put,
    fax_doc_put,
    fax_put,
    fax_util
   ]},
  
  % A list of the registered processes in your application.  Used to prevent collisions. 
  {registered, []},

  % This is a list of the applications that your application depends on. This list must be filled out
  % carefully so that dependency resolution systems can function properly.
  {applications, [eunit, kernel, stdlib, xmerl, ibrowse, epkg, ewrepo, ewlib, cryptographic, fslib, gas]},

  % configuration parameters similar to those in the config file specified
  % on the command line. can be fetched with gas:get_env
  {env, []}
 ]
}.

