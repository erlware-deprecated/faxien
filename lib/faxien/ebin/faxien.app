{application, faxien,
 [{description, "The erlang package management tool."},
  {vsn, "0.27.1.0"},
  {modules, [
	faxien,
	fax_publish,
	fax_put,
	fax_install,
	fax_manage,
	fax_cmdln,
	fax_util
             ]},
  {registered, []},
  {versioned_dependencies, [{ibrowse, "1.2.4.1", gte}]},
  {applications, [eunit, kernel, stdlib, xmerl, ibrowse, ewrepo, ewlib, fslib, gas]}]}.
