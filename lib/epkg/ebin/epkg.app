{application, epkg,
 [{description, "Manages local erlang/OTP packages. Faxien depends on this for local operations."},
  {vsn, "0.2.1.3"},
  {modules, [
	epkg,
	epkg_cmdln,
	epkg_installed_paths,
	epkg_install,
	epkg_manage,
	epkg_package_paths,
	epkg_util,
	epkg_validation
             ]},
  {registered, []},
  {versioned_dependencies, []},
  {applications, [eunit, kernel, stdlib, ewrepo, ewlib, gas, fslib]}]}.
