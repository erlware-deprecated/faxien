#!/bin/sh

# Thanks to Scott Parish for this tip. Halts the shell execution on 
# the failure of a simple command (see sh SHELL GRAMMER for more).
set -e

echo " Starting the Faxien/Epkg Smoke Test"
echo "====================================="
echo ""

faxien search cos 
faxien installed #| head -n 5

echo "describe the sinan application at its latest version"
faxien describe_app sinan #| head -n 5

(sleep 1; echo "yes") | faxien rr sinan
faxien install-release sinan 0.8.6

faxien outdated_apps
echo "upgrade the sinan app"
faxien upgrade_app sinan

faxien outdated-releases
echo "upgrade all installed releases"
faxien upgrade-all-releases

echo "add and remove publish repos"
faxien add_publish_repo http://test_publish_repo.com
faxien show_publish_repos
faxien remove_publish_repo http://test_publish_repo.com
faxien show_publish_repos

echo "add and remove repos"
faxien add_repo http://testrepo.com
faxien show_repos
faxien remove_repo http://testrepo.com
faxien show_repos

echo "show request timeout"
faxien show_request_timeout
echo "update request timeout to 99999"
faxien set_request_timeout 99999
echo "show request altered timeout"
faxien show_request_timeout

echo "show faxien and epkg versions"
faxien version
