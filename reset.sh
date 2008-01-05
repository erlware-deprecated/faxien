rm -rf /usr/local/erlware
cd /usr/local
tar -zxf erlware.tar.gz
cd -
./make_release.sh
rm -rf /usr/local/erlware/lib/epkg-0.1.0.0
rm -rf /usr/local/erlware/lib/faxien-0.24.0.1
cd ./faxien-0.24.0.1
./install.sh
