
rm -rf /usr/local/erlware
cd /usr/local
tar -zxf erlware.tar.gz
cd -
make_release.sh

PROJECT_NAME=$(basename `pwd`)
VSN=$(basename $(dirname $(ls _build/development/releases/*/$PROJECT_NAME".boot" 2> /dev/null | sed s/.boot// 2> /dev/null)))
RELEASE_NAME=$PROJECT_NAME"-"$VSN

cp install.sh $RELEASE_NAME/
cd $RELEASE_NAME
./install.sh
