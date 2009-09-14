#!/bin/sh

SINAN=`which sinan`
if [ -e $SINAN ];then
	rm _build.cfg
	echo "project : {\nname : faxien\nvsn  : \"0.42.2.4\"\n},\n" > _build.cfg
	sinan
	cp _build/development/apps/faxien*/ebin/*beam ./ebin
	rm -rf _build.cfg _build
else
	echo "$0 build failed, could not find Sinan"
fi
