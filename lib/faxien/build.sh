#!/bin/sh

SINAN=`which sinan`
if [ -e $SINAN ];then
	touch _build.cfg
	sinan
else
	echo "$0 build failed, could not find Sinan"
fi
