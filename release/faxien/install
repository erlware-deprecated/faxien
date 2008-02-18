#!/bin/sh

# This file is meant to run in the base directory of the faxien package. It is not meant for development. 

PREFIX="/usr/local/erlware"
if [ "$1" != "" ]; then
PREFIX="$1"
fi

if [ "$1" = "help" ] || [ "$1" = "--help" ];then
    echo "just run install with no arguments to install to the default location"
    echo "or run install <path> to install in a non standard location"
    exit 0
fi

# Follow symlinks to the binary 
echo $1 | sed -e 's;--;;' |  grep -e 'prefix=.*' > /dev/null
if [ "$?" = "0" ];then
	echo "$0 Error: use --prefix without an equals sign"
	exit 1
fi

PROG_NAME="$0"
BASE_DIR=`dirname $PROG_NAME`
cd $BASE_DIR
BASE_DIR=`pwd`
cd -
CONFIG_FILE=`ls $BASE_DIR/releases/*/sys.config 2> /dev/null`
FAX_DIR=$(dirname `ls $BASE_DIR/lib/faxien*/ebin/faxien.app 2> /dev/null`)
uname | grep CYGWIN

if [ "$?" = "0" ];then
	CONFIG_FILE=`echo $CONFIG_FILE | sed -e 's;\/cygdrive\/.;;'`
	FAX_DIR=`echo $FAX_DIR | sed -e 's;\/cygdrive\/.;;'`
	BASE_DIR=`echo $BASE_DIR | sed -e 's;\/cygdrive\/.;;'`
fi

WILDCARD_DIRS="[\"$BASE_DIR/lib/*\"]"
echo "PROG_NAME: $PROG_NAME"
echo "BASE_DIR: $BASE_DIR"
echo "FAX_DIR: $FAX_DIR"
echo "CONFIG_FILE $CONFIG_FILE"
echo "WILDCARD_DIRS $WILDCARD_DIRS"


erl -pz $FAX_DIR -config $CONFIG_FILE -s fax_cmdln faxien_apply application load faxien -s fax_cmdln faxien_apply fax_util add_pzs $WILDCARD_DIRS -s ibrowse start -s fax_cmdln faxien_apply faxien install $BASE_DIR -s init stop -noshell -prefix $PREFIX
