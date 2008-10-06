#!/bin/sh

# Make sure a patch identifier has been provided as an argument
if [ -z "$1" ]; then
	echo "You must provide the patch identifier to make a release!"
	exit 1
fi

# Get ERTS version from erlang 
ERTS_VSN=`erl -eval 'io:format("~s\n", [erlang:system_info(version)]).' -noshell -s erlang halt`

# Construct release version by appending patch ID to ERTS version
REL_VSN="$ERTS_VSN.$1"

# Setup the directories
TARGET_DIR=release/erl-$REL_VSN
mkdir -p $TARGET_DIR/bin
mkdir -p $TARGET_DIR/releases/$REL_VSN

# Process template scripts
for f in templates/*; do
    basef=`basename $f`
    cat $f |sed "s/@ERTS_VSN@/$ERTS_VSN/g"|sed "s/@REL_VSN@/$REL_VSN/g" > $TARGET_DIR/bin/$basef
done

# Create the release file
escript create-rel $REL_VSN $TARGET_DIR/releases/$REL_VSN
