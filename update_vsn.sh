#!/bin/sh


if [ -e "./_build.cfg" ];then
	echo "found _build.cfg beginning version update"
else
	echo "no _build.cfg in `pwd`, exiting"
	exit 1
fi

CURRENT_VSN=$(grep vsn _build.cfg | awk '{print $3}' | sed -e 's/"//g')

NEW_LAST_DIGIT=$(echo "$(echo $CURRENT_VSN | sed -e 's/.*\.\(.*\)/\1/') + 1" | bc -l)
VSN_STEM=$(echo $CURRENT_VSN | sed -e 's/\(.*\)\..*/\1/')
NEW_VSN="$VSN_STEM.$NEW_LAST_DIGIT"
if [ $# != 0 ];then
	NEW_VSN=$1
fi

if [ $# = 2 ];then
	CURRENT_VSN=$1
	NEW_VSN=$1
	echo "using versions specified entirely by the user"
fi

echo "Starting version upgrade from `pwd`"
FILES=$(find . -type f | xargs grep $CURRENT_VSN | awk -F: '{print $1}')

echo "Changing version from $CURRENT_VSN to $NEW_VSN"
echo "within $FILES"

for FILE in $FILES;do
	TMP_FILE="$FILE.tmp"
	sed -e "s/$CURRENT_VSN/$NEW_VSN/" $FILE > $TMP_FILE
	mv $TMP_FILE $FILE
done
