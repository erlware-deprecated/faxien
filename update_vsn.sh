#!/bin/sh


if [ $# != 1 ];then
	echo "Usage $0 <new-vsn>"
	exit 1
fi

CURRENT_VSN=$(grep vsn _build.cfg | awk '{print $3}' | sed -e 's/"//g')
NEW_VSN=$1
FILES=$(find . -type f | xargs grep $CURRENT_VSN | awk -F: '{print $1}')

echo "Changing version from $CURRENT_VSN to $NEW_VSN"
echo "within $FILES"

for FILE in $FILES;do
	TMP_FILE="$FILE.tmp"
	sed -e "s/$CURRENT_VSN/$NEW_VSN/" $FILE > $TMP_FILE
	mv $TMP_FILE $FILE
done
