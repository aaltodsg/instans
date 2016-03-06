#!/bin/bash

DIR=`dirname $0`
DIR=`cd $DIR`
OPTIONS_FILE=$DIR/test-options
[ -f $OPTIONS_FILE ] || (echo "Missing options file $OPTIONS_FILE"; exit 1)

if [ $# -eq 3 ]; then
    LABEL=$1:$2:$3
elif [ $# -eq 1 ]; then
    LABEL=$1
else
    echo "Usage: $0 suite collection name"
    exit 1
fi
# echo "LABEL=$LABEL"
fgrep $LABEL $OPTIONS_FILE



