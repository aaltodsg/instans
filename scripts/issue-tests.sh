#!/bin/sh
die() {
    echo $*
    exit 1
}

cd `dirname $0`/../tests/input/issues
STAMP=`date "+%Y%m%d%H%M%S"`
mkdir -p output
for i in issue?.sh issue??.sh; do
    CORRECT=`basename $i .sh`.correct
    OUTPUT=`basename $i .sh`.output
    test -f ${CORRECT} || CORRECT="/dev/null"
    if (sh $i 2>&1 | sed -e '/^"/s///' -e '/",/s//,/g' -e '/,"/s//,/g' -e '/"$/s///' > ${OUTPUT}); then
	if cmp -s ${CORRECT} ${OUTPUT}; then
	    rm -f ${OUTPUT}
	    echo "OK $i"
	else
	    echo "Unexpected results from $i"
	fi
    else
	echo "Failed $i"
    fi
done
