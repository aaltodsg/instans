#!/bin/sh
ECHO=/bin/echo
${ECHO} 
${ECHO} "==================="
${ECHO} "Running issue tests"
${ECHO} "==================="
${ECHO}

die() {
    ${ECHO} $*
    exit 1
}

cd `dirname $0`/../tests/input/issues > /dev/null
STAMP=`date "+%Y%m%d%H%M%S"`
for i in issue?.sh issue??.sh; do
    CORRECT=`basename $i .sh`.correct
    OUTPUT=`basename $i .sh`.output
    test -f ${CORRECT} || CORRECT="/dev/null"
    if (sh $i 2>&1 | sed -e '/^"/s///' -e '/",/s//,/g' -e '/,"/s//,/g' -e '/"$/s///' > ${OUTPUT}); then
	if cmp -s ${CORRECT} ${OUTPUT}; then
	    rm -f ${OUTPUT}
	    ${ECHO} "OK $i"
	else
	    ${ECHO} "Unexpected results from $i"
	fi
    else
	${ECHO} "Failed $i"
    fi
done
${ECHO}
${ECHO} "====================="
${ECHO} "Issue tests completed"
${ECHO} "====================="
${ECHO}
