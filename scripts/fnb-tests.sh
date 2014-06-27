#!/bin/sh
ECHO=/bin/echo
${ECHO} 
${ECHO} "================="
${ECHO} "Running fnb tests"
${ECHO} "================="
${ECHO}
cd `dirname $0`/.. > /dev/null
ROOT=`pwd`
BIN=${ROOT}/bin
TESTS=${ROOT}/tests
cd ${TESTS} > /dev/null
mkdir -p output
STAMP=`date "+%Y%m%d%H%M%S"`
FNBDIR=${TESTS}/input/CloseFriends
INSTANS=${BIN}/instans
# ${ECHO} "INSTANS=${INSTANS}"
# ${ECHO} "FNBDIR=${FNBDIR}"
# ${ECHO}
for COUNT in $*; do
    if test $COUNT != 100 -a $COUNT != 1000 -a $COUNT != 10000; then
	${ECHO} "usage: $0 {100|1000|10000}"
	exit 1
    fi
    CORRECT=correct-output/fnb${COUNT}.out
    OUT=output/fnb${COUNT}.out-${STAMP}
#     ${ECHO} '${INSTANS}' -b file:/// -d '${FNBDIR}' -r CF-Queries-default.rq --report=all -t 5actors${COUNT}events.ttl
    ${ECHO} "FNB $COUNT"
    ${INSTANS} -b file:/// -d ${FNBDIR} -r CF-Queries-default.rq --report=select:modify -t 5actors${COUNT}events.ttl > ${OUT} 2>&1
    if test -f ${CORRECT} ; then
	if cmp ${CORRECT} ${OUT}; then
	    ${ECHO}
	    ${ECHO} "Test OK: Same results as previous time. Output is in ${CORRECT}"
	    ${ECHO}
	    rm ${OUT}
	else
	    ${ECHO}
	    ${ECHO} "Test not OK!"
	    ${ECHO}
	    ${ECHO} -n "Diff ${OUT} ${CORRECT} (yes)? "
	    read answer
	    if test "$answer" != "no"; then
		diff ${OUT} ${CORRECT} | less
	    fi
	    ${ECHO} -n "Accept ${OUT} as ${CORRECT} (no)? "
	    read answer
	    if test "$answer" = "yes"; then
		mv ${CORRECT} ${CORRECT}.${STAMP}
		mv ${OUT} ${CORRECT}
	    fi
	    ${ECHO}
	fi
    else
	${ECHO}
	${ECHO} "No CORRECT=${CORRECT}"
	${ECHO} -n "Less ${OUT} (yes)? "
	read answer
	if test "$answer" != "no"; then
	    less ${OUT}
	fi
	${ECHO} -n "Accept ${OUT} as ${CORRECT} (no)? "
	read answer
	if test "$answer" = "yes"; then
	    mv ${OUT} ${CORRECT}
	fi
	${ECHO}
    fi
done
${ECHO}
${ECHO} "==================="
${ECHO} "FNB tests completed"
${ECHO} "==================="
${ECHO}
