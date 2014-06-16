#!/bin/sh
test $# -eq 1 || (echo "Usage: fnb-test <100|1000>"; exit 1)
COUNT=$1
cd `dirname $0`/../tests
STAMP=`date "+%Y%m%d%H%M%S"`
CORRECT=correct-output/fnb${COUNT}.out
mkdir -p output
OUT=output/fnb${COUNT}.out-${STAMP}
# echo ../bin/instans -b file:/// -d input/CloseFriends -r CF-Queries-default.rq --report=all -t 5actors${COUNT}events.ttl
# ../bin/instans -b file:/// -d input/CloseFriends -r CF-Queries-default.rq --report=all -t 5actors${COUNT}events.ttl > ${OUT} 2>&1
echo ../bin/instans -b file:/// -d input/CloseFriends -r CF-Queries-default.rq --report all -t 5actors${COUNT}events.ttl
../bin/instans -b file:/// -d input/CloseFriends -r CF-Queries-default.rq --report all -t 5actors${COUNT}events.ttl > ${OUT} 2>&1
if test -f ${CORRECT} ; then
 if cmp ${CORRECT} ${OUT}; then
     echo
     echo "Test OK: Same results as previous time. Output is in ${CORRECT}"
     rm ${OUT}
 else
     echo
     echo "Test not OK!"
     echo
     /bin/echo -n "Diff ${OUT} ${CORRECT} (yes)? "
     read answer
     if test "$answer" != "no"; then
	 diff ${OUT} ${CORRECT} | less
     fi
     /bin/echo -n "Accept ${OUT} as ${CORRECT} (no)? "
     read answer
     if test "$answer" = "yes"; then
	 mv ${CORRECT} ${CORRECT}.${STAMP}
	 mv ${OUT} ${CORRECT}
     fi
 fi
fi
