#!/bin/sh
test $# -eq 1 || (echo "Usage: fnb-test <100|1000>"; exit 1)
COUNT=$1
cd `dirname $0`/../tests
STAMP=`date "+%Y%m%d%H%M%S"`
CORRECT=correct-output/fnb${COUNT}.out
OUT=output/fnb${COUNT}.out-${STAMP}
../bin/instans -b file:///. -d input -r CF-Queries-default.rq --report all -t 5actors${COUNT}events.ntriples | tee ${OUT} 2>&1
if test -f ${CORRECT} ; then
 if cmp ${CORRECT} ${OUT}; then
     echo "Test OK: Same results as previous time"
 else
     echo "Test not OK: ${CORRECT} and ${OUT} differ"
 fi
fi




