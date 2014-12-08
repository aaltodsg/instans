#!/bin/sh
ROOT=`dirname $0`
cd $ROOT > /dev/null
INSTANS=../../../bin/instans
test -x ${INSTANS} || (echo "No INSTANS?"; exit 1)

${INSTANS} --prefix-encoding=true -r test1.rq --report=all -t test.ttl
${INSTANS} --prefix-encoding=true -r test2.rq --report=all -t test.ttl
${INSTANS} --prefix-encoding=true -r test3.rq --report=all -t test.ttl
${INSTANS} --prefix-encoding=true -r test4.rq --report=all -t test.ttl
