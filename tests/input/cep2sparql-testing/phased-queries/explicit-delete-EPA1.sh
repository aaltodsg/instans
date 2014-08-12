#!/bin/sh

runtest () {
    echo '------------------------------'
    echo $1
    INPUT=$2
    OUTPUT=$3
    echo ../../../../bin/instans --prefix-encoding=true --rdf-operations=add:execute:flush --allow-rule-instance-removal=false --construct-output-ttl=${OUTPUT} -r explicit-delete-EPA1.rq -g http://instans.org/source --time=- --input-blocks=${INPUT}
    ../../../../bin/instans --prefix-encoding=true --rdf-operations=add:execute:flush --allow-rule-instance-removal=false --construct-output-ttl=${OUTPUT} -r explicit-delete-EPA1.rq -g http://instans.org/source --time=- --input-blocks=${INPUT}
}
runtest '100 event warm-up' ../data/i5sensors100events.ttl /dev/null
#runtest '1 Event' ../data/i5sensors1event.ttl ../data/o5sensors1event.ttl
runtest '100 Events' ../data/i5sensors100events.ttl ../data/o5sensors100events.ttl
runtest '1000 Events' ../data/i5sensors1000events.ttl ../data/o5sensors1000events.ttl
runtest '10000 Events' ../data/i5sensors10000events.ttl ../data/o5sensors10000events.ttl
