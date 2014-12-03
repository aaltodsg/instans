#!/bin/sh
../../../../bin/instans --prefix-encoding=true -r construct-event-output.rq -r EPA3.rq --allow-rule-instance-removal=true --rdf-operations=add:execute-snapshot:remove:execute:flush --time=- --input-blocks=../data/for-EPA3.trig
