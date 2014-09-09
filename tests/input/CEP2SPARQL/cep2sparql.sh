#!/bin/sh
../../../bin/instans --prefix-encoding=true -r construct-event-output.rq -r EPA-All.rq --input-blocks=CEP2SPARQL_SamplePattern.trig --allow-rule-instance-removal=true --rdf-operations=add:execute-snapshot:remove:execute:flush --time=- --input-blocks=CEP2SPARQL_SampleEvents.trig
