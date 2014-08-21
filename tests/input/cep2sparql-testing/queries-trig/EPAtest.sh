#!/bin/sh
../../../../bin/instans --prefix-encoding=true -r $1 --allow-rule-instance-removal=true --rdf-operations=add:execute-snapshot:remove:execute:flush --time=- --input-blocks=../CEP2SPARQL_SampleEvents.trig
