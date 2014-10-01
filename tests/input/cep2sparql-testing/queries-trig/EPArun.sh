#!/bin/sh
../../../../bin/instans --prefix-encoding=true -r $1 --input-blocks=../data/CEP2SPARQL_SamplePattern.trig --allow-rule-instance-removal=true --rdf-operations=add:execute-snapshot:remove:execute:flush --time=- --input-blocks=../data/$2
