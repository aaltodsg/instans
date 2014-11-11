#!/bin/zsh
# This script prints all predicates in all manifest files
cd ~/instans/tests/

PREFIXES=`sed -n '/@prefix/p' data-*/**/manifest.ttl |awk '{print "PREFIX", $2, $3;}'|sort -u|egrep -v 'PREFIX :'`
echo $PREFIXES

for m in data-*/**/manifest.ttl; do
    ../bin/instans --prefix-encoding=true -b `dirname $m` -r <(cat <<EOF
$PREFIXES

SELECT DISTINCT ?p WHERE {
       ?s ?p ?o .
}
EOF
) --input=$m
done | egrep -v '(^PREFIX|^p$)'|sort -u

# PREFIX dawgt: <http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#>
# PREFIX ent: <http://www.w3.org/ns/entailment/>
# PREFIX mf: <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#>
# PREFIX mfx: <http://jena.hpl.hp.com/2005/05/test-manifest-extra#>
# PREFIX pr: <http://www.w3.org/ns/owl-profile/>
# PREFIX qt: <http://www.w3.org/2001/sw/DataAccess/tests/test-query#>
# PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# PREFIX sd: <http://www.w3.org/ns/sparql-service-description#>
# PREFIX sparql: <http://www.w3.org/ns/sparql#>
# PREFIX ut: <http://www.w3.org/2009/sparql/tests/test-update#>
# <>
# dawgt:approval
# dawgt:approvedBy
# mf:action
# mf:description
# mf:entries
# mf:feature
# mf:name
# mf:notable
# mf:requires
# mf:result
# mf:resultCardinality
# qt:data
# qt:endpoint
# qt:graphData
# qt:query
# qt:queryForm
# qt:serviceData
# rdf:type
# rdfs:comment
# rdfs:label
# rdfs:seeAlso
# sd:EntailmentProfile
# sd:entailmentRegime
# ut:data
# ut:graph
# ut:graphData
# ut:request
# ut:result



