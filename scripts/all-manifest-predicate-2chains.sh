#!/bin/zsh
# This script prints all predicates in all manifest files
cd ~/instans/tests/

PREFIXES=`sed -n '/@prefix/p' data-*/**/manifest.ttl |awk '{print "PREFIX", $2, $3;}'|sort -u|egrep -v 'PREFIX :'`
echo $PREFIXES

for m in data-*/**/manifest.ttl; do
    ../bin/instans --prefix-encoding=true -b `dirname $m` -r <(cat <<EOF
$PREFIXES

SELECT DISTINCT ?p1 ?p2 WHERE {
       ?x ?p1 ?y .
       ?y ?p2 ?z
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
# <>,<>
# <>,dawgt:approval
# <>,dawgt:approvedBy
# <>,mf:action
# <>,mf:description
# <>,mf:feature
# <>,mf:name
# <>,mf:notable
# <>,mf:requires
# <>,mf:result
# <>,mf:resultCardinality
# <>,qt:queryForm
# <>,rdf:type
# <>,rdfs:comment
# <>,rdfs:seeAlso
# mf:action,qt:data
# mf:action,qt:graphData
# mf:action,qt:query
# mf:action,qt:serviceData
# mf:action,sd:EntailmentProfile
# mf:action,sd:entailmentRegime
# mf:action,ut:data
# mf:action,ut:graphData
# mf:action,ut:request
# mf:entries,<>
# mf:result,ut:data
# mf:result,ut:graphData
# mf:result,ut:result
# p1,p2
# qt:serviceData,qt:data
# qt:serviceData,qt:endpoint
# sd:EntailmentProfile,<>
# sd:entailmentRegime,<>
# ut:graphData,rdfs:label
# ut:graphData,ut:graph
