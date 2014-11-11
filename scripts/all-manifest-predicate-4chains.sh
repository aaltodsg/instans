#!/bin/zsh
# This script prints all predicates in all manifest files
cd ~/instans/tests/

PREFIXES=`sed -n '/@prefix/p' data-*/**/manifest.ttl |awk '{print "PREFIX", $2, $3;}'|sort -u|egrep -v 'PREFIX :'`
echo $PREFIXES

for m in data-*/**/manifest.ttl; do
    ../bin/instans --prefix-encoding=true -b `dirname $m` -r <(cat <<EOF
$PREFIXES

SELECT DISTINCT ?p1 ?p2 ?p3 ?p4 WHERE {
       ?x ?p1 [ ?p2 [ ?p3 [ ?p4 ?y ] ] ]
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
# <>,<>,<>,<>
# <>,<>,<>,dawgt:approval
# <>,<>,<>,dawgt:approvedBy
# <>,<>,<>,mf:action
# <>,<>,<>,mf:description
# <>,<>,<>,mf:feature
# <>,<>,<>,mf:name
# <>,<>,<>,mf:notable
# <>,<>,<>,mf:requires
# <>,<>,<>,mf:result
# <>,<>,<>,qt:queryForm
# <>,<>,<>,rdf:type
# <>,<>,<>,rdfs:comment
# <>,<>,<>,rdfs:seeAlso
# <>,<>,mf:action,qt:data
# <>,<>,mf:action,qt:graphData
# <>,<>,mf:action,qt:query
# <>,<>,mf:action,qt:serviceData
# <>,<>,mf:action,sd:EntailmentProfile
# <>,<>,mf:action,sd:entailmentRegime
# <>,<>,mf:action,ut:data
# <>,<>,mf:action,ut:graphData
# <>,<>,mf:action,ut:request
# <>,<>,mf:result,ut:data
# <>,<>,mf:result,ut:graphData
# <>,<>,mf:result,ut:result
# <>,mf:action,qt:serviceData,qt:data
# <>,mf:action,qt:serviceData,qt:endpoint
# <>,mf:action,sd:EntailmentProfile,<>
# <>,mf:action,sd:entailmentRegime,<>
# <>,mf:action,ut:graphData,rdfs:label
# <>,mf:action,ut:graphData,ut:graph
# <>,mf:result,ut:graphData,rdfs:label
# <>,mf:result,ut:graphData,ut:graph
# mf:action,sd:EntailmentProfile,<>,<>
# mf:action,sd:entailmentRegime,<>,<>
# mf:entries,<>,<>,<>
# mf:entries,<>,<>,dawgt:approval
# mf:entries,<>,<>,dawgt:approvedBy
# mf:entries,<>,<>,mf:action
# mf:entries,<>,<>,mf:feature
# mf:entries,<>,<>,mf:name
# mf:entries,<>,<>,mf:result
# mf:entries,<>,<>,mf:resultCardinality
# mf:entries,<>,<>,qt:queryForm
# mf:entries,<>,<>,rdf:type
# mf:entries,<>,<>,rdfs:comment
# mf:entries,<>,mf:action,qt:data
# mf:entries,<>,mf:action,qt:graphData
# mf:entries,<>,mf:action,qt:query
# mf:entries,<>,mf:action,qt:serviceData
# mf:entries,<>,mf:action,sd:entailmentRegime
# mf:entries,<>,mf:action,ut:data
# mf:entries,<>,mf:action,ut:graphData
# mf:entries,<>,mf:action,ut:request
# mf:entries,<>,mf:result,ut:data
# mf:entries,<>,mf:result,ut:graphData
# mf:entries,<>,mf:result,ut:result
# p1,p2,p3,p4
# sd:EntailmentProfile,<>,<>,<>
# sd:entailmentRegime,<>,<>,<>
