#!/bin/sh

# http://www.w3.org/1999/02/22-rdf-syntax-ns#first
# http://www.w3.org/1999/02/22-rdf-syntax-ns#rest
# http://www.w3.org/1999/02/22-rdf-syntax-ns#type
# http://www.w3.org/2000/01/rdf-schema#comment
# http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action
# http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#entries
# http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name
# http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result
# http://www.w3.org/ns/rdftest#approval

FUSEKILS=`ps alx |grep fuseki-server|grep java`
if test $? -eq 0; then
    FUSEKIPID=`echo $FUSEKILS | awk '{print $2;}'`
#    echo "Killing fuseki-server $FUSEKIPID"
    kill $FUSEKIPID
fi
#echo "Starting new fuseki-server"
fuseki-server --update --mem /ds &> /dev/null &
sleep 2
echo "Inspecting $1:"
s-put http://localhost:3030/ds/data default $1
function query () {
    TMP=$$_tmp
    echo $1
    shift
    s-query --service http://localhost:3030/ds/query --output=csv "$*" > $TMP
#    head -1 $TMP
    sed -n '2,$p' $TMP|sort
    rm -f $TMP
}
#echo
#query "Predicates" 'SELECT DISTINCT ?p {?s ?p ?o}'
echo
query "Tests grouped by type with counts" 'SELECT ?type (COUNT(?type) AS ?c) { ?test a ?type . ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> ?test } GROUP BY ?type'
echo
query "Number of tests with no action"  'SELECT (COUNT(?type) AS ?c) { ?test a ?type . ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> ?test FILTER NOT EXISTS { ?test <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action> ?a }  }'
echo
query "Tests with result grouped by type with counts"  'SELECT ?type (COUNT(?type) AS ?c) { ?test a ?type . ?test <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result> ?r } GROUP BY ?type'
