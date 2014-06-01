#!/bin/sh
FUSEKILS=`ps alx |grep fuseki-server|grep java`
if test $? -eq 0; then
    FUSEKIPID=`echo $FUSEKILS | awk '{print $2;}'`
#    echo "Killing fuseki-server $FUSEKIPID"
    kill $FUSEKIPID
fi
#echo "Starting new fuseki-server"
fuseki-server --update --mem /ds &> /dev/null &
sleep 2
s-put http://localhost:3030/ds/data default $1
TMP=$$_tmp
s-query --service http://localhost:3030/ds/query --output=csv 'SELECT DISTINCT ?p {?s ?p ?o}' > $TMP
head -1 $TMP
sed -n '2,$p' $TMP|sort
rm -f $TMP



