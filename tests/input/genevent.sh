#!/bin/sh
MAX=$1
cat <<EOF
@prefix tl:<http://purl.org/NET/c4dm/timeline.owl#> .
@prefix geo:<http://www.w3.org/2003/01/geo/wgs84_pos#> .
@prefix xsd:<http://www.w3.org/2001/XMLSchema#> .
@prefix : <http://instans.org/> .

EOF
ec=0
ts=0
r=0
sensor=":sensor1"
location="[ geo:lat 60.15922234475837; geo:long 24.87610729902513 ]"
while test $ec -lt $MAX ; do
 ec=$(($ec+1))
 ts=$ec
 r=$(($ec*$ec))
 echo ":ev$ec { $sensor a :Thermometer; :location $location; :timestamp $ts; :reading $r }"
done
