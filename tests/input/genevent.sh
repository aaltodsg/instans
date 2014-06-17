#!/bin/sh
if test $# -eq 0; then
    UNLIMITED="y"
    MAX=0
    SENSORID=1
elif test $# -eq 1; then
    MAX=$1
    SENSORID=1
else
    MAX=$1
    SENSORID=$2
fi
cat <<EOF
@prefix tl:<http://purl.org/NET/c4dm/timeline.owl#> .
@prefix geo:<http://www.w3.org/2003/01/geo/wgs84_pos#> .
@prefix xsd:<http://www.w3.org/2001/XMLSchema#> .
@prefix : <http://instans.org/> .

EOF
ec=0
ts=0
r=0
sensor=":sensor$SENSORID"
location="[ geo:lat 60.15922234475837; geo:long 24.87610729902513 ]"
while test "$UNLIMITED" = "y" -o $ec -lt $MAX ; do
 ec=$(($ec+1))
 ts=$ec
 r=$(($ec*$ec))
 echo ":ev$ec { $sensor a :Thermometer; :location $location; :timestamp $ts; :reading $r }"
done
