#!/bin/sh

INSTANS=../../../../bin/instans
PHOUTDIR=phase-output-files
mkdir -p $PHOUTDIR

INPUT1=$1
INPUT2=$PHOUTDIR/`basename $INPUT1 .ttl`-output1.trig
INPUT3=$PHOUTDIR/`basename $INPUT1 .ttl`-output2.trig
INPUT4=$PHOUTDIR/`basename $INPUT1 .ttl`-output3.trig
INPUT5=$PHOUTDIR/`basename $INPUT1 .ttl`-output4.trig
INPUT6=$PHOUTDIR/`basename $INPUT1 .ttl`-output5.trig

echo $INSTANS --prefix-encoding=true --construct-output=$INPUT2 -r construct-EPA1.rq --rdf-operations=add:execute:flush -g http://instans.org/source --time=- --input-single=$INPUT1
$INSTANS --prefix-encoding=true --construct-output=$INPUT2 -r construct-EPA1.rq --rdf-operations=add:execute:flush -g http://instans.org/source --time=- --input-single=$INPUT1

echo $INSTANS --prefix-encoding=true --construct-output=$INPUT3 -r construct-EPA2.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$INPUT2
$INSTANS --prefix-encoding=true --construct-output=$INPUT3 -r construct-EPA2.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$INPUT2

echo $INSTANS --prefix-encoding=true --construct-output=$INPUT4 -r construct-EPA3.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$INPUT3
$INSTANS --prefix-encoding=true --construct-output=$INPUT4 -r construct-EPA3.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$INPUT3

echo $INSTANS --prefix-encoding=true --construct-output=$INPUT5 -r construct-EPA4.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$INPUT4
$INSTANS --prefix-encoding=true --construct-output=$INPUT5 -r construct-EPA4.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$INPUT4

echo $INSTANS --prefix-encoding=true --construct-output=$INPUT6 -r construct-EPA5.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$INPUT3
$INSTANS --prefix-encoding=true --construct-output=$INPUT6 -r construct-EPA5.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$INPUT3

