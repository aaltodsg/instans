#!/bin/sh

INSTANS=../../../../bin/instans
PHOUTDIR=`pwd`/phase-output-files
mkdir -p $PHOUTDIR

SOURCE=`(cd $(dirname $1); pwd)`/`basename $1`
BASE=`basename $SOURCE`
BASE=${BASE%.*}
POSTSTATELESS=$PHOUTDIR/${BASE}-poststateless.trig
POSTSTATEFUL=$PHOUTDIR/${BASE}-poststateful.trig
TRANSLATED=$PHOUTDIR/${BASE}-translated.trig
PROJECTED=$PHOUTDIR/${BASE}-projected.trig
GEOEVENTSTIMEEVENTS=$PHOUTDIR/${BASE}-geoeventstimeevents.trig
EVENTCOUNTS=$PHOUTDIR/${BASE}-eventcounts.trig

TIME=""

TMP=$$tmp
cat <<EOF >$TMP
$INSTANS --prefix-encoding=true --construct-output=$POSTSTATELESS -r construct-EPA1.rq --rdf-operations=add:execute:flush -g http://instans.org/source ${TIME} --input-blocks=$SOURCE

$INSTANS --prefix-encoding=true --construct-output=$POSTSTATEFUL -r `pwd`/construct-EPA2.rq --rdf-operations=add:execute:remove:execute:flush ${TIME} --input-blocks=$POSTSTATELESS

$INSTANS --prefix-encoding=true --construct-output=$TRANSLATED -r `pwd`/construct-EPA3.rq --rdf-operations=add:execute:remove:execute:flush ${TIME} --input-blocks=$POSTSTATEFUL

$INSTANS --prefix-encoding=true --construct-output=$PROJECTED -r `pwd`/construct-EPA4.rq --rdf-operations=add:execute:remove:execute:flush ${TIME} --input-blocks=$TRANSLATED

$INSTANS --prefix-encoding=true --construct-output=$GEOEVENTSTIMEEVENTS -r `pwd`/construct-EPA5.rq --rdf-operations=add:execute:remove:execute:flush ${TIME} --input-blocks=$POSTSTATEFUL

$INSTANS --prefix-encoding=true --construct-output=$EVENTCOUNTS -r `pwd`/construct-EPA6.rq --rdf-operations=add:execute-snapshot:remove:execute:flush -g http://instans.org/source ${TIME} --input-blocks=$SOURCE
EOF

cat $TMP
bash $TMP
rm $TMP
