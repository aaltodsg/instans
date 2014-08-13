#!/bin/sh -v

INSTANS=../../../../bin/instans
PHOUTDIR=phase-output-files
mkdir -p $PHOUTDIR

SOURCE=$1
POSTSTATELESS=$PHOUTDIR/`basename $SOURCE .ttl`-poststateless.trig
POSTSTATEFUL=$PHOUTDIR/`basename $SOURCE .ttl`-poststateful.trig
TRANSLATED=$PHOUTDIR/`basename $SOURCE .ttl`-translated.trig
PROJECTED=$PHOUTDIR/`basename $SOURCE .ttl`-projected.trig
GEOEVENTSTIMEEVENTS=$PHOUTDIR/`basename $SOURCE .ttl`-geoeventstimeevents.trig
EVENTCOUNTS=$PHOUTDIR/`basename $SOURCE .ttl`-eventcounts.trig

$INSTANS --prefix-encoding=true --construct-output=$POSTSTATELESS \
         -r construct-EPA1.rq --rdf-operations=add:execute:flush -g http://instans.org/source --time=- --input-blocks=$SOURCE

$INSTANS --prefix-encoding=true --construct-output=$POSTSTATEFUL \
         -r construct-EPA2.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$POSTSTATELESS

$INSTANS --prefix-encoding=true --construct-output=$TRANSLATED \
         -r construct-EPA3.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$POSTSTATEFUL

$INSTANS --prefix-encoding=true --construct-output=$PROJECTED \
         -r construct-EPA4.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$TRANSLATED

$INSTANS --prefix-encoding=true --construct-output=$GEOEVENTSTIMEEVENTS \
         -r construct-EPA5.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$POSTSTATEFUL

$INSTANS --prefix-encoding=true --construct-output=$EVENTCOUNTS \
         -r construct-EPA6.rq --rdf-operations=add:execute:remove:execute:flush --time=- --input-blocks=$SOURCE


