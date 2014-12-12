#!/bin/sh

cd `dirname $0`/.. > /dev/null
./configure > /dev/null
INSTANS=bin/instans
#RULES=tests/input/collect-tests3.rq
RULES=tests/input/collect-tests.rq

if test $# -eq 0; then
    MANIFESTS=`ls tests/data-r2/*/manifest.ttl tests/data-sparql11/*/manifest.ttl`
else
    MANIFESTS=$*
fi

TMP=collect-tmp$$

#for m in tests/data-sparql11/bindings/manifest.ttl ; do
ETOT=0
ITOT=0
for m in $MANIFESTS; do
#for m in tests/data-sparql11/add/manifest.ttl ;
#for m in tests/data-r2/*/manifest.ttl tests/data-sparql11/*/manifest.ttl; do
#    E=`egrep -v '^[ 	]*#' $m|egrep '(mf:CSVResultFormatTest|mf:NegativeSyntaxTest|mf:NegativeSyntaxTest11|mf:NegativeUpdateSyntaxTest11|mf:PositiveSyntaxTest|mf:PositiveSyntaxTest11|mf:PositiveUpdateSyntaxTest11|mf:ProtocolTest|mf:QueryEvaluationTest|mf:ServiceDescriptionTest|mf:UpdateEvaluationTest)'| wc -l`
#    ETOT=$[$ETOT+$E]
#    echo $INSTANS -b $m --prefix-encoding=true --print-prefix-encodings=false -r tests/input/collect-tests.rq --input=$m
    $INSTANS -b $m --prefix-encoding=true --print-prefix-encodings=false -r $RULES --input-blocks=$m
    # I=`$INSTANS -b $m --prefix-encoding=true --print-prefix-encodings=false -r tests/input/collect-tests.rq --input=$m|wc -l`
    # I=$[$I-1]
    # ITOT=$[$ITOT+$I]
    # if test "$[$I-$E]" -ne 0; then
    # 	echo Mismatch $m: $[I] != $E
    # fi
done > $TMP
#done
#exit 
# if test $ETOT -eq $ITOT; then
#    echo "ETOT=ITOT=$ITOT"
# else
#    echo "Mismatch: ETOT=$ETOT != ITOT=$ITOT"
# fi
FIRST=`head -1 $TMP`
echo $FIRST
egrep -v "$FIRST" $TMP
rm -f $TMP
