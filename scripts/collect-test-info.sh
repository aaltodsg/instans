#!/bin/sh

cd `dirname $0`/.. > /dev/null
INSTANS=bin/instans

# TMP=collect-tmp$$

#for m in tests/data-r2/algebra/manifest.ttl ; do
#for m in tests/data-sparql11/bindings/manifest.ttl ; do
ETOT=0
ITOT=0
for m in tests/data-r2/*/manifest.ttl tests/data-sparql11/*/manifest.ttl; do
    E=`egrep -v '^[ 	]*#' $m|egrep '(mf:CSVResultFormatTest|mf:NegativeSyntaxTest|mf:NegativeSyntaxTest11|mf:NegativeUpdateSyntaxTest11|mf:PositiveSyntaxTest|mf:PositiveSyntaxTest11|mf:PositiveUpdateSyntaxTest11|mf:ProtocolTest|mf:QueryEvaluationTest|mf:ServiceDescriptionTest|mf:UpdateEvaluationTest)'| wc -l`
    ETOT=$[$ETOT+$E]
    I=`$INSTANS -b $m --prefix-encoding=true --print-prefix-encodings=false -r tests/input/collect-tests.rq --input=$m|wc -l`
    I=$[$I-1]
    ITOT=$[$ITOT+$I]
    if test "$[$I-$E]" -ne 0; then
    	echo Mismatch $m: $[I] != $E
    fi
done 
echo "ETOT=$ETOT, ITOT=$ITOT"
# FIRST=`head -1 $TMP`
# echo $FIRST
# egrep -v "$FIRST" $TMP
# rm -f $TMP
