#!/bin/sh
LOGFILE=MKHTML_LOG

msg() {
    if test "${LOGFILE}" != ""; then
	test -f ${LOGFILE} || touch ${LOGFILE}
	echo `date`: $* >> ${LOGFILE}
    fi
#    echo $*
}

if test $# -eq 1 ; then
    SPARQL=$1
    OUTPUT_DIR="."
elif test $# -eq 2; then
    SPARQL=$1
    OUTPUT_DIR=$2
else
    msg "Usage: mk-html sparql [output]"
    exit 1
fi
BN=`basename $SPARQL .rq`
BN=`basename $BN .sparql`
RQ=${OUTPUT_DIR}/$BN.rq
SA=${OUTPUT_DIR}/$BN.sa
BND=${OUTPUT_DIR}/$BN.bnd
DOT=${OUTPUT_DIR}/$BN.dot
OUTPUT=${OUTPUT_DIR}/$BN.html

msg SPARQL=${SPARQL}, SA=${SA}, BND=${BND}, DOT=${DOT}, OUTPUT=${OUTPUT}
{
cat > $OUTPUT <<EOF
<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
<html>
<head>
<meta charset="utf-8"> 
<title>$BN</title>
</head>
<body>
  <table border="1" cellspacing="0" cellpadding="4">
    <tr>
      <td>
      <pre>
EOF
if test `expr $SPARQL : 'http://'` -eq 7 -o `expr $SPARQL : 'https://'` -eq 8 ; then
msg "curl $SPARQL"
curl $SPARQL | sed '/</s//\&lt;/g' | sed '/>/s//\&gt;/g' >> $OUTPUT
elif test `expr $SPARQL : 'file://'` -eq 7  ; then
SPARQL=${SPARQL#file://}
cat $SPARQL | sed '/</s//\&lt;/g' | sed '/>/s//\&gt;/g' >> $OUTPUT
else
cat $SPARQL | sed '/</s//\&lt;/g' | sed '/>/s//\&gt;/g' >> $OUTPUT
fi
cat >> $OUTPUT <<EOF
      </pre>
      </td>
      <td rowspan="2">
      <pre>
EOF
cat $SA >> $OUTPUT
cat >> $OUTPUT <<EOF
      </pre>
      </td>
    </tr>
    <tr>
      <td>
      <pre>
EOF
cat $BND >> $OUTPUT
cat >> $OUTPUT <<EOF
      </pre>
      </td>
    </tr>
  </table>
  <hr/>
EOF
dot -Tsvg -O $DOT
cat $DOT.svg >> $OUTPUT
cat >> $OUTPUT <<EOF
<hr/>
</body>
</html>
EOF
} >> ${LOGFILE} 2>&1
