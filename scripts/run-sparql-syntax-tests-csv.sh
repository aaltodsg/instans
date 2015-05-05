#!/bin/sh
ECHO=/bin/echo
${ECHO} 
${ECHO} "===================="
${ECHO} "Running syntax tests"
${ECHO} "===================="
${ECHO}
cd `dirname $0`/.. > /dev/null
ROOT=`pwd`
TESTS=${ROOT}/tests
RULES=${TESTS}/input/syntax-test.rq
cd ${TESTS} > /dev/null
INSTANS=../../instans/bin/instans
RESULTSDIR=syntax-test-results
TEST_OUTPUT=${RESULTSDIR}/results
REPORT_HTML=${RESULTSDIR}/index.html
if test ! -d $RESULTSDIR; then
  ${ECHO} "Directory \"$RESULTSDIR\" does not exist; creating it"
  mkdir $RESULTSDIR
  ${ECHO}
fi
SAVEDIR=${RESULTSDIR}/save/`date "+%Y-%m-%dT%H:%M:%S"`
mkdir -p ${SAVEDIR}
cp -f ${TEST_OUTPUT} ${SAVEDIR}/
cp -f ${REPORT_HTML} ${SAVEDIR}/
cp -f ${REPORT_HTML} ${REPORT_HTML}.prev
rm -f [0-9][0-9]*__tmp_out
if test $# -eq 0 ; then
    ${ECHO} > $TEST_OUTPUT
    TMPOUT1=$$__tmp_out1
    TMPOUT2=$$__tmp_out2
#    TMPOUT=ask.out
#    for i in data-r2/ask/manifest.ttl ; do 
#     ${ECHO} "INSTANS=${BIN}/instans"
#     ${ECHO} "TESTS=${TESTS}"
#     ${ECHO} "RULES=${TESTS}/input/syntax-test.rq"
#     ${ECHO}
    CNT=0
    TC=0
    for i in data-r2/*/manifest.ttl data-sparql11/*/manifest.ttl ; do 
	CNT=$(($CNT+1))
        MANIFEST=`pwd`/$i
	x=${MANIFEST%manifest.ttl}
	printf "%-35s" ${x#$TESTS/}
#         ${ECHO} MANIFEST=${MANIFEST}
# 	${ECHO}
	${INSTANS} -b file://`dirname $MANIFEST`/ -r ${RULES} -t ${MANIFEST} > ${TMPOUT1} 2>&1
	cat ${TMPOUT1} | egrep -v '(^[ \t]*;|^[ \t]*$|^parsed_ok,translated_ok,testtype,queryfile_short,error_msg$)' \
	    | egrep '(^(T|NIL|true|false))' > ${TMPOUT2}
	LC=`wc -l $TMPOUT2|awk '{print $1;}'`
	TC=$(($TC+$LC))
	printf " %3d tests\n" $LC
	cat ${TMPOUT2} >> ${TEST_OUTPUT} 2>&1
#	rm -f ${TMPOUT1} ${TMPOUT2}
    done
    ${ECHO}
    ${ECHO} "$CNT test sets, $TC tests"
    ${ECHO}
    ${ECHO} "File \"$TEST_OUTPUT\" contains the test output."
elif test -f $TEST_OUTPUT; then
    ${ECHO} "Using the old results in $TEST_OUTPUT"  
else
    ${ECHO} "No results in $TEST_OUTPUT!"
    exit 1
fi
REPORT_HTML=${RESULTSDIR}/index.html
cat > $REPORT_HTML <<EOF
<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
<html>
<head>
<meta charset="utf-8"> 
<title>Syntax tests</title>
<style type="text/css" title="currentStyle">
  @import "DataTables/media/css/demo_page.css";
  @import "DataTables/media/css/demo_table.css";
  @import "DataTables/extras/ColumnFilterWidgets/media/css/ColumnFilterWidgets.css";
</style>
<style type="text/css">
tr.test_positive_failed td.test_type { color: red; }
tr.test_positive_failed td.test_error { color: red; }
tr.test_positive_failed td.test_error_message { color: red; }
tr.test_negative_succeeded td.test_type { color: orange; }
tr.test_negative_succeeded td.test_error { color: orange; }
tr.test_negative_succeeded td.test_error_message { color: orange; }
td.test_passed { }
</style>
<script type="text/javascript" src="DataTables/media/js/jquery.js"></script>
<script type="text/javascript" src="DataTables/media/js/jquery.dataTables.js"></script>
<script type="text/javascript" src="DataTables/extras/ColumnFilterWidgets/media/js/ColumnFilterWidgets.js"></script>
<script type="text/javascript">
//<![CDATA[
	\$(document).ready( function () {
				\$('#TestResults').dataTable( { "bPaginate": false, "sDom": 'W<"clear">lfrtip', "oColumnFilterWidgets": { "aiExclude": [ 0, 4, 8 ] } } );
			} );
// \$(document).ready(function() {
//        \$('#TestResults').dataTable();
// } );
//\$(document).ready(function() 
//    { 
//        \$("#TestResults").tablesorter(); 
//    } 
//); 
//]]>
</script>
</head>
<body>
EOF
AWK=$$-syntax-test-filter.awk
touch $AWK
cat >> $AWK <<EOF
function output() {
    if (queryfile) {
        entry_number++;
        split(queryfile, parts, "/");
        long_name = sprintf("%s/%s/%s", parts[1], parts[2], parts[3]);
        is_negative = (index(type, "Negative") == 1);
        parsed= (index(parse_result, "true") == 1);
        translated= (index(translate_result, "true") == 1);
	printf "parsed=%d, translated=%d, is_negative=%d, long_name=%s\n",parsed,translated,is_negative,long_name>>"log"
        if (!is_negative && !parsed) { status = 0; test_outcome="test_positive_failed"; test_positive_failed_count++}
        else if (is_negative && parsed) { status = 0; test_outcome = "test_negative_succeeded"; test_negative_succeeded_count++}
        else { status = 1; test_outcome = "test_ok"; test_ok_count++}
	printf "<TR class=\"%s\">", test_outcome;
	printf "<TD class=\"test_entry_number\">%d</TD>", entry_number;
	printf "<TD class=\"test_type\">%s</TD>", type;
	printf "<TD class=\"test_collection\">%s</TD>", parts[1];
	printf "<TD class=\"test_set\">%s</TD>", parts[2];
	printf "<TD class=\"test_name\"><a href=\"%s\" type=\"text/plain\">%s</a></TD>", linkuri, parts[3];
	printf "<TD class=\"test_status\">%s</TD>", (status == 1 ? "Yes" : (status == 0 ? "No" : "Skipped"));
	printf "<TD class=\"test_parsing\">%s</TD>", (parsed ? "OK" : "Error");
	printf "<TD class=\"test_translating\">%s</TD>", (translated ? "OK" : (!parsed ? "---" : "Error"));
	if (error_msg) printf "<TD class=\"test_error_message\">%s</TD></TR>\n", error_msg;
	else printf "<TD class=\"error_test_error_message\">&nbsp;</TD></TR>\n";
	# printf "<TR class=\"%s\"><TD class=\"test_entry_number\">%d</TD><TD class=\"test_type\">%s</TD><TD class=\"test_status\">%s</TD><TD class=\"test_collection\">%s</TD><TD class=\"test_set\">%s</TD><TD class=\"test_name\"><a href=\"%s\" type=\"text/plain\">%s</a></TD><TD class=\"test_parsing\">%s</TD><TD class=\"test_translating\">%s</TD>",
        #   test_outcome, entry_number, type, (status == 1 ? "Yes" : (status == 0 ? "No" : "Skipped")), parts[1], parts[2], linkuri, parts[3], (parsed ? "OK" : "Error"), (translated ? "OK" : (!parsed ? "---" : "Error"));
	# if (error_msg && length(error_msg) > 2) printf "<TD class=\"test_error_message\">%s</TD></TR>\n", error_msg;
	# else printf "<TD class=\"error_test_error_message\">&nbsp;</TD></TR>\n";
	# error_msg = "";
    }
}
BEGIN {
  printf "  <table id=\"TestResults\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" class=\"display\" width=\"100%%\">\n";
  printf "  <thead>\n";
  printf "    <tr><th>Test#</th><th>Type</th><th>Collection</th><th>Set</th><th>Name</th><th>Passed</th><th>Parsing</th><th>Translating</th><th>Error message</th></tr>\n";
  printf "  </thead>\n";
  printf "  <tbody>\n";
}
/parsed_ok,translated_ok,testtype,queryfile_short,error_msg/ {
  next;
}
/[^,]*,[^,]*,[^,]*,[^,]*,[^,]*/ {
    split(\$0, fields, ",");
    parse_result = fields[1];
    translate_result=fields[2];
    type = fields[3];
    queryfile = fields[4];
    linkuri=sprintf("../../tests/data-%s", queryfile);
    error_msg = fields[5];        
    printf "parse_result=%s\ntranslate_result=%s\ntype=%s\nqueryfile=%s\nlinkuri=%s\nerror_msg=%s\n", parse_result,translate_result,type,queryfile,linkuri,error_msg>>"log"
    output();
}
END {
  output();
  printf "  <tbody>\n";
  printf "  </table>\n";
  printf "<hr/>\n";
EOF
if test -f ${REPORT_HTML}.prev; then
   PREVTOTAL=`cat ${REPORT_HTML}.prev|sed -n '/^.*Total \([0-9]*\) tests.*$/s//\1/p'`
   PREVOK=`cat ${REPORT_HTML}.prev|sed -n '/^[^0-9]*\([0-9]*\) tests OK.*$/s//\1/p'`
   PREVPOSFAIL=`cat ${REPORT_HTML}.prev|sed -n '/^[^0-9]*\([0-9]*\) positive tests failed.*$/s//\1/p'`
   PREVNEGSUCC=`cat ${REPORT_HTML}.prev|sed -n '/^[^0-9]*\([0-9]*\) negative tests succeeded.*$/s//\1/p'`
   # PREVIGNORED=`cat ${REPORT_HTML}.prev|sed -n '/^[^0-9]*\([0-9]*\) malformed tests ignored.*$/s//\1/p'`
cat >> $AWK <<EOF
  printf "<p>Total %d tests (previously $PREVTOTAL)\n", test_ok_count + test_positive_failed_count + test_negative_succeeded_count;
  printf "<ul><li>%d tests OK (previously $PREVOK)</li>\n", test_ok_count;
  printf "<li>%d positive tests failed (previously $PREVPOSFAIL)</li>\n", test_positive_failed_count;
  printf "<li>%d negative tests succeeded (previously $PREVNEGSUCC)</li>\n", test_negative_succeeded_count;
  # printf "<li>%d malformed tests ignored (previously $PREVIGNORED)</li></ul>\n", test_malformed_count;
EOF
else
    cat >> $AWK <<EOF
  printf "<p>Total %d tests\n", test_ok_count + test_positive_failed_count + test_negative_succeeded_count;
  printf "<ul><li>%d tests OK</li>\n", test_ok_count;
  printf "<li>%d positive tests failed</li>\n", test_positive_failed_count;
  printf "<li>%d negative tests succeeded</li>\n", test_negative_succeeded_count;
  # printf "<li>%d malformed tests ignored</li></ul>\n", test_malformed_count;
EOF
fi
cat >> $AWK <<EOF
  printf "</body>\n";
  printf "</html>\n";
}
EOF
rm -f log; touch log
awk -f $AWK < $TEST_OUTPUT >> $REPORT_HTML
rm -rf $AWK
tail -7 $REPORT_HTML | sed '/<[^>]*>/s///g'|sed '/^$/d'
${ECHO}
${ECHO} "File \"$REPORT_HTML\" contains the test results in HTML form."
${ECHO} 
${ECHO} "======================"
${ECHO} "Syntax tests completed"
${ECHO} "======================"
${ECHO}
