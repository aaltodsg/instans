#!/bin/sh
ECHO=/bin/echo
${ECHO} 
${ECHO} "===================="
${ECHO} "Running SPARQL tests"
${ECHO} "===================="
${ECHO}
cd `dirname $0`/.. > /dev/null
ROOT=`pwd`
BIN=${ROOT}/bin
INSTANS=$BIN/instans
TESTS=${ROOT}/tests
RULES=${TESTS}/input/sparql-test.rq
cd ${TESTS} > /dev/null
RESULTSDIR="sparql-tests"
TEST_OUTPUT=${RESULTSDIR}/sparql-tests-results.csv
REPORT_HTML=${RESULTSDIR}/index.html
if test ! -d $RESULTSDIR; then
  ${ECHO} "Directory \"$RESULTSDIR\" does not exist; creating it"
  mkdir $RESULTSDIR
  ${ECHO}
fi
SAVEDIR=${RESULTSDIR}/save/`date "+%Y-%m-%dT%H:%M:%S"`
if test -f ${TEST_OUTPUT} ; then
mkdir -p ${SAVEDIR}
cp -f ${TEST_OUTPUT} ${SAVEDIR}/
cp -f ${REPORT_HTML} ${SAVEDIR}/
cp -f ${REPORT_HTML} ${REPORT_HTML}.prev
rm -f [0-9][0-9]*__tmp_out
fi
if test $# -eq 0 ; then
    ${INSTANS} --run-sparql-test-suite
elif test -f $TEST_OUTPUT; then
    ${ECHO} "Using the old results in $TEST_OUTPUT"  
else
    ${ECHO} "No results in $TEST_OUTPUT!"
    exit 1
fi
REPORT_HTML=${RESULTSDIR}/index.html
AWK=${RESULTSDIR}/sparql-test-filter.awk
rm -f $AWK
touch $AWK
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
td { text-align: center }
tr.syntax_failed td.test_type { color: red; }
tr.translate_failed td.test_type{ color: red; }
tr.run_failed td.test_type { color: red; }
tr.negative_syntax_parseok td.test_type { color: orange; }
td.translate_ok_not_runnable{ }
td.run_ok { }
</style>
<script type="text/javascript" src="DataTables/media/js/jquery.js"></script>
<script type="text/javascript" src="DataTables/media/js/jquery.dataTables.js"></script>
<script type="text/javascript" src="DataTables/extras/ColumnFilterWidgets/media/js/ColumnFilterWidgets.js"></script>
<script type="text/javascript">
//<![CDATA[
	\$(document).ready( function () {
				\$('#TestResults').dataTable( { "bPaginate": false, "sDom": 'W<"clear">lfrtip', "oColumnFilterWidgets": { "aiExclude": [ 0, 4 ] } } );
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
cat >> $AWK <<EOF
function output() {
    if (testshort) {
        entry_number++;
        split(testshort, parts, "/");
        long_name = sprintf("%s/%s/%s", parts[1], parts[2], parts[3]);
        is_negative = (index(testtype, "Negative") == 1);
        if (!is_negative) { 
          if (!parseok) { status = 0; test_outcome="syntax_failed"; syntax_failed_count++; }
          else { if (!translateok) { status = 0; test_outcome="translate_failed"; translate_failed_count++; }
                 else { if (!runnable) { status = 1; test_outcome="translate_ok_not_runnable" translate_ok_not_runnable_count++; }
                        else { if (!runok) { status = 0; test_outcome="run_failed";  run_failed_count++; }
                               else { status = 1; test_outcome="run_ok"; run_ok_count++; }}}}}
        else { if (parseok) { status = 0; test_outcome="negative_syntax_parseok"; negative_syntax_parseok_count++; }
               else { status = 1; test_outcome="negative_syntax_no_parse"; negative_syntax_no_parse_count++; } }
	printf "<TR class=\"%s\">", test_outcome;
	printf "<TD class=\"test_entry_number\">%d</TD>", entry_number;
	printf "<TD class=\"test_type\">%s</TD>", testtype;
	printf "<TD class=\"test_collection\">%s</TD>", parts[1];
	printf "<TD class=\"test_set\">%s</TD>", parts[2];
	printf "<TD class=\"test_name\"><a href=\"%s\">%s</a></TD>", testshort, parts[3];
        split(queryfile, parts, "/");
	printf "<TD class=\"test_queryfile\"><a href=\"../%s\" type=\"text/plain\">%s</a></TD>", queryfile, parts[3];
        split(datafile, parts, "/");
	printf "<TD class=\"test_datafile\"><a href=\"../%s\" type=\"text/plain\">%s</a></TD>", datafile, parts[3];
	printf "<TD class=\"test_status\">%s</TD>", (status == 1 ? "Yes" : (status == 0 ? "No" : "Skipped"));
	printf "<TD class=\"test_parsing\">%s</TD>", (parseok ? "OK" : "Error");
	printf "<TD class=\"test_translating\">%s</TD>", (translateok ? "OK" : (!parseok ? "---" : "Error"));
	printf "<TD class=\"test_runnable\">%s</TD>", (runnable ? "Yes" : "No");
	printf "<TD class=\"test_runok\">%s</TD>", (runok ? "OK" : (!runnable ? "---" : "Error"));
	printf "<TD class=\"test_status\">%s</TD>", (testok == 1 ? "Yes" : "No");
	# if (error_msg) printf "<TD class=\"test_error_message\">%s</TD></TR>\n", error_msg;
	# else printf "<TD class=\"error_test_error_message\">&nbsp;</TD></TR>\n";
	# printf "<TR class=\"%s\"><TD class=\"test_entry_number\">%d</TD><TD class=\"test_type\">%s</TD><TD class=\"test_status\">%s</TD><TD class=\"test_collection\">%s</TD><TD class=\"test_set\">%s</TD><TD class=\"test_name\"><a href=\"%s\" type=\"text/plain\">%s</a></TD><TD class=\"test_parsing\">%s</TD><TD class=\"test_translating\">%s</TD>",
        #   test_outcome, entry_number, type, (status == 1 ? "Yes" : (status == 0 ? "No" : "Skipped")), parts[1], parts[2], linkuri, parts[3], (parsed ? "OK" : "Error"), (translated ? "OK" : (!parsed ? "---" : "Error"));
	# if (error_msg && length(error_msg) > 2) printf "<TD class=\"test_error_message\">%s</TD></TR>\n", error_msg;
	# else printf "<TD class=\"error_test_error_message\">&nbsp;</TD></TR>\n";
	# error_msg = "";
        printf "</TR>\n";
    }
}
BEGIN {
  printf "  <table id=\"TestResults\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" class=\"display\" width=\"100%%\">\n";
  printf "  <thead>\n";
  printf "    <tr><th>Test#</th><th>Type</th><th>Collection</th><th>Set</th><th>Name</th><th>Query</th><th>Data</th><th>Passed</th><th>ParseOK</th><th>TranslateOK</th><th>Runnable</th><th>RunOK</th><th>TestOK</th></tr>\n";
  printf "  </thead>\n";
  printf "  <tbody>\n";
}

#/parsed_ok,translated_ok,testtype,queryfile_short,error_msg/ {
/testshort,queryfile,testtype,parseok,translateok,runnable,runok,testok/ {
  next;
}
#queryfile,testshort,testtype,datafile,graphdatafile,resultfile,resultgraphfile,resultgraphlabel
/[^,]*,[^,]*,[^,]*,[^,]*,[^,]*/ {
    split(\$0, fields, ",");
    testshort = fields[1];
    queryfile = fields[2];
    datafile = fields[3];
    testtype = fields[4];
    parseok = (index(fields[5], "true") == 1);
    translateok = (index(fields[6], "true") == 1);
    runnable = (index(fields[7], "true") == 1);
    runok = (index(fields[8], "true") == 1);
    testok = (index(fields[9], "true") == 1);
    if (testok) test_ok_count++;
    test_count++;
    testuri=sprintf("../../tests/%s", queryfile);
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
   PREVTOTAL=" (previously $PREVTOTAL)"
   PREVOK=" (previously $PREVOK)"
fi
cat >> $AWK <<EOF
  printf "<ul>\n";
  printf "<li>Total %d tests$PREVTOTAL</li>\n", test_count;
  printf "<li>%d tests OK$PREVOK</li>\n", test_ok_count;
  printf "</ul>\n";
EOF
cat >> $AWK <<EOF
  printf "</body>\n";
  printf "</html>\n";
}
EOF
rm -f log; touch log
cat $TEST_OUTPUT|sed '/"/s///g'|awk -f $AWK >> $REPORT_HTML
#rm -rf $AWK
tail -7 $REPORT_HTML | sed '/<[^>]*>/s///g'|sed '/^$/d'
${ECHO}
${ECHO} "File \"$REPORT_HTML\" contains the test results in HTML form."
${ECHO} 
${ECHO} "======================"
${ECHO} "Syntax tests completed"
${ECHO} "======================"
${ECHO}
