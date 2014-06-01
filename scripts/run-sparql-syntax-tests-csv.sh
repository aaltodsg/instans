#!/bin/sh
CNT=0
cd `dirname $0`/.. > /dev/null
ROOT=`pwd`
BIN=${ROOT}/bin
TESTS=${ROOT}/tests
cd ${TESTS} > /dev/null
RESULTSDIR=syntax-test-results
TEST_OUTPUT=${RESULTSDIR}/results
REPORT_HTML=${RESULTSDIR}/index.html
if test ! -d $RESULTSDIR; then
  echo "Directory \"$RESULTSDIR\" does not exist; creating it"
  mkdir $RESULTSDIR
  echo
fi
SAVEDIR=${RESULTSDIR}/save/`date "+%Y-%m-%dT%H:%M:%S"`
mkdir -p ${SAVEDIR}
cp -f ${TEST_OUTPUT} ${SAVEDIR}/
cp -f ${REPORT_HTML} ${SAVEDIR}/
cp -f ${REPORT_HTML} ${REPORT_HTML}.prev
if test $# -eq 0 ; then
    pwd
    echo > $TEST_OUTPUT
    /bin/echo -n "Running tests ... "
    TMPOUT=$$__tmp_out
    for i in data-r2/*/manifest.ttl data-sparql11/*/manifest.ttl ; do 
        MANIFEST=`pwd`/$i
	echo ${BIN}/instans -b "file://`dirname $MANIFEST`/" -r ${TESTS}/input/syntax-test.rq -t $MANIFEST
	${BIN}/instans -b "file://`dirname $MANIFEST`/"  -r ${TESTS}/input/syntax-test.rq -t $MANIFEST > ${TMPOUT} 2>&1
	cat ${TMPOUT} | egrep -v '(^[ \t]*;|^[ \t]*$|^queryfile,testtype,parsed_ok,translated_ok,error_msg,status$)' >> ${TEST_OUTPUT} 2>&1
	rm ${TMPOUT}
    done
    echo "File \"$TEST_OUTPUT\" contains the test output."
    # pwd
    # /bin/echo -n "Running tests ... "
    # ${BIN}/sbcl-instans --noinform --eval '(run-all-syntax-tests)' --quit > $TEST_OUTPUT 2>&1
    # echo "File \"$TEST_OUTPUT\" contains the test output."
elif test -f $TEST_OUTPUT; then
    echo "Using the old results in $TEST_OUTPUT"  
else
    echo "No results in $TEST_OUTPUT!"
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
#rm -rf $AWK
echo
echo "File \"$REPORT_HTML\" contains the test results in HTML form."
