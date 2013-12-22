#!/bin/sh
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
if test -f $REPORT_HTML ; then
    cp -f $REPORT_HTML ${REPORT_HTML}.prev
fi
if test $# -eq 0 ; then
    pwd
    echo > $TEST_OUTPUT
    /bin/echo -n "Running tests ... "
    for i in data-r2/*/manifest.ttl data-sparql11/*/manifest.ttl ; do 
        MANIFEST=`pwd`/$i
	echo ${BIN}/instans -b "file://`dirname $MANIFEST`/" --report all -r ${TESTS}/input/syntax-test.rq -t $MANIFEST
	${BIN}/instans -b "file://`dirname $MANIFEST`/" --report all -r ${TESTS}/input/syntax-test.rq -t $MANIFEST >> ${TEST_OUTPUT} 2>&1
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
				\$('#TestResults').dataTable( { "bPaginate": false, "sDom": 'W<"clear">lfrtip', "oColumnFilterWidgets": { "aiExclude": [ 0, 5, 7 ] } } );
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
INVALID_TESTS=invalid-sparql-tests
LOG=log
touch $LOG
if test -f $INVALID_TESTS; then
cat >> $AWK <<EOF
function is_invalid_test(name) {
  printf "is_invalid_test(%s): grep --silent %s $INVALID_TESTS", name, name >> $LOG
  return (system(sprintf("grep --silent %s $INVALID_TESTS", name)) == 0);
}
EOF
else
cat >> $AWK <<EOF
function is_invalid_test(name) {
  return 0;
}
EOF
fi
cat >> $AWK <<EOF
function output() {
    if (queryfile) {
        entry_number++;
        split(queryfile, parts, "/");
        long_name = sprintf("%s/%s/%s", parts[1], parts[2], parts[3]);
        is_negative = (index(type, "Negative") == 1) || is_invalid_test(long_name);
        if (!is_negative && has_error) { passed = 0; test_outcome="test_positive_failed"; test_positive_failed_count++}
        else if (is_negative && !has_error) { passed = 0; test_outcome = "test_negative_succeeded"; test_negative_succeeded_count++}
        else { passed = 1; test_outcome = "test_ok"; test_ok_count++}
	printf "<TR class=\"%s\"><TD class=\"test_entry_number\">%d</TD><TD class=\"test_passed\">%s</TD><TD class=\"test_collection\">%s</TD><TD class=\"test_set\">%s</TD><TD class=\"test_type\">%s</TD><TD class=\"test_name\"><a href=\"%s\" type=\"text/plain\">%s</a></TD><TD class=\"test_error\">%s</TD>",
          test_outcome, entry_number, (passed ? "Yes" : "No"), parts[1], parts[2], type, linkuri, parts[3], (has_error ? "Error" : "OK");
	if (error_msg && length(error_msg) > 2) printf "<TD class=\"test_error_message\">%s</TD></TR>\n", error_msg;
	else printf "<TD class=\"error_test_error_message\">&nbsp;</TD></TR>\n";
	error_msg = "";
    }
}
BEGIN {
  printf "  <table id=\"TestResults\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" class=\"display\" width=\"100%%\">\n";
  printf "  <thead>\n";
  printf "    <tr><th>Test#</th><th>Passed</th><th>Collection</th><th>Set</th><th>Type</th><th>Name</th><th>OK/Error</th><th>Error message</th></tr>\n";
  printf "  </thead>\n";
  printf "  <tbody>\n";
}
/^Rule/ {
  output();
}
/\?QUERYFILE = .*/ {
    split(\$4, n, ">");
    queryfile = n[1];
    pos = index(queryfile, "/tests/data-");
    if (pos)
       queryfile = substr(queryfile, pos + 7, length(queryfile) - pos - 6);
    sub("//", "/", queryfile);
    linkuri=sprintf("../../tests/%s", queryfile);
}
/\?TESTTYPE = #<RDF-IRI / {
    split(\$4, a, "#");
    split(a[2], t, ">");
    type = t[1];
}
/\?PARSED_OK = #<SPARQL-UNBOUND.*/ {
    has_error = 1;
    error_msg = "?PARSED_OK == UNBOUND!";
}
/\?PARSED_OK = T/ {
    has_error = 0;
}
/\?PARSED_OK = NIL/ {
    has_error = 1;
}
/\?ERROR_MSG = / {
    i = index(\$0, "\"");
    error_msg = substr(\$0, i, length(\$0) - i + 1);
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
cat >> $AWK <<EOF
  printf "<p>Total %d tests (previously $PREVTOTAL)\n", test_ok_count + test_positive_failed_count + test_negative_succeeded_count;
  printf "<ul><li>%d tests OK (previously $PREVOK)</li>\n", test_ok_count;
  printf "<li>%d positive tests failed (previously $PREVPOSFAIL)</li>\n", test_positive_failed_count;
  printf "<li>%d negative tests succeeded (previously $PREVNEGSUCC)</li></ul>\n", test_negative_succeeded_count;
EOF
else
    cat >> $AWK <<EOF
  printf "<p>Total %d tests\n", test_ok_count + test_positive_failed_count + test_negative_succeeded_count;
  printf "<ul><li>%d tests OK</li>\n", test_ok_count;
  printf "<li>%d positive tests failed</li>\n", test_positive_failed_count;
  printf "<li>%d negative tests succeeded</li></ul>\n", test_negative_succeeded_count;
EOF
fi
cat >> $AWK <<EOF
  printf "</body>\n";
  printf "</html>\n";
}
EOF
awk -f $AWK < $TEST_OUTPUT >> $REPORT_HTML
rm -rf $AWK
echo
echo "File \"$REPORT_HTML\" contains the test results in HTML form."
