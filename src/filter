#!/usr/bin/env awk -f
/^Rule/ {
    if (queryfile) {
	printf "%s %s returns %s", type, queryfile, success;
	if (error_msg && length(error_msg) > 2) printf ", error_msg = %s\n", error_msg;
	else printf "\n";
	error_msg = "";
    }
}
/\?QUERYFILE = .*/ {
    split($4, n, ">");
    queryfile = n[1];
    sub("file:///Users/enu/Sparql/sparql11-test-suite/syntax-query/", "", queryfile);
#    printf "\nQUERYFILE = %s\n", queryfile;
}
/\?TESTTYPE = #<RDF-IRI / {
    split($4, a, "#");
    split(a[2], t, ">");
    type = t[1];
#    printf "TYPE = %s\n", type;
}
/\?PARSED_OK = #<SPARQL-UNBOUND.*/ {
    success = "UNBOUND";
#    printf "PARSED_OK = UNBOUND\n"
}
/\?PARSED_OK = T/ {
    success = "true";
#    printf "PARSED_OK = true\n"
}
/\?PARSED_OK = NIL/ {
    success = "false";
#    printf "parse_ok = false\n"
}
/\?ERROR_MSG = / {
    i = index($0, "\"");
    error_msg = substr($0, i, length($0) - i + 1);
}
END {
    if (queryfile) {
	printf "%s %s returns %s", type, queryfile, success;
	if (error_msg && length(error_msg) > 2) printf ", error_msg = %s\n", error_msg;
	else printf "\n";
    }
}
