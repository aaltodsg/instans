SCRIPTS=../scripts
SYNTAX_TESTS=$(SCRIPTS)/run-sparql-syntax-tests-csv.sh
FNB_TESTS=$(SCRIPTS)/fnb-tests.sh
ISSUE_TESTS=$(SCRIPTS)/issue-tests.sh
INSTANS=../bin/instans

all: configure syntax-tests fnb-tests issue-tests

configure:
	../configure

fnb-tests:
	$(FNB_TESTS) 100 1000

syntax-tests:
	$(SYNTAX_TESTS)

issue-tests:
	$(ISSUE_TESTS)

.PHONY: all syntax-tests fnb-tests
