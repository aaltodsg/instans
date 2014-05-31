SCRIPTS=../scripts
SYNTAX_TESTS=$(SCRIPTS)/run-sparql-syntax-tests-csv.sh
FNB-TEST=$(SCRIPTS)/fnb-test.sh
ISSUE_TESTS=$(SCRIPTS)/issue-tests.sh
INSTANS=../bin/instans

all: syntax-tests fnb-tests issue-tests

fnb-tests: fnb100 fnb1000

fnb100:
	$(FNB-TEST) 100

fnb1000:
	$(FNB-TEST) 1000

syntax-tests:
	$(SYNTAX_TESTS)

issue-tests:
	$(ISSUE_TESTS)

.PHONY: all syntax-tests fnb-tests fnb100 fnb1000
