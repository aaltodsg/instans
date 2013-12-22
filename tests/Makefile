SCRIPTS=../scripts
SYNTAX_TESTS=$(SCRIPTS)/run-sparql-syntax-tests.sh
INSTANS=../bin/instans

all: syntax-tests

fnb100:
	$(INSTANS) -b file:///. -d input -r CF-Queries-default.rq --report all -t 5actors100events.ntriples

fnb1000:
	$(INSTANS) -b file:///. -d input -r CF-Queries-default.rq --report all -t 5actors1000events.ntriples

syntax-tests:
	$(SYNTAX_TESTS)

.SILENT: all syntax-tests 

.PHONY: all syntax-tests
