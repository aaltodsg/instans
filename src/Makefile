VERSION := $(subst SBCL ,,$(shell sbcl --version))
CACHE=$(subst VERSION,$(VERSION),$(HOME)/.cache/common-lisp/sbcl-VERSION-macosx-x64$(HOME)/instans/)
QUICKLISP=$(HOME)/quicklisp
CORE=sbcl.core-for-instans

all: clean initialize download_testsuites

clean:
	rm -rf $(CACHE)

clean_all: clean
	rm -rf $(CORE) $(QUICKLISP)

compile:
	sbcl --load docompile.lisp --quit

initialize:
	sh check-configuration.sh

run: $(CORE)
	sbcl --core $(CORE)

syntax_tests:
	sh run-syntax-tests

download_testsuites:
	(cd ../tests; sh download-testsuites)

.SILENT: all clean clean_all compile initialize run download_testsuites

.PHONY: all clean clean_all compile initialize run download_testsuites
