VERSION := $(subst SBCL ,,$(shell sbcl --version))
CACHE=$(subst VERSION,$(VERSION),$(HOME)/.cache/common-lisp/sbcl-VERSION-macosx-x64$(HOME)/instans/)

all: clean compile

clean:
	rm -rf $(CACHE)

compile:
	sbcl --load docompile.lisp --quit

run-tests:
	./run-tests

.SILENT: run-tests

.PHONY: all clean compile run-tests
