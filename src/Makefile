VERSION := $(subst SBCL ,,$(shell sbcl --version))
CACHE=$(subst VERSION,$(VERSION),$(HOME)/.cache/common-lisp/sbcl-VERSION-macosx-x64$(HOME)/instans/)
QUICKLISP=$(HOME)/quicklisp
CORE=sbcl.core-for-instans

all: clean compile

clean:
	rm -rf $(CACHE)

compile:
	sbcl --load docompile.lisp --quit

.SILENT: all clean compile

.PHONY: all clean compile

