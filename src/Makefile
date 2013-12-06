VERSION := $(subst SBCL ,,$(shell sbcl --version))
CACHE=$(subst VERSION,$(VERSION),$(HOME)/.cache/common-lisp/sbcl-VERSION-macosx-x64$(HOME)/instans/)
QUICKLISP=$(HOME)/quicklisp
CORE=sbcl.core-for-instans

all: clean initialize

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

.SILENT: all clean clean_all compile initialize run

.PHONY: all clean clean_all compile initialize run
