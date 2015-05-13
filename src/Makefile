VERSION := $(subst SBCL ,,$(shell sbcl --version))
CACHE=$(subst VERSION,$(VERSION),$(HOME)/.cache/common-lisp/sbcl-VERSION-macosx-x64$(HOME)/instans/)
SOURCES=$(wildcard */*.lisp)

all: compile

clean:
	rm -rf $(CACHE) LAST_COMPILED

compile: LAST_COMPILED

LAST_COMPILED: $(SOURCES)
	sbcl --non-interactive --load docompile.lisp
	date > LAST_COMPILED

#.SILENT: all clean compile

.PHONY: all clean compile
