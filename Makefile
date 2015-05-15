DYNAMIC_SPACE_SIZE=4096
VERSION := $(subst SBCL ,,$(shell sbcl --version))
CACHE=$(subst VERSION,$(VERSION),$(HOME)/.cache/common-lisp/sbcl-VERSION-macosx-x64$(HOME)/instans/)
SOURCES=$(wildcard src/*/*.lisp)
EXECUTABLE=bin/instans.bin
BUILDAPP=buildapp/buildapp

all: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES) Makefile
	(cd src; sbcl --non-interactive --load docompile.lisp)
	$(BUILDAPP) --output $(EXECUTABLE) --dynamic-space-size $(DYNAMIC_SPACE_SIZE)  \
		    --entry instans:main \
		    --load $(HOME)/quicklisp/setup.lisp \
	            --asdf-path src \
	            --require sb-bsd-sockets \
	            --require sb-posix \
	            --require sb-introspect \
	            --require sb-cltl2 \
	            --require asdf \
	            --eval "(setf asdf:*central-registry* (list \"src/\"))" \
                    --load-system instans \
                    --eval '(in-package :instans)'

clean:
	rm -rf $(CACHE) $(EXECUTABLE)

.PHONY: all clean

#.SILENT: all clean
