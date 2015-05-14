#!/bin/sh
. config
$BUILDAPP --output $EXECUTABLE $SBCLOPTS \
	  --entry instans:main \
	  --load $HOME/quicklisp/setup.lisp \
	  --asdf-path src \
	  --require sb-bsd-sockets \
	  --require sb-posix \
	  --require sb-introspect \
	  --require sb-cltl2 \
	  --require asdf \
	  --eval "(setf asdf:*central-registry* (list \"src/\"))" \
          --load-system instans \
          --eval '(in-package :instans)'


