(require (quote sb-bsd-sockets))
(require (quote sb-posix))
(require (quote sb-introspect))
(require (quote sb-cltl2))
(require (quote asdf))
(push (probe-file ".") asdf:*central-registry*)
(asdf:compile-system :instans)
(asdf:load-system :instans)
(in-package :instans)