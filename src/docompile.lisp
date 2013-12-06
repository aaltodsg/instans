(require 'asdf)
(push (probe-file ".") asdf:*central-registry*)
(asdf:compile-system "instans")
