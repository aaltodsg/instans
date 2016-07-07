;;; -*- Mode: Common-Lisp -*-

;(defvar SB-IMPL::*DEFAULT-EXTERNAL-FORMAT*)
(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8)
(in-package :asdf)

(ql:quickload "cl-ppcre")
(ql:quickload "cl-unicode")
(ql:quickload "html-entities")
(ql:quickload "drakma")
(ql:quickload "xmls")
(ql:quickload "xml-emitter")
(ql:quickload "yason")
(ql:quickload "uuid")
(ql:quickload "percent-encoding")
(ql:quickload "sb-concurrency")
(ql:quickload "csv-parser")
;(ql:quickload "lisp-unit")
(ql:quickload "cl-json")
;; (load "lisp-unit/lisp-unit")
;; (setq lisp-unit:*print-failures* t)
;; (setq lisp-unit:*print-errors* t)
;; (setq lisp-unit:*print-summary* t)
;; (require 'sb-mailbox)
;; (require 'sb-thread)

(defpackage #:instans
  (:use #:common-lisp) ; #:lisp-unit
  (:export "SPARQL-TESTS" "MAIN"))

(defpackage #:instans-internal ;;; for interning some generated names
  (:use #:common-lisp))

(defsystem "instans"
  :description "A Standing SPARQL Query Evaluator"
  :version "0.4.0.10"
  :author "Esko Nuutila <esko.nuutila@aalto.fi>"
  :licence "The MIT License (MIT). See the file LICENSE in .."
  :depends-on (#:cl-ppcre #:cl-ppcre-unicode)
  :components ((:module "util"
			:components ((:file "version")
				     (:file "misc0")
				     (:file "macros" :depends-on ("misc0"))
				     (:file "misc" :depends-on ("macros"))
				     (:file "trie" :depends-on ("macros"))
				     ;; (:file "binary-tree" :depends-on ("macros"))
				     (:file "avl-tree" :depends-on ("macros"))
				     (:file "csv" :depends-on ("macros"))
				     (:file "char-ops")
				     (:file "table-bindings" :depends-on ("macros"))
				     (:file "mix")
				     (:file "where-am-i")))
	       (:module "sparql" :depends-on ("util")
			:components ((:file "parse-values")
				     (:file "datetime")
				     (:file "sparql-macros")
				     (:file "sparql-classes" :depends-on ("sparql-macros"))
				     (:file "iri" :depends-on ("sparql-classes"))
				     (:file "sparql-accessor-functions" :depends-on ("sparql-classes" "iri"))
				     (:file "sparql-types" :depends-on ("datetime" "parse-values" "sparql-accessor-functions" "iri"))
				     (:file "sparql-helper-functions" :depends-on ("sparql-types" "sparql-macros"))
				     (:file "sparql-query-results" :depends-on ("sparql-types" "sparql-macros"))
				     (:file "sparql-ops" :depends-on ("sparql-types" "sparql-macros" "sparql-helper-functions"))
				     (:file "sparql-instans-extension-ops" :depends-on ("sparql-types" "sparql-macros" "sparql-helper-functions"))))
	       (:module "cl-ll1" :depends-on ("util")
			:components ((:file "debug" :depends-on ())
				     (:file "misc" :depends-on ("debug"))
				     (:file "datastruct" :depends-on ("debug"))
				     (:file "abstract-lexer" :depends-on ("datastruct"))
				     (:file "printing" :depends-on ("datastruct"))
				     (:file "ebnf2ll1" :depends-on ("datastruct"))
				     (:file "generator" :depends-on ("abstract-lexer" "printing" "ebnf2ll1"))
				     (:file "with-ll1-parser" :depends-on ("generator"))))
	       (:module "parser" :depends-on ("util" "cl-ll1" "sparql")
	       		:components ((:file "chbuf")
	       			     (:file "lexer-classes" :depends-on ("chbuf"))
	       			     (:file "lexer" :depends-on ("chbuf" "lexer-classes"))
	       			     (:file "n-statements-parser" :depends-on ("lexer"))
	       			     (:file "trig-parser" :depends-on ("lexer"))
	       			     (:file "sparql-parser" :depends-on ("lexer"))
				     (:file "srx-parser")
				     (:file "parsers" :depends-on ("n-statements-parser" "trig-parser" "sparql-parser" "srx-parser"))))
	       (:module "rete" :depends-on ("util" "sparql" "parser")
			:components  (
				      (:file "rete-classes")
				      (:file "instans-io" :depends-on ("rete-classes"))
				      (:file "parallel-classes" :depends-on ("rete-classes"))
				      (:file "variables-and-bindings" :depends-on ("rete-classes"))
				      (:file "quad-store" :depends-on ("rete-classes"))
				      (:file "token-store" :depends-on ("rete-classes"))
				      (:file "indices" :depends-on ("rete-classes"))
				      (:file "tokens" :depends-on ("rete-classes"))
				      (:file "token-map" :depends-on ("rete-classes"))
				      (:file "rete-access-functions" :depends-on ("rete-classes" "instans-io" "variables-and-bindings" "quad-store" "tokens" "token-store" "token-map" "indices"))
				      (:file "optimize-rete-network" :depends-on ("rete-classes"))
				      (:file "lisp-compile" :depends-on ("rete-classes"))
				      (:file "interpreter" :depends-on ("rete-classes" "parallel-classes"))
				      (:file "triple-pattern-matcher" :depends-on ("rete-classes"))
				      (:file "json-functions" :depends-on ("rete-classes"))
				      (:file "dot" :depends-on ("rete-classes"))))
	       (:module "main" :depends-on ("util" "sparql" "parser" "rete")
	       		:components ((:file "translate-sparql-algebra-to-rete")
	       			     (:file "compile-sparql-file")
				     (:file "main")
				     (:file "test")
;				     (:file "ptest")
				     ))
	       (:module "tests" :depends-on ("main")
			:components ((:file "sparql-test")))))

(progn (setf *print-circle* nil) (setf *print-right-margin* 250))
