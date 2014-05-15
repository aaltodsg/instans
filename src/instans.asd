;;; -*- Mode: Common-Lisp -*-

;(defvar SB-IMPL::*DEFAULT-EXTERNAL-FORMAT*)
(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8)
(in-package :asdf)

(ql:quickload "cl-ppcre")
(ql:quickload "cl-unicode")
(ql:quickload "drakma")
(ql:quickload "xmls")
(ql:quickload "yason")
;(ql:quickload "lisp-unit")
;(ql:quickload "cl-json")
;; (load "lisp-unit/lisp-unit")
;; (setq lisp-unit:*print-failures* t)
;; (setq lisp-unit:*print-errors* t)
;; (setq lisp-unit:*print-summary* t)

(defpackage #:instans
  (:use #:common-lisp) ; #:lisp-unit
  (:export "SPARQL-TESTS" "MAIN"))

(defpackage #:instans-internal ;;; for interning some generated names
  (:use #:common-lisp))

(defsystem "instans"
  :description "A Standing SPARQL Query Evaluator"
  :version "0.0.2.4"
  :author "Esko Nuutila <esko.nuutila@aalto.fi>"
  :licence "The MIT License (MIT). See the file LICENSE in .."
  :depends-on (#:cl-ppcre #:cl-ppcre-unicode)
  :components ((:module "util"
			:components ((:file "version")
				     (:file "macros")
				     (:file "misc" :depends-on ("macros"))
				     (:file "mix")
				     (:file "where-am-i")))
	       (:module "sparql" :depends-on ("util")
			:components ((:file "parse-values")
				     (:file "datetime")
				     (:file "sparql-macros")
				     (:file "sparql-classes")
				     (:file "sparql-types" :depends-on ("sparql-macros" "datetime" "parse-values" "sparql-classes"))
				     (:file "iri" :depends-on ("sparql-types" "sparql-macros"))
				     (:file "sparql-helper-functions" :depends-on ("sparql-types" "sparql-macros"))
				     (:file "sparql-ops" :depends-on ("sparql-types" "sparql-macros" "sparql-helper-functions"))
				     (:file "sparql-instans-extension-ops" :depends-on ("sparql-types" "sparql-macros" "sparql-helper-functions"))
				     (:file "srx-reader" :depends-on ("sparql-types"))))
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
				     (:file "char-ops")
	       			     (:file "prefixed-names")
	       			     (:file "lexer" :depends-on ("chbuf" "prefixed-names"))
	       			     (:file "turtle-parser" :depends-on ("lexer"))
	       			     (:file "sparql-parser" :depends-on ("lexer"))))
	       (:module "rete" :depends-on ("util" "sparql" "parser")
			:components  (
				      (:file "rete-classes")
				      (:file "variables-and-bindings" :depends-on ("rete-classes"))
				      (:file "quad-store" :depends-on ("rete-classes"))
				      (:file "store" :depends-on ("rete-classes"))
				      (:file "indices" :depends-on ("rete-classes"))
				      (:file "tokens" :depends-on ("rete-classes"))
				      (:file "rete-access-functions" :depends-on ("rete-classes" "variables-and-bindings" "quad-store" "tokens" "store" "indices"))
				      (:file "lisp-compile" :depends-on ("rete-classes"))
				      (:file "interpreter" :depends-on ("rete-classes"))
				      (:file "triple-pattern-matcher" :depends-on ("rete-classes"))
				      (:file "json-functions" :depends-on ("rete-classes"))
				      (:file "dot" :depends-on ("rete-classes"))))
	       (:module "main" :depends-on ("util" "sparql" "parser" "rete")
	       		:components ((:file "translate-sparql-algebra-to-rete")
	       			     (:file "compile-sparql-file")
				     (:file "main")
				     ))))

(progn (setf *print-circle* nil) (setf *print-right-margin* 250))

