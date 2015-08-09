;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Type conventions:
;;; - All string literals with no lang tag are always converted to xsd-string-value's. That is, no sparql function should ever expect to get an rdf-literal with
;;;   type = xsd:string or with type = nil and lang = nil
;;; - All number literals are always converted to some xsd-number-value type.

(defvar *rdf-first*)
(defvar *rdf-rest*)
(defvar *rdf-nil*)
(defvar *rdf-type*)
(defvar *xsd-value-type-descriptors*)
(defvar *rdf-lang-string-iri*)
(defvar *sparql-standard-op-library*)
(defvar *sparql-xsd-op-library*)
(defvar *sparql-ops*)
(defvar *instans-op-library*)
(defvar *instans-math-extension-op-library*)
(defvar *instans-datetime-extension-op-library*)

(define-class sparql-error ()
  ((format :accessor sparql-error-format :initarg :format :initform nil)
   (arguments :accessor sparql-error-message-arguments :initarg :arguments :initform nil)))

(define-class hashkeyed () ((hashkey :accessor hashkeyed-hashkey :initform nil)))

(define-class rdf-term (hashkeyed) ())

;;; Should we canonize IRIs?
(define-class rdf-iri (rdf-term)
  ((string :accessor rdf-iri-string :initarg :string)
   (scheme :accessor rdf-iri-scheme :initarg :scheme :initform nil)
   (authority :accessor rdf-iri-authority :initarg :authority :initform nil)
   (path :accessor rdf-iri-path :initarg :path :initform nil)
   (query :accessor rdf-iri-query :initarg :query :initform nil)
   (fragment :accessor rdf-iri-fragment :initarg :fragment :initform nil)
;   (had-dot-segments-p :accessor rdf-iri-had-dot-segments-p :initarg :had-dot-segments-p :initform nil)
))

(define-class rdf-literal (rdf-term)
  ((string :accessor rdf-literal-string :initarg :string :initform nil)
   (type :accessor rdf-literal-type :initarg :type :initform nil)
   (lang :accessor rdf-literal-lang :initarg :lang :initform nil)
   (value :accessor rdf-literal-value :initarg :value)))

(define-class uniquely-named-object ()
  ((name :accessor uniquely-named-object-name :initarg :name)
   (pretty-name :accessor uniquely-named-object-pretty-name :initarg :pretty-name :initform nil)))

(define-class rdf-blank-node (rdf-term uniquely-named-object) ())

(define-class rdf-anonymous-blank-node (rdf-blank-node) ())

(define-class rdf-named-blank-node (rdf-blank-node) ())

(define-class sparql-var (uniquely-named-object hashkeyed) ())

(define-class internal-var (sparql-var) ())

(define-class sparql-unbound (rdf-term) ())

(define-class sparql-distinct () ())

(define-class type-descriptor ()
  ((iri :accessor type-descriptor-iri :initarg :iri)
   (iri-string :accessor type-descriptor-iri-string :initarg :iri-string)
   (lisp-type :accessor type-descriptor-lisp-type :initarg :lisp-type)
   (value-parser :accessor type-descriptor-value-parser :initarg :value-parser)))

(define-class type-descriptors ()
  ((string-map :accessor type-descriptors-string-map :initform (make-hash-table :test #'equal))))

(define-class uniquely-named-object-factory ()
  ((object-type :initarg :object-type)
   (name-counter :initform -1)
   (objects-by-name :initform (make-hash-table))))

(define-class sparql-op ()
  ((name :accessor sparql-op-name :initarg :name)
   (lisp-name :accessor sparql-op-lisp-name :initarg :lisp-name)
   (arguments :accessor sparql-op-arguments :initarg :arguments)
   (containing-library :accessor sparql-op-containing-library :initarg :containing-library)
   (returns :accessor sparql-op-returns :initarg :returns)
   (body :accessor sparql-op-body :initarg :body)
   (hiddenp :accessor sparql-op-hiddenp :initarg :hiddenp :initform nil)))

(define-class sparql-function (sparql-op) ())

;; (define-class sparql-form (sparql-op) ())

(define-class sparql-macro (sparql-op) ())

(define-class sparql-op-library ()
  ((prefix :accessor sparql-op-library-prefix :initarg :prefix)
   (iri-string :accessor sparql-op-library-iri-string :initarg :iri-string)
   (ops :accessor sparql-op-library-ops :initform (make-hash-table :test #'equal))))

(define-class sparql-ops ()
  ((ops :accessor sparql-ops-ops :initform (make-hash-table :test #'equal))
   (libraries :accessor sparql-ops-libraries :initform (make-hash-table :test #'equal))))

