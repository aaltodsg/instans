;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; TODO:
;;; Should the name in define-xsd-value-type be a string (for xsd:dateTime)?

;;; Type conventions:
;;; - All string literals with no lang tag are always converted to xsd-string-value's. That is, no sparql function should ever expect to get an rdf-literal with
;;;   type = xsd:string or with type = nil and lang = nil
;;; - All number literals are always converted to some xsd-number-value type.

(define-xsd-value-type "boolean" (member t nil))
(define-xsd-value-type "integer" fixnum)
(define-xsd-value-type "decimal" single-float)
(define-xsd-value-type "float" single-float)
(define-xsd-value-type "double" double-float)
(define-xsd-value-type "string" string)
(defun datetime-cons-p (x) (and (consp x) (eq (car x) 'datetime)))
(define-xsd-value-type "dateTime" (satisfies datetime-cons-p))

;; (deftype xsd-number-value () (or xsd-integer-value xsd-decimal-value xsd-float-value xsd-double-value))
;; (deftype xsd-value () (or xsd-boolean-value xsd-integer-value xsd-decimal-value xsd-float-value xsd-double-value xsd-string-value xsd-datetime-value))
;; (deftype term-or-value () (or xsd-value rdf-term))
;; (deftype term-or-value-or-error () (or term-or-value sparql-error))
;; (deftype literal () (or rdf-literal xsd-value))
;; (deftype iri-or-literal () (or literal rdf-iri))
;; (deftype iri-or-string () (or rdf-iri xsd-string-value))
;; (deftype literal-or-string () (or rdf-literal xsd-string-value))
;; (deftype ebv () (or xsd-boolean-value xsd-string-value xsd-number-value rdf-literal))

(deftype xsd-number-value () '(or xsd-integer-value xsd-decimal-value xsd-float-value xsd-double-value))
(deftype xsd-value () '(or xsd-boolean-value xsd-integer-value xsd-decimal-value xsd-float-value xsd-double-value xsd-string-value xsd-datetime-value))
(deftype rdf-anonymous-blank-node () '(satisfies rdf-anonymous-blank-node-p))
(deftype rdf-named-blank-node () '(satisfies rdf-named-blank-node-p))
(deftype rdf-blank-node () '(or rdf-anonymous-blank-node rdf-named-blank-node))
(defun rdf-iri-cons-p (x) (and (consp x) (eq (car x) 'rdf-iri)))
(deftype rdf-iri () '(satisfies rdf-iri-cons-p))
(deftype rdf-literal () '(satisfies rdf-literal-p))
(deftype rdf-term () '(or rdf-iri rdf-literal rdf-blank-node sparql-unbound))
(deftype term-or-value () '(or xsd-value rdf-term))
(deftype term-or-value-or-group () '(or term-or-value group))
(deftype term-or-value-or-error () '(or term-or-value sparql-error))
(deftype literal () '(or rdf-literal xsd-value))
(deftype iri-or-literal () '(or literal rdf-iri))
(deftype iri-or-string () '(or rdf-iri xsd-string-value))
(deftype literal-or-string () '(or rdf-literal xsd-string-value))
(deftype ebv () '(or xsd-boolean-value xsd-string-value xsd-number-value rdf-literal))
(deftype sparql-var () '(satisfies sparql-var-p))
(deftype sparql-unbound () '(satisfies sparql-unbound-p))
(deftype sparql-distinct () '(satisfies sparql-distinct-p))
