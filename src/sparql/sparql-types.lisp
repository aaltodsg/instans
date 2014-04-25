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
(define-xsd-value-type "dateTime" datetime)

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
(deftype term-or-value () '(or xsd-value rdf-term))
(deftype term-or-value-or-group () '(or term-or-value group))
(deftype term-or-value-or-error () '(or term-or-value sparql-error))
(deftype literal () '(or rdf-literal xsd-value))
(deftype iri-or-literal () '(or literal rdf-iri))
(deftype iri-or-string () '(or rdf-iri xsd-string-value))
(deftype literal-or-string () '(or rdf-literal xsd-string-value))
(deftype ebv () '(or xsd-boolean-value xsd-string-value xsd-number-value rdf-literal))

