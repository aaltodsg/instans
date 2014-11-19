;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class sparql-test2 ()
  ((type :accessor sparql-test-type :initarg :type)
   (suite :accessor sparql-test-suite :initarg :suite)
   (collection :accessor sparql-test-collection :initarg :collection)
   (name :accessor sparql-test-name :initarg :name)))

(define-class sparql-syntax-test (sparql-test)
  ((queryfile :accessor sparql-test-queryfile :initarg :queryfile)))

(define-class sparql-positive-syntax-test (sparql-syntax-test) ())

(define-class sparql-negative-syntax-test (sparql-syntax-test) ())

(define-class sparql-positive-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-negative-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-positive-update-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-negative-update-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-query-evaluation-test (sparql-positive-syntax-test)
  ((datafile :accessor sparql-test-datafile :initarg :datafile)
   (resultfile :accessor sparql-test-resultfile :initarg :resultfile)))

(define-class sparql-update-evaluation-test (sparql-query-evaluation-test)
  ((resultgraphs :accessor sparql-test-resultgraphs :initarg :resultgraphs)
   (updateresult :accessor sparql-test-updateresult :initarg :updateresult)))

(define-class sparql-csv-result-format-test (sparql-query-evaluation-test) ())

(define-class sparql-protocol-test (sparql-test2) ())

(define-class sparql-service-description-test (sparql-test2) ())


