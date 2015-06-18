;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class rdf-lisp-parser (ll-parser)
  ((stream :accessor rdf-lisp-parser-stream :initarg :stream)
   (instans :accessor rdf-lisp-parser-instans :initarg :instans)))

(define-class rdf-n-statement-lisp-parser (rdf-lisp-parser) ())
(define-class rdf-lisp-block-parser (rdf-lisp-parser) ())

(defun make-n-statements-lisp-parser (instans input-stream &key &allow-other-keys)
  (make-instance 'rdf-n-statements-lisp-parser :instans instans :stream input-stream))

(defun make-lisp-block-parser (instans input-stream &key &allow-other-keys)
  (make-instance 'rdf-lisp-block-parser :instans instans :stream input-stream))


