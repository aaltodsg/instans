;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class rdf-lisp-parser (ll-parser)
  ((stream :accessor rdf-lisp-parser-stream :initarg :stream)
   (instans :accessor rdf-lisp-parser-instans :initarg :instans)
   (callback :accessor rdf-lisp-parser-callback :initarg :callback)))

(defmethod initialize-instance :after ((this rdf-lisp-parser) &key &allow-other-keys)
  (setf (ll-parser-parse this)
	#'(lambda (parser)
	    (let* ((*parser* parser)
		   (stream (rdf-lisp-parser-stream parser))
		   (form (read stream nil :eof)))
	      (cond ((eq form :eof)
		     (ll-parser-success))
		    (t
		     (apply (rdf-lisp-parser-callback this) (eval form))))))))

(define-class rdf-n-statement-lisp-parser (rdf-lisp-parser) ())
(define-class rdf-lisp-block-parser (rdf-lisp-parser) ())

(defun make-n-statements-lisp-parser (instans input-stream &key triple-callback &allow-other-keys)
  (make-instance 'rdf-n-statements-lisp-parser :instans instans :stream input-stream :callback triple-callback))

(defun make-lisp-block-parser (instans input-stream &key block-callback &allow-other-keys)
  (make-instance 'rdf-n-statements-lisp-parser :instans instans :stream input-stream :callback block-callback))




