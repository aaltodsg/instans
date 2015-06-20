;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class rdf-lisp-parser (ll-parser)
  ((stream :accessor rdf-lisp-parser-stream :initarg :stream)
   (instans :accessor rdf-lisp-parser-instans :initarg :instans)
   (callback :accessor rdf-lisp-parser-callback :initarg :callback)))

(defmethod parse ((this rdf-lisp-parser))
  (catch 'parsed
    (let* ((*parser* this)
	   (stream (rdf-lisp-parser-stream this))
	   (form (read stream nil :eof)))
      (cond ((eq form :eof)
	     (ll-parser-success))
	    (t
	     (funcall (rdf-lisp-parser-callback this) (eval form))
	     this)))))

(define-class rdf-n-statements-lisp-parser (rdf-lisp-parser) ())
(define-class rdf-lisp-block-parser (rdf-lisp-parser) ())

(defun make-n-statements-lisp-parser (instans input-stream &key triple-callback &allow-other-keys)
  (make-instance 'rdf-n-statements-lisp-parser :instans instans :stream input-stream :callback triple-callback))

(defun make-lisp-block-parser (instans input-stream &key block-callback &allow-other-keys)
  (make-instance 'rdf-n-statements-lisp-parser :instans instans :stream input-stream :callback block-callback))
