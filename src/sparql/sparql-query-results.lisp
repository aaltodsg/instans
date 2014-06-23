;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class sparql-query-results ()
  ((variables :accessor sparql-query-results-variables)
   (links :accessor sparql-query-results-links)
   (results :accessor sparql-query-results-results)
   (tail :accessor sparql-query-results-tail)
   (boolean :accessor sparql-query-boolean-boolean)))

(defgeneric add-sparql-result (query-results variables values)
  (:method ((this sparql-query-results) variables values)
    (cond ((slot-boundp this 'variables)
	   (cond ((not (every #'uniquely-named-object-equal (sparql-query-results-variables this) variables))
		  (error* "Trying to add incompatible results ~S to ~S, variables ~S" (sparql-query-results-variables this) this variables))
		 (t
		  (setf (cdr (sparql-query-results-tail this)) (list values))
		  (setf (sparql-query-results-tail this) (cdr (sparql-query-results-tail this))))))
	  (t
	   (setf (sparql-query-results-variables this) variables)
	   (setf (sparql-query-results-results this) (list values))
	   (setf (sparql-query-results-tail this) (sparql-query-results-results this))))))

