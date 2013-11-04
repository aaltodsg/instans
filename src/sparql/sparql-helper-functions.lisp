;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Errors

(defvar *sparql-error-op* :throw)

(defun sparql-warn-on-errors ()
  (setf *sparql-error-op* :warn))

(defun sparql-throw-on-errors ()
  (setf *sparql-error-op* :throw))

(defun sparql-silent-on-errors ()
  (setf *sparql-error-op* nil))

(defun sparql-error (fmt &rest args)
  (let ((result (make-instance 'sparql-error :format fmt :arguments args)))
    (case *sparql-error-op*
      (:warn (warn "Warning: ~S" result))
      (:throw (throw :sparql-error result))
      (:error (error* "~S" result)))
    result))

;;; This is a trick. Evaluating the var causes it to be bound to a sparql-unbound object
(defun sparql-var-boundp (x)
  (not (sparql-unbound-p x)))

;;; This is not OK
(defmacro ieee-nan-p (x)
  (declare (ignore x))
  nil)

(defun sparql-check-and-divide (a b)
  (if (zerop b) (sparql-error "Divide by zero") (/ a b)))

(defun rdf-iri= (a b)
  (string= (rdf-iri-string a) (rdf-iri-string b)))

(defun rdf-iri-equal (a b)
  (and (rdf-iri-p a) (rdf-iri-p b)
       (string= (rdf-iri-string a) (rdf-iri-string b))))

(defun rdf-literal= (a b)
  (let ((type-a (getf (cddr a) :type))
	(type-b (getf (cddr b) :type)))
    (cond ((null type-a)
	   (cond ((null type-b)
		  (let ((lang-a (getf (cddr a) :lang))
			(lang-b (getf (cddr b) :lang)))
		    (cond ((null lang-a)
			   (cond ((null lang-b) (string= (second a) (second b)))
				 (t (sparql-error "Literals not compatible: ~S and ~S" a b))))
			  (t
			   (cond ((null lang-b)
				  (sparql-error "Literals not compatible: ~S and ~S" a b))
				 (t
				  (and (string= (second a) (second b)) (string= lang-a lang-b))))))))
		 (t (sparql-error "Literals not compatible: ~S and ~S" a b))))
	  ((null type-b)
	   (sparql-error "Literals not compatible: ~S and ~S" a b))
	  (t
	   (cond ((getf (cddr a) :lang) (error* "Malformed RDF-literal ~S has both type and lang tag" a))
		 ((getf (cddr b) :lang) (error* "Malformed RDF-literal ~S has both type and lang tag" b))
		 (t (and (string= (second a) (second b)) (string= (second type-a) (second type-b)))))))))

(defun create-sparql-call (op-name &rest args)
  (let ((sparql-op (find-sparql-op (string-downcase op-name))))
    (cond ((null sparql-op)
	   nil)
	  (t
	   (cons sparql-op args)))))

(defun sparql-var-lisp-name (var)
  (intern (string (uniquely-named-object-name var))))

(defun sparql-var-lisp-names (var-list)
  (mapcar #'sparql-var-lisp-name var-list))