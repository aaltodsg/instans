;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Errors

(defvar *sparql-error-op* :inform-and-throw)

(defun sparql-error-on-errors ()
  (setf *sparql-error-op* :error))

(defun sparql-inform-and-throw-on-errors ()
  (setf *sparql-error-op* :inform-and-throw))

(defun sparql-throw-on-errors ()
  (setf *sparql-error-op* :throw))

(defun sparql-silent-on-errors ()
  (setf *sparql-error-op* nil))

(defun signal-sparql-error (fmt &rest args)
  (let ((result (make-instance 'sparql-error :format fmt :arguments args)))
    (case *sparql-error-op*
      (:inform-and-throw (inform "; Warning: ~S~%" (apply #'format nil fmt args))
			 (throw :sparql-error result))
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
  (if (zerop b) (signal-sparql-error "Divide by zero") (/ a b)))

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
				 (t (signal-sparql-error "Literals not compatible: ~S and ~S" a b))))
			  (t
			   (cond ((null lang-b)
				  (signal-sparql-error "Literals not compatible: ~S and ~S" a b))
				 (t
				  (and (string= (second a) (second b)) (string= lang-a lang-b))))))))
		 (t (signal-sparql-error "Literals not compatible: ~S and ~S" a b))))
	  ((null type-b)
	   (signal-sparql-error "Literals not compatible: ~S and ~S" a b))
	  (t
	   (cond ((getf (cddr a) :lang) (error* "Malformed RDF-literal ~S has both type and lang tag" a))
		 ((getf (cddr b) :lang) (error* "Malformed RDF-literal ~S has both type and lang tag" b))
		 (t (and (string= (second a) (second b)) (string= (second type-a) (second type-b)))))))))

(defun create-sparql-call (op-name &rest args)
  (let ((sparql-op (find-sparql-op (string-downcase op-name))))
    (cond ((null sparql-op)
	   nil)
	  ;; ((sparql-form-p sparql-op)
	  ;;  (let ((expanded (apply (sparql-op-lisp-name sparql-op) args)))
	  ;;    expanded))
	  (t
	   (cons sparql-op args)))))

;; (defun create-sparql-aggregate-call (group-var index op-name &rest args)
;;   (declare (ignorable group-var index))
;;   (let ((sparql-op (find-sparql-op (format nil "~(aggregate_~A~)" op-name))))
;;     (cond ((null sparql-op)
;; 	   (error* "Unknown aggregate operation ~A" op-name))
;; 	  (t
;; 	   ;(append (list sparql-op group-var index) args)
;; 	   (cons sparql-op args)))))

(defun sparql-var-lisp-name (var)
  (intern (string (uniquely-named-object-name var)) :instans))

(defun sparql-var-lisp-names (var-list)
  (mapcar #'sparql-var-lisp-name var-list))
