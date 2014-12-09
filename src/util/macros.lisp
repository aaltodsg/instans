;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

;;; Shorthand

;;; Definition forms

(in-package #:instans)

(defmacro define-class (class-name superclasses slots &rest rest)
  `(progn 
     (defclass ,class-name ,superclasses ,slots ,@rest)
     (defun ,(predicate-name (string class-name)) (x) (typep x ',class-name))))

(defmacro define-output-function (function-name)
  (let ((indent-var-name (intern (format nil "*~A-INDENT*" function-name) :instans))
	(fmt-var (gensym "FMT"))
	(args-var (gensym "ARGS")))
    `(progn
       (defvar ,indent-var-name)
       (defun ,function-name (,fmt-var &rest ,args-var)
	 (apply #'format *error-output* (concatenate 'string "~%~V@T" ,fmt-var) ,indent-var-name ,args-var))
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf ,indent-var-name 0)))))

;;; Access forms

(defmacro push-to-end (x access)
  `(setf ,access (append ,access (list ,x))))

(defmacro push-to-end-new (x access &rest args)
  (let ((x-var (gensym "X")))
    `(let ((,x-var ,x))
       (unless (member ,x-var ,access ,@args)
	 (push-to-end ,x-var ,access)))))

(defmacro filter (predicate list)
  `(remove-if-not ,predicate ,list))

(defmacro filter-not (predicate list)
  `(remove-if ,predicate ,list))

(defmacro filter-non-null (list)
  `(remove-if #'null ,list))

(defmacro get-or-else-update (access update)
  (let ((update-var (gensym "UPDATE")))
    `(or ,access (let ((,update-var ,update))
		   (setf ,access ,update-var)
		   ,update-var))))

;;;

(defmacro informing (fmt &body body)
  (let ((v (gensym)))
    `(let ((,v (progn ,@body)))
       (inform ,fmt ,v)
       ,v)))

;;; Assertions

(defmacro assert* (test fmt &rest args)
  `(unless ,test
     (warn ,fmt ,@args)
     (error* ,fmt ,@args)))

(defvar *checkit* t)
(setf *checkit* t)

(defmacro checkit (test fmt &rest args)
  (cond ((null *checkit*) nil)
	(t
	 `(assert* ,test ,fmt ,@args))))

(defmacro when-checkit (form)
  (if *checkit* form nil))

;;; Shorthand for debugging
(defmacro m (x) `(macroexpand-1 ',x))
(defmacro mm (x) `(macroexpand-1 (macroexpand-1 ',x)))

(defmacro tail-insert (new-item container head-access tail-access)
  (let ((container-var (gensym "CONTAINER")))
    `(let ((,container-var ,container))
       (cond ((null (,head-access ,container-var))
	      (setf (,head-access ,container-var) (list ,new-item))
	      (setf (,tail-access ,container-var) (,head-access ,container-var)))
	     (t
	      (setf (cdr (,tail-access ,container-var)) (list ,new-item))
	      (setf (,tail-access ,container-var) (cdr (,tail-access ,container-var))))))))

  
