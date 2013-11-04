;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

;;; Definition forms

(in-package #:instans)

(defmacro define-class (class-name superclasses slots &rest rest)
  `(progn 
     (defclass ,class-name ,superclasses ,slots ,@rest)
     (defun ,(predicate-name (string class-name)) (x) (typep x ',class-name))))

(defmacro define-type (name spec)
  (let ((predicate-arg (gensym "X")))
    `(progn
       (deftype ,name  () ',spec)
       (defmacro ,(predicate-name name) (,predicate-arg) (typep ,predicate-arg ',name)))))

(defmacro define-output-function (function-name &key (stream-init-form '*error-output*))
  (let ((stream-var-name (intern (format nil "*~A-STREAM*" function-name)))
	(indent-var-name (intern (format nil "*~A-INDENT*" function-name)))
	(fmt-var (gensym "FMT"))
	(args-var (gensym "ARGS")))
    `(progn
       (defvar ,stream-var-name)
       (defvar ,indent-var-name)
       (defun ,function-name (,fmt-var &rest ,args-var)
	 (apply #'format ,stream-var-name (concatenate 'string "~%~V@T" ,fmt-var) ,indent-var-name ,args-var))
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf ,stream-var-name ,stream-init-form)
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

;;; Shorthand

(defmacro fmt-intern (fmt &rest args)
  `(intern (format nil ,fmt ,@args)))

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