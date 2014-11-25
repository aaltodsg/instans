;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class sparql-test2 ()
  ((type :accessor sparql-test-type :initarg :type)
   (suite :accessor sparql-test-suite :initarg :suite)
   (collection :accessor sparql-test-collection :initarg :collection)
   (name :accessor sparql-test-name :initarg :name)
   (directory :accessor sparql-test-directory)
   (instans :accessor sparql-test-instans)
   (not-implemented-p :accessor sparql-test-not-implemented-p :initarg :name)
   (output-directory :accessor sparql-test-output-directory)
   (base :accessor sparql-test-base)
   (status :accessor sparql-test-status :initform :not-completed)))
					; One of :not-completed, :succeeded, :failed

(defgeneric sparql-test-failed-p (sparql-test2))

(defmethod sparql-test-failed-p ((this sparql-test2))
  (sparql-test-not-implemented-p this))

(defgeneric expand-sparql-test-filename (sparql-test2 filename)
  (:method ((this sparql-test2) filename)
    (truename (format nil "~A/~A" (sparql-test-directory this) filename))))

(defmethod initialize-instance :after ((this sparql-test2) &key &allow-other-keys)
  (setf (sparql-test-directory this)
	(truename (format nil "~Atests/~A/~A/" (find-instans-root-directory) (sparql-test-suite this) (sparql-test-collection this))))
  (setf (sparql-test-base this) (parse-iri (format nil "file://~A" (sparql-test-directory this))))
  (setf (sparql-test-instans this) (create-instans (sparql-test-base this)))
  (setf (sparql-test-output-directory this) (expand-sparql-test-filename this (sparql-test-output-directory this)))
  (ensure-directories-exist (sparql-test-output-directory this)))

(define-class sparql-syntax-test (sparql-test2)
  ((queryfile :accessor sparql-test-queryfile :initarg :queryfile)
   (parsing-succeeded-p :accessor sparql-test-parsing-succeeded-p)
   (translation-succeeded-p :accessor sparql-test-translation-succeeded-p)
   (initialization-succeeded-p :accessor sparql-test-initialization-succeeded-p)
   (rule-type :accessor sparql-test-rule-type)))

(defmethod initialize-instance :after ((this sparql-syntax-test) &key queryfile &allow-other-keys)
  (setf (sparql-test-queryfile this) (expand-sparql-test-filename this queryfile)))

(define-class sparql-positive-syntax-test (sparql-syntax-test) ())

(defmethod sparql-test-failed-p ((this sparql-positive-syntax-test))
  (or (call-next-method)
      (not (and (sparql-test-parsing-succeeded-p this)
		(sparql-test-translation-succeeded-p this)
		(sparql-test-initialization-succeeded-p this)))))

(define-class sparql-negative-syntax-test (sparql-syntax-test) ())

(defmethod sparql-test-failed-p ((this sparql-negative-syntax-test))
  (or (call-next-method)
      (sparql-test-parsing-succeeded-p this)))

(define-class sparql-positive-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-negative-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-positive-update-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-negative-update-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-query-evaluation-test (sparql-positive-syntax-test)
  ((datafile :accessor sparql-test-datafile :initarg :datafile)
   (expected-resultfile :accessor sparql-test-expected-resultfile :initarg :expected-resultfile)
   (running-succeeded-p :accessor sparql-test-running-succeeded-p )
   (comparison-succeeded-p :accessor sparql-test-comparison-succeeded-p)))

(defmethod sparql-test-failed-p ((this sparql-query-evaluation-test))
  (or (call-next-method)
      (not (and (sparql-test-running-succeeded-p this)
		(sparql-test-comparison-succeeded-p this)))))

(defmethod initialize-instance :after ((this sparql-query-evaluation-test) &key datafile expected-resultfile &allow-other-keys)
  (setf (sparql-test-datafile this) (expand-sparql-test-filename this datafile))
  (setf (sparql-test-expected-resultfile this) (expand-sparql-test-filename this expected-resultfile)))

(define-class sparql-update-evaluation-test (sparql-query-evaluation-test)
  ((graphfiles :accessor sparql-test-graphfiles :initarg :graphfiles)
   (graphlabels :accessor sparql-test-graphlabels :initarg :graphlabels)
   (expected-resultgraphfiles :accessor sparql-test-expected-resultgraphfiles :initarg :expected-resultgraphfiles)
   (expected-resultgraphlabels :accessor sparql-test-expected-resultgraphlabels :initarg :expected-resultgraphlabels)
   (updateexpected-result :accessor sparql-test-updateexpected-result :initarg :updateexpected-result)))

(defmethod initialize-instance :after ((this sparql-update-evaluation-test) &key &allow-other-keys)
  (setf (sparql-test-expected-resultgraphfiles this)
	(loop for file in (sparql-test-expected-resultgraphfiles this)
	      collect (expand-sparql-test-filename this file))))

(define-class sparql-csv-result-format-test (sparql-query-evaluation-test) ())

(define-class sparql-protocol-test (sparql-test2) ())

(define-class sparql-service-description-test (sparql-test2) ())

(defgeneric sparql-test-parsing-phase (sparql-test2)
  (:method ((this sparql-syntax-test))
    (let ((instans (sparql-test-instans this)))
    ;;; Add rules
      (instans-add-rules instans (sparql-test-queryfile this))
    ;;; Set status
      (setf (sparql-test-parsing-succeeded-p this) (instans-has-status instans 'instans-rule-parsing-succeeded))
					;    (inform "Test ~A parse = ~A" (sparql-test-name this) (sparql-test-parsing-succeeded-p this))
      (setf (sparql-test-translation-succeeded-p this) (instans-has-status instans 'instans-rule-translation-succeeded))
      (setf (sparql-test-initialization-succeeded-p this) (instans-has-status instans 'instans-rule-initialization-succeeded))
      (let ((rule-types (instans-rule-types instans)))
	(cond ((null rule-types) nil)
	      ((null (cdr rule-types))
	       (let ((type (string (car rule-types))))
		 (setf (sparql-test-rule-type this) (intern-instans (subseq type (search "-node" type :from-end t))))))
	      (t
	       (error* "Several rule types in test query file ~A: ~A" (sparql-test-queryfile this) rule-types))))
      ))
  (:method ((this sparql-test2))
    (declare (ignore this))
    nil))

(defgeneric sparql-test-add-datafile (sparql-test2)
  (:method ((this sparql-query-evaluation-test))
    (let ((datafile (sparql-test-datafile this)))
      (and datafile
	   (let ((file-type (intern-keyword (string-upcase (file-type datafile))))
		 (instans (sparql-test-instans this)))
	     (instans-add-stream-input-processor instans datafile :base (sparql-test-base this) :input-type file-type)
	     file-type)))))

(defgeneric sparql-test-add-inputs (sparql-test2)
  (:method ((this sparql-update-evaluation-test))
    (call-next-method)
    (loop for datafile in (sparql-test-graphfiles this)
	  for file-type = (intern-keyword (string-upcase (file-type datafile)))
	  for label in (sparql-test-graphlabels this)
	  do (instans-add-stream-input-processor (sparql-test-instans this) datafile :graph label :base (sparql-test-base this) :input-type file-type)))
  (:method ((this sparql-query-evaluation-test))
    (sparql-test-add-datafile this))
  (:method ((this sparql-test2))
    (declare (ignore this))
    nil))

(defun sparql-test-output-file-name-and-type (test)
  (let ((resultfile (sparql-test-resultfile test)))
    (and resultfile
	 (values (format nil "~A~A.ttl" (sparql-test-output-directory test) (subseq (file-namestring resultfile) 0 (position #\. (file-namestring resultfile) :from-end t)))
		 :ttl))))

(defgeneric sparql-test-add-outputs (sparql-test2)
  (:method ((this sparql-query-evaluation-test))
    (case (sparql-test-type this)
      (SELECT
       (multiple-value-bind (output-file file-type) (sparql-test-output-file-name this)
	 (when output-file
	   (let ((instans (sparql-test-instans this)))
	     (setf (instans-select-output-processor instans) (create-select-output-processor instans output-file file-type))))))
      (CONSTRUCT
       (multiple-value-bind (output-file file-type) (sparql-test-output-file-name this)
	 (when output-file
	   (let ((instans (sparql-test-instans this)))
	     (setf (instans-construct-output-processor instans) (create-construct-output-processor instans output-file file-type))))))
      (ASK)
      (DESCRIBE)))
  (:method ((this sparql-test2))
    (declare (ignore this))
    nil))

(defgeneric sparql-test-running-phase (sparql-test2)
  (:method ((this sparql-query-evaluation-test))
    (let ((instans (sparql-test-instans this)))
    ;;; Add inputs
      (sparql-test-add-inputs this)
    ;;; Add outputs
      (sparql-test-add-outputs this)
    ;;; Run instans
      (sparql-test-run-system this)
    ;;; Set status
      (setf (sparql-test-not-implemented-p this) (not (instans-has-status instans 'instans-feature-not-implemented-yet)))
      (setf (sparql-test-running-succeeded-p this) (instans-has-status instans 'instans-rule-running-succeeded))))
  (:method ((this sparql-test2))
    (declare (ignore this))
    nil))

(defgeneric sparql-test-comparison-phase (sparql-test2)
  (:method ((this sparql-update-evaluation-test))
    (call-next-method this)
    (sparql-test-compare-graphs this))
  (:method ((this sparql-query-evaluation-test))
    (sparql-test-compare-datafile this))
  (:method ((this sparql-test2))
    (declare (ignore this))
    nil))

(defgeneric sparql-test-execute (sparql-test2)
  (:method ((this sparql-test2))
    (unless (sparql-test-failed-p this)
      (sparql-test-parsing-phase this))
    (unless (sparql-test-failed-p this)
      (sparql-test-running-phase this))
    (unless (sparql-test-failed-p this)
      (sparql-test-comparison-phase this))
    (sparql-test-failed-p this)))
