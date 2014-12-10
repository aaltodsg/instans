;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;
;;; Last updated: Tue Dec  9 14:09:16 EET 2014

(in-package #:instans)

(define-class sparql-test2 ()
  ((type :accessor sparql-test-type :initarg :type)
   (suite :accessor sparql-test-suite :initarg :suite)
   (collection :accessor sparql-test-collection :initarg :collection)
   (name :accessor sparql-test-name :initarg :name)
   (directory :accessor sparql-test-directory)
   (instans :accessor sparql-test-instans)
   (not-implemented-p :accessor sparql-test-not-implemented-p :initarg :not-implemented-p)
   (output-directory :accessor sparql-test-output-directory :initform "test-output")
   (base :accessor sparql-test-base)
   (status :accessor sparql-test-status :initform :not-completed)))
					; One of :not-completed, :succeeded, :failed

(defmethod initialize-instance :after ((this sparql-test2) &key &allow-other-keys)
  (setf (sparql-test-directory this)
	(truename (format nil "~Atests/~A/~A/" (find-instans-root-directory) (sparql-test-suite this) (sparql-test-collection this))))
  (setf (sparql-test-base this) (parse-iri (format nil "file://~A" (sparql-test-directory this))))
  (setf (sparql-test-instans this) (create-instans (sparql-test-base this)))
  (setf (sparql-test-output-directory this) (expand-sparql-test-filename this (sparql-test-output-directory this)))
  (ensure-directories-exist (sparql-test-output-directory this)))

(defmethod print-object ((this sparql-test2) stream)
  (format stream "#<~A \"~A/~A/~A\">" (type-of this) (sparql-test-suite this) (sparql-test-collection this) (sparql-test-name this)))

(defgeneric print-test (sparql-test2 &optional stream))

(defmethod print-test ((this sparql-test2) &optional (stream *standard-output*))
  (format stream "~%~A \"~A/~A/~A\"" (type-of this) (sparql-test-suite this) (sparql-test-collection this) (sparql-test-name this))
  (when (slot-boundp this 'directory)
    (format stream "~%  :directory ~A" (sparql-test-directory this)))
  ;; (when (slot-boundp this 'instans)
  ;;   (format stream "~%  :instans ~A" (sparql-test-instans this)))
  (when (slot-boundp this 'not-implemented-p)
    (format stream "~%  :not-implemented-p ~A" (sparql-test-not-implemented-p this)))
  (when (slot-boundp this 'output-directory)
    (format stream "~%  :output-directory ~A" (sparql-test-output-directory this)))
  (when (slot-boundp this 'base)
    (format stream "~%  :base ~A" (sparql-test-base this)))
  (when (slot-boundp this 'status)
    (format stream "~%  :status ~A" (sparql-test-status this))))

(defgeneric sparql-test-failed-p (sparql-test2))

(defmethod sparql-test-failed-p ((this sparql-test2))
  (sparql-test-not-implemented-p this))

(defun expand-sparql-test-filename (test filename)
  (assert (stringp filename))
  (format nil "~A~A" (sparql-test-directory test) filename))

(defun expand-sparql-test-filename* (test name-or-names)
  (cond ((null name-or-names) nil)
	((not (consp name-or-names)) (expand-sparql-test-filename test name-or-names))
	((= 1 (length name-or-names))
	 (expand-sparql-test-filename test (first name-or-names)))
	((not (null name-or-names))
	 (error* "More than one name in ~A: ~A" test name-or-names))))

(define-class sparql-syntax-test (sparql-test2)
  ((queryfile :accessor sparql-test-queryfile :initarg :queryfile :initform nil)
   (parsing-succeeded-p :accessor sparql-test-parsing-succeeded-p)
   (translation-succeeded-p :accessor sparql-test-translation-succeeded-p)
   (initialization-succeeded-p :accessor sparql-test-initialization-succeeded-p)
   (rule-type :accessor sparql-test-rule-type :initform nil)))

(defmethod initialize-instance :after ((this sparql-syntax-test) &key queryfile &allow-other-keys)
  (setf (sparql-test-queryfile this) (expand-sparql-test-filename* this queryfile)))

(defmethod print-test ((this sparql-syntax-test) &optional (stream *standard-output*))
  (call-next-method)
  (when (slot-boundp this 'queryfile)
    (format stream "~%  :queryfile ~A" (sparql-test-queryfile this)))
  (when (slot-boundp this 'parsing-succeeded-p)
    (format stream "~%  :parsing-succeeded-p ~A" (sparql-test-parsing-succeeded-p this)))
  (when (slot-boundp this 'translation-succeeded-p)
    (format stream "~%  :translation-succeeded-p ~A" (sparql-test-translation-succeeded-p this)))
  (when (slot-boundp this 'initialization-succeeded-p)
    (format stream "~%  :initialization-succeeded-p ~A" (sparql-test-initialization-succeeded-p this)))
  (when (slot-boundp this 'rule-type)
    (format stream "~%  :rule-type ~A" (sparql-test-rule-type this))))

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
  ((datafile :accessor sparql-test-datafile :initarg :datafile :initform nil)
   (expected-resultfile :accessor sparql-test-expected-resultfile :initarg :expected-resultfile :initform nil)
   (running-succeeded-p :accessor sparql-test-running-succeeded-p )
   (comparison-succeeded-p :accessor sparql-test-comparison-succeeded-p)))

(defmethod initialize-instance :after ((this sparql-query-evaluation-test) &key datafile expected-resultfile &allow-other-keys)
;  (inform "~A: datafile = ~A" this datafile)
  (setf (sparql-test-datafile this) (expand-sparql-test-filename* this datafile))
;  (inform "~A: datafile now = ~A" this (sparql-test-datafile this))
  (setf (sparql-test-expected-resultfile this) (expand-sparql-test-filename* this expected-resultfile)))

(defmethod print-test ((this sparql-query-evaluation-test) &optional (stream *standard-output*))
  (call-next-method)
  (when (slot-boundp this 'datafile)
    (format stream "~%  :datafile ~A" (sparql-test-datafile this)))
  (when (slot-boundp this 'expected-resultfile)
    (format stream "~%  :expected-resultfile ~A" (sparql-test-expected-resultfile this)))
  (when (slot-boundp this 'running-succeeded-p)
    (format stream "~%  :running-succeeded-p ~A" (sparql-test-running-succeeded-p this)))
  (when (slot-boundp this 'comparison-succeeded-p)
    (format stream "~%  :comparison-succeeded-p ~A" (sparql-test-comparison-succeeded-p this))))

(defmethod sparql-test-failed-p ((this sparql-query-evaluation-test))
  (or (call-next-method)
      (not (and (sparql-test-running-succeeded-p this)
		(sparql-test-comparison-succeeded-p this)))))

(define-class sparql-update-evaluation-test (sparql-query-evaluation-test)
  ((graphfiles :accessor sparql-test-graphfiles :initarg :graphfiles :initform nil)
   (graphlabels :accessor sparql-test-graphlabels :initarg :graphlabels :initform nil)
   (expected-resultgraphfiles :accessor sparql-test-expected-resultgraphfiles :initarg :expected-resultgraphfiles :initform nil)
   (expected-resultgraphlabels :accessor sparql-test-expected-resultgraphlabels :initarg :expected-resultgraphlabels :initform nil)
   (updateexpected-result :accessor sparql-test-updateexpected-result :initarg :updateexpected-result :initform nil)))

(defmethod initialize-instance :after ((this sparql-update-evaluation-test) &key &allow-other-keys)
  (setf (sparql-test-expected-resultgraphfiles this)
	(loop for file in (sparql-test-expected-resultgraphfiles this)
	      collect (expand-sparql-test-filename this file))))

(defmethod print-test ((this sparql-update-evaluation-test) &optional (stream *standard-output*))
  (call-next-method)
  (when (slot-boundp this 'graphfiles)
    (format stream "~%  :graphfiles ~A" (sparql-test-graphfiles this)))
  (when (slot-boundp this 'graphlabels)
    (format stream "~%  :graphlabels ~A" (sparql-test-graphlabels this)))
  (when (slot-boundp this 'expected-resultgraphfiles)
    (format stream "~%  :expected-resultgraphfiles ~A" (sparql-test-expected-resultgraphfiles this)))
  (when (slot-boundp this 'expected-resultgraphlabels)
    (format stream "~%  :expected-resultgraphlabels ~A" (sparql-test-expected-resultgraphlabels this)))
  (when (slot-boundp this 'updateexpected-result)
    (format stream "~%  :updateexpected-result ~A" (sparql-test-updateexpected-result this))))

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
       (multiple-value-bind (output-file file-type) (sparql-test-output-file-name-and-type this)
	 (when output-file
	   (let ((instans (sparql-test-instans this)))
	     (setf (instans-select-output-processor instans) (create-select-output-processor instans output-file file-type))))))
      (CONSTRUCT
       (multiple-value-bind (output-file file-type) (sparql-test-output-file-name-and-type this)
	 (when output-file
	   (let ((instans (sparql-test-instans this)))
	     (setf (instans-construct-output-processor instans) (create-construct-output-processor instans output-file file-type))))))
      (ASK)
      (DESCRIBE)))
  (:method ((this sparql-test2))
    (declare (ignore this))
    nil))

(defgeneric sparql-test-run-system (sparql-test2)
  (:method ((this sparql-test2))
    (let ((instans (sparql-test-instans this)))
      (run-input-processors instans t)
      (setf (sparql-test-running-succeeded-p this) t))))

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

(defgeneric sparql-test-compare-graphs (sparql-test2)
  (:method ((this sparql-test2))
    this))

(defgeneric sparql-test-compare-datafile (sparql-test2)
  (:method ((this sparql-test2))
    this))

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


(define-class sparql-tests2 ()
  ((entries :accessor sparql-tests-entries :initarg :entries :initform nil)
   (fields :accessor sparql-tests-fields :initarg :fields)))

(defun parse-tests-from-csv (&key (csv-file "/Users/enu/instans/tests/sparql-tests/sparql-tests.csv"))
  (multiple-value-bind (alist headers); linecount entrycount)
      (csv-file-to-alist csv-file :key-fields '("type" "suite" "collection" "name") :data-field-merge #'simple-field-merge)
    ;; (loop for entry in alist
    ;; 	  for key = (first entry)
    ;; 	  for values = (second entry)
    ;; 	  for name-value-list = (mapcar #'list (nthcdr 4 headers) values)
    ;; 	  do (assert (null (cddr entry)))
    ;; 	  do (inform "~A ~{~A~^/~}:~{~%  ~{:~A~^ ~A~}~}" (first key) (rest key) name-value-list))
    ;; 					;    alist
    (flet ((test-type-class-name (type-string)
	     (cond ((string= type-string "mf:PositiveSyntaxTest") 'sparql-positive-syntax-test)
		   ((string= type-string "mf:NegativeSyntaxTest") 'sparql-negative-syntax-test)
		   ((string= type-string "mf:PositiveSyntaxTest11") 'sparql-positive-syntax-test-11)
		   ((string= type-string "mf:NegativeSyntaxTest11") 'sparql-negative-syntax-test-11)
		   ((string= type-string "mf:PositiveUpdateSyntaxTest11") 'sparql-positive-update-syntax-test-11)
		   ((string= type-string "mf:NegativeUpdateSyntaxTest11") 'sparql-negative-update-syntax-test-11)
		   ((string= type-string "mf:QueryEvaluationTest") 'sparql-query-evaluation-test)
		   ((string= type-string "mf:UpdateEvaluationTest") 'sparql-update-evaluation-test)
		   ((string= type-string "mf:CSVResultFormatTest") 'sparql-csv-result-format-test)
		   ((string= type-string "mf:ProtocolTest") 'sparql-protocol-test)
		   ((string= type-string "mf:ServiceDescriptionTest") 'sparql-service-description-test)
		   (t (error* "Unknown test type ~A" type-string)))))
      (let* ((header-keywords (mapcar #'(lambda (h) (intern-keyword (string-upcase h))) headers)))
	(make-instance 'sparql-tests2
		       :fields header-keywords
		       :entries (loop for entry in alist
				      for key = (first entry)
				      for values = (second entry)
				      for type-class-name = (test-type-class-name (first key))
				      for test = (apply #'make-instance type-class-name :type (first key) :suite (second key) :collection (third key) :name (fourth key)
							(loop for keyword in (nthcdr 4 header-keywords)
							      for value in values
							      nconc (list keyword value)))
				      collect test))))))

;; (add-sparql-test tests
;; 							     :type type :negative (parse-xsd-boolean negative) :suite suite :collection collection :name name
;; 							     :queryfile queryfile :datafile datafile :graphdatafile graphdatafile
;; 							     :resultfile resultfile :resultgraphfile resultgraphfile :resultgraphlabel resultgraphlabel)
