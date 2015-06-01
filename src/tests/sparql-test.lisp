;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;
;;; Last updated: Tue Dec  9 14:09:16 EET 2014

(in-package #:instans)

(defvar *sparql-test-set*)
(defvar *current-sparql-test*)
(defvar *collect-sparql-test-set-script* nil)

;;; Classes

(define-class sparql-test ()
  ((test-set :accessor sparql-test-test-set :initarg :test-set :initform nil)
   (type :accessor sparql-test-type :initarg :type)
   (suite :accessor sparql-test-suite :initarg :suite)
   (collection :accessor sparql-test-collection :initarg :collection)
   (phases :accessor sparql-test-phases :initform (list :created))
   (available-phases :accessor sparql-test-available-phases :allocation :class :initform (list :created :parsed :ran :compared :completed))
   (name :accessor sparql-test-name :initarg :name)
   (directory :accessor sparql-test-directory)
   (instans :accessor sparql-test-instans :initform nil)
   (not-implemented-p :accessor sparql-test-not-implemented-p :initarg :not-implemented-p :initform nil)
   (output-directory :accessor sparql-test-output-directory :initform "output/")
   (base :accessor sparql-test-base :initarg :base)
   (output-options-stream :accessor sparql-test-output-options-stream :initarg :output-options-stream :initform nil)
   (results :accessor sparql-test-results :initform nil)))
					; One of :not-completed, :succeeded, :failed

(define-class sparql-syntax-test (sparql-test)
  ((queryfile :accessor sparql-test-queryfile :initarg :queryfile)
   (parsing-succeeded-p :accessor sparql-test-parsing-succeeded-p)
   (translation-succeeded-p :accessor sparql-test-translation-succeeded-p)
   (initialization-succeeded-p :accessor sparql-test-initialization-succeeded-p)
   (rule-type :accessor sparql-test-rule-type)))

(define-class sparql-positive-syntax-test (sparql-syntax-test) ())

(define-class sparql-negative-syntax-test (sparql-syntax-test) ())

(define-class sparql-positive-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-negative-syntax-test-11 (sparql-negative-syntax-test) ())

(define-class sparql-positive-update-syntax-test-11 (sparql-positive-syntax-test) ())

(define-class sparql-negative-update-syntax-test-11 (sparql-negative-syntax-test) ())

(define-class sparql-query-evaluation-test (sparql-positive-syntax-test)
  ((datafile :accessor sparql-test-datafile :initarg :datafile)
   (graphfiles :accessor sparql-test-graphfiles :initarg :graphfiles)
   (resultfile :accessor sparql-test-resultfile :initarg :resultfile)
   (running-succeeded-p :accessor sparql-test-running-succeeded-p )
   (comparing-succeeded-p :accessor sparql-test-comparing-succeeded-p)))

(define-class sparql-update-evaluation-test (sparql-query-evaluation-test)
  ((graphlabels :accessor sparql-test-graphlabels :initarg :graphlabels)
   (resultgraphfiles :accessor sparql-test-resultgraphfiles :initarg :resultgraphfiles)
   (resultgraphlabels :accessor sparql-test-resultgraphlabels :initarg :resultgraphlabels)
   (updateresult :accessor sparql-test-updateresult :initarg :updateresult)))

(define-class sparql-csv-result-format-test (sparql-query-evaluation-test) ())

(define-class sparql-protocol-test (sparql-test) ())

(define-class sparql-service-description-test (sparql-test) ())

;;; Initialize & reinitialize instance

(defmethod initialize-instance :after ((this sparql-test) &key base &allow-other-keys)
  (unless (rdf-iri-p (slot-value-with-default this 'base nil))
    (cond ((eql 0 (search "file://" base))
	   (setf (sparql-test-directory this) (truename (subseq base 0 7)))
	   (setf (sparql-test-base this) (parse-iri base)))
	  (t
	   (setf (sparql-test-directory this) (truename base))
	   (setf (sparql-test-base this) (parse-iri (format nil "file://~A" base))))))
;  (setf (sparql-test-instans this) (create-instans (sparql-test-base this)))
  (setf (sparql-test-output-directory this) (expand-sparql-test-filename this (sparql-test-output-directory this)))
  (ensure-directories-exist (sparql-test-output-directory this)))

(defmethod reinitialize-instance ((this sparql-test) &key &allow-other-keys)
  (setf (sparql-test-not-implemented-p this) nil)
  (setf (sparql-test-phases this) (list :created)))

(defmethod initialize-instance :after ((this sparql-syntax-test) &key &allow-other-keys)
  (unless (char= (char (sparql-test-queryfile this) 0) #\/)
    (setf (sparql-test-queryfile this) (expand-sparql-test-filename* this (sparql-test-queryfile this)))))

(defmethod reinitialize-instance ((this sparql-syntax-test) &key &allow-other-keys)
  (call-next-method)
  (slot-makunbound this 'parsing-succeeded-p)
  (slot-makunbound this 'translation-succeeded-p)
  (slot-makunbound this 'initialization-succeeded-p)
  (slot-makunbound this 'rule-type))

(defmethod initialize-instance :after ((this sparql-query-evaluation-test) &key datafile resultfile &allow-other-keys)
;  (inform "~A: datafile = ~A" this datafile)
  (setf (sparql-test-datafile this) (expand-sparql-test-filename* this datafile))
;  (inform "~A: datafile now = ~A" this (sparql-test-datafile this))
  (setf (sparql-test-graphfiles this)
	(loop for file in (sparql-test-graphfiles this)
	      collect (expand-sparql-test-filename this file)))
  (setf (sparql-test-resultfile this) (expand-sparql-test-filename* this resultfile)))

(defmethod reinitialize-instance ((this sparql-query-evaluation-test) &key &allow-other-keys)
  (call-next-method)
  (slot-makunbound this 'running-succeeded-p)
  (slot-makunbound this 'comparing-succeeded-p))

(defmethod initialize-instance :after ((this sparql-update-evaluation-test) &key &allow-other-keys)
  (setf (sparql-test-resultgraphfiles this)
	(loop for file in (sparql-test-resultgraphfiles this)
	      collect (expand-sparql-test-filename this file))))

(defmethod initialize-instance :after ((this sparql-csv-result-format-test) &key &allow-other-keys)
  (setf (sparql-test-not-implemented-p this) t))

(defmethod initialize-instance :after ((this sparql-protocol-test) &key &allow-other-keys)
  (sparql-test-add-phase this :completed)
  (setf (sparql-test-not-implemented-p this) t))

(defmethod initialize-instance :after ((this sparql-service-description-test) &key &allow-other-keys)
  (setf (sparql-test-not-implemented-p this) t))

;;; Print object

(defmethod print-object ((this sparql-test) stream)
  (format stream "#<~A \"~A/~A/~A\">" (type-of this) (sparql-test-suite this) (sparql-test-collection this) (sparql-test-name this)))

;;; Print test

(defgeneric print-test (sparql-test &optional stream))

(defmethod print-test ((this sparql-test) &optional (stream *standard-output*))
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
    (format stream "~%  :base ~A" (sparql-test-base this))))

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

(defmethod print-test ((this sparql-query-evaluation-test) &optional (stream *standard-output*))
  (call-next-method)
  (when (slot-boundp this 'datafile)
    (format stream "~%  :datafile ~A" (sparql-test-datafile this)))
  (when (slot-boundp this 'resultfile)
    (format stream "~%  :resultfile ~A" (sparql-test-resultfile this)))
  (when (slot-boundp this 'graphfiles)
    (format stream "~%  :graphfiles ~A" (sparql-test-graphfiles this)))
  (when (slot-boundp this 'running-succeeded-p)
    (format stream "~%  :running-succeeded-p ~A" (sparql-test-running-succeeded-p this)))
  (when (slot-boundp this 'comparing-succeeded-p)
    (format stream "~%  :comparing-succeeded-p ~A" (sparql-test-comparing-succeeded-p this))))

(defmethod print-test ((this sparql-update-evaluation-test) &optional (stream *standard-output*))
  (call-next-method)
  (when (slot-boundp this 'graphlabels)
    (format stream "~%  :graphlabels ~A" (sparql-test-graphlabels this)))
  (when (slot-boundp this 'resultgraphfiles)
    (format stream "~%  :resultgraphfiles ~A" (sparql-test-resultgraphfiles this)))
  (when (slot-boundp this 'resultgraphlabels)
    (format stream "~%  :resultgraphlabels ~A" (sparql-test-resultgraphlabels this)))
  (when (slot-boundp this 'updateresult)
    (format stream "~%  :updateresult ~A" (sparql-test-updateresult this))))

;;; Test failed p

(defgeneric sparql-test-failed-p (sparql-test))

(defmethod sparql-test-failed-p ((this sparql-test))
  (slot-value-with-default this 'not-implemented-p nil))

(defmethod sparql-test-failed-p ((this sparql-positive-syntax-test))
  (or (call-next-method)
      (not (and (slot-value-with-default this 'parsing-succeeded-p t)
		(slot-value-with-default this 'translation-succeeded-p t)
		(slot-value-with-default this 'initialization-succeeded-p t)))))

(defmethod sparql-test-failed-p ((this sparql-negative-syntax-test))
  (or (call-next-method)
      (not (slot-value-with-default this 'parsing-succeeded-p t))))

(defmethod sparql-test-failed-p ((this sparql-query-evaluation-test))
  (or (call-next-method)
      (not (and (slot-value-with-default this 'running-succeeded-p t)
		(slot-value-with-default this 'comparing-succeeded-p t)))))


;;; Aux functions

(defun sparql-test-add-phase (test phase)
  (push-to-end-new phase (sparql-test-phases test)))

(defun sparql-test-completed-p (test)
  (member :completed (sparql-test-phases test)))

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

;;; Phase aux functions

(defgeneric sparql-test-add-datafile (sparql-test)
  (:method ((this sparql-query-evaluation-test))
    (let ((datafile (sparql-test-datafile this)))
      (and datafile
	   (let ((file-type (intern-keyword (string-upcase (file-type datafile))))
		 (instans (sparql-test-instans this)))
	     (instans-add-stream-input-processor instans datafile :base (sparql-test-base this) :input-type file-type :output-options-stream (sparql-test-output-options-stream this))
	     file-type)))))

(defgeneric sparql-test-add-inputs (sparql-test)
  (:method ((this sparql-update-evaluation-test))
    (call-next-method)
    (loop for datafile in (sparql-test-graphfiles this)
	  for file-type = (intern-keyword (string-upcase (file-type datafile)))
	  for label in (sparql-test-graphlabels this)
	  do (instans-add-stream-input-processor (sparql-test-instans this) datafile :graph label :base (sparql-test-base this) :input-type file-type :output-options-stream (sparql-test-output-options-stream this))))
  (:method ((this sparql-query-evaluation-test))
    (sparql-test-add-datafile this)))

(defun sparql-test-output-file-name-and-type (test)
  (let ((resultfile (sparql-test-resultfile test)))
    (and resultfile
	 (values (format nil "~A~A.ttl" (sparql-test-output-directory test) (subseq (file-namestring resultfile) 0 (position #\. (file-namestring resultfile) :from-end t)))
		 :ttl))))

(defgeneric sparql-test-add-outputs (sparql-test)
  (:method ((this sparql-query-evaluation-test))
    (case (sparql-test-rule-type this)
      (SELECT
       (multiple-value-bind (output-file file-type) (sparql-test-output-file-name-and-type this)
	 (when output-file
	   (when (sparql-test-output-options-stream this)
	     (format (sparql-test-output-options-stream this) "--select-output-~(~A~)=~A" file-type output-file))
	   (let ((instans (sparql-test-instans this)))
	     (setf (instans-select-output-processor instans) (create-select-output-processor instans output-file file-type))))))
      (CONSTRUCT
       (multiple-value-bind (output-file file-type) (sparql-test-output-file-name-and-type this)
	 (when output-file
	   (when (sparql-test-output-options-stream this)
	     (format (sparql-test-output-options-stream this) "--construct-output-~(~A~)=~A" file-type output-file))
	   (let ((instans (sparql-test-instans this)))
	     (setf (instans-construct-output-processor instans) (create-construct-output-processor instans output-file file-type))))))
      (ASK)
      (DESCRIBE)
      (MODIFY)
      (t (error* "Illegal rule-type ~A in ~A" (sparql-test-rule-type this) this)))))

(defgeneric sparql-test-run-system (sparql-test)
  (:method ((this sparql-query-evaluation-test))
    (let* ((instans (sparql-test-instans this))
	   (result (catch 'sparql-op-not-implemented-yet (run-input-processors instans t))))
      (instans-close-open-streams instans)
      (when (eq result :sparql-op-not-implemented-yet)
	(instans-add-status instans 'instans-feature-not-implemented-yet))
      result)))

(defgeneric sparql-test-compare-graphs (sparql-test)
  (:method ((this sparql-test))
    this))

(defun maybe-convert-result-file-to-ttl (instans input-file input-file-type output-file)
  (case input-file-type
    (:srx
     (let ((results (with-open-file (input input-file :direction :input)
		     (parse-srx-stream instans input input-file))))
       (with-open-file (output output-file :direction :output :if-exists :supersede)
	 (output-results-in-ttl results output))
       output-file))
    (t (inform "Cannot convert yet ~A" input-file)
       nil)))

(defgeneric sparql-test-compare-datafile (sparql-test)
  (:method ((this sparql-query-evaluation-test))
    (let ((resultfile (sparql-test-resultfile this))
	  (instans (sparql-test-instans this)))
      (when resultfile
	(unless (multiple-value-bind (dir namebase type)
		    (split-path-to-name-and-type-strings resultfile)		    
		  (declare (ignore dir))
		  (let* ((resulttype (intern-keyword (string-upcase type)))
			 (output-dir (sparql-test-output-directory this))
			 (output-ttl-file (format nil "~A/~A.ttl" output-dir namebase)))
		    (case (sparql-test-rule-type this)
		      (SELECT
           ;;; If the results file is not ttl-file, we must create a comparable ttl file (if it does not exist already)
		       (setf resultfile 
			     (cond ((eq resulttype :ttl) resultfile)
				   (t
				    (let ((ttl-resultfile (format nil "~A/~A-converted.ttl" output-dir namebase)))
				      (maybe-convert-result-file-to-ttl instans resultfile resulttype ttl-resultfile)))))
		       (when resultfile (sparql-compare-ttl-result-files resultfile output-ttl-file)))
		      (CONSTRUCT
		       (unless (eq resulttype :ttl)
			 (error* "Not a ttl resultfile ~A" resultfile))
		       (sparql-compare-ttl-result-files resultfile output-ttl-file))
		      ((ASK DESCRIBE MODIFY) nil))))
		  (instans-add-status instans 'instans-rule-comparing-failed)))))
    (:method ((this sparql-test))
      this))
  
(defgeneric sparql-test-collect-results (sparql-test)
  (:method ((this sparql-test))
    (flet ((sv (slot) (if (and (slot-exists-p this slot) (slot-boundp this slot)) (slot-value this slot) :unbound)))
      (list 
       (list :syntax-test-p (typep this 'sparql-syntax-test))
       (list :negative-syntax-test-p (typep this 'sparql-negative-syntax-test))
       (list :query-evaluation-test-p (typep this 'sparql-query-evaluation-test))
       (list :type (sv 'type))
       (list :suite (sv 'suite))
       (list :collection (sv 'collection))
       (list :name (sv 'name))
       (list :failedp (sparql-test-failed-p this))
       (list :implementedp (let ((v (sv 'not-implemented-p))) (if (eq v :unbound) :unbound (not v))))
       (list :parsing-succeeded-p (sv 'parsing-succeeded-p))
       (list :translation-succeeded-p (sv 'translation-succeeded-p))
       (list :initialization-succeeded-p (sv 'initialization-succeeded-p))
       (list :rule-type (sv 'rule-type))
       (list :running-succeeded-p (sv 'running-succeeded-p))
       (list :comparing-succeeded-p (sv 'comparing-succeeded-p))))))

;;; Phases

(defgeneric sparql-test-parse-phase (sparql-test &key &allow-other-keys)
  (:method ((this sparql-syntax-test) &key &allow-other-keys)
    (let ((instans (or (sparql-test-instans this) (setf (sparql-test-instans this) (create-instans (sparql-test-base this))))))
    ;;; Add rules
      (instans-add-rules instans (sparql-test-queryfile this) :output-options-stream (sparql-test-output-options-stream this))
    ;;; Set status
;      (inform "      Test ~A status ~{~(~A~)~^, ~}" this (mapcar #'type-of (instans-status instans)))
      (setf (sparql-test-parsing-succeeded-p this) (instans-has-status instans 'instans-rule-parsing-succeeded))
      (setf (sparql-test-translation-succeeded-p this) (instans-has-status instans 'instans-rule-translation-succeeded))
      (setf (sparql-test-initialization-succeeded-p this) (instans-has-status instans 'instans-rule-initialization-succeeded))
      (let ((rule-types (instans-rule-types instans)))
	(cond ((null rule-types) nil)
	      ((null (cdr rule-types))
	       (let ((type (string (car rule-types))))
;		 (inform "(search \"-NODE\" ~S :from-end t :test #'equal) = ~S" type (search "-NODE" type :from-end t :test #'equal))
		 (setf (sparql-test-rule-type this) (intern-instans (subseq type 0 (search "-NODE" type :from-end t :test #'equal))))))
	      (t
	       (error* "Several rule types in test query file ~A: ~A" (sparql-test-queryfile this) rule-types))))
      (inform "  Parsed ~(~A~) -> ~{~(~A~)~^ ~}, ~{~(~A~)~^ ~}" this (mapcar #'(lambda (st) (subseq (string (type-of st)) 8)) (instans-status (sparql-test-instans this))) (sparql-test-phases this))
      (sparql-test-add-phase this :parsed)))
  (:method ((this sparql-test) &key &allow-other-keys)
    (declare (ignore this))
    nil))

(defgeneric sparql-test-run-phase (sparql-test &key &allow-other-keys)
  (:method ((this sparql-query-evaluation-test) &key &allow-other-keys)
    (let ((instans (sparql-test-instans this)))
    ;;; Add inputs
      (sparql-test-add-inputs this)
    ;;; Add outputs
      (sparql-test-add-outputs this)
    ;;; Run instans
      (sparql-test-run-system this)
    ;;; Set status
      (cond ((instans-has-status instans 'instans-feature-not-implemented-yet)
	     (setf (sparql-test-not-implemented-p this) t))
	    ((not (or (instans-has-status instans 'instans-rule-running-succeeded)
		      (instans-has-status instans 'instans-rule-running-failed)))
	     (instans-add-status instans 'instans-rule-running-succeeded)))
      (setf (sparql-test-running-succeeded-p this) (instans-has-status instans 'instans-rule-running-succeeded))
      (inform "  Ran ~(~A~) -> ~{~(~A~)~^ ~}, ~{~(~A~)~^ ~}" this (mapcar #'(lambda (st) (subseq (string (type-of st)) 8)) (instans-status (sparql-test-instans this))) (sparql-test-phases this))
      (sparql-test-add-phase this :ran)))
  (:method ((this sparql-test) &key &allow-other-keys)
    (declare (ignore this))
    nil))

(defgeneric sparql-test-compare-phase (sparql-test &key &allow-other-keys)
  (:method ((this sparql-update-evaluation-test) &key &allow-other-keys)
    (call-next-method this)
    (sparql-test-compare-graphs this))
  (:method ((this sparql-query-evaluation-test) &key &allow-other-keys)
    (let ((instans (sparql-test-instans this)))
      (sparql-test-compare-datafile this)
      (unless (instans-has-status instans 'instans-rule-comparing-failed)
	(instans-add-status instans 'instans-rule-comparing-succeeded))
      (setf (sparql-test-comparing-succeeded-p this) (instans-has-status instans 'instans-rule-comparing-succeeded))
      (sparql-test-add-phase this :compared)))
  (:method ((this sparql-test) &key &allow-other-keys)
    (declare (ignorable this))
    nil))

(defgeneric sparql-test-complete-phase (sparql-test &key &allow-other-keys)
  (:method ((this sparql-test) &key &allow-other-keys)
    (setf (sparql-test-instans this) nil)
    (setf (sparql-test-results this) (sparql-test-collect-results this))
    (sparql-test-add-phase this :completed)))

;;; Phase around methods

(defun sparql-test-phase-pre-test (test phase skip-list verbosep)
  (cond ((sparql-test-failed-p test)
	 (when verbosep (inform "Test ~A already failed, skipping." test))
	 nil)
	((sparql-test-set-skip-test-p test skip-list)
	 (when verbosep (inform "Test ~A in ~(~A~) skip list, skipping" phase test))
	 nil)
	((member phase (sparql-test-phases test))
	 (when verbosep (inform "Test ~A: phase ~A alread done, skipping" test phase))
	 nil)
	(t t)))

(defmethod sparql-test-parse-phase :around ((this sparql-syntax-test) &key forcep verbosep)
  (when (or forcep (sparql-test-phase-pre-test this :parsed (sparql-test-set-skip-parse-list (sparql-test-test-set this)) verbosep))
    (call-next-method)))

(defmethod sparql-test-run-phase :around ((this sparql-syntax-test) &key forcep verbosep)
  (when (or forcep (sparql-test-phase-pre-test this :ran (sparql-test-set-skip-run-list (sparql-test-test-set this)) verbosep))
    (call-next-method)))

(defmethod sparql-test-compare-phase :around ((this sparql-syntax-test) &key forcep verbosep)
  (when (or forcep (sparql-test-phase-pre-test this :compared (sparql-test-set-skip-compare-list (sparql-test-test-set this)) verbosep))
    (call-next-method)))

(defmethod sparql-test-complete-phase :around ((this sparql-syntax-test) &key forcep verbosep)
  (declare (ignorable forcep verbosep))
  (call-next-method))

;;; Test execute method

(defgeneric sparql-test-execute (sparql-test &key target-phase forcep verbosep)
  (:method ((this sparql-test) &key (target-phase :completed) forcep verbosep)
    (let* ((phases (sparql-test-available-phases this)))
      (loop for phase in phases
	    while (<= (position phase phases) (position target-phase phases))
	    when verbosep
	    do (progn
		 (when verbosep (inform "Phase ~A for ~A" phase this))
		 (case phase
		   (:created)
		   (:parsed (sparql-test-parse-phase this :forcep forcep :verbosep verbosep))
		   (:ran (sparql-test-run-phase this :forcep forcep :verbosep verbosep))
		   (:compared (sparql-test-compare-phase this :forcep forcep :verbosep verbosep))
		   (:completed (sparql-test-complete-phase this :forcep forcep :verbosep verbosep))
		   (t (error* "Illegal phase ~A for test ~A" phase this))))))))

;;;
;;; Test sets
;;;

(define-class sparql-test-set ()
  ((root-directory :accessor sparql-test-set-root-directory :initarg :root-directory)
   (collector-script :accessor sparql-test-set-collector-script)
   (collector-rules-file :accessor sparql-test-set-collector-rules-file)
   (collected-tests-csv-file :accessor sparql-test-set-collected-tests-csv-file)
   (results-csv-file :accessor sparql-test-set-results-csv-file)
   (manifests :accessor sparql-test-set-manifests)
   (skip-parse-list :accessor sparql-test-set-skip-parse-list :initarg :skip-parse-list :initform nil)
   (skip-run-list :accessor sparql-test-set-skip-run-list :initarg :skip-run-list :initform nil)
   (skip-compare-list :accessor sparql-test-set-skip-compare-list :initarg :skip-compare-list :initform nil)
   (skip-complete-list :accessor sparql-test-set-skip-complete-list :initarg :skip-complete-list :initform nil)
   (headers :accessor sparql-test-set-headers :initarg :headers)
   (verbosep :accessor sparql-test-set-verbose-p :initarg :verbosep :initform t)
   (tests :accessor sparql-test-set-tests)))

(defmethod initialize-instance :after ((this sparql-test-set) &key root-directory &allow-other-keys)
  (setf (sparql-test-set-collector-rules-file this) (format nil "~A/tools/collect-tests-from-manifests.rq" root-directory))
  (setf (sparql-test-set-collected-tests-csv-file this) (format nil "~A/suites/tests-from-manifests.csv" root-directory))
  (setf (sparql-test-set-results-csv-file this) (format nil "~A/suites/results.csv" root-directory))
  (setf (sparql-test-set-manifests this) (directory (format nil "~A/suites/data-*/*/manifest.ttl" root-directory)))
  (when (sparql-test-set-verbose-p this) (inform "Manifest files:~{~%  ~A~}" (sparql-test-set-manifests this)))
  (init-sparql-test-set-script this)
  (skip-tests this)
  (sparql-test-set-collect-tests this))

(defun skip-tests (tests)
  (setf (sparql-test-set-skip-run-list tests) '("delete-insert"
					     "dawg-delete-insert-01"
					     "dawg-delete-insert-01c"
					     ;; "md5-01"
					     ;; "md5-02"
					     ;; "sha1-01"
					     ;; "sha1-02"
					     ;; "sha256-01"
					     ;; "sha256-02"
					     ;; "sha512-01"
					     ;; "sha512-02"
					     ;; "replace01"
					     ;; "replace02"
					     ;; "replace03"
					     ("data-sparql11" "service")
					     ("data-sparql11" "subquery" "subquery04")
					     ("data-sparql11" "subquery" "subquery06")
					     ("data-sparql11" "subquery" "subquery08")
					     ("data-sparql11" "subquery" "subquery09")
					     ("data-sparql11" "subquery" "subquery10")
						))
  (setf (sparql-test-set-skip-compare-list tests) (sparql-test-set-skip-run-list tests)))

(defgeneric sparql-test-set-print (sparql-test-set &optional stream)
  (:method ((this sparql-test-set) &optional (stream *error-output*))
    (loop for test in (sparql-test-set-tests this)
	  do (print-test test stream))))

(defun sparql-test-set-skip-test-p (test skip-list)
  (loop for skip in skip-list
        when (and (stringp skip)
		  (or (equal (sparql-test-suite test) skip)
		      (equal (sparql-test-collection test) skip)
		      (equal (sparql-test-name test) skip)))
        return t
        when (and (consp skip)
		  (or (null (first skip))
		      (equal (sparql-test-suite test) (first skip)))
		  (or (null (second skip))
		      (equal (sparql-test-collection test) (second skip)))
		  (or (null (third skip))
		      (equal (sparql-test-name test) (third skip))))
	return t
	finally (return nil)))

(defun init-sparql-test-set-script (test-set)
  (setf *collect-sparql-test-set-script*
	(setf (sparql-test-set-collector-script test-set)
	      (let ((su (format nil "~A/suites" (sparql-test-set-root-directory test-set))))
		(format nil
"PREFIX dawgt: <http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#>
PREFIX ent: <http://www.w3.org/ns/entailment/>
PREFIX mf: <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#>
PREFIX mfx: <http://jena.hpl.hp.com/2005/05/test-manifest-extra#>
PREFIX pr: <http://www.w3.org/ns/owl-profile/>
PREFIX qt: <http://www.w3.org/2001/sw/DataAccess/tests/test-query#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sd: <http://www.w3.org/ns/sparql-service-description#>
PREFIX sparql: <http://www.w3.org/ns/sparql#>
PREFIX ut: <http://www.w3.org/2009/sparql/tests/test-update#>

# @SyntaxTest
SELECT ?base ?type ?suite ?collection ?name ?queryfile ?datafile ?graphfiles ?graphlabels ?resultfile ?resultgraphfiles ?resultgraphlabels ?updateresult ?queryserviceendpoint ?queryservicedata ?entailmentprofile ?entailmentregime WHERE {
       ?test rdf:type ?type
       FILTER(?type = mf:NegativeSyntaxTest
           || ?type = mf:NegativeSyntaxTest11
           || ?type = mf:NegativeUpdateSyntaxTest11
           || ?type = mf:PositiveSyntaxTest
           || ?type = mf:PositiveSyntaxTest11
           || ?type = mf:PositiveUpdateSyntaxTest11)

       ?test mf:action ?queryfile1
       FILTER(!isblank(?queryfile1))
       BIND(strbefore(strafter(str(<>), \"~A/\"), \"/\") AS ?suite)
       BIND(strbefore(strafter(strafter(str(<>), \"~A/\"), \"/\"),\"/\") AS ?collection)
       BIND(strafter(str(?test), \"#\") AS ?name)
       BIND(str(<>) AS ?base)
       BIND(strafter(str(?queryfile1), ?base) AS ?queryfile)
};

# @QueryEvaluationTest
SELECT ?base ?type ?suite ?collection ?name ?queryfile ?datafile ?graphfiles ?graphlabels ?resultfile ?resultgraphfiles ?resultgraphlabels ?updateresult ?queryserviceendpoint ?queryservicedata ?entailmentprofile ?entailmentregime WHERE {
       ?test rdf:type ?type
       FILTER(?type = mf:QueryEvaluationTest)

       ?test mf:action ?a .
       ?a qt:query ?queryfile1
       OPTIONAL { ?a qt:data ?datafile1 }
       OPTIONAL { ?a qt:graphData ?graphfiles1 }
       OPTIONAL { ?test mf:result ?resultfile1 FILTER(!isblank(?resultfile1)) }

       BIND(strbefore(strafter(str(<>), \"~A/\"), \"/\") AS ?suite)
       BIND(strbefore(strafter(strafter(str(<>), \"~A/\"), \"/\"),\"/\") AS ?collection)
       BIND(strafter(str(?test), \"#\") AS ?name)
       BIND(str(<>) AS ?base)
       BIND(strafter(str(?queryfile1), ?base) AS ?queryfile)
       BIND(strafter(str(?datafile1),?base) AS ?datafile)
       BIND(strafter(str(?graphfiles1),?base) AS ?graphfiles)
       BIND(strafter(str(?resultfile1),?base) AS ?resultfile)
};

# @UpdateEvaluationTest
SELECT ?base ?type ?suite ?collection ?name ?queryfile ?datafile ?graphfiles ?graphlabels ?resultfile ?resultgraphfiles ?resultgraphlabels ?updateresult ?queryserviceendpoint ?queryservicedata ?entailmentprofile ?entailmentregime WHERE {
       ?test rdf:type ?type
       FILTER(?type = mf:UpdateEvaluationTest)

       ?test mf:action ?a .
       ?a ut:request ?queryfile1
       OPTIONAL { ?a ut:data ?datafile1 }
       OPTIONAL { ?a ut:graphData [ ut:graph ?graphfiles1 ; rdfs:label ?graphlabels ] }
       ?test mf:result ?r1 .
       FILTER(isblank(?r1))
       OPTIONAL { ?r1 ut:data ?resultfile2 }
       OPTIONAL { ?r1 ut:graphData [ ut:graph ?resultgraphfiles1; rdfs:label ?resultgraphlabels ] }
       OPTIONAL { ?r1 ut:result ?updateresult }

       BIND(strbefore(strafter(str(<>), \"~A/\"), \"/\") AS ?suite)
       BIND(strbefore(strafter(strafter(str(<>), \"~A/\"), \"/\"),\"/\") AS ?collection)
       BIND(strafter(str(?test), \"#\") AS ?name)
       BIND(str(<>) AS ?base)

       BIND(strafter(str(?queryfile1), ?base) AS ?queryfile)
       BIND(strafter(str(?datafile1),?base) AS ?datafile)
       BIND(strafter(str(?graphfiles1),?base) AS ?graphfiles)
       BIND(strafter(str(?resultfile1),?base) AS ?resultfile)
       BIND(strafter(str(?resultgraphfiles1),?base) AS ?resultgraphfiles)
};

# @CSVResultFormatTest
SELECT ?base ?type ?suite ?collection ?name ?queryfile ?datafile ?graphfiles ?graphlabels ?resultfile ?resultgraphfiles ?resultgraphlabels ?updateresult ?queryserviceendpoint ?queryservicedata ?entailmentprofile ?entailmentregime WHERE {
       ?test rdf:type ?type
       FILTER(?type = mf:CSVResultFormatTest)

       ?test mf:action ?a .
       ?a qt:query ?queryfile1
       OPTIONAL { ?a qt:data ?datafile1 }
       OPTIONAL { ?test mf:result ?resultfile1 FILTER(!isblank(?resultfile1)) }

       BIND(strbefore(strafter(str(<>), \"~A/\"), \"/\") AS ?suite)
       BIND(strbefore(strafter(strafter(str(<>), \"~A/\"), \"/\"),\"/\") AS ?collection)
       BIND(strafter(str(?test), \"#\") AS ?name)
       BIND(str(<>) AS ?base)
       BIND(strafter(str(?queryfile1), ?base) AS ?queryfile)
       BIND(strafter(str(?datafile1),?base) AS ?datafile)
       BIND(strafter(str(?resultfile1),?base) AS ?resultfile)
};

# @ProtocolTest
SELECT ?base ?type ?suite ?collection ?name ?queryfile ?datafile ?graphfiles ?graphlabels ?resultfile ?resultgraphfiles ?resultgraphlabels ?updateresult ?queryserviceendpoint ?queryservicedata ?entailmentprofile ?entailmentregime WHERE {
       ?test rdf:type ?type
       FILTER(?type = mf:ProtocolTest)
       BIND(strbefore(strafter(str(<>), \"~A/\"), \"/\") AS ?suite)
       BIND(strbefore(strafter(strafter(str(<>), \"~A/\"), \"/\"),\"/\") AS ?collection)
       BIND(strafter(str(?test), \"#\") AS ?name)
       BIND(str(<>) AS ?base)
};

# @ServiceDescriptionTest
SELECT ?base ?type ?suite ?collection ?name ?queryfile ?datafile ?graphfiles ?graphlabels ?resultfile ?resultgraphfiles ?resultgraphlabels ?updateresult ?queryserviceendpoint ?queryservicedata ?entailmentprofile ?entailmentregime WHERE {
       ?test rdf:type ?type
       FILTER(?type = mf:ServiceDescriptionTest)
       BIND(strbefore(strafter(str(<>), \"~A/\"), \"/\") AS ?suite)
       BIND(strbefore(strafter(strafter(str(<>), \"~A/\"), \"/\"),\"/\") AS ?collection)
       BIND(strafter(str(?test), \"#\") AS ?name)
       BIND(str(<>) AS ?base)
};
" su su su su su su su su su su su su))))
  (inform "~A" (sparql-test-set-collector-script test-set)))

(defgeneric sparql-test-set-convert-manifests-to-csv-file (sparql-test-set)
  (:method ((this sparql-test-set))
    (let ((collected-tests-csv-file (sparql-test-set-collected-tests-csv-file this))
	  (collector-rules-file (sparql-test-set-collector-rules-file this)))
      (when (sparql-test-set-verbose-p this)
	(inform "Converting manifests to a csv file ~A~%" collected-tests-csv-file))
      (when (sparql-test-set-verbose-p this)
	(inform "  Writing rules to file ~A~%" collector-rules-file))
      (with-open-file (query collector-rules-file :direction :output :if-exists :supersede)
	(format query "~A~%" *collect-sparql-test-set-script*))
      (loop for manifest in (sparql-test-set-manifests this)
	    for firstp = t then nil
	    for cmd = (format nil "-b ~A --prefix-encoding=true --print-prefix-encodings=false ~A=~A --rules=~A --input-blocks=~A"
			      manifest (if firstp "--select-output" "--select-output-append") collected-tests-csv-file collector-rules-file manifest)
	    when (sparql-test-set-verbose-p this) do (inform "  instans ~A" cmd)
	    do (main cmd)))
    this))

(defgeneric sparql-test-set-collect-tests (sparql-test-set)
  (:method ((this sparql-test-set))
    (sparql-test-set-convert-manifests-to-csv-file this)
    (when (sparql-test-set-verbose-p this)
      (inform "~%Create tests from file ~A~%" (sparql-test-set-collected-tests-csv-file this)))
    (let ((lines nil)
	  (end nil))
      (csv-parser:map-csv-file (sparql-test-set-collected-tests-csv-file this)
			       #'(lambda (line)
				   (cond ((null lines)
					  (setf lines (list line))
					  (setf end lines))
					 (t
					  (setf (cdr end) (list line))
					  (setf end (cdr end))))))
      ;;    lines
					;	(inform "lines = ~A" lines)
      (let* ((headers ;(informing "headers = ~A"
	      (mapcar #'(lambda (h) (intern-keyword (string-upcase h))) (pop lines)))
					;)
	     (test-lines1 ;(informing "test-lines1 ~A"
	      (loop for line in lines
		    collect (loop for item in line
				  for name in headers
				  collect (list name (if (equal item "UNBOUND") nil item)))))
					;)
	     (test-lines2 (loop for line in test-lines1
				for key = (list (cdr (assoc :type line))
						(cdr (assoc :suite line))
						(cdr (assoc :collection line))
						(cdr (assoc :name line)))
				do (progn
				     (unless (second (assoc :graphfiles line))
				       (setf (rest (assoc :graphfiles line)) nil))
				     (unless (second (assoc :graphlabels line))
				       (setf (rest (assoc :graphlabels line)) nil))
				     (unless (second (assoc :resultgraphfiles line))
				       (setf (rest (assoc :resultgraphfiles line)) nil))
				     (unless (second (assoc :resultgraphlabels line))
				       (setf (rest (assoc :resultgraphlabels line)) nil))
				     (push (list :resultgraphs (and (rest (assoc :resultgraphfiles line))
								    (list (second (assoc :resultgraphfiles line)) (second (assoc :resultgraphlabels line)))))
					   line)
				     (push (list :graphs (and (second (assoc :graphfiles line))
							      (list (second (assoc :graphfiles line)) (second (assoc :graphlabels line)))))
					   line)
				     (push (list :key key) line))
				collect line))
	     (test-alist nil))
	;; (inform "~D lines in test-lines2" (length test-lines2))
	(loop for line in test-lines2
	      for key = (assoc :key line)
	      for test-item = (assoc key test-alist :test #'equal)
	      do (cond ((null test-item)
			(push line test-alist))
		       (t
			;; (inform "Merging~%~Swith~%~S" line test-item)
			(let ((graphs-item (assoc :graphs test-item))
			      (new-graphs (second (assoc :graphs line))))
			  ;; (inform "graphs-item =~S" graphs-item)
			  ;; (inform "new-graphs =~S" new-graphs)
			  (unless (member new-graphs (cdr graphs-item) :test #'equal)
			    (push new-graphs (cdr graphs-item))
			    (push (second (assoc :graphfiles line)) (cdr (assoc :graphfiles test-item)))
			    (push (second (assoc :graphlabels line)) (cdr (assoc :graphlabels test-item)))))
			(let ((resultgraphs-item (assoc :resultgraphs test-item))
			      (new-resultgraphs (second (assoc :resultgraphs line))))
			  (unless (member new-resultgraphs (cdr resultgraphs-item) :test #'equal)
			    (push new-resultgraphs (cdr resultgraphs-item))
			    (push (second (assoc :resultgraphfiles line)) (cdr (assoc :resultgraphfiles test-item)))
			    (push (second (assoc :resultgraphlabels line)) (cdr (assoc :resultgraphlabels test-item)))))
			(flet ((maybe-replace-value (key)
				 (let ((item (assoc key test-item))
				       (new-value (second (assoc key line))))
				   (cond ((null (second item))
					  (setf (second item) new-value))
					 ((not (or (null new-value) (equal new-value (second item))))
					  (error* "Incompatible values ~S and ~S in~%  ~S and ~%~S" item new-value test-item line))))))
			  (maybe-replace-value :queryfile)
			  (maybe-replace-value :datafile)
			  (maybe-replace-value :updateresult)
			  (maybe-replace-value :queryserviceendpoint)
			  (maybe-replace-value :queryservicedata)
			  (maybe-replace-value :entailmentregime))
			;; (inform "Result~%~S" test-item)
			)))
	;;       test-alist
	;; )))
	;; (inform "~D lines in test-alist" (length test-alist))
	(setf test-alist (nreverse test-alist))
	(let ((tests (loop for test-item in test-alist
			   for type-string = (second (assoc :type test-item))
			   for type = (cond ((string= type-string "mf:PositiveSyntaxTest") 'sparql-positive-syntax-test)
					    ((string= type-string "mf:PositiveSyntaxTest11") 'sparql-positive-syntax-test-11)
					    ((string= type-string "mf:PositiveUpdateSyntaxTest11") 'sparql-positive-update-syntax-test-11)
					    ((string= type-string "mf:NegativeSyntaxTest") 'sparql-negative-syntax-test)
					    ((string= type-string "mf:NegativeSyntaxTest11") 'sparql-negative-syntax-test-11)
					    ((string= type-string "mf:NegativeUpdateSyntaxTest11") 'sparql-negative-update-syntax-test-11)
					    ((string= type-string "mf:QueryEvaluationTest") 'sparql-query-evaluation-test)
					    ((string= type-string "mf:UpdateEvaluationTest") 'sparql-update-evaluation-test)
					    ((string= type-string "mf:CSVResultFormatTest") 'sparql-csv-result-format-test)
					    ((string= type-string "mf:ProtocolTest") 'sparql-protocol-test)
					    ((string= type-string "mf:ServiceDescriptionTest") 'sparql-service-description-test)
					    (t (error* "Unknown test type ~A" type-string)))
			   for test = (apply #'make-instance type
					     (mapcan #'(lambda (key)
							 (list key
							       (if (member key '(:graphfiles :graphlabels :resultgraphfiles :resultgraphlabels))
								   (cdr (assoc key test-item))
								   (second (assoc key test-item)))))
						     headers))
					;	    do (describe test)
			   do (setf (sparql-test-test-set test) this)
			   collect test)))
	  (setf (sparql-test-set-headers this) headers)
	  (setf (sparql-test-set-tests this) tests)
	  (when (sparql-test-set-verbose-p this)
	    (inform "~%In total ~D tests~%" (length tests))))))
    this))

(defgeneric sparql-test-set-execute-tests (sparql-test-set &key verbosep forcep target-phase)
  (:method ((this sparql-test-set) &key verbosep forcep (target-phase :completed))
    (loop for test in (sparql-test-set-tests this)
	  do (sparql-test-execute test :verbosep verbosep :forcep forcep :target-phase target-phase))))

(defun init-sparql-test-set ()
  (setf *sparql-test-set* (make-instance 'sparql-test-set)))

(defun sparql-test-set-results-to-csv (test-set)
  (let* ((tests (sparql-test-set-tests test-set))
	 (fields (mapcar #'first (sparql-test-results (first tests)))))
    (with-open-file (output (sparql-test-set-results-csv-file test-set) :direction :output :if-exists :supersede)
      (format output "~&~(~{~A~^,~}~)~%" fields)
      (loop for test in tests do (format output "~{~A~^,~}~%"
					 (loop for item in (sparql-test-results test)
					       for value = (second item)
					       collect (cond ((eq value T) "True")
							     ((eq value nil) "False")
							     ((eq value :unbound) "")
							     ((and (stringp value) (eql 0 (search "mf:" value))) (subseq value 3))
							     (t value))))))))

(defun run-sparql-test-suites (suites-dir)
  (let ((test-set (make-instance 'sparql-test-set :root-directory suites-dir)))
    (setf *sparql-test-set* test-set)
    (sparql-test-set-execute-tests test-set :verbosep t)
    (sparql-test-set-results-to-csv test-set)))

(defun run-suite-collection-name (test-set &optional suite collection name)
  (let ((needs-reinitialization-p (not (null test-set))))
    (unless test-set (setf test-set (make-instance 'sparql-test-set)))
    (setf *sparql-test-set* test-set)
    (loop for test in (sparql-test-set-tests test-set)
	  do (when (and (or (null suite) (equal suite (sparql-test-suite test)))
			(or (null collection) (equal collection (sparql-test-collection test)))
			(or (null name) (equal name (sparql-test-name test))))
	       (setf *current-sparql-test* test)
	       (when needs-reinitialization-p (reinitialize-instance test))
	       (sparql-test-execute test :verbosep t)))))
