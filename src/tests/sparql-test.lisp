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
  ((queryfile :accessor sparql-test-queryfile :initarg :queryfile)
   (parsing-succeeded-p :accessor sparql-test-parsing-succeeded-p)
   (translation-succeeded-p :accessor sparql-test-translation-succeeded-p)
   (initialization-succeeded-p :accessor sparql-test-initialization-succeeded-p)
   (rule-type :accessor sparql-test-rule-type)))

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
  ((datafile :accessor sparql-test-datafile :initarg :datafile)
   (graphfiles :accessor sparql-test-graphfiles :initarg :graphfiles)
   (resultfile :accessor sparql-test-resultfile :initarg :resultfile)
   (running-succeeded-p :accessor sparql-test-running-succeeded-p )
   (comparison-succeeded-p :accessor sparql-test-comparison-succeeded-p)))

(defmethod initialize-instance :after ((this sparql-query-evaluation-test) &key datafile resultfile &allow-other-keys)
;  (inform "~A: datafile = ~A" this datafile)
  (setf (sparql-test-datafile this) (expand-sparql-test-filename* this datafile))
;  (inform "~A: datafile now = ~A" this (sparql-test-datafile this))
  (setf (sparql-test-graphfiles this)
	(loop for file in (sparql-test-graphfiles this)
	      collect (expand-sparql-test-filename this file)))
  (setf (sparql-test-resultfile this) (expand-sparql-test-filename* this resultfile)))

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
  (when (slot-boundp this 'comparison-succeeded-p)
    (format stream "~%  :comparison-succeeded-p ~A" (sparql-test-comparison-succeeded-p this))))

(defmethod sparql-test-failed-p ((this sparql-query-evaluation-test))
  (or (call-next-method)
      (not (and (sparql-test-running-succeeded-p this)
		(sparql-test-comparison-succeeded-p this)))))

(define-class sparql-update-evaluation-test (sparql-query-evaluation-test)
  ((graphlabels :accessor sparql-test-graphlabels :initarg :graphlabels)
   (resultgraphfiles :accessor sparql-test-resultgraphfiles :initarg :resultgraphfiles)
   (resultgraphlabels :accessor sparql-test-resultgraphlabels :initarg :resultgraphlabels)
   (updateresult :accessor sparql-test-updateresult :initarg :updateresult)))

(defmethod initialize-instance :after ((this sparql-update-evaluation-test) &key &allow-other-keys)
  (setf (sparql-test-resultgraphfiles this)
	(loop for file in (sparql-test-resultgraphfiles this)
	      collect (expand-sparql-test-filename this file))))

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
  ((entries :accessor sparql-tests-entries :initarg :entries)
   (fields :accessor sparql-tests-fields :initarg :fields)))

(defun test-from-csv-file (csv-file)
  (let ((lines nil)
	(end nil))
    (csv-parser:map-csv-file csv-file
			     #'(lambda (line)
				 (cond ((null lines)
					(setf lines (list line))
					(setf end lines))
				       (t
					(setf (cdr end) (list line))
					(setf end (cdr end))))))
;;    lines
    (let* ((headers (mapcar #'(lambda (h) (intern-keyword (string-upcase h))) (pop lines)))
	   (test-lines1 (loop for line in lines
			      collect (loop for item in line
					    for name in headers
					    collect (list name (if (equal item "UNBOUND") nil item)))))
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
      (loop for test-item in test-alist
	    for type-string = (second (assoc :type test-item))
            for type = (cond ((string= type-string "mf:PositiveSyntaxTest") 'sparql-positive-syntax-test)
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
			     (t (error* "Unknown test type ~A" type-string)))
	    for test = (apply #'make-instance type (mapcan #'(lambda (key)
							       (list key
								     (if (member key '(:graphfiles :graphlabels :resultgraphfiles :resultgraphlabels)) (cdr (assoc key test-item)) (second (assoc key test-item)))))
							   headers))
	    do (describe test)
	    collect test)
      )))

(defun parse-tests-from-csv (&key (csv-file "/Users/enu/instans/tests/sparql-tests/sparql-tests.csv"))
  (let ((key-field-names '("type" "suite" "collection" "name"))
	(list-field-names '("graphfiles" "graphlabels" "resultgraphfiles" "resultgraphlabels")))
    (multiple-value-bind (alist headers); linecount entrycount)
	(csv-file-to-alist csv-file
			   :key-fields key-field-names
			   :data-field-merge #'(lambda (field-key new-value old-value-or-values)
						 (cond ((member field-key list-field-names :test #'equal)
							(cond ((or (null new-value) (equal new-value "UNBOUND")) old-value-or-values)
							      (t (append old-value-or-values (list new-value)))))
						       ((or (null new-value) (equal new-value "UNBOUND")) old-value-or-values)
						       ((or (null old-value-or-values) (equal old-value-or-values "UNBOUND")) new-value)
						       ((equal new-value old-value-or-values) new-value)
						       (t (error* "Two different values for non-list field ~A: ~A vs. ~A" field-key new-value old-value-or-values)))))
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
	(let ((header-keywords (mapcar #'(lambda (h) (intern-keyword (string-upcase h))) headers)))
	  (make-instance 'sparql-tests2
			 :fields header-keywords
			 :entries (loop for entry in alist
					for key = (first entry)
					for values = (second entry)
					for type-class-name = (test-type-class-name (first key))
					for args = (append (list type-class-name :type (first key) :suite (second key) :collection (third key) :name (fourth key))
							   (loop for keyword in (nthcdr 4 header-keywords)
								 for value in values
								 nconc (list keyword value)))
					for test = (apply #'make-instance args)
					when (equal (fourth key) "add01")
					do (progn (inform "entry = ~S~%args = ~S" entry args) (describe test))
					collect test)))))))

;; (add-sparql-test tests
;; 							     :type type :negative (parse-xsd-boolean negative) :suite suite :collection collection :name name
;; 							     :queryfile queryfile :datafile datafile :graphdatafile graphdatafile
;; 							     :resultfile resultfile :resultgraphfile resultgraphfile :resultgraphlabel resultgraphlabel)

;; (defun parse-tests-from-csv (&key (csv-file "/Users/enu/instans/tests/sparql-tests/sparql-tests.csv"))
;;   (let ((key-field-names '("type" "suite" "collection" "name"))
;; 	(list-field-names '("graphfiles" "graphlabels" "resultgraphfiles" "resultgraphlabels")))
;;     (multiple-value-bind (alist headers); linecount entrycount)
;; 	(csv-file-to-alist csv-file
;; 			   :key-fields key-field-names
;; 			   :data-field-merge #'(lambda (field-key new-value old-value-or-values)
;; 						 (cond ((member field-key list-field-names :test #'equal)
;; 							(cond ((or (null new-value) (equal new-value "UNBOUND")) old-value-or-values)
;; 							      (t (append old-value-or-values (list new-value)))))
;; 						       ((or (null new-value) (equal new-value "UNBOUND")) old-value-or-values)
;; 						       ((or (null old-value-or-values) (equal old-value-or-values "UNBOUND")) new-value)
;; 						       ((equal new-value old-value-or-values) new-value)
;; 						       (t (error* "Two different values for non-list field ~A: ~A vs. ~A" field-key new-value old-value-or-values)))))
;;     ;; (loop for entry in alist
;;     ;; 	  for key = (first entry)
;;     ;; 	  for values = (second entry)
;;     ;; 	  for name-value-list = (mapcar #'list (nthcdr 4 headers) values)
;;     ;; 	  do (assert (null (cddr entry)))
;;     ;; 	  do (inform "~A ~{~A~^/~}:~{~%  ~{:~A~^ ~A~}~}" (first key) (rest key) name-value-list))
;;     ;; 					;    alist
;;       (flet ((test-type-class-name (type-string)
;; 	       (cond ((string= type-string "mf:PositiveSyntaxTest") 'sparql-positive-syntax-test)
;; 		     ((string= type-string "mf:NegativeSyntaxTest") 'sparql-negative-syntax-test)
;; 		     ((string= type-string "mf:PositiveSyntaxTest11") 'sparql-positive-syntax-test-11)
;; 		     ((string= type-string "mf:NegativeSyntaxTest11") 'sparql-negative-syntax-test-11)
;; 		     ((string= type-string "mf:PositiveUpdateSyntaxTest11") 'sparql-positive-update-syntax-test-11)
;; 		     ((string= type-string "mf:NegativeUpdateSyntaxTest11") 'sparql-negative-update-syntax-test-11)
;; 		     ((string= type-string "mf:QueryEvaluationTest") 'sparql-query-evaluation-test)
;; 		     ((string= type-string "mf:UpdateEvaluationTest") 'sparql-update-evaluation-test)
;; 		     ((string= type-string "mf:CSVResultFormatTest") 'sparql-csv-result-format-test)
;; 		     ((string= type-string "mf:ProtocolTest") 'sparql-protocol-test)
;; 		     ((string= type-string "mf:ServiceDescriptionTest") 'sparql-service-description-test)
;; 		     (t (error* "Unknown test type ~A" type-string)))))
;; 	(let ((header-keywords (mapcar #'(lambda (h) (intern-keyword (string-upcase h))) headers)))
;; 	  (make-instance 'sparql-tests2
;; 			 :fields header-keywords
;; 			 :entries (loop for entry in alist
;; 					for key = (first entry)
;; 					for values = (second entry)
;; 					for type-class-name = (test-type-class-name (first key))
;; 					for args = (append (list type-class-name :type (first key) :suite (second key) :collection (third key) :name (fourth key))
;; 							   (loop for keyword in (nthcdr 4 header-keywords)
;; 								 for value in values
;; 								 nconc (list keyword value)))
;; 					for test = (apply #'make-instance args)
;; 					when (equal (fourth key) "add01")
;; 					do (progn (inform "entry = ~S~%args = ~S" entry args) (describe test))
;; 					collect test)))))))

;; (add-sparql-test tests
;; 							     :type type :negative (parse-xsd-boolean negative) :suite suite :collection collection :name name
;; 							     :queryfile queryfile :datafile datafile :graphdatafile graphdatafile
;; 							     :resultfile resultfile :resultgraphfile resultgraphfile :resultgraphlabel resultgraphlabel)
