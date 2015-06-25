;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class sparql-query-results ()
  ((variables :accessor sparql-query-results-variables :initarg :variables)
   (links :accessor sparql-query-results-links :initarg :links)
   (tail :accessor sparql-query-results-tail)
   (results :accessor sparql-query-results-results :initarg :results)
   (boolean :accessor sparql-query-results-boolean :initarg :boolean)))

(define-class sparql-abstract-result ()
  ((rule :accessor sparql-result-rule :initarg :rule :initform nil)))

(define-class sparql-result (sparql-abstract-result)
  ((bindings :accessor sparql-result-bindings :initarg :bindings)))

(define-class sparql-binding ()
  ((variable :accessor sparql-binding-variable :initarg :variable)
   (value :accessor sparql-binding-value :initarg :value)))

(defmethod print-object ((this sparql-binding) stream)
  (format stream "#<~A ~A=~A>" (type-of this) (sparql-binding-variable this) (sparql-binding-value this)))

(define-class sparql-link (sparql-abstract-result)
  ((href :accessor sparql-link-href :initarg :href)))

(define-class sparql-boolean-result (sparql-abstract-result)
  ((value :accessor sparql-boolean-result-value :initarg :value)))

(defun create-sparql-binding (var value)
  (make-instance 'sparql-binding :variable var :value value))

(defun create-sparql-result (bindings)
;  (inform "create-sparql-result ~{~A~^ ~}" bindings)
  (make-instance 'sparql-result :bindings bindings))

(defun create-sparql-boolean-result (value)
  (make-instance 'sparql-boolean-result :value value))

(defun create-sparql-link (href)
  (make-instance 'sparql-link :href href))

(defun find-result-binding (result variable)
  (find-if #'(lambda (b) (equal variable (uniquely-named-object-name (sparql-binding-variable b)))) (sparql-result-bindings result)))

(defgeneric set-query-variables (query-results variables)
  (:method ((this sparql-query-results) variables)
    (when (slot-boundp this 'variables)
      (unless (every #'uniquely-named-object-equal (sparql-query-results-variables this) variables)
	(error* "Trying to set different variables ~S to ~S; variables were ~S" variables this (sparql-query-results-variables this))))
    (setf (sparql-query-results-variables this) variables)))

(defgeneric add-sparql-result (query-results result)
  (:method ((this sparql-query-results) result)
    (cond ((and (slot-boundp this 'results) (sparql-query-results-results this))
	   (setf (cdr (sparql-query-results-tail this)) (list result))
	   (setf (sparql-query-results-tail this) (cdr (sparql-query-results-tail this))))
	  (t
	   (setf (sparql-query-results-results this) (list result))
	   (setf (sparql-query-results-tail this) (sparql-query-results-results this))))))

(defgeneric add-sparql-result-values (query-results values)
  (:method ((this sparql-query-results) values)
    (let ((result (loop for var in (sparql-query-results-variables this)
		        for value in values
		        collect (create-sparql-binding var value) into bindings
		        finally (return (create-sparql-result bindings)))))
      (add-sparql-result this result))))

(defgeneric set-sparql-result-boolean (query-result value)
  (:method ((this sparql-query-results) value)
    (setf (sparql-query-results-boolean this) (create-sparql-boolean-result value))))

(defgeneric sparql-results-compare (query-results1 query-results2 &key order-dependent-p verbosep output-stream result-label1 result-label2 handle-error-values-p)
  (:method ((query-results1 sparql-query-results) (query-results2 sparql-query-results) &key order-dependent-p verbosep (output-stream *error-output*) (result-label1 "") (result-label2 "") (handle-error-values-p t))
    (format output-stream "~&sparql-results-compare ~A ~A~%" result-label1 result-label2)
    (cond ((and (slot-boundp query-results1 'boolean) (slot-boundp query-results2 'boolean))
	   (cond ((eq (sparql-boolean-result-value (sparql-query-results-boolean query-results1))
		      (sparql-boolean-result-value (sparql-query-results-boolean query-results2)))
		  (when verbosep (format output-stream "~%Compare SPARQL results: Equal solutions in ~A and ~A" result-label1 result-label2))
		  (values t t))
		 (t
		  (when verbosep (format output-stream "~%Compare SPARQL resultsNot equal solutions in ~A and ~A" result-label1 result-label2))
		  (values nil nil))))
	  ((and (slot-boundp query-results1 'results) (slot-boundp query-results2 'results))
	   (flet ((sparql-result-equal-extended (r1 r2)
		    (sparql-result-equal r1 r2 :handle-error-values-p handle-error-values-p)))
	     (let ((result-list1 (sparql-query-results-results query-results1))
		   (result-list2 (sparql-query-results-results query-results2)))
	       (cond ((and (= (length result-list1) (length result-list2))
			   (every #'(lambda (r1 r2) (sparql-result-equal-extended r1 r2)) result-list1 result-list2))
		      (when verbosep (format output-stream "~%Compare SPARQL results: Equal solutions in ~A and ~A" result-label1 result-label2))
		      (values t t))
		     ;; ((not (= (length result-list1) (length result-list2)))
		     ;;  (when verbosep (format output-stream "~%Compare SPARQL results: Different number of solutions in ~A (~D) and ~A (~D)" result-label1 (length result-list1) result-label2 (length result-list2)))
		     ;;  (values nil nil))
		     (verbosep
		      (flet ((show-solutions (sl) (loop for s in sl
							do (loop for b in (sparql-result-bindings s)
								 do (format output-stream "~%  ~A -> ~A~%"
									    (uniquely-named-object-name (sparql-binding-variable b))
									    (sparql-value-to-string (sparql-binding-value b)))))))
			(let ((result1-minus-result2 (set-difference result-list1 result-list2 :test #'sparql-result-equal-extended))
			      (result2-minus-result1 (set-difference result-list2 result-list1 :test #'sparql-result-equal-extended)))
			  (cond ((and (null result1-minus-result2) (null result2-minus-result1))
				 (cond ((not order-dependent-p)
					(format output-stream "~%Compare SPARQL results: Equal solutions* in ~A and ~A~%" result-label1 result-label2)
					(values t t))
				       (t
					(when verbosep
					  (format output-stream "~%Compare SPARQL results: Different order of solutions in ~A and ~A~%" result-label1 result-label2)
					  (format output-stream "~%~A:" result-label1)
					  (show-solutions result-list1)
					  (format output-stream "~%~A:" result-label2)
					  (show-solutions result-list2))
					(values t nil))))
				(t
				 (format output-stream "~%Compare SPARQL results: Not equal solutions in ~A (~D) and ~A (~D)~%"
					 result-label1 (length result-list1) result-label2 (length result-list2))
				 (when result2-minus-result1
				   (format output-stream "~%  Solutions not in ~A:" result-label1)
				   (show-solutions result2-minus-result1))
				 (when result1-minus-result2
				   (format output-stream "~%  Solutions not in ~A:" result-label2)
				   (show-solutions result1-minus-result2))
				 (values nil nil))))))
		     (t (values nil nil))))))
	  (t
	   (when verbosep (format output-stream "~%Cannot compare ~A and ~A" result-label1 result-label2))
	   (values nil nil)))))

(defgeneric print-sparql-results (sparql-query-results &key stream)
  (:method ((this sparql-query-results) &key (stream *standard-output*))
    (when (slot-boundp this 'variables)
      (format stream "Variables: ~{~A~^ ~}~%" (mapcar #'(lambda (var) (subseq (uniquely-named-object-pretty-name var) 1)) (sparql-query-results-variables this))))
    (when (slot-boundp this 'links)
      (format stream "Links: ~{~A~^ ~}~%" (mapcar #'(lambda (link) link) (sparql-query-results-links this))))
    (when (slot-boundp this 'boolean)
      (format stream "Boolean: ~A~%" (sparql-boolean-result-value (sparql-query-results-boolean this))))
    (when (slot-boundp this 'results)
      (format stream "Results:~%")
      (loop for result in (sparql-query-results-results this)
      	    do (loop for binding in (sparql-result-bindings result)
      	    	     do (format stream "  ~A=~A" (sparql-binding-variable binding) (sparql-binding-value binding)))
	    do (format stream "~%")))))

(defgeneric sparql-result-equal (r1 r2 &key handle-error-values-p)
  (:method ((r1 sparql-result) (r2 sparql-result) &key handle-error-values-p)
    (let ((bl1 (sparql-result-bindings r1))
	  (bl2 (sparql-result-bindings r2)))
      (and (every #'(lambda (b1) (or (sparql-unbound-p (sparql-binding-value b1))
				     (find b1 bl2 :test #'(lambda (x y) (sparql-binding-equal x y :handle-error-values-p handle-error-values-p))))) bl1)
	   (every #'(lambda (b2) (or (sparql-unbound-p (sparql-binding-value b2))
				     (find b2 bl1 :test #'(lambda (x y) (sparql-binding-equal x y :handle-error-values-p handle-error-values-p))))) bl2)))))

(defgeneric sparql-binding-equal (b1 b2 &key handle-error-values-p)
  (:method ((b1 sparql-binding) (b2 sparql-binding) &key handle-error-values-p)
    (and (sparql-var-equal (sparql-binding-variable b1) (sparql-binding-variable b2))
	 (or (and handle-error-values-p (sparql-error-p (sparql-binding-value b1)) (sparql-error-p (sparql-binding-value b2)))
	     (sparql-value-same-term (sparql-binding-value b1) (sparql-binding-value b2))))))

(defun sparql-compare-srx-result-files (file1 file2 &key output-stream)
  (declare (ignorable output-stream))
  (handler-case 
      (let* ((i (make-instance 'instans))
	     (res1 (parse-results-file i file1))
	     (res2 (parse-results-file i file2)))
	(sparql-results-compare res1 res2 :verbosep t :result-label1 file1 :result-label2 file2 :output-stream output-stream))
    (t (e) (values nil nil (format nil "~A" e)))))

(defun sparql-compare-ttl-result-files (file1 file2 &key output-stream verbosep)
  (declare (ignorable output-stream))
  (let* ((graph1 nil)
	 (res1 (parse-rdf-file file1 :document-callback #'(lambda (statements) (setf graph1 statements))))
	 (graph2 nil)
	 (res2 (parse-rdf-file file2 :document-callback #'(lambda (statements) (setf graph2 statements)))))
    (cond ((ll-parser-failed-p res1)
	   (inform "Parsing of ~A failed: ~A" file1 (ll-parser-error-messages res1))
	   nil)
	  ((ll-parser-failed-p res2)
	   (inform "Parsing of ~A failed: ~A" file2 (ll-parser-error-messages res2))
	   nil)
	  (t
	   (rdf-graphs-isomorphic-p graph1 graph2 :verbosep verbosep)))))

