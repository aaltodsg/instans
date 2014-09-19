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
  (make-instance 'sparql-result :bindings bindings))

(defun create-sparql-boolean-result (value)
  (make-instance 'sparql-boolean-result :value value))

(defun create-sparql-link (href)
  (make-instance 'sparql-link :href href))

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

(defgeneric output-srx (query-results stream)
  (:method ((this sparql-query-results) stream)
    (xml-emitter:with-xml-output (stream)
      (xml-emitter:with-tag ("sparql" '(("xmlns" "http://www.w3.org/2005/sparql-results#")))
	(xml-emitter:with-tag ("head")
	  (when (slot-boundp this 'variables)
	    (loop for variable in (sparql-query-results-variables this)
		  do (xml-emitter:with-simple-tag ("variable" `(("name" ,(format nil "~(~A~)" (subseq (uniquely-named-object-name variable) 1)))))))
	    (when (slot-boundp this 'links)
	      (loop for link in (sparql-query-results-links this)
		    do (xml-emitter:with-simple-tag ("link" `(("href" ,(sparql-link-href link )))))))))
	(cond ((slot-boundp this 'boolean)
	       (xml-emitter:simple-tag "boolean" (if (sparql-boolean-result-value (sparql-query-results-boolean this)) "true" "false")))
	      ((slot-boundp this 'results)
	       (xml-emitter:with-tag ("results")
		 (loop for result in (sparql-query-results-results this)
		       for bindings = (sparql-result-bindings result)
		       do (xml-emitter:with-tag ("result")
			    (loop for variable in (sparql-query-results-variables this)
				  for binding = (find-if #'(lambda (b) (sparql-var-equal variable (sparql-binding-variable b))) bindings)
				  when binding
				  do (let ((value (sparql-binding-value binding)))
				       (xml-emitter:with-tag ("binding" `(("name" ,(format nil "~(~A~)" (subseq (uniquely-named-object-name variable) 1)))))
					 (cond ((sparql-error-p value)
;						(inform "outputting ~S" value)
						(xml-emitter:simple-tag "literal" "SPARQL-ERROR"))
					       ((rdf-iri-p value)
						;(inform "writing uri ~S~%" (rdf-iri-string value))
						(xml-emitter:simple-tag "uri" (rdf-iri-string value)))
					       ((rdf-literal-p value)
						;(inform "about to write ~S" value)
						(cond ((rdf-literal-lang value)
						       ;(inform "writing literal with langtag ~A@~A~%" (rdf-literal-string value) (rdf-literal-lang value))
						       (xml-emitter:with-tag ("literal" (list (list "xml:lang" (rdf-literal-lang value))))
							 (xml-emitter:xml-as-is (rdf-literal-string value))))
						      ((rdf-literal-type value)
						       ;(inform "writing literal with type ~A^^~A~%" (rdf-literal-string value) (rdf-literal-type value))
						       (xml-emitter:with-tag ("literal" (list (list "datatype" (rdf-iri-string (rdf-literal-type value)))))
							 (xml-emitter:xml-as-is (rdf-literal-string value))))
						      (t
						       ;(inform "writing plain literal ~A~%" (rdf-literal-string value))
						       (xml-emitter:simple-tag "literal" (rdf-literal-string value)))))
					       ((rdf-blank-node-p value)
						;(inform "writing blank ~A~%" (uniquely-named-object-name value))
						(xml-emitter:with-simple-tag ("bnode") (xml-emitter:xml-as-is (uniquely-named-object-name value))))
					       ((typep value 'datetime)
						(xml-emitter:with-tag ("literal" (list (list "datatype" *xsd-datetime-iri-string*)))
						  (xml-emitter:xml-as-is (datetime-canonic-string value))))
					       ((typep value 'double-float)
						(xml-emitter:with-simple-tag ("literal")
						  (xml-emitter:xml-as-is (substitute #\e #\d (format nil "~A" value)))))
					       (t
						;(inform "writing literal value ~A~%" (sparql-value-to-string value))
						(xml-emitter:with-simple-tag ("literal") (xml-emitter:xml-as-is value))))))))))))))))

(defgeneric sparql-results-compare (query-results1 query-results2 &key order-dependent-p verbosep output-stream result-label1 result-label2 handle-error-values-p)
  (:method ((query-results1 sparql-query-results) (query-results2 sparql-query-results) &key order-dependent-p verbosep (output-stream *standard-output*) (result-label1 "") (result-label2 "") (handle-error-values-p t))
    (inform "sparql-results-compare ~A ~A~%" result-label1 result-label2)
    (cond ((and (slot-boundp query-results1 'boolean) (slot-boundp query-results2 'boolean))
	   (cond ((eq (sparql-boolean-result-value (sparql-query-results-boolean query-results1))
		      (sparql-boolean-result-value (sparql-query-results-boolean query-results2)))
		  (when verbosep (format output-stream "~%Compare SPARQL results: Equal solutions in ~A and ~A" result-label1 result-label2))
		  (values t t))
		 (t
		  (when verbosep (format output-stream "~%Compare SPARQL resultsNot equal solutions in ~A and ~A" result-label1 result-label2))
		  (values nil nil))))
	  ;; ((and (slot-boundp query-results1 'triples) (slot-boundp query-results2 'triples))
	  ;;  (let ((triple-list1 (sparql-query-results-triples query-results1))
	  ;; 	 (triple-list2 (sparql-query-results-triples query-results2)))
	  ;;    (cond ((and (= (length triple-list1) (length triple-list2))
	  ;; 		 (every #'(lambda (tr1 tr2) (equal-triples tr1 tr2)) triple-list1 triple-list2))
	  ;; 	    (when verbosep (format output-stream "~%~:(~A~) and ~(~A~) are equal" result-label1 result-label2))
	  ;; 	    (values t t))
	  ;; 	   ((not (= (length triple-list1) (length triple-list2)))
	  ;; 	    (when verbosep (format output-stream "~%~D ~:(~A~) triples, ~(~A~) ~D triples" result-label1 (length triple-list1) result-label2 (length triple-list2)))
	  ;; 	    (values nil nil))
	  ;; 	   (t
	  ;; 	    (flet ((show-triples (triples) (loop for triple in triples do (format output-stream "~%Triple: [~{~A~^ ~}]" triple))))
	  ;; 	      (let ((observed-minus-expected (set-difference triple-list1 triple-list2 :test #'equal-triples))
	  ;; 		    (expected-minus-observed (set-difference triple-list2 triple-list1 :test #'equal-triples)))
	  ;; 		(cond ((and (null observed-minus-expected) (null expected-minus-observed))
	  ;; 		       (when verbosep
	  ;; 			 (format output-stream "~%~:(~A~) triples and ~(~A~) triples are same, but in a different order~%" result-label1 result-label2)
	  ;; 			 (format output-stream "~%~:(~A~):" result-label1)
	  ;; 			 (show-triples triple-list1)
	  ;; 			 (format output-stream "~%~:(~A~):" result-label2)
	  ;; 			 (show-triples triple-list2)
	  ;; 			 (values t nil)))
	  ;; 		      (t
	  ;; 		       (format output-stream "~%~:(~A~) triples not in ~(~A~):" result-label1 result-label2)
	  ;; 		       (show-triples expected-minus-observed)
	  ;; 		       (format output-stream "~%~:(~A~) triples not ~(~A~):" result-label2 result-label1)
	  ;; 		       (show-triples observed-minus-expected)
	  ;; 		       (values nil nil)))))))))
	  ((and (slot-boundp query-results1 'results) (slot-boundp query-results2 'results))
	   (flet ((sparql-result-equal-extended (r1 r2)
		    (sparql-result-equal r1 r2 :handle-error-values-p handle-error-values-p)))
	     (let ((result-list1 (sparql-query-results-results query-results1))
		   (result-list2 (sparql-query-results-results query-results2)))
	       (cond ((and (= (length result-list1) (length result-list2))
			   (every #'(lambda (r1 r2) (sparql-result-equal-extended r1 r2)) result-list1 result-list2))
		      (when verbosep (format output-stream "~%Compare SPARQL results: Equal solutions in ~A and ~A" result-label1 result-label2))
		      (values t t))
		     ((not (= (length result-list1) (length result-list2)))
		      (when verbosep (format output-stream "~%Compare SPARQL results: Different number of solutions in ~A (~D) and ~A (~D)" result-label1 (length result-list1) result-label2 (length result-list2)))
		      (values nil nil))
		     (verbosep
		      (flet ((show-solutions (sl) (loop for s in sl
							do (loop for b in (sparql-result-bindings s)
								 do (format output-stream "~%  ~A=~A~%" (sparql-binding-variable b) (sparql-binding-value b))))))
			(let ((observed-minus-expected (set-difference result-list1 result-list2 :test #'sparql-result-equal-extended))
			      (expected-minus-observed (set-difference result-list2 result-list1 :test #'sparql-result-equal-extended)))
			  (cond ((and (null observed-minus-expected) (null expected-minus-observed))
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
				 (format output-stream "~%Compare SPARQL results: Not equal solutions in ~A and ~A~%" result-label1 result-label2)
				 (format output-stream "~%  Solutions not in ~A:" result-label2)
				 (show-solutions expected-minus-observed)
				 (format output-stream "~%  Solutions not in ~A:" result-label1)
				 (show-solutions observed-minus-expected)
				 (values nil nil))))))
		     (t (values nil nil))))))
	  (t
	   (when verbosep (format output-stream "~%Cannot compare ~A and ~A" result-label1 result-label2))
	   (values nil nil)))))

(defgeneric print-sparql-results (sparql-query-results &key stream)
  (:method ((this sparql-query-results) &key (stream *standard-output*))
    (when (slot-boundp this 'variables)
      (format stream "Variables: ~{~A~^ ~}~%" (mapcar #'(lambda (var) (subseq (uniquely-named-object-name var) 1)) (sparql-query-results-variables this))))
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

(defun sparql-compare-srx-files (file1 file2)
  (let* ((i (make-instance 'instans))
	 (res1 (parse-results-file i file1))
	 (res2 (parse-results-file i file2)))
    (sparql-results-compare res1 res2 :verbosep t :result-label1 file1 :result-label2 file2)))

(defun test-read-write-srx (in-file out-file)
  (let* ((i (make-instance 'instans))
	 (res1 (parse-results-file i in-file)))
    (with-open-file (stream out-file :direction :output :if-exists :supersede)
      (output-srx res1 stream))
    (let ((res2 (parse-results-file i out-file)))
      (unless (sparql-results-compare res1 res2)
      (print-sparql-results res1 :stream *error-output*)
      (print-sparql-results res2 :stream *error-output*)
      (sparql-results-compare res1 res2 :verbosep t :result-label1 in-file :result-label2 out-file)))))

(defun test-srx-compare (in-file out-file)
  (let ((i (make-instance 'instans)) res1 res2)
    (setf res1 (parse-results-file i in-file))
    (with-open-file (stream out-file :direction :output :if-exists :supersede)
      (output-srx res1 stream))
    (setf res2 (parse-results-file i out-file))
;    (inform "res2 = ~S" res2)
    (unless (sparql-results-compare res1 res2)
      (inform "Comparing results in ~S to a copy~%" in-file)
      (print-sparql-results res1 :stream *error-output*)
      (print-sparql-results res2 :stream *error-output*)
      (sparql-results-compare res1 res2 :verbosep t))
    (cond ((slot-boundp res1 'results)
	   (add-sparql-result-values res2 (loop for var in (sparql-query-results-variables res1) collect (string (gensym))))
	   (when (sparql-results-compare res1 res2)
	     (inform "Comparing results in ~S to a copy with an extra result~%" in-file)
	     (sparql-results-compare res1 res2 :verbosep t))
	   (unless (null (sparql-query-results-results res1))
	     (setf res2 (parse-results-file i out-file))
	     (setf (sparql-query-results-results res2) (cdr (sparql-query-results-results res2)))
	     (setf (sparql-query-results-tail res2) (if (sparql-query-results-results res2) (last (sparql-query-results-results res2))))
	     (when (sparql-results-compare res1 res2)
	       (inform "Comparing results in ~S to a copy with a missing result~%" in-file)
	       (sparql-results-compare res1 res2 :verbosep t))))
	  ((slot-boundp res1 'boolean)
	   (setf (sparql-boolean-result-value (sparql-query-results-boolean res2)) (not (sparql-boolean-result-value (sparql-query-results-boolean res2))))
	   (when (sparql-results-compare res1 res2)
	     (inform "Comparing results in ~S to a copy with an inverted value~%" in-file)
	     (sparql-results-compare res1 res2 :verbosep t))))))

(defun test-compare-all-srx-files ()
  (loop for file in (directory "../tests/data-r2/*/*.srx")
        do (inform "~%File ~A~%" file)
        do (test-srx-compare (namestring file) "x.srx")))


(defun test-manifest (directory)
  (let* ((rules-file (format nil "~A/tests/input/evaluation-test.rq" (find-instans-root-directory)))
	 (base (format nil "file://~A" (namestring directory)))
	 (args (format nil "-b ~A -r ~A --input=~A/manifest.ttl" base rules-file directory)))
    (inform "(main ~S)" args)
    (main args)))
