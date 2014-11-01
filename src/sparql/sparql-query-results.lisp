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
				       (unless (sparql-unbound-p value)
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
						  (xml-emitter:with-simple-tag ("literal") (xml-emitter:xml-as-is value)))))))))))))))))

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

(defun sparql-compare-srx-files (file1 file2 &key output-stream)
  (declare (ignorable output-stream))
  (let* ((i (make-instance 'instans))
	 (res1 (parse-results-file i file1))
	 (res2 (parse-results-file i file2)))
    (sparql-results-compare res1 res2 :verbosep t :result-label1 file1 :result-label2 file2 :output-stream output-stream)))

(defun sparql-compare-ttl-files (file1 file2 &key output-stream)
  (declare (ignorable output-stream))
  (let* ((graph1 nil)
	 (res1 (parse-rdf-file file1 :document-callback #'(lambda (statements) (setf graph1 statements))))
	 (graph2 nil)
	 (res2 (parse-rdf-file file2 :document-callback #'(lambda (statements) (setf graph2 statements)))))
    (cond ((ll-parser-failed-p res1)
	   (error* "Parsing of ~A failed: ~A" file1 (ll-parser-error-messages res1)))
	  ((ll-parser-failed-p res2)
	   (error* "Parsing of ~A failed: ~A" file2 (ll-parser-error-messages res2)))
	  (t
	   (rdf-graphs-isomorphic-p graph1 graph2)))))

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

(defun test-srx-compare (in-file out-file &key (stream *error-output*))
  (let ((i (make-instance 'instans)) res1 res2)
    (setf res1 (parse-results-file i in-file))
    (with-open-file (stream out-file :direction :output :if-exists :supersede)
      (output-srx res1 stream))
    (setf res2 (parse-results-file i out-file))
;    (format stream "~&res2 = ~S~%" res2)
    (unless (sparql-results-compare res1 res2)
      (format stream "~&Comparing results in ~S to a copy~%~%" in-file)
      (print-sparql-results res1 :stream stream)
      (print-sparql-results res2 :stream stream)
      (sparql-results-compare res1 res2 :verbosep t))
    (cond ((slot-boundp res1 'results)
	   (add-sparql-result-values res2 (loop for var in (sparql-query-results-variables res1) collect (string (gensym))))
	   (when (sparql-results-compare res1 res2)
	     (format stream "~&Comparing results in ~S to a copy with an extra result~%~%" in-file)
	     (sparql-results-compare res1 res2 :verbosep t))
	   (unless (null (sparql-query-results-results res1))
	     (setf res2 (parse-results-file i out-file))
	     (setf (sparql-query-results-results res2) (cdr (sparql-query-results-results res2)))
	     (setf (sparql-query-results-tail res2) (if (sparql-query-results-results res2) (last (sparql-query-results-results res2))))
	     (when (sparql-results-compare res1 res2)
	       (format stream "~&Comparing results in ~S to a copy with a missing result~%~%" in-file)
	       (sparql-results-compare res1 res2 :verbosep t))))
	  ((slot-boundp res1 'boolean)
	   (setf (sparql-boolean-result-value (sparql-query-results-boolean res2)) (not (sparql-boolean-result-value (sparql-query-results-boolean res2))))
	   (when (sparql-results-compare res1 res2)
	     (format stream "~&Comparing results in ~S to a copy with an inverted value~%~%" in-file)
	     (sparql-results-compare res1 res2 :verbosep t))))))

(defun test-compare-all-srx-files ()
  (loop for file in (directory "../tests/data-r2/*/*.srx")
        do (inform "~%File ~A~%" file)
        do (test-srx-compare (namestring file) "x.srx")))


(defun tests-from-manifest (directory &key (rules-file (format nil "~A/tests/input/evaluation-test.rq" (find-instans-root-directory))) (verbosep nil) select-output-name select-output-append-p)
  (let* ((base (format nil "file://~A" (namestring directory)))
	 (args (format nil "-b ~A~A -r ~A --input=~A/manifest.ttl" base (if select-output-name (format nil " --select-output~A=~A" (if select-output-append-p "-append" "") select-output-name) "") rules-file directory)))
    (when verbosep (inform "(main ~S)" args))
    (main args)))

;(defun tests-from-manifests (&key (rules-file (format nil "~A/tests/input/evaluation-test.rq" (find-instans-root-directory))) select-output-name verbosep)
(defun tests-from-manifests (&key (rules-file (format nil "~A/tests/input/gettestfiles.rq" (find-instans-root-directory))) select-output-name verbosep)
  (loop for manifest in (append (directory (format nil "~A/tests/data-r2/*/manifest.ttl" (find-instans-root-directory)))
				(directory (format nil "~A/tests/data-sparql11/*/manifest.ttl" (find-instans-root-directory))))
        for appendp = nil then (not (null select-output-name))
        for dir = (directory-namestring manifest)
        do (tests-from-manifest dir :rules-file rules-file :select-output-name select-output-name :select-output-append-p appendp :verbosep verbosep)))

(define-class sparql-test ()
  ((test-set :accessor sparql-test-test-set :initarg :test-set)
   (type :accessor sparql-test-type :initarg :type)
   (suite :accessor sparql-test-suite :initarg :suite)
   (collection :accessor sparql-test-collection :initarg :collection)
   (name :accessor sparql-test-name :initarg :name)
   (queryfile :accessor sparql-test-queryfile :initarg :queryfile)
   (datafile :accessor sparql-test-datafile :initarg :datafile)
   (graphdatafiles :accessor sparql-test-graphdatafiles :initarg :graphdatafiles)
   (resultfile :accessor sparql-test-resultfile :initarg :resultfile)
   (resultgraphs :accessor sparql-test-resultgraphs :initarg :resultgraphs)
   (error-message :accessor sparql-test-error-message :initform nil)
   (completed :accessor sparql-test-completed :initform nil)
   (implemented :accessor sparql-test-implemented :initform t)
   (negative :accessor sparql-test-negative :initarg :negative)
   (parse :accessor sparql-test-parse :initform nil)
   (translate :accessor sparql-test-translate :initform nil)
   (initialization :accessor sparql-test-initialization :initform nil)
   (run :accessor sparql-test-run :initform nil)
   (comparable :accessor sparql-test-comparable :initform nil)
   (compare :accessor sparql-test-compare :initform nil)
   (pass :accessor sparql-test-pass :initform nil)))

(defgeneric sparql-test-successful-p (sparql-test)
  (:method ((this sparql-test))
    (cond ((not (sparql-test-completed this)) (error* "Test ~A not completed yet!" (sparql-test-name this)))
	  ((not (sparql-test-implemented this)) nil)
	  ((sparql-test-negative this) (not (sparql-test-parse this)))
	  ((not (sparql-test-parse this)) nil)
	  ((not (sparql-test-translate this)) nil)
	  ((not (sparql-test-initialization this)) nil)
	  ((not (sparql-test-run this)) nil)
	  ((not (sparql-test-compare this)) (not (sparql-test-comparable this)))
	  (t t))))

(define-class sparql-tests ()
  ((entries :accessor sparql-tests-entries :initarg :entries :initform nil)
   (fields :accessor sparql-tests-fields
	   :allocation :class
	   :initform '(suite collection name queryfile datafile implemented negative parse translate run comparable compare pass))))

(defun sparql-test-fields (test) (sparql-tests-fields (sparql-test-test-set test)))

(defgeneric add-sparql-test (sparql-tests &rest keys &key type negative suite collection name queryfile datafile graphdatafile resultfile resultgraphfile resultgraphlabel)
  (:method ((this sparql-tests) &rest keys &key type negative suite collection name queryfile datafile graphdatafile resultfile resultgraphfile resultgraphlabel)
    (declare (ignorable keys))
;    (inform "enter add-sparql-test ~A" keys)
    (flet ((string=* (x y)
	     (cond ((null x) (null y))
		   ((null y) nil)
		   (t (string= x y)))))
      (let ((matching (find-if #'(lambda (test)
				   (and (string=* (sparql-test-name test) name)
					(string=* (sparql-test-suite test) suite)
					(string=* (sparql-test-collection test) collection)
					(string=* (sparql-test-queryfile test) queryfile)))
			       (sparql-tests-entries this))))
;	(inform "matching = ~A" matching)
	(cond ((null matching)
	       (push-to-end (setf matching (make-instance 'sparql-test
							  :test-set this
							  :type type :negative negative :suite suite :collection collection :name name
							  :queryfile queryfile :datafile datafile
							  :graphdatafiles (and graphdatafile (list graphdatafile))
							  :resultfile resultfile
							  :resultgraphs (and resultgraphfile (list (list resultgraphfile resultgraphlabel)))))
			    (sparql-tests-entries this)))
	      (t
	       (setf (sparql-test-datafile matching) (or (sparql-test-datafile matching) datafile))
	       (setf (sparql-test-resultfile matching) (or (sparql-test-resultfile matching) resultfile))
	       (when graphdatafile
		 (push-to-end-new graphdatafile (sparql-test-graphdatafiles matching) :test #'equal))
	       (when resultgraphfile
		 (push-to-end-new (list resultgraphfile resultgraphlabel) (sparql-test-resultgraphs matching)
				  :test #'(lambda (a b)
					    (and (equal (first a) (first b)) (equal (second a) (second b))))))
;	       (describe matching)
	       ))
	(setf (sparql-test-comparable matching) (not (null (or (sparql-test-resultfile matching) (sparql-test-resultgraphs matching)))))))
					;    (inform "exit add-sparql-test ~A" keys)
    ))

(defgeneric print-sparql-tests (sparql-tests &key output output-type)
  (:method ((this sparql-tests) &key output (output-type :csv))
    (let (stream)
      (unless output (setf output *error-output*))
      (unwind-protect
	   (progn
	     (setf stream
		   (cond ((typep output 'stream) stream)
			 (t ;(inform "open-file ~A" output)
			    (open-file output :direction :output :if-exists :supersede :fmt "print-sparql-tests: open ~{~A~^ ~}"))))
	     (print-sparql-test-headers this :stream stream :output-type output-type)
	     (loop for test-number from 1
		   for test in (sparql-tests-entries this)
		   do (print-sparql-test test :stream stream :output-type output-type :test-number test-number))
	     (print-sparql-test-trailers this :stream stream :output-type output-type))
	(unless (typep output 'stream)
;	  (inform "closing ~A" stream)
	  (close-stream stream "print-sparql-tests: close ~A"))))))

(defgeneric print-sparql-test-headers (sparql-tests &key stream output-type)
  (:method ((this sparql-tests) &key stream (output-type :csv))
    (case output-type
      (:txt)
      (:lisp (error* "Not implemented yet"))
      (:csv (format stream "~{~A~^,~}" (sparql-tests-fields this)))
      (:html ;(inform "(sparql-tests-fields this) = ~A, stream = ~A" (sparql-tests-fields this) stream)
	     (format stream "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<html>
<head>
<meta charset=\"utf-8\"> 
<title>Syntax tests</title>
<style type=\"text/css\" title=\"currentStyle\">
  @import \"DataTables/media/css/demo_page.css\";
  @import \"DataTables/media/css/demo_table.css\";
  @import \"DataTables/extras/ColumnFilterWidgets/media/css/ColumnFilterWidgets.css\";
</style>
<style type=\"text/css\">
table {
    border-collapse: collapse;
}

table, th, td {
    border: 1px solid black;
}

#Summary table th {
  width: 4em;
}

#Summary table td {
  height: 1.5em;
  vertical-align: bottom;
}

.succeeded { background: #BDFF9D;
}

.failed { background: #FF4D4D;
}

.note { background: #66CCFF;
}

table, tr, th, td {
  text-align: left;
  vertical-align: top;
}
</style>
<script type=\"text/javascript\" src=\"DataTables/media/js/jquery.js\"></script>
<script type=\"text/javascript\" src=\"DataTables/media/js/jquery.dataTables.js\"></script>
<script type=\"text/javascript\" src=\"DataTables/extras/ColumnFilterWidgets/media/js/ColumnFilterWidgets.js\"></script>
<script type=\"text/javascript\" src=\"DataTables/extras/ColReorder/media/js/ColReorder.js\"></script>
<script type=\"text/javascript\">
	$(document).ready( function () {

    $('#TestResults').dataTable( { \"bPaginate\": false, \"sDom\": 'W<\"clear\">ilfrtp', \"oColumnFilterWidgets\": { \"aiExclude\": [ 3, 4, 5 ] } } );
    function counts(elem) {
      var allTests = 0;
      var passYesTests = 0;
      var passNoTests = 0;
      var implementYesTests = 0;
      var implementNoTests = 0;
      var negativeSyntaxTests = 0;
      var positiveSyntaxTests = 0;
      var parseYesNegativeSyntaxTests = 0;
      var parseNoNegativeSyntaxTests = 0;
      var parseYesPositiveSyntaxTests = 0;
      var parseNoPositiveSyntaxTests = 0;
      var translateYesTests = 0;
      var translateNoTests = 0;
      var initializeYesTests = 0;
      var initializeNoTests = 0;
      var runYesTests = 0;
      var runNoTests = 0;
      var comparableYesTests = 0;
      var comparableNoTests = 0;
      var compareYesTests = 0;
      var compareNoTests = 0;
      console.log( 'Redraw occurred at: '+new Date().getTime() );
      $(elem).find('tr:visible').each(function () {
          allTests = allTests + 1;
          $(this).find('td.pass').each(function () {
            if ($(this).html() == 't') passYesTests = passYesTests + 1;
            else passNoTests = passNoTests + 1;
          });
          $(this).find('td.implemented').each(function () {
            if ($(this).html() == 't') implementYesTests = implementYesTests + 1;
            else implementNoTests = implementNoTests + 1;
          });
          $(this).find('td.negative').each(function () {
            if ($(this).html() == 't') {
              negativeSyntaxTests = negativeSyntaxTests + 1;
              $(this).parent().find('td.parse').each(function () {
                if ($(this).html() == 't') parseYesNegativeSyntaxTests = parseYesNegativeSyntaxTests + 1;
                else parseNoNegativeSyntaxTests = parseNoNegativeSyntaxTests + 1;
              });
            } else {
              positiveSyntaxTests = positiveSyntaxTests + 1;
              $(this).parent().find('td.parse').each(function () {
              if ($(this).html() == 't') {
                parseYesPositiveSyntaxTests = parseYesPositiveSyntaxTests + 1;
                $(this).parent().find('td.translate').each(function () {
                  if ($(this).html() == 't') {
                    translateYesTests = translateYesTests + 1;
                    $(this).parent().find('td.run').each(function () {
                      if ($(this).html() == 't') {
                        runYesTests = runYesTests + 1;
                        $(this).parent().find('td.comparable').each(function () {
                          if ($(this).html() == 't') {
                            comparableYesTests = comparableYesTests + 1;
                            $(this).parent().find('td.compare').each(function () {
                            if ($(this).html() == 't') {
                              compareYesTests = compareYesTests + 1;
                            } else compareNoTests = compareNoTests + 1;
                          });
                          } else comparableNoTests = comparableNoTests + 1;
                      });
                      } else runNoTests = runNoTests + 1;
                    });
                  } else translateNoTests = translateNoTests + 1;
                });
              } else parseNoPositiveSyntaxTests = parseNoPositiveSyntaxTests + 1;
              });
            }
          });
        });
        $('#pass span.yes').html(passYesTests);  
        $('#pass span.no').html(passNoTests);  
        $('#pass span.sum').html(passYesTests + passNoTests);  
        $('#implemented span.yes').html(implementYesTests);  
        $('#implemented span.no').html(implementNoTests);  
        $('#implemented span.sum').html(implementYesTests + implementNoTests);  
        $('#negativeSyntax span.yes').html(negativeSyntaxTests);  
        $('#negativeSyntax span.no').html(positiveSyntaxTests);  
        $('#negativeSyntax span.sum').html(negativeSyntaxTests + positiveSyntaxTests);  
        $('#parseNegative span.yes').html(parseYesNegativeSyntaxTests);  
        $('#parseNegative span.no').html(parseNoNegativeSyntaxTests);  
        $('#parseNegative span.sum').html(parseYesNegativeSyntaxTests + parseNoNegativeSyntaxTests);  
        $('#parsePositive span.yes').html(parseYesPositiveSyntaxTests);  
        $('#parsePositive span.no').html(parseNoPositiveSyntaxTests);  
        $('#parsePositive span.sum').html(parseYesPositiveSyntaxTests + parseNoPositiveSyntaxTests);  
        $('#translate span.yes').html(translateYesTests);  
        $('#translate span.no').html(translateNoTests);  
        $('#translate span.sum').html(translateYesTests + translateNoTests);  
        $('#run span.yes').html(runYesTests);  
        $('#run span.no').html(runNoTests);  
        $('#run span.sum').html(runYesTests + runNoTests);  
        $('#comparable span.yes').html(comparableYesTests);  
        $('#comparable span.no').html(comparableNoTests);  
        $('#comparable span.sum').html(comparableYesTests + comparableNoTests);  
        $('#compare span.yes').html(compareYesTests);  
        $('#compare span.no').html(compareNoTests);  
        $('#compare span.sum').html(compareYesTests + compareNoTests);  
    }
    $('#TestResults').each(function () { counts(this); });
    $('#TestResults').on('draw.dt', function () { counts(this);});
} );
</script>
</head>
<body>
<h2>Summary</h2>
<table id=\"Summary\">
<tr><th>Pass</th><th>Implemented</th><th>Negative syntax</th><th>Parse negative (of <sup class=\"note\">[1]</sup>)</th><th>Parse positive (of <sup class=\"note\">[1]</sup>)</th><th>Translate (of <sup class=\"note\">[2]</sup>)</th>
<!-- <th>Initialize (of <sup class=\"note\">[3]</sup>)</th> -->
<th>Run (of <sup class=\"note\">[4]</sup>)</th><th>Comparable (of <sup class=\"note\">[5]</sup>)</th><th>Compare (of <sup class=\"note\">[6]</sup>)</th></tr>
<tr>
  <td>
    <table id=\"pass\" width=\"100%\">
      <tr><th>Yes</th><th>No</th><th>Sum</th></tr>
      <tr><td class=\"succeeded\"><span class=\"yes\"></span></td><td class=\"failed\"><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr>
    </table>
  </td>
  <td>
    <table id=\"implemented\" width=\"100%\">
      <tr><th>Yes</th><th>No</th><th>Sum</th></tr>
      <tr><td><span class=\"yes\"></span><sup class=\"note\">[1]</sup></td><td class=\"failed\"><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr>
    </table>
  </td>
  <td>
    <table id=\"negativeSyntax\" width=\"100%\">
      <tr><th>Yes</th><th>No</th><th>Sum</th></tr>
      <tr><td><span class=\"yes\"></span></td><td><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr>
    </table>
  </td>
  <td>
    <table id=\"parseNegative\" width=\"100%\">
      <tr><th>Yes</th><th>No</th><th>Sum</th></tr>
      <tr><td class=\"failed\"><span class=\"yes\"></span></td><td class=\"succeeded\"><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr>
    </table>
  </td>
  <td>
    <table id=\"parsePositive\" width=\"100%\">
      <tr><th>Yes</th><th>No</th><th>Sum</th></tr>
      <tr><td><span class=\"yes\"></span><sup class=\"note\">[2]</sup></td><td class=\"failed\"><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr>
    </table>
  </td>
  <td>
    <table id=\"translate\" width=\"100%\">
      <tr><th>Yes</th><th>No</th><th>Sum</th></tr>
      <tr><td><span class=\"yes\"></span><sup class=\"note\">[3]</sup></td><td class=\"failed\"><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr>
    </table>
  </td>
  <!-- <td> -->
  <!--   <table id=\"initialize\" width=\"100%\"> -->
  <!--     <tr><th>Yes</th><th>No</th><th>Sum</th></tr> -->
  <!--     <tr><td class=\"succeeded\"><span class=\"yes\"></span><sup class=\"note\">[4]</sup></td><td class=\"failed\"><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr> -->
  <!--   </table> -->
  <!-- </td> -->
  <td>
    <table id=\"run\" width=\"100%\">
      <tr><th>Yes</th><th>No</th><th>Sum</th></tr>
      <tr><td><span class=\"yes\"></span><sup class=\"note\">[5]</sup></td><td class=\"failed\"><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr>
    </table>
  </td>
  <td>
    <table id=\"comparable\" width=\"100%\">
      <tr><th>Yes</th><th>No</th><th>Sum</th></tr>
      <tr><td><span class=\"yes\"></span><sup class=\"note\">[6]</sup></td><td class=\"succeeded\"><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr>
    </table>
  </td>
  <td>
    <table id=\"compare\" width=\"100%\">
      <tr><th>Yes</th><th>No</th><th>Sum</th></tr>
      <tr><td class=\"succeeded\"><span class=\"yes\"></span></td><td class=\"failed\"><span class=\"no\"></span></td><td><span class=\"sum\"></span></td></tr>
    </table>
  </td>
<tr>
</table>
<h2>Tests</h2>
<table id=\"TestResults\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" class=\"display\" width=\"100%\">
<thead>
<tr>
  <th class=\"test-number\">Number</th>
~{  ~{<th class=\"~(~A~)\">~(~A~)</th>~}~^~%~}
</tr>
</thead>
<tbody>~%"
		     (mapcar #'(lambda (f) (list f f)) (sparql-tests-fields this))))
      (t (error* "Unknown output-type ~A" output-type)))))

(defgeneric print-sparql-test-trailers (sparql-tests &key stream output-type)
  (:method ((this sparql-tests) &key stream (output-type :csv))
    (case output-type
      (:txt)
      (:lisp)
      (:csv)
      (:html (format stream "~%<tbody>
</table>
<hr/>
</body>
</html>~%"))
      (t (error* "Unknown output-type ~A" output-type)))))

(defgeneric print-sparql-test (sparql-test &key stream output-type test-number)
  (:method ((this sparql-test) &key stream (output-type :csv) test-number)
    (flet ((sparql-value-to-string* (v) (if (stringp v) v (sparql-value-to-string v))))
      (case output-type
	(:txt (format stream "~&Sparql-test:~%~{  ~{~(~A~) = ~A~}~^~%~}~%" (mapcar #'(lambda (field) (list field (slot-value this field))) (sparql-test-fields this))))
	(:lisp (error* "Not implemented yet"))
	(:csv (format stream "~&~{~A~^,~}~%" (mapcar #'(lambda (f) (sparql-value-to-string* (slot-value this f))) (sparql-test-fields this))))
	(:html (format stream "~&<tr>~%<td class=\"test-number\">~A</td>~%~{~{  <td class=\"~(~A~)\">~(~A~)</td>~}</td>~^~%~}~%</tr>~%"
		       (if test-number test-number "") (mapcar #'(lambda (f) (list f (slot-value this f))) (sparql-test-fields this))))
	(t (error* "Unknown output-type ~A" output-type))))))

;(defun evaluation-test (rule-file data-file &key graph-data-file select-results-file construct-results-file (output-directory "."))

(defgeneric run-sparql-test (sparql-test &key output-dir-name log-file)
  (:method ((this sparql-test) &key (output-dir-name "cmpoutput") log-file)
    (let (log-stream)
      (unwind-protect
	   (progn
	     (setf (sparql-test-completed this) nil)
	     (setf log-stream (if log-file (open-file log-file :direction :output :if-exists :append :if-does-not-exist :create :fmt "run-sparql-test: open ~{~A~^ ~}") *error-output*))
	     (format log-stream "~&---------------~%")
	     (print-sparql-test this :stream log-stream :output-type :txt)
	     (format log-stream "~&---------------~%")
	     (let ((directory (format nil "~Atests/~A/~A" (find-instans-root-directory) (sparql-test-suite this) (sparql-test-collection this))))
	       (flet ((expanded-filename (fn) (and fn (format nil "~A/~A" directory fn))))
		 (let* ((queryfile (expanded-filename (sparql-test-queryfile this)))
			(output-directory (format nil "~A/~A/" directory output-dir-name))
			(datafile (expanded-filename (sparql-test-datafile this)))
			(graph-datafiles (mapcar #'expanded-filename (sparql-test-graphdatafiles this)))
			(resultfile (expanded-filename (sparql-test-resultfile this)))
			(resulttype (and resultfile (intern-keyword (string-upcase (pathname-type (parse-namestring resultfile))))))
			(select-results-file (if (member resulttype '(:csv :srx)) resultfile))
			(select-output-file (if select-results-file (format nil "~A~A" output-directory (file-namestring select-results-file)) (format nil "~Adefault-select-output.csv" output-directory)))
			(select-results-type (intern-keyword (string-upcase (pathname-type (parse-namestring select-output-file)))))
			(construct-results-file (if (member resulttype '(:ttl)) resultfile))
			(construct-output-file (if construct-results-file (format nil "~A~A" output-directory (file-namestring construct-results-file)) (format nil "~Adefault-construct-output.ttl" output-directory)))
			(construct-results-type (intern-keyword (string-upcase (pathname-type (parse-namestring construct-output-file)))))
			(resultgraphs (mapcar #'(lambda (x) (list (expanded-filename (first x)) (second x))) (sparql-test-resultgraphs this)))
			(base (parse-iri (format nil "file://~A" (directory-namestring queryfile))))
			(instans-iri (parse-iri (format nil "file://~A" queryfile)))
			(instans (create-instans instans-iri)))
		   (declare (ignorable resultgraphs graph-datafiles))
		   (ensure-directories-exist output-directory)
		   (inform "~&~A~%" (probe-file output-directory))
;		   (trace translate-sparql-algebra-to-rete)
		   (instans-add-rules instans queryfile)
;		   (untrace)
		   (setf (sparql-test-parse this) (instans-has-status instans (intern-instans "INSTANS-RULE-PARSING-SUCCEEDED")))
		   (setf (sparql-test-translate this) (instans-has-status instans (intern-instans "INSTANS-RULE-TRANSLATION-SUCCEEDED")))
		   (setf (sparql-test-initialization this) (instans-has-status instans (intern-instans "INSTANS-RULE-INITIALIZATION-SUCCEEDED")))
		   (when (and (sparql-test-parse this) (sparql-test-translate this))
		     (when select-output-file
		       (setf (instans-select-output-processor instans) (create-select-output-processor instans select-output-file select-results-type)))
		     (when construct-output-file
		       (setf (instans-construct-output-processor instans) (create-construct-output-processor instans construct-output-file construct-results-type)))
		     (when datafile
		       (instans-add-stream-input-processor instans datafile :base base :input-type (intern-keyword (string-upcase (file-type datafile)))))
		     (loop for graph-datafile in graph-datafiles
			   do (instans-add-stream-input-processor instans graph-datafile :base base :input-type (intern-keyword (string-upcase (file-type graph-datafile)))))
;		     (trace-rete)
		     (instans-run instans)
		     (setf (sparql-test-run this) (instans-has-status instans (intern-instans "INSTANS-RULE-RUNNING-SUCCEEDED")))
		     (setf (sparql-test-implemented this) (not (instans-has-status instans (intern-instans "INSTANS-FEATURE-NOT-IMPLEMENTED-YET"))))
;		     (untrace)
		     (instans-close-open-streams instans)
		     (when (sparql-test-comparable this)
		       (case resulttype
			 (:srx
			  (multiple-value-bind (samep same-order-p) (sparql-compare-srx-files select-results-file select-output-file :output-stream log-stream)
			    (declare (ignorable same-order-p))
			    (setf (sparql-test-compare this) samep)))
			 (:ttl
			  (setf (sparql-test-compare this) (sparql-compare-ttl-files  construct-results-file construct-output-file)))
			 (t (inform "Cannot compare files of type ~A yet" resulttype)
			    (describe this))))))))
	     (setf (sparql-test-completed this) t)
	     (setf (sparql-test-pass this) (sparql-test-successful-p this)))
	(when log-file
	  (close-stream log-stream "run-sparql-test: closing ~A"))))))

(defgeneric run-sparql-tests (sparql-tests &key output-dir-name names-of-tests-to-run names-of-tests-to-avoid name-of-first-test-to-start-at log-file)
  (:method ((this sparql-tests) &key (output-dir-name "cmpoutput") names-of-tests-to-run
				  (names-of-tests-to-avoid (list "dawg-delete-insert-01" "dawg-delete-insert-05b" ;; These loop on INSTANS
								 "md5-01" "md5-02" "sha1-01" "sha1-02" "sha256-01" "sha256-02" "sha512-01" "sha512-02" ;; Not implemented
								 "replace01" "replace02" "replace03"
								 "subquery01" "subquery02" "subquery03" "subquery04" "subquery05" "subquery06" "subquery07" ;; Missing RDF input format
								 "subquery08" "subquery09" "subquery10" "subquery11" "subquery12" "subquery13" "subquery14"))
				  name-of-first-test-to-start-at log-file)
    (when (and log-file (probe-file log-file)) (delete-file log-file))
    (let ((entries (sparql-tests-entries this)))
      (when name-of-first-test-to-start-at
	(loop for test = (first entries)
	      while (not (search name-of-first-test-to-start-at (sparql-test-name test) :test #'equalp))
	      do (pop entries)))
      (loop for test in entries
	    for test-name = (sparql-test-name test)
	    when (and (or (null names-of-tests-to-run) (find test-name names-of-tests-to-run :test #'(lambda (x y) (search y x :test #'equalp))))
		      (not (find test-name names-of-tests-to-avoid :test #'(lambda (x y) (search y x :test #'equalp)))))
	    do (run-sparql-test test :output-dir-name output-dir-name :log-file log-file)))))

(defun run-csv-tests (csv-file &key (skip-lines 1) test-iri-list)
  (declare (ignorable test-iri-list))
  (let ((tests (make-instance 'sparql-tests)))
;    (inform "test-iri-list ~A" test-iri-list)
    (csv-parser:map-csv-file csv-file #'(lambda (line)
					  (destructuring-bind (type negative suite collection name queryfile datafile graphdatafile resultfile resultgraphfile resultgraphlabel)
					      (mapcar #'(lambda (x) (if (string= "UNBOUND" x) nil x)) line)
;					    (inform "resultfile=~A" resultfile)
					    (add-sparql-test tests
							     :type type :negative (parse-xsd-boolean negative) :suite suite :collection collection :name name
							     :queryfile queryfile :datafile datafile :graphdatafile graphdatafile
							     :resultfile resultfile :resultgraphfile resultgraphfile :resultgraphlabel resultgraphlabel)))
			     :skip-lines skip-lines)
    tests))

(defvar *oink* nil)

(defun process-sparql-test-suite (&key (rules-file (format nil "~A/tests/input/gettestfiles.rq" (find-instans-root-directory)))
				    (tests-csv-file (format nil "~A/tests/sparql-tests/sparql-tests.csv" (find-instans-root-directory)))
				    (results-type :html)
				    (results-file (format nil "~A/tests/sparql-tests/sparql-tests-results.~(~A~)" (find-instans-root-directory) results-type))
				    (log-file (format nil "~A/tests/sparql-tests/sparql-tests-log" (find-instans-root-directory)))
				    test-iri-list)
  (tests-from-manifests :rules-file rules-file :select-output-name tests-csv-file)
  (let ((sparql-tests (run-csv-tests tests-csv-file :test-iri-list test-iri-list)))
    (declare (special *oink*))
    (let ((*oink* t))
      (run-sparql-tests sparql-tests :log-file log-file))
    (print-sparql-tests sparql-tests :output-type results-type :output results-file)))

(defun psts (&rest test-iri-list)
  (process-sparql-test-suite :results-file nil :results-type :txt :log-file nil :test-iri-list test-iri-list))

