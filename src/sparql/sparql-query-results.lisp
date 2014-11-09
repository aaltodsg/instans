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

(defun sparql-compare-srx-result-files (file1 file2 &key output-stream)
  (declare (ignorable output-stream))
  (handler-case 
      (let* ((i (make-instance 'instans))
	     (res1 (parse-results-file i file1))
	     (res2 (parse-results-file i file2)))
	(sparql-results-compare res1 res2 :verbosep t :result-label1 file1 :result-label2 file2 :output-stream output-stream))
    (t (e) (values nil nil (format nil "~A" e)))))

(defun sparql-compare-ttl-result-files (file1 file2 &key output-stream)
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
      (output-results-in-srx res1 stream))
    (let ((res2 (parse-results-file i out-file)))
      (unless (sparql-results-compare res1 res2)
      (print-sparql-results res1 :stream *error-output*)
      (print-sparql-results res2 :stream *error-output*)
      (sparql-results-compare res1 res2 :verbosep t :result-label1 in-file :result-label2 out-file)))))

(defun test-srx-compare (in-file out-file &key (stream *error-output*))
  (let ((i (make-instance 'instans)) res1 res2)
    (setf res1 (parse-results-file i in-file))
    (with-open-file (stream out-file :direction :output :if-exists :supersede)
      (output-results-in-srx res1 stream))
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

      var suitesCollections = [''];
      var suitesCollectionsHash = new Object();
    $('#TestResults').dataTable( { \"bPaginate\": false, \"sDom\": 'W<\"clear\">ilfrtp', \"oColumnFilterWidgets\": { \"aiExclude\": [ 3, 4, 5 ] } } );
    function updateCountsSuiteCollection(testResults, suiteCollection) {
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
//      alert('updateCountsSuiteCollection ' + suiteCollection);
      var trSel = (suiteCollection != '' ? 'tr.'+suiteCollection : 'tr');
      $(testResults).find(trSel+':visible').each(function () {
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
        $('#'+suiteCollection+'-pass-yes').html(passYesTests);  
        $('#'+suiteCollection+'-pass-no').html(passNoTests);  
        $('#'+suiteCollection+'-pass-sum').html(passYesTests + passNoTests);  
        $('#'+suiteCollection+'-implemented-yes').html(implementYesTests);  
        $('#'+suiteCollection+'-implemented-no').html(implementNoTests);  
        $('#'+suiteCollection+'-implemented-sum').html(implementYesTests + implementNoTests);  
        $('#'+suiteCollection+'-negativeSyntax-yes').html(negativeSyntaxTests);  
        $('#'+suiteCollection+'-negativeSyntax-no').html(positiveSyntaxTests);  
        $('#'+suiteCollection+'-negativeSyntax-sum').html(negativeSyntaxTests + positiveSyntaxTests);  
        $('#'+suiteCollection+'-parseNegative-yes').html(parseYesNegativeSyntaxTests);  
        $('#'+suiteCollection+'-parseNegative-no').html(parseNoNegativeSyntaxTests);  
        $('#'+suiteCollection+'-parseNegative-sum').html(parseYesNegativeSyntaxTests + parseNoNegativeSyntaxTests);  
        $('#'+suiteCollection+'-parsePositive-yes').html(parseYesPositiveSyntaxTests);  
        $('#'+suiteCollection+'-parsePositive-no').html(parseNoPositiveSyntaxTests);  
        $('#'+suiteCollection+'-parsePositive-sum').html(parseYesPositiveSyntaxTests + parseNoPositiveSyntaxTests);  
        $('#'+suiteCollection+'-translate-yes').html(translateYesTests);  
        $('#'+suiteCollection+'-translate-no').html(translateNoTests);  
        $('#'+suiteCollection+'-translate-sum').html(translateYesTests + translateNoTests);  
        $('#'+suiteCollection+'-run-yes').html(runYesTests);  
        $('#'+suiteCollection+'-run-no').html(runNoTests);
        $('#'+suiteCollection+'-run-sum').html(runYesTests + runNoTests);  
        $('#'+suiteCollection+'-comparable-yes').html(comparableYesTests);  
        $('#'+suiteCollection+'-comparable-no').html(comparableNoTests);  
        $('#'+suiteCollection+'-comparable-sum').html(comparableYesTests + comparableNoTests);  
        $('#'+suiteCollection+'-compare-yes').html(compareYesTests);  
        $('#'+suiteCollection+'-compare-no').html(compareNoTests);  
        $('#'+suiteCollection+'-compare-sum').html(compareYesTests + compareNoTests);  
    }
    function addSummaryRow(suiteCollection) {
      $('#Summary').append('<tr><td>'+suiteCollection+'</td><td class=\"succeeded\"><span id=\"'+suiteCollection+'-pass-yes\" class=\"yes\"></span></td><td class=\"failed\"><span id=\"'+suiteCollection+'-pass-no\" class=\"no\"></span></td><td><span id=\"'+suiteCollection+'-pass-sum\" class=\"sum\"></span></td><td><span id=\"'+suiteCollection+'-implemented-yes\" class=\"yes\"></span></td><td class=\"failed\"><span id=\"'+suiteCollection+'-implemented-no\" class=\"no\"></span></td><td><span id=\"'+suiteCollection+'-implemented-sum\" class=\"sum\"></span></td><td><span id=\"'+suiteCollection+'-negativeSyntax-yes\" class=\"yes\"></span></td><td><span id=\"'+suiteCollection+'-negativeSyntax-no\" class=\"no\"></span></td><td><span id=\"'+suiteCollection+'-negativeSyntax-sum\" class=\"sum\"></span></td><td class=\"failed\"><span id=\"'+suiteCollection+'-parseNegative-yes\" class=\"yes\"></span></td><td class=\"succeeded\"><span id=\"'+suiteCollection+'-parseNegative-no\" class=\"no\"></span></td><td><span id=\"'+suiteCollection+'-parseNegative-sum\" class=\"sum\"></span></td><td><span id=\"'+suiteCollection+'-parsePositive-yes\" class=\"yes\"></span></td><td class=\"failed\"><span id=\"'+suiteCollection+'-parsePositive-no\" class=\"no\"></span></td><td><span id=\"'+suiteCollection+'-parsePositive-sum\" class=\"sum\"></span></td><td><span id=\"'+suiteCollection+'-translate-yes\" class=\"yes\"></span></td><td class=\"failed\"><span id=\"'+suiteCollection+'-translate-no\" class=\"no\"></span></td><td><span id=\"'+suiteCollection+'-translate-sum\" class=\"sum\"></span></td><td><span id=\"'+suiteCollection+'-run-yes\" class=\"yes\"></span></td><td class=\"failed\"><span id=\"'+suiteCollection+'-run-no\" class=\"no\"></span></td><td><span id=\"'+suiteCollection+'-run-sum\" class=\"sum\"></span></td><td><span id=\"'+suiteCollection+'-comparable-yes\" class=\"yes\"></span></td><td class=\"succeeded\"><span id=\"'+suiteCollection+'-comparable-no\" class=\"no\"></span></td><td><span id=\"'+suiteCollection+'-comparable-sum\" class=\"sum\"></span></td><td class=\"succeeded\"><span id=\"'+suiteCollection+'-compare-yes\" class=\"yes\"></span></td><td class=\"failed\"><span id=\"'+suiteCollection+'-compare-no\" class=\"no\"></span></td><td><span id=\"'+suiteCollection+'-compare-sum\" class=\"sum\"></span></td></tr>');
      $('#Summary tr td span').each(function () {
        if ($(this).html() == '0') {
          $(this).parent().removeClass('succeeded');
          $(this).parent().removeClass('failed');
        }
      });
    }
    function updateCounts(testResults) {
      $('#Summary tr').detach();
      $('#Summary').append('<tr><th></th><th colspan=\"3\">Pass</th><th colspan=\"3\">Implemented</th><th colspan=\"3\">Negative syntax</th><th colspan=\"3\">Parse negative (of <sup class=\"note\">[1]</sup>)</th><th colspan=\"3\">Parse positive (of <sup class=\"note\">[1]</sup>)</th><th colspan=\"3\">Translate (of <sup class=\"note\">[2]</sup>)</th><th colspan=\"3\">Run (of <sup class=\"note\">[3]</sup>)</th><th colspan=\"3\">Comparable (of <sup class=\"note\">[4]</sup>)</th><th colspan=\"3\">Compare (of <sup class=\"note\">[5]</sup>)</th></tr>');
      $('#Summary').append('<tr><th></th><th>Yes</th><th>No</th><th>Sum</th><th>Yes<sup class=\"note\">[1]</sup></th><th>No</th><th>Sum</th><th>Yes</th><th>No</th><th>Sum</th><th>Yes</th><th>No</th><th>Sum</th><th>Yes<sup class=\"note\">[2]</sup></th><th>No</th><th>Sum</th><th>Yes<sup class=\"note\">[3]</sup></th><th>No</th><th>Sum</th><th>Yes<sup class=\"note\">[4]</sup></th><th>No</th><th>Sum</th><th>Yes<sup class=\"note\">[5]</sup></th><th>No</th><th>Sum</th><th>Yes</th><th>No</th><th>Sum</th></tr>');
      addSummaryRow('');
      updateCountsSuiteCollection(testResults, '');
      $(testResults).find('tr:visible').each(function () {
        if ($(this).attr('id')) {
          var suiteCollectionName = $(this).attr('id');
          var suiteCollection = suiteCollectionName.slice(0, suiteCollectionName.indexOf('_'));
	  if (!suitesCollectionsHash[suiteCollection]) {
//            alert('Found ' + suiteCollectionName + ', suiteCollection = ' + suiteCollection);
	    suitesCollectionsHash[suiteCollection] = true;
	    suitesCollections.push(suiteCollection);
            addSummaryRow(suiteCollection);
            updateCountsSuiteCollection(testResults, suiteCollection);
	  }
        }
      });
      suitesCollections.reverse();
    }
    $('#TestResults').each(function () { updateCounts(this); });
    $('#TestResults').on('draw.dt', function () { updateCounts(this);});
} );
</script>
</head>
<body>
<h2>Summary</h2>
<table id=\"Summary\">
</table>
<h2>Tests</h2>
<table id=\"TestResults\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" class=\"display\" width=\"100%\">
<thead>
<tr>
  <th class=\"test-number\">Number</th>
~{  ~{<th class=\"~(~A~)\">~(~A~)</th>~}~^~%~}
</tr>
</thead>
<tbody>~%" (mapcar #'(lambda (f) (list f f)) (sparql-tests-fields this))))
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
	(:html (format stream "~&<tr class=\"~(~A~)-~(~A~)\" id=\"~(~A~)-~(~A~)_~(~A~)\">~%<td class=\"test-number\">~A</td>~%~{~{  <td class=\"~(~A~)\">~(~A~)</td>~}</td>~^~%~}~%</tr>~%"
		       (sparql-test-suite this) (sparql-test-collection this)(sparql-test-suite this) (sparql-test-collection this) (sparql-test-name this) (if test-number test-number "") (mapcar #'(lambda (f) (list f (slot-value this f))) (sparql-test-fields this))))
	(t (error* "Unknown output-type ~A" output-type))))))

;(defun evaluation-test (rule-file data-file &key graph-data-file select-results-file construct-results-file (output-directory "."))

(defgeneric expand-sparql-test-path (sparql-test pathname)
  (:method ((this sparql-test) pathname)
    (and pathname (format nil "~Atests/~A/~A/~A" (find-instans-root-directory) (sparql-test-suite this) (sparql-test-collection this) pathname))))

(defgeneric run-one-sparql-test (sparql-test &key output-dir-name log-file print-queryfile print-datafile print-resultfile)
  (:method ((this sparql-test) &key (output-dir-name "cmpoutput") log-file print-queryfile print-datafile (print-resultfile t))
    (unless (char= (char output-dir-name (1- (length output-dir-name))) #\/)
      (setf output-dir-name (format nil "~A/" output-dir-name)))
    (let (log-stream)
      (unwind-protect
	   (progn
	     (setf (sparql-test-completed this) nil)
	     (setf log-stream (if log-file (open-file log-file :direction :output :if-exists :append :if-does-not-exist :create :fmt "run-one-sparql-test: open ~{~A~^ ~}") *error-output*))
	     (format log-stream "~&---------------~%")
	     (print-sparql-test this :stream log-stream :output-type :txt)
	     (format log-stream "~&---------------~%")
	     (let* ((queryfile (expand-sparql-test-path this (sparql-test-queryfile this)))
		    (output-directory (expand-sparql-test-path this output-dir-name))
		    (datafile (expand-sparql-test-path this (sparql-test-datafile this)))
		    (graph-datafiles (mapcar #'(lambda (file) (expand-sparql-test-path this file)) (sparql-test-graphdatafiles this)))
		    (resultfile (expand-sparql-test-path this (sparql-test-resultfile this)))
		    (resultfilebase (if resultfile (subseq (file-namestring resultfile) 0 (position #\. (file-namestring resultfile) :from-end t))))
		    (resulttype (and resultfile (intern-keyword (string-upcase (pathname-type (parse-namestring resultfile))))))
;		    (resultgraphs (mapcar #'(lambda (x) (list (expand-sparql-test-path this (first x)) (second x))) (sparql-test-resultgraphs this)))
		    output-ttl-file compare-result
		    (instans-iri (parse-iri (format nil "file://~A" queryfile)))
		    (instans (create-instans instans-iri))
		    (base (parse-iri (format nil "file://~A" (directory-namestring queryfile)))))
	       (ensure-directories-exist output-directory)
					;		   (inform "~&~A~%" (probe-file output-directory))
					;		   (trace translate-sparql-algebra-to-rete)
	       (when (and print-queryfile queryfile)
		 (inform "Query ~A:~%~A~%" queryfile (file-contents-to-string queryfile)))
	       ;;; Add rules and test for parsing, translation and initialization
	       (instans-add-rules instans queryfile)
					;		   (untrace)
	       (setf (sparql-test-parse this) (instans-has-status instans (intern-instans "INSTANS-RULE-PARSING-SUCCEEDED")))
	       (setf (sparql-test-translate this) (instans-has-status instans (intern-instans "INSTANS-RULE-TRANSLATION-SUCCEEDED")))
	       (setf (sparql-test-initialization this) (instans-has-status instans (intern-instans "INSTANS-RULE-INITIALIZATION-SUCCEEDED")))
	       (when (and (sparql-test-parse this) (sparql-test-translate this))
		 ;;; Set select and construct output processors
		 (let ((select-rules-p (some #'select-node-p (instans-nodes instans)))
		       (ask-rules-p (some #'ask-node-p (instans-nodes instans)))
		       (describe-rules-p (some #'describe-node-p (instans-nodes instans)))
		       (construct-rules-p (some #'construct-node-p (instans-nodes instans)))
		       (modify-rules-p (some #'modify-node-p (instans-nodes instans))))
		   (unless (= 1 (count t (list select-rules-p ask-rules-p describe-rules-p construct-rules-p modify-rules-p)))
		     (error* "Query file contains several kinds of rules ~A" queryfile))
		   (cond (select-rules-p
			  (when resultfile
			    (setf output-ttl-file (format nil "~A~A.ttl" output-directory resultfilebase))
			    (setf (instans-select-output-processor instans) (create-select-output-processor instans output-ttl-file :ttl))))
			 ((or ask-rules-p describe-rules-p)
			  nil)
			 (construct-rules-p
			  (when resultfile
			    (setf output-ttl-file (format nil "~A~A.ttl" output-directory resultfilebase))
			    (setf (instans-construct-output-processor instans) (create-construct-output-processor instans output-ttl-file :ttl)))))
		   (when datafile
		     (when print-datafile
		       (inform "Datafile ~A:~%~A~%" datafile (file-contents-to-string datafile)))
		     (instans-add-stream-input-processor instans datafile :base base :input-type (intern-keyword (string-upcase (file-type datafile)))))
		   (loop for graph-datafile in graph-datafiles
			 do (instans-add-stream-input-processor instans graph-datafile :base base :input-type (intern-keyword (string-upcase (file-type graph-datafile)))))
					;		     (trace-rete)
		   ;;; Run the tests, check for status
		   (instans-run instans)
		   (instans-close-open-streams instans)
		   (setf (sparql-test-run this) (instans-has-status instans 'instans-rule-running-succeeded))
		   (setf (sparql-test-implemented this) (not (instans-has-status instans 'instans-feature-not-implemented-yet)))
					;		     (untrace)
		   (when (sparql-test-comparable this)
		     ;;; Compare results
		     (cond (select-rules-p
			    ;;; If the results file is not ttl-file, we must create a comparable ttl file (if it does not exist already)
			    (setf resultfile 
				  (cond ((eq resulttype :ttl) resultfile)
					(t
					 (let* ((fn (file-namestring resultfile))
						(ttl-resultfile (format nil "~A~A-converted.ttl" output-directory (subseq fn 0 (position #\. fn :from-end t)))))
;					   (or (probe-file ttl-resultfile)
					       (maybe-convert-result-file-to-ttl instans resultfile resulttype ttl-resultfile)
;					       )
					       ttl-resultfile))))
			    (setf compare-result (sparql-compare-ttl-result-files resultfile output-ttl-file)))
			   (construct-rules-p 
			    (unless (eq resulttype :ttl)
			      (error* "Not a ttl resultfile ~A" resultfile))
			    (setf compare-result (sparql-compare-ttl-result-files resultfile output-ttl-file)))
			   ((or ask-rules-p describe-rules-p)
			    (setf compare-result nil)))
		     (setf (sparql-test-compare this) compare-result)
		     (inform "Compare result ~A -> ~A" resultfile compare-result)
		     (when (and (not (or ask-rules-p describe-rules-p)) (not compare-result) print-resultfile)
		       (inform "Expected results ~A:~%~A~%" resultfile (file-contents-to-string resultfile))
		       (inform "Actual results ~A:~%~A~%" output-ttl-file (file-contents-to-string output-ttl-file))))))
	       (setf (sparql-test-completed this) t)
	       (setf (sparql-test-pass this) (sparql-test-successful-p this))))
	(when log-file (close-stream log-stream "run-sparql-test: closing ~A"))))))

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
	    do (run-one-sparql-test test :output-dir-name output-dir-name :log-file log-file)))))

(defvar *sparql-tests* nil)

(defun rst (&optional suite collection name)
  (loop for test in (sparql-tests-entries (or *sparql-tests* (parse-tests)))
	when (and (or (null suite) (equal suite (sparql-test-suite test)))
		  (or (null collection) (equal collection (sparql-test-collection test)))
		  (or (null name) (equal name (sparql-test-name test))))
	do (progn
	     (inform "Running ~A-~A-~A" (sparql-test-suite test) (sparql-test-collection test) (sparql-test-name test))
	     (run-one-sparql-test test :log-file nil :print-queryfile t :print-datafile t :print-resultfile t))))

(defun parse-tests (&key (rules-file (format nil "~A/tests/input/gettestfiles.rq" (find-instans-root-directory)))
		      (tests-csv-file (format nil "~A/tests/sparql-tests/sparql-tests.csv" (find-instans-root-directory))))
  (tests-from-manifests :rules-file rules-file :select-output-name tests-csv-file)
  (setf *sparql-tests* (parse-csv-tests tests-csv-file)))

(defun parse-csv-tests (csv-file &key (skip-lines 1))
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
				    (log-file (format nil "~A/tests/sparql-tests/sparql-tests-log" (find-instans-root-directory))))
  (let ((sparql-tests (or *sparql-tests* (parse-tests :rules-file rules-file :tests-csv-file tests-csv-file)))
	(*oink* t))
    (declare (special *oink*))
    (run-sparql-tests sparql-tests :log-file log-file)
    (print-sparql-tests sparql-tests :output-type results-type :output results-file)))

(defun psts ()
  (process-sparql-test-suite :results-file nil :results-type :txt :log-file nil))

(defun maybe-convert-result-file-to-ttl (instans input-file input-file-type output-file)
  (case input-file-type
    (:srx
     (let ((results (with-open-file (input input-file :direction :input)
		     (parse-srx-stream instans input input-file))))
       (with-open-file (output output-file :direction :output :if-exists :supersede)
	 (output-results-in-ttl results output))))
    (t (error* "Cannot convert yet ~A" input-file))))
