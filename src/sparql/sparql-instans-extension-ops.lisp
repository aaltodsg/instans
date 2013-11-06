;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-sparql-function "math:expt" (:arguments ((x xsd-number-value) (y xsd-number-value)) :returns xsd-number-value)
  (:method ((x xsd-number-value) (y xsd-number-value)) (expt x y)))

(define-sparql-function "math:sqrt" (:arguments ((x xsd-number-value)) :returns xsd-number-value)
  (:method ((x xsd-number-value)) (sqrt x)))

(define-sparql-function "datetime:datetime_in_seconds" (:arguments ((x xsd-datetime-value)) :returns xsd-number-value)
  (:method ((x xsd-datetime-value)) (datetime-in-seconds x)))

(define-sparql-function "instans:create_rete" (:arguments () :returns rdf-iri)
  (:method ()
    (instans-create-rete)))

(define-sparql-function "instans:add_rules" (:arguments ((rete-iri rdf-iri) (rules-url rdf-iri)) :returns rdf-iri)
  (:method ((rete-iri rdf-iri) (rules-url rdf-iri))
    (instans-add-rules rete-iri rules-url)))

(define-sparql-function "instans:add_triples_from_url" (:arguments ((rete-iri rdf-iri) (triples-url rdf-iri) &optional (graph-iri rdf-iri)) :returns rdf-iri)
  (:method ((rete-iri rdf-iri) (triples-url rdf-iri) &optional (graph-iri rdf-iri))
    (instans-add-triples-from-url rete-iri triples-url graph-iri)))

(define-sparql-function "instans:execute_system" (:arguments ((rules-url rdf-iri) (triples-url rdf-iri) &optional (graph-iri rdf-iri) (expected-results-iri rdf-iri)) :returns xsd-boolean)
  (:method ((rules-url rdf-iri) (triples-url rdf-iri) &optional (graph-iri rdf-iri) (expected-results-iri rdf-iri))
    (flet ((show-solutions (sl) (loop for s in sl do (inform "Solution: ~{~A~^ ~}" (sparql-result-bindings s)))))
      (let* ((rete-iri (instans-create-rete))
	     (comparep (not (null expected-results-iri)))
	     (expected-query-results (if comparep (parse-srx-from-url expected-results-iri)))
	     (expected-result-list (if comparep (sparql-query-results-results expected-query-results)))
	     (observed-result-list (list nil))
	     (observed-result-list-tail observed-result-list)
	     (report-function (if comparep #'(lambda (node token)
					       (let ((solution (make-instance 'sparql-result
									      :bindings (loop for canonic-var in (node-use (node-prev node))
											      for var = (car (rassoc canonic-var (bindings-alist (node-bindings node))))
											      collect (make-instance 'sparql-binding :variable var :value (token-value node token canonic-var))))))
						 (inform "Node ~S, (node-use (node-prev node)) ~S, token ~S, solution ~S" node (node-use (node-prev node)) token solution)
						 (setf (cdr observed-result-list-tail) (list solution))
						 (setf observed-result-list-tail (cdr observed-result-list-tail)))))))
	(instans-add-rules rete-iri rules-url :report-function report-function :output-directory "/Users/enu/instans/tests/output")
	(instans-add-triples-from-url rete-iri triples-url graph-iri)
	(pop observed-result-list)
	(when comparep
	  (cond ((and (= (length observed-result-list) (length expected-result-list))
		      (every #'(lambda (r1 r2) (sparql-result-equal r1 r2)) observed-result-list expected-result-list))
		 (inform "Expected and observed solutions are equal")
		 t)
		(t
		 (when (not (= (length observed-result-list) (length expected-result-list)))
		   (inform "Observed ~D results, expected ~D results" (length observed-result-list) (length expected-result-list)))
		 (let ((observed-minus-expected (set-difference observed-result-list expected-result-list :test #'sparql-result-equal))
		       (expected-minus-observed (set-difference expected-result-list observed-result-list :test #'sparql-result-equal)))
		   (cond ((and (null observed-minus-expected) (null expected-minus-observed))
			  (inform "Expected and observed solutions same, but in a different order")
			  (inform "Expected:")
			  (show-solutions expected-result-list)
			  (inform "Observed:")
			  (show-solutions observed-result-list))
			 (t
			  (inform "Expected solutions that were not observed:")
			  (show-solutions expected-minus-observed)
			  (inform "Observed solutions that were not expected:")
			  (show-solutions observed-minus-expected)))))
		nil))))))

			





