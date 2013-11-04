;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun sparql-tests (directory-path &rest keys &key warn-on-errors-p  &allow-other-keys)
  (let ((saved *sparql-error-op*))
    (when (getf keys :warn-on-errors-p) (remf keys :warn-on-errors-p))
    (unwind-protect
	 (progn 
	   (setf *sparql-error-op* (if warn-on-errors-p :warn nil))	
	   (loop for file in (directory directory-path)
		 for parsing = (apply #'sparql-parse-file file :test-mode-p t keys)
		 when (parsing-failed-p parsing) do (inform "Parsing failed: ~A" (parsing-error-message parsing))
		 else do (let ((results (parsing-result parsing)))
			   (cond ((getf (rest (first results)) :select)
				  (do-select-tests (first results) file))
				 (t
				  (do-filter/expression-tests results file))))))
      (setf *sparql-error-op* saved))))

(defun do-filter/expression-tests (forms input-file)
  (loop with passed = 0
	with failed = 0
	for form in forms
        for test = (translate-filter/expression-test form)
	do (inform "~S" test)
	do (multiple-value-bind (p f)
	       (let ((func (compile nil test)))
		 (funcall func))
	     (incf passed p)
	     (incf failed f))
	finally (inform "~A: Passed ~D tests, failed ~D tests~%" input-file passed failed)))

(defun translate-filter/expression-test (form)
  (let* ((attrs (cdr form))
	 (name (getf attrs :name))
	 (test-expr (if (getf attrs :filter)
			`(error-safe-ebv ,(sparql-expr-to-lisp (getf attrs :filter)))
			(sparql-expr-to-lisp (getf attrs :expression))))
	 (inputdata (getf attrs :input))
	 (vars (second inputdata))
	 (values (third inputdata))
	 (output (getf attrs :output))
	 (result-var (gensym "RESULT"))
	 (expect-var (gensym "EXPECT")))
    (unless inputdata (error* "Missing :input in ~A" attrs))
    (unless output (error* "Missing :output in ~A" attrs))
    (unless (= (length vars) (length values)) (error* "There is not the same number of variables and value lists in ~A:~%vars=~S~%values~S" attrs vars values))
    (unless (every #'(lambda (vl) (= (length vl) (length output))) values)
      (error* "There is not the same number of values and output in ~A:~%values=~S~%output=~S" attrs values output))
    (let ((args (sparql-var-lisp-names vars)))
      `(lambda ()
	 (loop ,@(loop for arg in args
		       for arg-values in values
		       nconc `(for ,arg in (list ,@(loop for arg-value in arg-values collect (quotify-sparql-value arg-value)))))
	    for ,expect-var in (list ,@(loop for output in output collect (quotify-sparql-value output)))
	    for ,result-var = (handler-case 
				  ,test-expr
				(t (v) (describe v) (make-instance 'sparql-runtime-exception :data v)))
	    count (compare-and-report-filter/expression-test-results ,name (list ,@args) ,expect-var ,result-var) into passed
	    count t into tests
	    finally (return (values passed (- tests passed))))))))

(defun compare-and-report-filter/expression-test-results (name args expect result)
  (cond ((or (equal expect result)
	     (and (sparql-error-p expect) (sparql-error-p result)
		  (or (null (sparql-error-format expect))
		      (and (sparql-error-format result) 
			   (string= (apply #'format nil (sparql-error-format result) (sparql-error-message-arguments result))
				    (apply #'format nil (sparql-error-format expect) (sparql-error-message-arguments expect))))))
	     (and (sparql-runtime-exception-p expect) (sparql-runtime-exception-p result)))
	 (inform "Test passed: ~@[~A~] (~{~S~^ ~}) -> ~S" name args result)
	 t)
	(t
	 (inform "Test failed: ~@[~A~] (~{~S~^ ~}) -> ~S does not match expected value ~S" name args result expect)
	 nil)))

(defun quotify-sparql-value (x)
  (cond ((rdf-literal-p x)
	 (cond ((not (null (rdf-literal-lang x)))
		`(create-rdf-literal-with-lang ,(rdf-literal-string x) ,(rdf-literal-lang x)))
	       ((not (null (rdf-literal-type x)))
		`(create-rdf-literal-with-type ,(rdf-literal-string x)  (parse-iri ,(rdf-iri-string (rdf-literal-type x)))))
	       (t
		(error* "Unexpected simple literal ~S" x))))
	((rdf-iri-p x) `(parse-iri ,(rdf-iri-string x)))
	((typep x 'xsd-value) x)
	((sparql-unbound-p x) `(sparql-unbound))
	((sparql-error-p x) `(make-instance 'sparql-error :format ,(sparql-error-format x) :arguments (list ,@(quotify-list (sparql-error-message-arguments x)))))
	(t
	 (error* "Unexpected value ~S" x))))

(defun do-select-tests (forms input-file)
  (let* ((attrs (rest forms))
	 (name (getf attrs :name))
	 (select (getf attrs :select))
	 (triplesdata (getf attrs :triples))
	 (expected-solutions (getf attrs :solutions))
	 (expected-solution-vars (second expected-solutions))
	 (expected-solution-values (third expected-solutions))
	 (expected-solution-tuples (apply #'(lambda (&rest args) args) expected-solution-values)))
    (unless triplesdata (error* "Missing :triples in ~A" attrs))
    (unless expected-solutions (error* "Missing :solutions in ~A" attrs))
    (unless (= (length expected-solution-vars) (length expected-solution-values)) (error* "There is not the same number of variables and value lists in ~A:~%vars=~S~%values~S" attrs expected-solution-vars expected-solution-values))
    (unless (apply #'= (mapcar #'length expected-solution-values)) (error* "There is not the same number of values in for each variable in:~%values=~S" expected-solution-values))
    (when (some #'(lambda (triple) (some #'sparql-var-p triple)) (rest triplesdata))
      (error* "Input triples cannot contain variables (in ~A)" attrs))
    (let ((network (make-instance 'network :name name :bindings (make-bindings)))
	  (triples (rest triplesdata)))
      (compile-sparql-algebra-expr select network)
      (setf (network-input-function network) #'(lambda () (and triples (pop triples))))
      (let* ((bindings (network-bindings network))
	     (canonic-vars (loop for var in expected-solution-vars for canonic-var = (cdr (assoc var bindings)) do (assert canonic-var) collect canonic-var))
	     (actual-solution-tuples nil))
	(setf (network-select-function network)
	      #'(lambda (node token)
		  (push (loop for canonic-var in canonic-vars
			      for value = (token-value node token canonic-var)
			      collect value)
			actual-solution-tuples)))
	(execute-system network)
	(let ((missing-solutions (list-difference expected-solution-tuples actual-solution-tuples :test #'sparql-tuple=))
	      (unexpected-solution-tuples (list-difference actual-solution-tuples expected-solution-tuples :test #'sparql-tuple=))
	      (equal-number-of-solutions (= (length expected-solution-tuples) (length actual-solution-tuples))))
	  (cond ((and equal-number-of-solutions (every #'sparql-tuple= expected-solution-tuples actual-solution-tuples))
		 (inform "~A: Test passed, expected and actual solutions are the same and in the same order" input-file))
		((and (null unexpected-solution-tuples))
		 (inform "~A: Test failed, expected and actual solutions are the same, but in a different order" input-file))
		(t
		 (inform "~A: Test failed." input-file)
		 (when missing-solutions
		   (inform "~D missing solutions: ~S" (length missing-solutions) missing-solutions))
		 (when unexpected-solution-tuples
		   (inform "~D unexpected solutions: ~S" (length unexpected-solution-tuples) unexpected-solution-tuples)))))))))

(defun sparql-tuple= (t1 t2)
  (and (equal (length t1) (length t2))
       (every #'(lambda (x1 x2)
		  (or (eql x1 x2)
		      (and (rdf-iri-p x1) (rdf-iri-p x2) (rdf-iri= x1 x2))
		      (and (rdf-literal-p x1) (rdf-literal-p x2) (rdf-literal= x1 x2))))
	      t1 t2)))

