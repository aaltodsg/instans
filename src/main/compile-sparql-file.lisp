;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun compile-sparql-file (file &key instans instans-name output-directory (mkhtml-script "mk-html1") base silentp)
  (with-open-file (input-stream file)
    (apply #'compile-sparql-stream input-stream :input-name file :instans instans :instans-name instans-name :output-directory output-directory :mkhtml-script mkhtml-script :base base :silentp silentp)))

(defun compile-sparql-stream (stream &key input-name instans instans-name output-directory (mkhtml-script "mk-html1") (parser #'sparql-parse-stream) base silentp)
  (declare (special *node-color-alist*))
  (setf *node-color-alist* nil)
  (when (null instans)
    (setf instans (make-instance 'instans :name instans-name)))
  (let* ((parsing (funcall parser instans stream :base base))
	 (colors (list "Black" "Red" "Blue" "Green" "Orange"))
	 (*gen-var-counter* 0)
	 (algebra-expr-list nil))
    (cond ((not (parsing-succeeded-p parsing))
	   (warn "~%~@[~A: ~]~A~%" input-name (parsing-error-message parsing))
	   (return-from compile-sparql-stream (values nil (parsing-error-message parsing))))
	  (t
	   (unless silentp
	     (inform "Parsed ~S" (first (parsing-result-stack parsing))))
	   (setf algebra-expr-list (filter-not #'(lambda (x) (member (car x) '(PREFIX BASE))) (first (parsing-result-stack parsing))))))
    (setf (rest (last colors)) colors)
    (handler-case
	(loop for algebra-expr in algebra-expr-list
	      for color in colors
	      do (compile-sparql-algebra-expr instans algebra-expr :color color :silentp silentp))
      (t (e) (return-from compile-sparql-stream (values nil e))))
    (unless (null output-directory)
      (let* ((name-part (pathname-name input-name))
	     (truedirname (pathname-directory (truename output-directory)))
	     (dot-output-file (make-pathname :directory truedirname :name name-part :type "dot"))
	     (sa-output-file (make-pathname :directory truedirname :name name-part :type "sa")))
	(with-open-file (out sa-output-file :direction :output :if-exists :supersede)
	  (loop for expr in algebra-expr-list
;	        do (inform "algebra-expr:~%~A" expr)
		do (let* ((*print-circle* nil)
			  (*print-pretty* t)
			  (*print-right-margin* 110))
		     (let ((string (with-output-to-string (str) (print expr str))))
		       (setf string (cl-ppcre:regex-replace-all ">" (cl-ppcre:regex-replace-all "<"  string "&lt;") "&gt;"))
		       (format out "~A" string)))))
	(print-dot-file instans dot-output-file :html-labels-p nil)
	(assert (probe-file mkhtml-script))
	(unless silentp
	  (inform "Running ~S on ~S" mkhtml-script input-name))
	(shell-script mkhtml-script input-name output-directory)))
      instans))

(defun compile-sparql-algebra-expr (instans algebra-expr &key (color "Black") silentp)
  (declare (special *node-color-alist*))
  (unless silentp
    (inform "compiling ~S~%" algebra-expr))
  (let* ((canonic (canonize-sparql-algebra-variables instans algebra-expr))
	 (new-nodes (translate-sparql-algebra-to-rete canonic instans)))
    (compute-node-uses-defs-and-vars new-nodes)
    (loop for node in new-nodes
	  do (push-to-end (cons node color) *node-color-alist*)
	  do (cond ((filter-node-p node)
		    (let ((filter-lambda `(lambda ,(sparql-var-lisp-names (node-use node)) ;(let ((v 
					    (eq ,(sparql-expr-to-lisp (filter-test node)) t))))
		      (setf (filter-test-lambda node) filter-lambda)
		      (setf (filter-test-func node) (compile nil filter-lambda))))
		   ((bind-node-p node)
		    (let ((bind-lambda `(lambda ,(sparql-var-lisp-names (node-use node)) ,(sparql-expr-to-lisp (bind-form node)))))
;		      (warn "bind-lambda = ~S" bind-lambda)
		      (setf (bind-form-lambda node) bind-lambda)
		      (setf (bind-form-func node) (compile nil bind-lambda))))
		   ((aggregate-join-node-p node)
		    (setf (aggregate-join-group-form-func node) (compile nil `(lambda ,(sparql-var-lisp-names (node-use node)) ,(aggregate-join-group-form node)))))
		   ((modify-node-p node)
		    (unless silentp
		      (inform "compiling modify-insert-lambda ~S" (modify-insert-lambda node)))
		    (setf (modify-delete-func node) (and (modify-delete-template node) (compile nil (modify-delete-lambda node))))
		    (setf (modify-insert-func node) (and (modify-insert-template node) (compile nil (modify-insert-lambda node)))))))

;    (initialize-new-nodes instans new-nodes)
    new-nodes))

(defun sparql-expr-to-lisp (expr)
  (cond ((consp expr)
	 (cons (sparql-op-lisp-name (first expr)) (mapcar #'sparql-expr-to-lisp (rest expr))))
	((sparql-var-p expr)
	 (intern (string (uniquely-named-object-name expr))))
	(t expr)))

(defun supply-defaults (overriding-keylist default-keylist)
  (loop for default-key in default-keylist by #'cddr
	for default-value in (cdr default-keylist) by #'cddr
	unless (getf overriding-keylist default-key)
	do (setf (getf overriding-keylist default-key) default-value))
  overriding-keylist)

(defun report-execution (instans)
  (let ((queue (instans-rule-instance-queue instans)))
    (inform "Read ~D inputs~&Added ~D quads~&Removed ~D quads~&Executed ~D rules: ~D select rules, ~D modify rules, and ~D construct rules"
	    (instans-input-count instans)
	    (instans-add-quad-count instans)
	    (instans-remove-quad-count instans)
	    (rule-instance-queue-execute-count queue)
	    (rule-instance-queue-select-count queue)
	    (rule-instance-queue-modify-count queue)
	    (rule-instance-queue-construct-count queue))
    instans))

(defun build-and-execute-sparql-system (rules-file triples-file &key (report-function ) (report-function-arguments ) (output-directory "/Users/enu/instans/tests/output") base);  (show-turtle-parse-p nil))
  (catch 'done
  (let* ((instans nil))
    (with-open-file (triples-stream triples-file)
      (setf instans (compile-sparql-file rules-file :output-directory output-directory))
      (setf (instans-select-function instans) report-function)
      (setf (instans-select-function-arguments instans) report-function-arguments)
      (let* ((triples-lexer (make-instance 'turtle-lexer :instans instans :input-stream triples-stream :base base))
	     (triples-parser (make-turtle-parser :triples-callback #'(lambda (triples)
								       (inform "~%Event callback: ~D triples~%" (length triples))
								       (loop for tr in triples do (inform " ~S~%" tr))
								       (process-triple-input instans triples '(:add :execute))))))
	(initialize-execution instans)
	(warn "~%Processing triples:~%")
	(time (funcall triples-parser triples-lexer))
	instans)))))

(defgeneric process-triple-input (instans triples ops &key graph)
  (:method ((this instans) triples ops &key graph)
    (when (symbolp ops)
      (setf ops (list ops)))
    (loop for op in ops 
	 do (case op
	      (:add
	       (loop for (subj pred obj) in triples
		     do (rete-add this subj pred obj graph)))
	      (:remove
	       (loop for (subj pred obj) in triples 
		     do (rete-remove this subj pred obj graph)))
	      (:execute
	       (execute-rules this))
	      (t
	       (error* "Illegal op ~S" op))))))

(defvar *instanses*)
(eval-when (:load-toplevel :execute)
  (setf *instanses*  (make-hash-table :test #'equal)))

(defun create-instans ()
  (let* ((instans-iri (parse-iri (format nil "http://www.cse.aalto.fi/instans/instanses/~A" (string (gensym "INSTANS")))))
	 (instans-name (rdf-iri-string instans-iri))
	 (instans (make-instance 'instans :name instans-name)))
    (setf (gethash instans-name  *instanses*) instans)
    (values instans instans-iri)))

(defun get-instans (instans-iri)
  (let ((instans-name (rdf-iri-string instans-iri)))
    (or (gethash instans-name *instanses*)
	(sparql-error "INSTANS named ~S does not exist!" instans-name))))


(defun read-from-url-or-file (name)
  (cond ((rdf-iri-p name)
	 (cond ((string= (rdf-iri-scheme name) "file")
		(read-from-url-or-file (rdf-iri-path name)))
	       (t
		(let ((data (drakma:http-request (rdf-iri-string name))))
		  (cond ((stringp data) data)
			(t (coerce (mapcar #'code-char (coerce data 'list)) 'string)))))))
	((pathnamep name)
	 (with-open-file (input name)
	   (with-output-to-string (output)
	     (loop for line = (read-line input nil nil)
		   while line
		   do (format output "~A~%" line)))))
	((stringp name)
	 (cond ((http-or-file-iri-string-p name)
		(read-from-url-or-file (parse-iri name)))
	       (t
		(read-from-url-or-file (parse-namestring name)))))
	(t (error* "Cannot read from ~S" name))))

(defun instans-add-rules (instans-iri rules &key output-directory report-function report-function-arguments base silentp)
  (let ((instans (get-instans instans-iri)))
    (cond ((sparql-error-p instans) instans)
	  (t
	   (let ((string (read-from-url-or-file rules)))
	     (unless silentp
	       (inform "~S" string))
	     (with-input-from-string (stream string)
					;      (sparql-parse-stream stream)
	       (multiple-value-bind (compile-result error)
		   (compile-sparql-stream stream :instans instans :input-name (if (stringp rules) rules (rdf-iri-string rules)) :output-directory output-directory :base base :silentp silentp)
		 (cond ((not (null error))
			(sparql-error "~A:~A" rules error))
		       (t
			(when report-function
			  (setf (instans-select-function instans) report-function))
			(when report-function-arguments
			  (setf (instans-select-function-arguments instans) report-function-arguments))
			(initialize-execution instans)
			compile-result)))))))))

(defun instans-add-triples-from-url (instans-iri triples &key graph base silentp)
  (let ((instans (get-instans instans-iri)))
    (cond ((sparql-error-p instans) instans)
	  (t
	   (let ((string (read-from-url-or-file triples)))
	     (unless silentp
	       (inform "~S" string))
	     (with-input-from-string (triples-stream string)
	       (let* ((triples-lexer (make-instance 'turtle-lexer :instans instans :input-stream triples-stream :base base))
		      (triples-parser (make-turtle-parser :triples-callback #'(lambda (triples)
										(unless silentp
										  (inform "~%Event callback: ~D triples~%" (length triples))
										  (loop for tr in triples do (inform " ~S~%" tr)))
										(process-triple-input instans triples '(:add :execute) :graph (if (and graph (rdf-iri-equal graph *rdf-nil*)) nil graph))))))
		 (unless silentp
		   (inform "~%Processing triples:~%"))
;		 (time 
		  (funcall triples-parser triples-lexer)
;		  )
		 instans-iri)))))))

(defvar *instans-execute-system-previous-rules* nil)
(defvar *instans-execute-system-previous-triples* nil)
(defvar *instans-execute-system-previous-expected-results* nil)
(defvar *instans-execute-system-previous-graph* nil)
(defvar *instans-execute-system-previous-base* nil)

(defun instans-execute-system (rules triples &key expected-results graph base (output-directory nil) ; "/Users/enu/instans/tests/output"
			       (silentp t) (use-previous-args-p nil))
  (cond ((not use-previous-args-p)
	 (setf *instans-execute-system-previous-rules* rules)
	 (setf *instans-execute-system-previous-triples* triples)
	 (setf *instans-execute-system-previous-expected-results* expected-results)
	 (setf *instans-execute-system-previous-graph* graph)
	 (setf *instans-execute-system-previous-base* base))
	(t
	 (setf rules *instans-execute-system-previous-rules*)
	 (setf triples *instans-execute-system-previous-triples*)
	 (setf expected-results *instans-execute-system-previous-expected-results*)
	 (setf graph *instans-execute-system-previous-graph*)
	 (setf base *instans-execute-system-previous-base*)))
  (when (equalp graph "DEFAULT") (setf graph nil))
  (inform "execute_system ~A ~A ~A ~A ~A" rules triples expected-results graph base)
					;  (handler-case 
  (multiple-value-bind (instans instans-iri) (create-instans)
    (let* ((comparep (and expected-results (not (rdf-iri-equal expected-results *rdf-nil*))))
	   (expected-query-results (if comparep (if (stringp expected-results) (parse-results-file instans expected-results) (parse-results-from-url instans expected-results))))
	   (expected-result-list (if comparep (sparql-query-results-results expected-query-results)))
	   (observed-result-list (list nil))
	   (observed-result-list-tail observed-result-list)
	   (report-function (if comparep #'(lambda (node token)
					     (let ((solution (make-instance 'sparql-result
									    :bindings (loop for canonic-var in (node-use (node-prev node))
											    for var = (reverse-resolve-binding instans canonic-var)
											    collect (make-instance 'sparql-binding :variable var :value (token-value node token canonic-var))))))
					       (inform "Node ~S, (node-use (node-prev node)) ~S, token ~S, solution ~S" node (node-use (node-prev node)) token solution)
					       (setf (cdr observed-result-list-tail) (list solution))
					       (setf observed-result-list-tail (cdr observed-result-list-tail))))))
	   (observed-query-results (make-instance 'sparql-query-results)))
      (let ((add-rules-result (instans-add-rules instans-iri rules :report-function report-function :output-directory output-directory :base base :silentp silentp)))
	(when (sparql-error-p add-rules-result)
	  (return-from instans-execute-system add-rules-result)))
      (unless silentp
	(print-triple-pattern-matcher (instans-triple-pattern-matcher instans) *error-output*))
      (instans-add-triples-from-url instans-iri triples :graph graph :base base :silentp silentp)
      (pop observed-result-list)
      (unless silentp
	(inform "Täällä ~S" rules)
	(inform "Expected-results ~S" expected-results)
	(inform "Expected ~S" expected-result-list)
	(inform "Observed-result-list ~S~%Observed-query-results ~S" observed-result-list observed-query-results))
      (setf (sparql-query-results-variables observed-query-results)
	    (loop with vars = nil
		  for result in observed-result-list
		  do (setf vars (union vars (mapcar #'sparql-binding-variable (sparql-result-bindings result))))
		  finally (return vars)))
      (setf (sparql-query-results-results observed-query-results) observed-result-list)
      (unless silentp
	(sparql-query-results-to-json instans observed-query-results)
	(when comparep
	  (sparql-query-results-to-json instans expected-query-results)))
      (multiple-value-bind (similarp same-order-p)
	  (cond ((null comparep) (values t t))
		(t
		 (sparql-results-compare expected-query-results observed-query-results :verbosep t :result-label1 "expected" :result-label2 "observed")))
	(values similarp same-order-p (get-instans instans-iri))))))

(defun execute-prev ()
  (instans-execute-system nil nil :use-previous-args-p t))

(defun metasuite ()
  (sparql-call "instans:execute_system" "/Users/enu/instans/tests/input/metasuite.rq"  "/Users/enu/instans/tests/input/manifest-all.ttl" nil nil (parse-iri "file:///Users/enu/Sparql/sparql11-test-suite/")))

(defun run-testsuites (&rest test-suite-names)
  (loop with rules-iri-string = "file:///Users/enu/instans/tests/input/testsuite.rq"
	with root-iri-string = "file:///Users/enu/Sparql/sparql11-test-suite"
	for name in test-suite-names
        for base-iri-string = (format nil "~A/~A/" root-iri-string name)
	for manifest-iri-string = (format nil "~A/manifest.ttl" base-iri-string)
	do (sparql-call "instans:execute_system" (parse-iri rules-iri-string) (parse-iri manifest-iri-string) nil nil (parse-iri base-iri-string))))

; add
; aggregates
; basic-update
; bind
; bindings
; clear
; construct
; copy
; csv-tsv-res
; delete-data
; delete-insert
; delete-where
; delete
; drop
; entailment
; exists
; functions
; grouping
; json-res
; move
; negation
; project-expression
; property-path
; service
; subquery
; syntax-query
; syntax-update-1
; syntax-update-2
; update-silent
; syntax-fed
; service-description
; protocol
