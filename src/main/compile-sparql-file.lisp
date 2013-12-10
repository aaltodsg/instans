;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun compile-sparql-file (file &key instans instans-name rete-html-page-dir make-rete-html-page-script base (silentp t))
  (with-open-file (input-stream file)
    (compile-sparql-stream input-stream :input-name file :instans instans :instans-name instans-name :rete-html-page-dir rete-html-page-dir
			   :make-rete-html-page-script make-rete-html-page-script :base base :silentp silentp)))

(defun compile-sparql-stream (stream &key input-name instans instans-name rete-html-page-dir (make-rete-html-page-script (find-make-rete-html-script)) (parser #'sparql-parse-stream) base silentp)
  (declare (special *node-color-alist*))
  (setf *node-color-alist* nil)
  (when (null instans)
    (setf instans (make-instance 'instans :name instans-name)))
  (let* ((parsing (funcall parser instans stream :base base :show-parse-p (not silentp)))
	 (colors (list "Black" "Red" "Blue" "Green" "Orange"))
	 (algebra-expr-list nil))
    (cond ((not (parsing-succeeded-p parsing))
	   (setf (instans-error-messages instans) (parsing-error-messages parsing))
	   (return-from compile-sparql-stream (values nil (instans-error-message instans))))
	  (t
	   (unless silentp
	     (inform "Parsed ~S" (first (parsing-result-stack parsing))))
	   (setf algebra-expr-list (filter-not #'(lambda (x) (member (car x) '(PREFIX BASE))) (first (parsing-result-stack parsing))))))
    (setf (rest (last colors)) colors)
    (loop for algebra-expr in algebra-expr-list
	  for color in colors
	  do (multiple-value-bind (new-nodes error-msg)
		 (compile-sparql-algebra-expr instans algebra-expr :color color :silentp silentp)
	       (declare (ignorable new-nodes))
	       (when error-msg
		 (push error-msg (instans-error-messages instans))
		 (return-from compile-sparql-stream (values nil error-msg)))))
    (unless (null rete-html-page-dir)
      (let* ((name-part (pathname-name input-name))
	     (truedirname (pathname-directory (truename rete-html-page-dir)))
	     (dot-output-file (make-pathname :directory truedirname :name name-part :type "dot"))
	     (bnd-output-file (make-pathname :directory truedirname :name name-part :type "bnd"))
	     (sa-output-file (make-pathname :directory truedirname :name name-part :type "sa")))
	(with-open-file (out sa-output-file :direction :output :if-exists :supersede)
	  (loop for expr in algebra-expr-list
;	        do (inform "algebra-expr:~%~A" expr)
		do (let* ((*print-circle* nil)
			  (*print-pretty* t)
			  (*print-right-margin* 110))
		     (let ((string (with-output-to-string (str)
				     (print expr str))))
		       (setf string (cl-ppcre:regex-replace-all ">" (cl-ppcre:regex-replace-all "<"  string "&lt;") "&gt;"))
		       (format out "~A" string)))))
	(with-open-file (out bnd-output-file :direction :output :if-exists :supersede)
	  (let ((string (with-output-to-string (str)
			  (format str "Bindings:")
			  (loop for (from . to) in (instans-bindings instans)
				do (format str "~%  ~A -> ~A" (uniquely-named-object-name from) (uniquely-named-object-name to))))))
	    (setf string (cl-ppcre:regex-replace-all ">" (cl-ppcre:regex-replace-all "<"  string "&lt;") "&gt;"))
	    (format out "~A" string)))
	(print-dot-file instans dot-output-file :html-labels-p nil)
	(when (pathnamep make-rete-html-page-script) (setf make-rete-html-page-script (namestring make-rete-html-page-script)))
	(assert (probe-file make-rete-html-page-script))
	(unless silentp
	  (inform "Running ~S on ~S" make-rete-html-page-script input-name))
	(shell-script make-rete-html-page-script input-name rete-html-page-dir)))
      instans))

(defun compile-sparql-algebra-expr (instans algebra-expr &key (color "Black") silentp)
  (declare (special *node-color-alist*))
  (unless silentp
    (inform "compiling ~S~%" algebra-expr))
  (let ((canonic (canonize-sparql-algebra-variables instans algebra-expr)))
    (multiple-value-bind (new-nodes error-msg) (translate-sparql-algebra-to-rete canonic instans)
      (when error-msg
	(return-from compile-sparql-algebra-expr (values nil error-msg)))
      (multiple-value-bind (result error-msg)
	  (compute-node-vars new-nodes)
	(declare (ignorable result))
	(when error-msg
	  (return-from compile-sparql-algebra-expr (values nil error-msg))))
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
      (values new-nodes nil))))

(defun sparql-expr-to-lisp (expr)
  (cond ((consp expr)
	 (let ((sparql-op (first expr))
	       (args-in-lisp (mapcar #'sparql-expr-to-lisp (rest expr))))
	   (cond ((sparql-form-p sparql-op)
		  (apply (sparql-op-lisp-name sparql-op) args-in-lisp))
		 (t
		  (cons (sparql-op-lisp-name sparql-op) args-in-lisp)))))
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

(defun build-and-execute-sparql-system (rules-file triples-file &key (report-function ) (report-function-arguments ) (rete-html-page-dir "/Users/enu/instans/tests/output") base);  (show-turtle-parse-p nil))
  (catch 'done
  (let* ((instans nil))
    (with-open-file (triples-stream triples-file)
      (setf instans (compile-sparql-file rules-file :rete-html-page-dir rete-html-page-dir))
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

(defvar *instanses*)
(eval-when (:load-toplevel :execute)
  (setf *instanses*  (make-hash-table :test #'equal)))

(defvar *current-instans* nil)

(defun create-instans (&optional instans-iri)
  (unless instans-iri (setf instans-iri (parse-iri (format nil "http://www.cse.aalto.fi/instans/instanses/~A" (string (gensym "INSTANS"))))))
  (let* ((instans-name (rdf-iri-string instans-iri))
	 (instans (make-instance 'instans :name instans-name)))
    (setf (gethash instans-name  *instanses*) instans)
    (values instans instans-iri)))

(defun get-instans (instans-iri)
  (let ((instans-name (rdf-iri-string instans-iri)))
    (gethash instans-name *instanses*)))

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

(defun instans-error-message (instans)
;  (inform "msgs = ~S" (instans-error-messages instans))
  (apply #'concatenate 'string (loop for lines on (instans-error-messages instans)
				     when (null (cdr lines))
				     collect (car lines)
				     else collect (format nil "~A~%" (car lines)))))

(defun instans-add-rules (instans-iri rules &key rete-html-page-dir base (silentp t) (create-instans-p t))
  (let ((instans (if create-instans-p (create-instans instans-iri) (get-instans instans-iri))))
    (cond ((sparql-error-p instans) instans)
	  ((file-or-uri-exists-p rules)
	   (let ((string (read-from-url-or-file rules)))
	     (unless silentp
	       (inform "~S" string))
	     (with-input-from-string (stream string)
	       (multiple-value-bind (compile-result error)
		   (compile-sparql-stream stream :instans instans :input-name (if (stringp rules) rules (rdf-iri-string rules))
					  :rete-html-page-dir rete-html-page-dir :base base :silentp silentp)
		 (declare (ignorable compile-result))
		 (cond ((not error)
			(values t nil))
		       (t
			(values nil error)))))))
	  (t
	   (inform "Cannot read SPARQL from ~S" rules)
	   nil))))

(defun instans-add-triples (instans-iri triples &key expected-results graph base (silentp t))
  (let* ((instans (get-instans instans-iri))
	 (comparep (and expected-results (not (rdf-iri-equal expected-results *rdf-nil*))))
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
	 (report-function-arguments nil)
	 (observed-query-results (make-instance 'sparql-query-results))
	 (string (read-from-url-or-file triples)))
    (unless silentp
      (inform "~S" string))
    (setf (instans-rule-instance-removal-policy instans) :remove)
    (when report-function
      (setf (instans-select-function instans) report-function))
    (setf (instans-select-function-arguments instans) report-function-arguments)
    (with-input-from-string (triples-stream string)
      (let* ((triples-lexer (make-instance 'turtle-lexer :instans instans :input-stream triples-stream :base base))
	     (triples-parser (make-turtle-parser :triples-callback #'(lambda (triples)
								       (unless silentp
									 (inform "~%Event callback: ~D triples~%" (length triples))
									 (loop for tr in triples do (inform " ~S~%" tr)))
								       (process-triple-input instans triples '(:add :execute) :graph (if (and graph (rdf-iri-equal graph *rdf-nil*)) nil graph))))))
	(unless silentp
	  (inform "~%Processing triples:~%"))
	;; Is this OK?
	(initialize-execution instans)
	(funcall triples-parser triples-lexer)
	(pop observed-result-list)
	(unless silentp
	  (inform "Expected-results ~S" expected-results)
	  (inform "Expected ~S" expected-result-list)
	  (inform "Observed-result-list ~S~%Observed-query-results ~S" observed-result-list observed-query-results))
	(unless silentp
	  (sparql-query-results-to-json instans observed-query-results)
	  (when comparep
	    (sparql-query-results-to-json instans expected-query-results)))
	(setf (sparql-query-results-variables observed-query-results)
	      (loop with vars = nil
		    for result in observed-result-list
		    do (setf vars (union vars (mapcar #'sparql-binding-variable (sparql-result-bindings result))))
		    finally (return vars)))
	(setf (sparql-query-results-results observed-query-results) observed-result-list)
	(multiple-value-bind (similarp same-order-p)
	    (cond ((null comparep) (values t t))
		  (t
		   (sparql-results-compare expected-query-results observed-query-results :verbosep t :result-label1 "expected" :result-label2 "observed")))
	  (values similarp same-order-p instans))))))

(defvar *instans-execute-system-previous-rules* nil)
(defvar *instans-execute-system-previous-triples* nil)
(defvar *instans-execute-system-previous-expected-results* nil)
(defvar *instans-execute-system-previous-graph* nil)
(defvar *instans-execute-system-previous-base* nil)

(defun instans-execute-system (rules &key triples expected-results graph base (rete-html-page-dir nil) ; "/Users/enu/instans/tests/output") ; nil) ;
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
  (when (or (equalp graph "DEFAULT") (rdf-iri-equal graph *rdf-nil*)) (setf graph nil))
  (when (rdf-iri-equal base *rdf-nil*) (setf base nil))
  (when (rdf-iri-equal triples *rdf-nil*) (setf triples nil))
  (when (rdf-iri-equal expected-results *rdf-nil*) (setf expected-results nil))
					;  (handler-case 
  (multiple-value-bind (instans instans-iri) (create-instans)
    (format (instans-default-output instans) "~%execute_system ~A ~A ~A ~A ~A~&" rules triples expected-results graph base)
    (multiple-value-bind (add-rules-result error)
	(instans-add-rules instans-iri rules :rete-html-page-dir rete-html-page-dir :base base :silentp silentp)
      (declare (ignore add-rules-result))
      (when (not (null error))
	(return-from instans-execute-system (values nil error))))
    (if triples
      (instans-add-triples instans-iri triples :graph graph :base base :silentp silentp))))

(defun execute-prev ()
  (instans-execute-system nil :use-previous-args-p t))

(defun metasuite ()
  (sparql-call "instans:execute_system" "/Users/enu/instans/tests/input/metasuite.rq"  "/Users/enu/instans/tests/input/manifest-all.ttl" nil nil (parse-iri "file:///Users/enu/Sparql/sparql11-test-suite/")))

(defun test-root-subdir-names (test-root-directory)
  (loop for path in (directory (concatenate 'string test-root-directory "/*"))
	when (null (pathname-name path))
        collect (first (last (pathname-directory path)))))

(defun file-or-uri-exists-p (name)
  (flet ((probe-http-uri (name)
	   (handler-case (eq (second (multiple-value-list (drakma:http-request name))) 200)
	     (t () nil))))
    (cond ((rdf-iri-p name) (file-or-uri-exists-p (rdf-iri-string name)))
	  ((pathnamep name) (probe-file name))
	  ((file-iri-string-p name) (probe-file (file-iri-string-path name)))
	  ((http-iri-string-p name) (probe-http-uri name))
	  ((stringp name) (probe-file name))
	  (t
	   (error* "~A does not name a file or uri" name)))))

(defun run-syntax-tests (rules test-root-directory test-root-uri test-sets)
  (setf rules (concatenate 'string "file://" (namestring (probe-file rules))))
  (let* ((root-iri-string (let ((path (probe-file test-root-directory)))
			    (cond ((and path (null (pathname-name path)))
				   (when (member test-sets '(t :all))
				     (setf test-sets (test-root-subdir-names test-root-directory)))
				   (concatenate 'string "file://" (namestring path)))
				  (t
				   (format t "~&NOTE! Sparql test data directory not found in ~A" test-root-directory)
				   (format t "~&      If you want the tests run faster, download file")
				   (format t "~&      ~A.tar.gz" test-root-uri)
				   (format t "~&      and extract directory test-suite-archive/data-r2/ into ~A" test-root-uri)
				   (format t "~&      Using ~A instead" test-root-uri)
				   test-root-uri))))
	 (output-dir (or (let ((path (probe-file "../tests/output")))
			   (and path (string= (namestring path) (directory-namestring path)) (namestring path)))
			 (progn
			   (format t "~&NOTE! The output directory ../tests/output does not exist.")
			   (format t "~&      Create it if you want to have an HTML page showing the RETE network")
			   nil))))
    (loop for name in test-sets
	  for base-iri-string = (format nil "~A~A/" root-iri-string name)
	  for manifest-iri-string = (format nil "~A/manifest.ttl" base-iri-string)
	  do (cond ((not (file-or-uri-exists-p manifest-iri-string))
		    (format t "~&Manifest file ~A not found~&" manifest-iri-string))
		   (t
		    (format t "~&Running tests ~A~&" name)
		    (instans-execute-system rules :triples (parse-iri manifest-iri-string) :base (parse-iri base-iri-string) :silentp t :rete-html-page-dir output-dir))))))

(defun run-data-r2-syntax-tests (&optional (test-sets '("syntax-sparql1" "syntax-sparql2" "syntax-sparql3" "syntax-sparql4" "syntax-sparql5")))
  (run-syntax-tests "../tests/input/syntax-test.rq" "../tests/data-r2" "http://www.w3.org/2001/sw/DataAccess/tests/data-r2" test-sets))

(defun run-data-sparql11-syntax-tests (&optional (test-sets '("syntax-query" "syntax-update-1" "syntax-update-2")))
  (run-syntax-tests "../tests/input/syntax-test.rq" "../tests/data-sparql11" "http://www.w3.org/2009/sparql/docs/tests/data-sparql11" test-sets))

(defun run-all-syntax-tests ()
  (run-syntax-tests "../tests/input/syntax-test.rq" "../tests/data-r2" "http://www.w3.org/2001/sw/DataAccess/tests/data-r2" t)
  (run-syntax-tests "../tests/input/syntax-test.rq" "../tests/data-sparql11" "http://www.w3.org/2009/sparql/docs/tests/data-sparql11" t))

;; (defun run-all-syntax-tests ()
;;   (sparql-call "instans:execute_system" "/Users/enu/instans/tests/input/syntax-test-runner.rq" )

;(progn (untrace) (trace rete-add token-value make-token add-token remove-token add-alpha-token remove-alpha-token add-beta-token remove-beta-token))
