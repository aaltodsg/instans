;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun compile-sparql-file (file &key network network-name output-directory (mkhtml-script "mk-html1"))
  (with-open-file (input-stream file)
    (apply #'compile-sparql-stream input-stream :input-name file :network network :network-name network-name :output-directory output-directory :mkhtml-script mkhtml-script)))

(defun compile-sparql-stream (stream &key input-name network network-name output-directory (mkhtml-script "mk-html1") (parser #'sparql-parse-stream))
  (declare (special *node-color-alist*))
  (setf *node-color-alist* nil)
  (when (null network)
    (setf network (make-instance 'network :name network-name :bindings (make-bindings))))
  (let* ((parsing (funcall parser stream))
	 (colors (list "Black" "Red" "Blue" "Green" "Orange"))
	 (*gen-var-counter* 0)
	 (algebra-expr-list nil))
    (cond ((not (parsing-succeeded-p parsing))
	   (inform "~%~@[~A: ~]~A~%" input-name (parsing-error-message parsing))
	   (return-from compile-sparql-stream parsing))
	  (t
	   (inform "Parsed ~S" (first (parsing-result-stack parsing)))
	   (setf algebra-expr-list (filter-not #'(lambda (x) (member (car x) '(PREFIX BASE))) (first (parsing-result-stack parsing))))))
    (setf (rest (last colors)) colors)
    (loop for algebra-expr in algebra-expr-list
	  for color in colors
	  do (compile-sparql-algebra-expr network algebra-expr color))
    (unless (null output-directory)
      (let* ((name-part (pathname-name input-name))
	     (truedirname (pathname-directory (truename output-directory)))
	     (dot-output-file (make-pathname :directory truedirname :name name-part :type "dot"))
	     (sa-output-file (make-pathname :directory truedirname :name name-part :type "sa")))
	(with-open-file (out sa-output-file :direction :output :if-exists :supersede)
	  (loop for expr in algebra-expr-list
		do (let* ((*print-circle* nil)
			  (*print-pretty* t)
			  (*print-right-margin* 110))
		     (print expr *error-output*)
		     (let ((string (with-output-to-string (str) (print expr str))))
		       (setf string (cl-ppcre:regex-replace-all ">" (cl-ppcre:regex-replace-all "<"  string "&lt;") "&gt;"))
		       (print string *error-output*)
		       (format out "~A" string)))))
	(print-dot-file network dot-output-file :html-labels-p nil)
	(assert (probe-file mkhtml-script))
	(inform "Running ~S on ~S" mkhtml-script input-name)
	(shell-script mkhtml-script input-name output-directory)))
      network))

(defun compile-sparql-algebra-expr (network algebra-expr &optional (color "Black"))
  (declare (special *node-color-alist*))
  (inform "compiling ~S~%" algebra-expr)
  (let* ((canonic (canonize-sparql-algebra-variables algebra-expr (network-bindings network)))
	 (new-nodes (translate-sparql-algebra-to-rete canonic network)))
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
		    (setf (modify-delete-func node)
			  (and (rest (modify-delete-template node))
			       (compile nil `(lambda (,(first (modify-delete-template node))
						      ,@(mapcar #'second (modify-delete-parameters node)))
					       ,@(rest (modify-delete-template node))))))
		    (setf (modify-insert-func node)
			  (and (rest (modify-insert-template node))
			       (compile nil `(lambda (,(first (modify-insert-template node))
						      ,@(mapcar #'second (modify-insert-parameters node)))
					       ,@(rest (modify-insert-template node)))))))))
    (initialize-new-nodes network new-nodes)
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

(defun report-execution (network)
  (let ((queue (network-rule-instance-queue network)))
    (inform "Read ~D inputs~&Added ~D quads~&Removed ~D quads~&Executed ~D rules: ~D select rules, ~D modify rules, and ~D construct rules"
	    (network-input-count network)
	    (network-add-quad-count network)
	    (network-remove-quad-count network)
	    (rule-instance-queue-execute-count queue)
	    (rule-instance-queue-select-count queue)
	    (rule-instance-queue-modify-count queue)
	    (rule-instance-queue-construct-count queue))
    network))

(defun build-and-execute-sparql-system (rules-file triples-file &key (report-function ) (report-function-arguments ) (output-directory "/Users/enu/instans/tests/output"));  (show-turtle-parse-p nil))
  (catch 'done
  (let* ((network nil))
    (with-open-file (triples-stream triples-file)
      (setf network (compile-sparql-file rules-file :output-directory output-directory))
      (setf (network-select-function network) report-function)
      (setf (network-select-function-arguments network) report-function-arguments)
      (let* ((triples-lexer (make-turtle-lexer triples-stream))
	     (triples-parser (make-turtle-parser :triples-callback #'(lambda (triples)
								       (inform "~%Event callback: ~D triples~%" (length triples))
								       (loop for tr in triples do (inform " ~S~%" tr))
								       (process-triple-input network triples '(:add :execute))))))
	(initialize-execution network)
	(warn "~%Processing triples:~%")
	(time (funcall triples-parser triples-lexer))
	network)))))

(defgeneric process-triple-input (network triples ops &key graph)
  (:method ((this network) triples ops &key graph)
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

(defvar *retes* (make-hash-table :test #'equal))

(defun instans-create-rete ()
  (let* ((rete-iri (parse-iri (format nil "http://www.cse.aalto.fi/instans/retes/~A" (string (gensym "RETE")))))
	 (rete-name (rdf-iri-string rete-iri)))
    (setf (gethash rete-name  *retes*) (make-instance 'network :name rete-name :bindings (make-bindings)))
    rete-iri))

(defun instans-get-rete (rete-iri)
  (let ((rete-name (rdf-iri-string rete-iri)))
    (or (gethash rete-name *retes*)
	(sparql-error "RETE named ~S does not exist!" rete-name))))

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
	 (cond ((or (and (>= (length name) 5) (or (string= (subseq name 0 5) "http:") (string= (subseq name 0 5) "file:")))
		    (and (>= (length name) 6) (string= (subseq name 0 6) "https:")))
		(read-from-url-or-file (parse-iri name)))
	       (t
		(read-from-url-or-file (parse-namestring name)))))
	(t (error* "Cannot read from ~S" name))))

(defun instans-add-rules (rete-iri rules &key output-directory report-function report-function-arguments)
  (let ((network (instans-get-rete rete-iri)))
    (cond ((sparql-error-p network) network)
	  (t
	   (let ((string (read-from-url-or-file rules)))
	     (inform "~S" string)
	     (with-input-from-string (stream string)
					;      (sparql-parse-stream stream)
	       (compile-sparql-stream stream :network network :input-name (if (stringp rules) rules (rdf-iri-string rules)) :output-directory output-directory)
	       (when report-function
		 (setf (network-select-function network) report-function))
	       (when report-function-arguments
		 (setf (network-select-function-arguments network) report-function-arguments))
	       (initialize-execution network)
	       rete-iri))))))

(defun instans-add-triples-from-url (rete-iri triples graph)
  (let ((network (instans-get-rete rete-iri)))
    (cond ((sparql-error-p network) network)
	  (t
	   (let ((string (read-from-url-or-file triples)))
	     (inform "~S" string)
	     (with-input-from-string (triples-stream string)
	       (let* ((triples-lexer (make-turtle-lexer triples-stream))
		      (triples-parser (make-turtle-parser :triples-callback #'(lambda (triples)
;										(inform "~%Event callback: ~D triples~%" (length triples))
;										(loop for tr in triples do (inform " ~S~%" tr))
										(process-triple-input network triples '(:add :execute) :graph (if (equalp graph "DEFAULT") nil graph))))))
		 (warn "~%Processing triples:~%")
		 (time (funcall triples-parser triples-lexer))
		 rete-iri)))))))

(defun instans-execute-system (rules triples expected-results graph)
  (inform "execute_system ~A ~A ~A ~A" rules triples expected-results graph)
;  (handler-case 
      (flet ((show-solutions (sl) (loop for s in sl do (inform "Solution: ~{~A~^ ~}" (sparql-result-bindings s)))))
	(let* ((rete-iri (instans-create-rete))
	       (comparep (not (null expected-results)))
	       (expected-query-results (if comparep (if (stringp expected-results) (parse-srx-file expected-results) (parse-srx-from-url expected-results))))
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
	  (instans-add-rules rete-iri rules :report-function report-function :output-directory "/Users/enu/instans/tests/output")
	  (instans-add-triples-from-url rete-iri triples graph)
	  (pop observed-result-list)
	  (cond ((not comparep) t)
		((and (= (length observed-result-list) (length expected-result-list))
		      (every #'(lambda (r1 r2) (sparql-result-equal r1 r2)) observed-result-list expected-result-list))
		 (inform "Expected and observed solutions are equal")
		 (values t t))
		((not (= (length observed-result-list) (length expected-result-list)))
		 (inform "Observed ~D results, expected ~D results" (length observed-result-list) (length expected-result-list))
		 (values nil nil))
		(t
		 (let ((observed-minus-expected (set-difference observed-result-list expected-result-list :test #'sparql-result-equal))
		       (expected-minus-observed (set-difference expected-result-list observed-result-list :test #'sparql-result-equal)))
		   (cond ((and (null observed-minus-expected) (null expected-minus-observed))
			  (inform "Expected and observed solutions same, but in a different order")
			  (inform "Expected:")
			  (show-solutions expected-result-list)
			  (inform "Observed:")
			  (show-solutions observed-result-list)
			  (values t nil))
			 (t
			  (inform "Expected solutions not observed:")
			  (show-solutions expected-minus-observed)
			  (inform "Observed solutions not expected:")
			  (show-solutions observed-minus-expected))
			 (values nil nil)))))))
)
;)
;    (t (e) (values nil (sparql-error "Got an error: ~S" e)))))
  