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
	(warn "~%Processing triples triples:~%")
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

(defun instans-compile-rules (rules-iri &key output-directory)
  (let* ((bytes (drakma:http-request rules-iri))
	 (string (coerce (mapcar #'code-char (coerce bytes 'list)) 'string)))
    (inform "~S" string)
    (with-input-from-string (stream string)
;      (sparql-parse-stream stream)
      (compile-sparql-stream stream :input-name rules-iri :output-directory output-directory))))

(defun instans-add-rules (rete-iri rules-iri &key output-directory)
  (let ((network (instans-get-rete rete-iri)))
    (cond ((sparql-error-p network) network)
	  (t
	   (let* ((input-name (rdf-iri-string rules-iri))
		  (bytes (drakma:http-request input-name))
		  (string (coerce (mapcar #'code-char (coerce bytes 'list)) 'string)))
	     (inform "~S" string)
	     (with-input-from-string (stream string)
					;      (sparql-parse-stream stream)
	       (compile-sparql-stream stream :network network :input-name input-name :output-directory output-directory)))))))
