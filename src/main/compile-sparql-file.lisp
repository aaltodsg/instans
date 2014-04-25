;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun compile-sparql-file (file &key instans instans-name rete-html-page-dir base subscribe)
  (with-open-file (input-stream file)
    (compile-sparql-stream input-stream :input-name file :instans instans :instans-name instans-name
			   :rete-html-page-dir rete-html-page-dir :base base :subscribe subscribe)))

(defun compile-sparql-stream (stream &key input-name instans instans-name rete-html-page-dir (make-rete-html-page-script (find-make-rete-html-script)) base subscribe)
  (declare (special *node-color-alist*))
  (setf *node-color-alist* nil)
  (when (null instans)
    (setf instans (make-instance 'instans :name instans-name)))
  (let* ((ll-parser (sparql-parse-stream instans stream :base base :subscribe subscribe))
	 (colors (list "Black" "Red" "Blue" "Green" "Orange"))
	 (algebra-expr-list nil))
    (cond ((not (ll-parser-succeeded-p ll-parser))
	   (setf (instans-error-messages instans) (ll-parser-error-messages ll-parser))
	   (return-from compile-sparql-stream (values nil (instans-error-message instans))))
	  (t
	   (when (debugp subscribe :sparql-parsing)
	     (inform "Parsed ~S" (first (ll-parser-result-stack ll-parser))))
	   (setf algebra-expr-list (filter-not #'(lambda (x) (member (car x) '(PREFIX BASE))) (first (ll-parser-result-stack ll-parser))))))
    (setf (rest (last colors)) colors)
    (loop for algebra-expr in algebra-expr-list
	  for color in colors
	  do (multiple-value-bind (new-nodes error-msg)
		 (compile-sparql-algebra-expr instans algebra-expr :color color :show-transform-p subscribe)
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
	(when subscribe
	  (inform "Running ~S on ~S" make-rete-html-page-script input-name))
	(shell-script make-rete-html-page-script input-name rete-html-page-dir)))
      instans))

(defun compile-sparql-algebra-expr (instans algebra-expr &key (color "Black") show-transform-p)
  (declare (special *node-color-alist*))
  (when show-transform-p
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
	 ;do (inform "compiling ~A" node)
	    do (push-to-end (cons node color) *node-color-alist*)
	    do (cond ((filter-node-p node)
		      (let ((filter-lambda `(lambda ,(sparql-var-lisp-names (node-use node)) ;(let ((v 
					      (eq ,(sparql-expr-to-lisp (filter-test node)) t))))
			(setf (filter-test-lambda node) filter-lambda)
			(setf (filter-test-func node) (compile nil filter-lambda))))
		     ((bind-node-p node)
;		      (inform "bind-form ~A = ~A" node (bind-form node))
		      (let ((bind-lambda `(lambda ,(sparql-var-lisp-names (node-use node)) ,(sparql-expr-to-lisp (bind-form node)))))
;			(warn "bind-lambda = ~S" bind-lambda)
			(setf (bind-form-lambda node) bind-lambda)
			(setf (bind-form-func node) (compile nil bind-lambda))))
		     ((aggregate-join-node-p node)
		      (let* ((key-lambda `(lambda ,(sparql-var-lisp-names (aggregate-join-key-vars node))
					    (list ,@(loop for key-expr in (aggregate-join-key-exprs node)
						       collect (sparql-expr-to-lisp key-expr)))))
			     (group-arg (gensym "GROUP"))
			     (instans-arg (gensym "INSTANS"))
			     (aggrs-temp (gensym "AGGRS"))
			     (aggr-temp (gensym "AGGR"))
			     (aggr-add-lambda `(lambda (,instans-arg ,group-arg ,@(sparql-var-lisp-names (aggregate-join-aggr-vars node)))
						 (declare (ignorable ,instans-arg))
;						 (inform "begin aggr-add~%")
						 ,@(loop for arg in (sparql-var-lisp-names (aggregate-join-aggr-vars node))
						      collect `(inform "aggr-add ~A = ~A" ',arg ,arg))
						 (inform "")
						 ;;; aggr-expr = (append (list sparql-op op-name group-var index) args)
						 (let ((,aggrs-temp (or (group-aggregates ,group-arg)
									(setf (group-aggregates ,group-arg)
									      (list ,@(loop for aggr-expr in (aggregate-join-aggr-exprs node)
;											 do (inform "aggr-expr = ~A" aggr-expr)
											 collect (cond ((eq (second aggr-expr) 'SAMPLE)
													`(make-instance 'aggregate-sample))
												       ((eq (second aggr-expr) 'COUNT)
													`(make-instance 'aggregate-count))
												       ((eq (second aggr-expr) 'SUM)
													`(make-instance 'aggregate-sum))
												       ((eq (second aggr-expr) 'AVG)
													`(make-instance 'aggregate-avg))
												       ((eq (second aggr-expr) 'MIN)
													`(make-instance 'aggregate-min))
												       ((eq (second aggr-expr) 'MAX)
													`(make-instance 'aggregate-max))
												       ((eq (second aggr-expr) 'GROUP_CONCAT)
													`(make-instance 'aggregate-group-concat :separator (sparql-expr-to-lisp (sixth aggr-expr)))))))))))
						   ,@(loop for aggr-expr in (aggregate-join-aggr-exprs node)
							collect `(let ((,aggr-temp (pop ,aggrs-temp)))
;								   (inform "add-value ~A to ~A" ',(sparql-expr-to-lisp (fifth aggr-expr)) ,aggr-temp)
								   (aggregate-add-value ,aggr-temp ,(sparql-expr-to-lisp (fifth aggr-expr)))))
;						   (inform "end aggr-add~%")
						   )))
			     (aggr-remove-lambda `(lambda (,instans-arg ,group-arg ,@(sparql-var-lisp-names (aggregate-join-aggr-vars node)))
						    (declare (ignorable ,instans-arg))
						    ,@(loop for arg in (sparql-var-lisp-names (aggregate-join-aggr-vars node))
							 collect `(inform "aggr-remove ~A = ~A" ',arg ,arg))
						    (let ((,aggrs-temp (group-aggregates ,group-arg)))
						      ,@(loop for aggr-expr in (aggregate-join-aggr-exprs node)
							   collect `(let ((,aggr-temp (pop ,aggrs-temp)))
;								      (inform "remove-value ~A from ~A" ',(sparql-expr-to-lisp (fifth aggr-expr)) ,aggr-temp)
								      (aggregate-remove-value ,aggr-temp ,(sparql-expr-to-lisp (fifth aggr-expr)))))))))
;			(inform "(aggregate-join-key-lambda node ~A) = ~%~A" node key-lambda)
;			(inform "(aggregate-join-aggr-add-lambda node ~A) = ~%~A" node aggr-add-lambda)
;			(inform "(aggregate-join-aggr-remove-lambda node ~A) = ~%~A" node aggr-remove-lambda)
			(setf (aggregate-join-key-lambda node) key-lambda)
			(setf (aggregate-join-key-func node) (compile nil key-lambda))
			(setf (aggregate-join-aggr-add-lambda node) aggr-add-lambda)
			(setf (aggregate-join-aggr-add-func node) (compile nil aggr-add-lambda))
			(setf (aggregate-join-aggr-remove-lambda node) aggr-remove-lambda)
			(setf (aggregate-join-aggr-remove-func node) (compile nil aggr-remove-lambda))
			))
		     ((modify-node-p node)
		      (when show-transform-p
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
	 (intern (string (uniquely-named-object-name expr)) :instans))
	(t expr)))

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

(defvar *instanssi*)
(defun instans-add-rules (instans-iri rules &key rete-html-page-dir base subscribe (create-instans-p t))
  (when (debugp subscribe :parse-rules)
    (inform "instans-add-rules ~S ~S :rete-html-page-dir ~S :base ~S" instans-iri rules rete-html-page-dir base))
  (let ((instans (if create-instans-p (create-instans instans-iri) (get-instans instans-iri))))
    (cond ((sparql-error-p instans) instans)
	  ((file-or-uri-exists-p rules)
	   (let ((string (read-from-url-or-file rules)))
	     (when (debugp subscribe :parse-rules)
	       (inform "~S" string))
	     (with-input-from-string (stream string)
	       (multiple-value-bind (compile-result error)
		   (compile-sparql-stream stream :instans instans :input-name (if (stringp rules) rules (rdf-iri-string rules))
					  :rete-html-page-dir rete-html-page-dir :base base :subscribe subscribe)
		 (declare (ignorable compile-result))
		 (cond ((not error)
			(values t nil))
		       (t
			(values nil error)))))))
	  (t
	   (inform "Cannot read SPARQL from ~S" rules)
	   nil))))

(defun file-type (file)
  (cond ((pathnamep file) (pathname-type file))
	((rdf-iri-p file) (file-type (rdf-iri-path file)))
	((http-or-file-iri-string-p file) (file-type (parse-iri file)))
	((stringp file) (file-type (pathname file)))
	(t
	 nil)))
	 

(defun create-output-stream (spec)
  (cond ((listp spec)
	 (let* ((file (getf spec :file))
		(iri (getf spec :iri))
		(type (or (getf spec :type) (file-type (or file iri)))))
	   (if iri (values nil nil (format nil "Cannot output to IRI (~A) yet" iri))
	       (values (open file :if-exists :supersede) type))))
	((file-iri-string-p spec)
	 (create-output-stream (list :file (probe-file (subseq spec 7)))))
	((http-iri-string-p spec)
	 (create-output-stream (list :iri (parse-iri spec))))
	((stringp spec)
	 (create-output-stream (list :file (probe-file spec))))
	((pathnamep spec)
	 (create-output-stream (list :file spec)))
	(t
	 (error* "Cannot create output file from '~A'" spec))))

(defun create-input-stream (input)
  (cond ((rdf-iri-p input)
	 (cond ((member (rdf-iri-scheme input) '("http" "https") :test #'equal)
		(values (make-string-input-stream (http-get-to-string (rdf-iri-string input))) (file-type input)))
	       ((equal (rdf-iri-scheme input) "file")
		(values (open (rdf-iri-path input) :direction :input) (file-type input)))
	       (t (values nil nil (format nil "Cannot create an input stream based on ~S" input)))))
	((http-or-file-iri-string-p input)
	 (create-input-stream (parse-iri input)))
	((or (stringp input) (pathnamep input))
	 (values (open input) (file-type input)))
	(t (values nil nil (format nil "Cannot create an input stream based on ~S" input)))))

(defun create-triple-input-stream (spec)
  (cond ((listp spec)
	 (let* ((file (getf spec :file))
		(iri (getf spec :iri))
		(type (getf spec :type))
		(input-policy (getf spec :triple-input-policy)))
	   (multiple-value-bind (stream name-type error-message) (create-input-stream (or file iri))
	     (cond ((not error-message)
		    (values stream (or type name-type) input-policy))
		   (t
		      (values nil nil nil error-message))))))
	(t
	 (multiple-value-bind (stream type error-message) (create-input-stream spec)
	   (cond ((not error-message)
		  (values stream type))
		 (t
		  (values nil nil nil error-message)))))))

(defun instans-add-triple-processor (instans-iri input &key graph base subscribe output policies)
  (declare (ignorable instans-iri input graph base subscribe output policies))
  nil)
  ;; (let* ((instans (get-instans instans-iri))
  ;; 	 input-stream input-type error-message)
  ;;   (multiple-value-setq (input-stream input-type error-message) (create-input-stream input))
  ;;   (when error-message (error* error-message))
  ;;   (let ((processor (make-instance 'triple-processor
  ;; 				    :instans instans
  ;; 				    :input-policy (getf policies :triple-input-policy)
  ;; 				    :operations (getf policies :triple-processing-operations)
  ;; 				    :graph graph
  ;; 				    :base base
  ;; 				    :lexer (make-instance 'turtle-lexer :instans instans :input-stream input-stream :base base)
  ;; 				    :subscribe (debugp subscribe :parse-triples))))
  ;;     (setf (triple-processor-parser processor)
  ;; 	    (make-turtle-parser :triples-block-callback #'(lambda (triples) (process-triples processor triples) (ll-parser-yields nil))))
  ;;     (add-triple-processor instans processor))))

(defun instans-run (instans-iri)
  (declare (ignorable instans-iri))
  nil)
  ;; (let ((instans (get-instans instans-iri)))
  ;;   (run-triple-processors instans)))

(defun instans-add-triples (instans-iri input &key select-processor expected-results graph base subscribe reporting report-execution-status-p)
  (when (debugp subscribe :parse-triples :execute)
    (inform "instans-add-triples ~S ~S :graph ~S :base ~S :subscribe ~S :reporting ~S" instans-iri input graph base subscribe reporting))
  (let* ((instans (get-instans instans-iri))
	 (comparep (and expected-results (not (rdf-iri-equal expected-results *rdf-nil*))))
	 (expected-query-results (if comparep (if (stringp expected-results) (parse-results-file instans expected-results) (parse-results-from-url instans expected-results))))
	 (expected-result-list (if comparep (sparql-query-results-results expected-query-results)))
	 (observed-result-list (list nil))
	 (observed-result-list-tail observed-result-list)
	 (compare-function (if comparep #'(lambda (node token)
	 				    (let ((solution (make-instance 'sparql-result
	 								   :bindings (loop for canonic-var in (node-use (node-prev node))
	 										for var = (reverse-resolve-binding instans canonic-var)
	 										collect (make-instance 'sparql-binding :variable var :value (token-value node token canonic-var))))))
	 				      (inform "Node ~S, (node-use (node-prev node)) ~S, token ~S, solution ~S" node (node-use (node-prev node)) token solution)
	 				      (setf (cdr observed-result-list-tail) (list solution))
	 				      (setf observed-result-list-tail (cdr observed-result-list-tail))))))
	 (observed-query-results (make-instance 'sparql-query-results))
	 (string (stream-contents-to-string (create-input-stream input))))
    (setf (instans-rule-instance-removal-policy instans) :remove)
    (setf (rule-instance-queue-report-p (instans-rule-instance-queue instans)) reporting)
    (when compare-function
      (setf (instans-select-compare-function instans) compare-function))
    (setf (instans-select-processor instans) select-processor)
    (with-input-from-string (triples-stream string)
      (let ((triples-parser (make-turtle-parser instans triples-stream :base base :subscribe subscribe
						:triples-block-callback #'(lambda (triples)
									    (when (debugp subscribe :execute)
									      (inform "~%Event callback: ~D triples~%" (length triples))
									      (loop for tr in triples do (inform " ~S~%" tr)))
									    (process-triple-input instans triples :ops '(:add :execute) :graph (if (and graph (rdf-iri-equal graph *rdf-nil*)) nil graph))))))
	(when (debugp subscribe :execute :parse-triples)
	  (inform "~%Processing triples:~%"))
	;; Is this OK?
	(initialize-execution instans)
	(parse triples-parser)
	(unless (ll-parser-succeeded-p triples-parser)
	  (inform "~A:~A" input (instans-error-message instans)))
	(when report-execution-status-p
	  (report-execution-status instans))
	(pop observed-result-list)
	(when (debugp subscribe :execute)
	  (inform "Expected-results ~S" expected-results)
	  (inform "Expected ~S" expected-result-list)
	  (inform "Observed-result-list ~S~%Observed-query-results ~S" observed-result-list observed-query-results))
	(when (debugp subscribe :execute)
	  ;; (sparql-query-results-to-json instans observed-query-results)
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
			       subscribe reporting (use-previous-args-p nil))
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
	(instans-add-rules instans-iri rules :rete-html-page-dir rete-html-page-dir :base base :subscribe subscribe)
      (declare (ignore add-rules-result))
      (when (not (null error))
	(return-from instans-execute-system (values nil error))))
    (if triples
	(instans-add-triples instans-iri triples :graph graph :base base :subscribe subscribe :reporting reporting))))

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
		    (instans-execute-system rules :triples (parse-iri manifest-iri-string) :base (parse-iri base-iri-string)
					    :subscribe nil :reporting '(:all) :rete-html-page-dir output-dir))))))

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
