;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun compile-sparql-file (file &key instans instans-name rete-html-page-dir base)
  (with-open-file (input-stream file)
    (setf instans (compile-sparql-stream input-stream :input-name file :instans instans :instans-name instans-name :base base))
    (when (and instans rete-html-page-dir) (output-rete-html-page instans input-name rete-html-page-dir))
    instans))

(defun compile-sparql-stream (stream &key input-name instans instans-name base)
  (when (null instans)
    (setf instans (make-instance 'instans :name instans-name)))
  (let* ((ll-parser (sparql-parse-stream instans stream :base base))
	 (algebra-expr-list nil))
    (cond ((not (ll-parser-succeeded-p ll-parser))
	   (instans-add-status instans 'instans-rule-parsing-failed (ll-parser-error-messages ll-parser))
	   (return-from compile-sparql-stream nil))
	  (t
	   (instans-add-status instans 'instans-rule-parsing-succeeded)
	   (instans-debug-message instans :sparql-parsing "Parsed ~S" (first (ll-parser-result-stack ll-parser)))
	   (setf algebra-expr-list (filter-not #'(lambda (x) (member (car x) '(PREFIX BASE))) (first (ll-parser-result-stack ll-parser))))))
    (loop for algebra-expr in algebra-expr-list
	  for canonic = (canonize-sparql-algebra-variables instans algebra-expr)
	  for new-nodes = (translate-sparql-algebra-to-rete instans canonic)
	  when (instans-find-status instans 'instans-rule-translation-failed)
	  do (return-from compile-sparql-stream nil)
	  else do (lisp-compile-nodes instans new-nodes))
    (instans-add-status instans 'instans-rule-translation-succeeded)
    instans))

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

(defvar *instanssi*)

(defun instans-add-rules (instans-iri rules &key (create-instans-p t))
  (instans-debug-message instans :parse-rules "instans-add-rules ~S ~S :base ~S" instans-iri rules base)
  (let ((instans (if create-instans-p (create-instans instans-iri) (get-instans instans-iri))))
    (cond ((sparql-error-p instans) nil)
	  ((file-or-uri-exists-p rules)
	   (let ((string (read-from-url-or-file rules)))
	     (instans-debug-message instans :parse-rules "~S" string)
	     (with-input-from-string (stream string)
	       (compile-sparql-stream stream :instans instans :input-name (if (stringp rules) rules (rdf-iri-string rules)) :base base)
	       (cond ((instans-find-status instans 'instans-rule-translation-succeeded)
		      (initialize-execution instans)
		      instans)
		     (t
		      nil)))))
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

(defun instans-add-triple-processor (instans-iri input &key graph base output policies)
  (declare (ignorable instans-iri input graph base output policies))
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
;;     (setf (triple-processor-parser processor)
;; 	    (make-turtle-parser :triples-block-callback #'(lambda (triples) (process-triples processor triples) (ll-parser-yields nil))))
;;     (add-triple-processor instans processor))))

(defun instans-run (instans-iri)
  (declare (ignorable instans-iri))
  nil)
;; (let ((instans (get-instans instans-iri)))
;;   (run-triple-processors instans)))

(defun instans-add-triples (instans-iri input &key graph base)
  (instans-debug-message instans '(:parse-triples :execute) "instans-add-triples ~S ~S :graph ~S :base ~S" instans-iri input graph base))
  (let* ((instans (get-instans instans-iri))
	 (string (stream-contents-to-string (create-input-stream input))))
    (with-input-from-string (triples-stream string)
      (let ((triples-parser (make-turtle-parser instans triples-stream
						:base base
						:triples-block-callback #'(lambda (triples)
									    (instans-debug-message instans :execute "~%Event callback: ~D triples~%~{ ~S~%~}"
												   (length triples) triples)
									    (process-triple-input instans triples :ops '(:add :execute) :graph (if (and graph (rdf-iri-equal graph *rdf-nil*)) nil graph))))))
	(instans-debug-message instans '(:execute :parse-triples) "~%Processing triples:~%")
	;; Is this OK?;	(initialize-execution instans)
	(parse triples-parser)
	(unless (ll-parser-succeeded-p triples-parser)
	  (inform "~A:~A" input (instans-error-message instans)))
	(report-execution-status instans)))
    instans))

(defvar *instans-execute-system-previous-rules* nil)
(defvar *instans-execute-system-previous-triples* nil)
(defvar *instans-execute-system-previous-expected-results* nil)
(defvar *instans-execute-system-previous-graph* nil)
(defvar *instans-execute-system-previous-base* nil)

(defun instans-execute-system (rules &key triples expected-results graph base (use-previous-args-p nil))
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
	(instans-add-rules instans-iri rules :base base)
      (declare (ignore add-rules-result))
      (when (not (null error))
	(return-from instans-execute-system (values nil error))))
    (if triples
	(instans-add-triples instans-iri triples :graph graph :base base))))

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
		    (instans-execute-system rules :triples (parse-iri manifest-iri-string) :base (parse-iri base-iri-string)))))))

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
