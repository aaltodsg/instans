;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun compile-sparql-file (file &key instans instans-name rete-html-output base)
  (with-open-file (input-stream file)
    (setf instans (compile-sparql-stream input-stream :instans instans :instans-name instans-name :base base))
    (when (and instans rete-html-output) (output-rete-html-page instans rete-html-output))
    instans))

(defun compile-sparql-stream (stream &key instans instans-name base)
  (when (null instans)
    (setf instans (make-instance 'instans :name instans-name)))
  (setf (instans-algebra-expr-list instans) nil)
  (let* ((ll-parser (parse-sparql-stream instans stream :base base)))
    (cond ((not (ll-parser-succeeded-p ll-parser))
	   (instans-add-status instans 'instans-rule-parsing-failed (ll-parser-error-messages ll-parser))
	   (return-from compile-sparql-stream nil))
	  (t
	   (instans-add-status instans 'instans-rule-parsing-succeeded)
	   (instans-debug-message instans :sparql-parsing "Parsed ~S" (first (ll-parser-result-stack ll-parser)))
	   (setf (instans-algebra-expr-list instans) (filter-not #'(lambda (x) (member (car x) '(PREFIX BASE))) (first (ll-parser-result-stack ll-parser))))))
    (loop for algebra-expr in (instans-algebra-expr-list instans)
	  for canonic = (canonize-sparql-algebra-variables instans algebra-expr)
	  for new-nodes = (translate-sparql-algebra-to-rete instans canonic)
	  when (instans-find-status instans 'instans-rule-translation-failed)
	  do (return-from compile-sparql-stream nil)
	  else do (lisp-compile-nodes new-nodes))
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

(defun instans-add-rules (instans-iri rules &key (create-instans-p t) base)
  (let ((instans (if create-instans-p (create-instans instans-iri) (get-instans instans-iri))))
    (instans-debug-message instans :parse-rules "instans-add-rules ~S ~S :base ~S" instans-iri rules base)
    (cond ((sparql-error-p instans) nil)
	  ((file-or-uri-exists-p rules)
	   (let ((string (read-from-url-or-file rules)))
	     (instans-debug-message instans :parse-rules "~S" string)
	     (with-input-from-string (stream string)
	       (compile-sparql-stream stream :instans instans :base base)
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

(defun instans-add-query-input-processor (instans-iri input-iri &key graph base input-type)
  (let* ((instans (get-instans instans-iri)))
    (instans-debug-message instans '(:parse-rdf :execute) "instans-add-query-input-processor ~S ~S :input-type ~S :graph ~S :base ~S" instans-iri input-iri input-type graph base)
    (let* ((input-policy (instans-rdf-input-unit instans))
	   (processor (make-instance 'query-input-processor
				     :instans instans
				     :input-policy input-policy
				     :operations (instans-rdf-operations instans)
				     :base base
				     :graph graph))
	   (input-stream (create-input-stream input-iri))
	   (parser-creator (case input-type (:trig #'make-trig-parser) (:ttl #'make-turtle-parser) (t (error* "Unknown input type ~S" input-type))))
	   (callback (case input-policy
		       (:single (list :triple-callback #'(lambda (&rest input)
							   (process-query-input processor (list input))
;							   (ll-parser-yields nil)
							   )))
		       (:block (list :block-callback #'(lambda (inputs) (process-query-input processor inputs)
;							       (ll-parser-yields nil)
							       )))
		       (:document (list :document-callback #'(lambda (inputs) (process-query-input processor inputs))))
		       (t (error* "Illegal query input policy ~A" input-policy))))
	   (parser (apply parser-creator instans input-stream :base base callback)))
;      (make-turtle-parser )
      (setf (query-input-processor-parser processor) parser)
      (add-query-input-processor instans processor)
;      (push-to-end processor (instans-query-input-processors instans))
      )
    instans))

(defun instans-run (instans-iri &key select-output-name (select-output-type :csv) construct-output-name (construct-output-type :trig))
  (let ((instans (get-instans instans-iri)))
    (unless (instans-select-output-processor instans)
      (setf (instans-select-output-processor instans) (create-select-output-processor select-output-name select-output-type)))
    (unless (instans-construct-output-processor instans)
      (setf (instans-construct-output-processor instans) (create-construct-output-processor construct-output-name construct-output-type)))
    (run-query-input-processors instans)))

(defun instans-parse-rdf-file (instans-iri input-iri &key subscribe base graph triple-callback block-callback document-callback)
  (let ((instans (create-instans instans-iri))
	input-stream file-type error-msg)
    (instans-debug-message instans '(:parse-rdf :execute) "instans-parse-rdf-file ~S ~S" instans-iri input-iri)
    (unwind-protect
	 (progn
	   (multiple-value-setq (input-stream file-type error-msg) (create-input-stream input-iri))
	   (unless input-stream (error* error-msg))
	   (let ((rdf-parser (make-rdf-parser instans input-stream file-type
					      :subscribe subscribe :base base :graph graph
					      :triple-callback triple-callback :block-callback block-callback :document-callback document-callback)))
	     (instans-debug-message instans '(:execute :parse-rdf) "~%Reading RDF:~%")
	     (parse rdf-parser)
	     (cond ((ll-parser-succeeded-p rdf-parser)
		    (instans-add-status instans 'instans-rdf-parsing-succeeded))
		   (t
		    (instans-add-status instans 'instans-rdf-parsing-failed (ll-parser-error-messages rdf-parser))
		    ;(inform "~A:~A~%~%" input-iri (ll-parser-error-messages rdf-parser))
		    ))))
      (when input-stream (close input-stream)))
    instans))

(defun instans-compare-rdf-files (instans-iri input1-iri input2-iri &key base)
  (let* ((instans (or (get-instans instans-iri) (create-instans instans-iri)))
	 (result1 nil)
	 (result2 nil))
      (instans-parse-rdf-file instans-iri input1-iri :base base :document-callback #'(lambda (result) (setf result1 result)))
      (cond ((and input2-iri (not (sparql-unbound-p input2-iri)))
	     (instans-parse-rdf-file instans-iri input2-iri :base base :document-callback #'(lambda (result) (setf result2 result)))
	     (cond ((instans-has-status instans 'instans-rdf-parsing-failed)
		    nil)
		   ((rdf-graphs-isomorphic-p result1 result2)
		    (instans-add-status instans 'instans-rdf-compare-files-similar)
		    t)
		   (t
		    (instans-add-status instans 'instans-rdf-compare-files-not-similar)
		    nil)))
	    (t t))))

(defun instans-add-triples (instans-iri input-iri &key graph base)
  (let* ((instans (get-instans instans-iri))
	 input-stream file-type error-msg)
    (instans-debug-message instans '(:parse-rdf :execute) "instans-add-triples ~S ~S :graph ~S :base ~S" instans-iri input-iri graph base)
    (unwind-protect
	 (progn
	   (multiple-value-setq (input-stream file-type error-msg) (create-input-stream input-iri))
	   (unless input-stream (error* error-msg))
	   (flet ((handle-triples (triples)
		    (instans-debug-message instans :execute "~%Event callback: ~D triples~%~{ ~S~%~}"
					   (length triples) triples)
		    (process-query-input instans triples :ops '(:add :execute)
					 :graph (if (and graph (rdf-iri-equal graph *rdf-nil*)) nil graph))))
	     (let* ((kind (intern (string-upcase file-type) :keyword))
		    (rdf-parser (apply #'make-rdf-parser instans input-stream file-type
				       :base base
				       :graph graph
				       (if (member kind '(:trig :turtle))
					 (list :block-callback #'handle-triples)
					 (list :triple-callback #'(lambda (&rest triple) (handle-triples (list triple))))))))
	       (instans-debug-message instans '(:execute :parse-rdf) "~%Processing triples:~%")
	       (parse rdf-parser)
	       (parse rdf-parser)
	       (cond ((ll-parser-succeeded-p rdf-parser)
		      (instans-add-status instans 'instans-rdf-parsing-succeeded))
		     (t
		      (instans-add-status instans 'instans-rdf-parsing-failed (ll-parser-error-messages rdf-parser))
		      (inform "~A:~A~%~%" input-iri (ll-parser-error-messages rdf-parser))
		      )))))
      (when input-stream (close input-stream)))
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
    (format (instans-default-output instans) "~%execute_system rules=~A, triples=~A, expected_results=~A, graph=~A,base=~A~&" rules triples expected-results graph base)
    (or (instans-add-rules instans-iri rules :base base)
	(return-from instans-execute-system nil))
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
