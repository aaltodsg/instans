;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun compile-sparql-file (file &key instans instans-name rete-html-file base)
  (with-open-file (input-stream file)
    (setf instans (compile-sparql-stream input-stream :instans instans :instans-name instans-name :base base))
    (when (and instans rete-html-file) (output-rete-html-page instans rete-html-file))
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
	  do (push-to-end canonic (instans-canonic-algebra-expr-list instans))
	  when (instans-find-status instans 'instans-rule-translation-failed)
	  do (return-from compile-sparql-stream nil)
	  else do (lisp-compile-nodes new-nodes))
;    (instans-add-status instans 'instans-rule-translation-succeeded)
    (optimize-rete-network instans)
    instans))

(defvar *instanses*)
(eval-when (:load-toplevel :execute)
  (setf *instanses*  (make-hash-table :test #'equal)))

(defvar *current-instans* nil)

(defun create-instans (&optional instans-iri)
  (unless instans-iri (setf instans-iri (parse-iri (format nil "http://www.instans.org/~A" (string (gensym "INSTANS"))))))
  (let* ((instans-name (rdf-iri-string instans-iri))
	 (instans (make-instance 'instans :name instans-name)))
    (setf (gethash instans-name  *instanses*) instans)
    (values instans instans-iri)))

(defun get-instans (instans-iri)
  (let ((instans-name (rdf-iri-string instans-iri)))
    (gethash instans-name *instanses*)))

(defun get-or-create-instans (instans-iri)
  (or (get-instans instans-iri) (create-instans instans-iri)))

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

(defun instans-set-output-processors (instans)
  ;; (describe instans)
  (when (and (instans-ask-output-type instans) (instans-ask-output-name instans) (null (instans-ask-output-processor instans)))
    ;; (inform "(instans-ask-output-type instans) ~A (instans-ask-output-name instans) ~A" (instans-ask-output-type instans) (instans-ask-output-name instans))
    (setf (instans-ask-output-processor instans) (create-ask-output-processor instans (instans-ask-output-name instans) (instans-ask-output-type instans) :appendp (instans-ask-output-append-p instans))))
  (when (and (instans-select-output-type instans) (null (instans-select-output-processor instans)))
    (setf (instans-select-output-processor instans) (create-select-output-processor instans (instans-select-output-name instans) (instans-select-output-type instans) :appendp (instans-select-output-append-p instans))))
  (when (and (instans-construct-output-type instans) (null (instans-construct-output-processor instans)))
    (setf (instans-construct-output-processor instans) (create-construct-output-processor instans (instans-construct-output-name instans) (instans-construct-output-type instans) :appendp (instans-construct-output-append-p instans)))))

(defun instans-add-rules (instans rules &key (base (instans-base instans)) (output-options-stream nil))
    (instans-debug-message instans :parse-rules "instans-add-rules ~S ~S :base ~S" (instans-name instans) rules base)
    (instans-set-output-processors instans)
    (when output-options-stream
      (format output-options-stream " --rules=~A" rules))
    (cond ((sparql-error-p instans) nil)
	  (t
	   (let ((string (cond ((file-or-uri-exists-p rules)
				(read-from-url-or-file rules))
			       ((stringp rules) rules)
			       (t (error* "Cannot read SPARQL from ~S" rules)))))
	     (instans-debug-message instans :parse-rules "~S" string)
	     (with-input-from-string (stream string)
	       (compile-sparql-stream stream :instans instans :base base)
	       (cond ((instans-find-status instans 'instans-rule-translation-succeeded)
		      (initialize-execution instans))
		     (t nil)))))))

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
	       (values (open-file file :if-exists :supersede :message "create-output-stream: open ~{~A~^ ~}") type))))
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
		(values (open-file (rdf-iri-path input) :direction :input :message "create-input-processor: open ~{~A~^ ~}") (file-type input)))
	       (t (values nil nil (format nil "Cannot create an input stream based on ~S" input)))))
	((http-or-file-iri-string-p input)
	 (create-input-stream (parse-iri input)))
	((or (stringp input) (pathnamep input))
	 (values (open-file input :message "create-input-stream ~{~A~^ ~}") (file-type input)))
	(t (values nil nil (format nil "Cannot create an input stream based on ~S" input)))))

(defun instans-add-stream-input-processor (instans input-iri &key (graph (instans-graph instans)) (base (instans-base instans)) input-type subscribe (output-options-stream nil))
  (instans-debug-message instans '(:parse-rdf :execute) "instans-add-stream-input-processor ~S ~S :input-type ~S :graph ~S :base ~S" (instans-name instans) input-iri input-type graph base)
  (when output-options-stream
    (format output-options-stream " ~@[--graph=~A ~]~@[--base=~A ~]--input-~(~A~)=~A" (and graph (rdf-iri-string graph)) (and base (rdf-iri-string base)) input-type input-iri))
  (multiple-value-bind (processor-type parser-creator)
      (case input-type
	(:trig (values 'instans-trig-input-processor #'make-trig-parser))
	(:ttl (values 'instans-turtle-input-processor #'make-turtle-parser))
	(:nt (values 'instans-nt-input-processor #'make-n-triples-parser))
	(:nq (values 'instans-nq-input-processor #'make-n-quads-parser))
	(t (error* "Unknown input type ~S" input-type)))
    (let* ((input-unit (instans-rdf-input-unit instans))
	   (processor (make-instance processor-type
				     :name input-iri
				     :instans instans
				     :input-unit input-unit
				     :operations (instans-rdf-operations instans)
				     :base base
				     :graph graph
				     :subscribe subscribe))
	   (input-stream (create-input-stream input-iri))
	   (parser nil)
	   (callback (case input-unit
		       (:single (list :triple-callback #'(lambda (&rest input)
							   (process-query-input processor (list input))
					;							   (setf (ll-parser-print-snapshot-p parser) t)
							   (ll-parser-yields)
							   input
							   )))
		       (:block (list :block-callback #'(lambda (inputs)
							 (process-query-input processor inputs)
							 (ll-parser-yields)
							 inputs
							 )))
		       (:document (list :document-callback #'(lambda (inputs) (process-query-input processor inputs))))
		       (t (error* "Illegal query input unit ~A" input-unit))))
	   (prefix-callback (and (member input-type '(:trig :ttl))
				 (list :prefix-callback #'(lambda (prefix expansion) (instans-store-prefix-binding instans prefix expansion))))))
      (setf parser (apply parser-creator instans input-stream :base base :graph graph :subscribe subscribe (append callback prefix-callback)))
					;      (make-turtle-parser )
      (setf (instans-stream-input-processor-parser processor) parser)
      (add-input-processor instans processor)
      instans)))

(defun instans-add-agent-input-processor (instans &key name)
  (instans-debug-message instans '(:parse-rdf :execute) "instans-add-agent-input-processor ~S :name ~S" (instans-name instans) name)
  (let ((processor (make-instance 'instans-agent-input-processor
				  :instans instans
				  :name name
				  :operations (instans-rdf-operations instans))))
    (add-input-processor instans processor)
    instans))

(defun instans-run (instans &key select-output-name (select-output-type :csv) ask-output-name (ask-output-type :srx) construct-output-name (construct-output-type :trig))
  (declare (ignorable select-output-name select-output-type ask-output-name ask-output-type construct-output-name construct-output-type))
  ;; (inform "Instans-run ~S" instans)
  (instans-set-output-processors instans)
;  (handler-case
      (progn
	(run-input-processors instans t)
	(instans-add-status instans 'instans-rule-running-succeeded)
	t)
    ;; (t (e)
    ;;   (declare (ignore e))
    ;;   (instans-add-status instans 'instans-rule-running-failed)
    ;;   nil))
)

(defun instans-parse-rdf-file (instans input-iri &key subscribe base graph triple-callback block-callback document-callback)
  (let (input-stream file-type error-msg)
    (instans-debug-message instans '(:parse-rdf :execute) "instans-parse-rdf-file ~S ~S" (instans-name instans) input-iri)
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
      (when input-stream (close-stream input-stream :message "instans-parse-rdf-file: close ~A")))
    instans))

(defun instans-compare-rdf-files (instans input1-iri input2-iri &key base)
  (let ((result1 nil)
	(result2 nil))
      (instans-parse-rdf-file instans input1-iri :base base :document-callback #'(lambda (result) (setf result1 result)))
      (cond ((and input2-iri (not (sparql-unbound-p input2-iri)))
	     (instans-parse-rdf-file instans input2-iri :base base :document-callback #'(lambda (result) (setf result2 result)))
	     (cond ((instans-has-status instans 'instans-rdf-parsing-failed)
		    nil)
		   ((rdf-graphs-isomorphic-p result1 result2)
		    (instans-add-status instans 'instans-rdf-compare-files-similar)
		    t)
		   (t
		    (instans-add-status instans 'instans-rdf-compare-files-not-similar)
		    nil)))
	    (t t))))

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
