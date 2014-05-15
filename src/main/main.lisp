;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defvar *instanssi*)

;(save-lisp-and-die "executable" :toplevel 'main :executable t)

(defun run-configuration (configuration)
  (multiple-value-bind (instans instans-iri) (create-instans)
    (let* ((policies (copy-list (instans-policies instans)))
	   (directory (parse-iri (format nil "file://~A" (expand-dirname "."))))
	   (query-output-type :csv)
	   (query-output-name nil)
	   base graph expected debug reporting rete-html-page-dir)
      (setf *instanssi* instans)
      (labels ((valid-value-p (value accepted-values &key test)
		 (or (funcall test value accepted-values)
		     (error* "Value ~A not one of ~A" value accepted-values)))
	       (set-policy (key value accepted-values &key (test #'equal))
		 (when (valid-value-p value accepted-values :test test)
		   (setf (getf policies key) value)
		   (case key
		     (:triple-input-policy (setf (instans-triple-input-policy instans) value))
		     (:triple-processing-operations (setf (instans-triple-processing-operations instans) value))
		     (:rule-instance-removal-policy (setf (instans-rule-instance-removal-policy instans) value))
		     (:queue-execution-policy (setf (instans-queue-execution-policy instans) value))
		     (t (error* "Unknown policy ~A" key)))))
	       (parse-parameters (string &key colon-expand-fields)
		 (loop for param in (parse-spec-string string)
		       for (key value) = param
		       collect (if (member key colon-expand-fields) (list key (parse-colon-separated-values value)) param))))
	(unwind-protect
;	     (handler-case
	     (loop for (key value) in configuration
					;		    do (inform "key = ~S, value = ~S~%" key value)
		   do (case key
			(:name (setf (instans-name instans) value))
			(:directory (setf directory (parse-iri (if (http-or-file-iri-string-p value)
								   value
								   (format nil "file://~A" (expand-dirname value))))))
			(:base (setf base (parse-iri value)))
			(:graph (if (string= (string-downcase value) "default") nil (setf graph (parse-iri value))))
			(:execute (instans-run instans-iri))
			(:rules
			 (when (null (instans-query-output-processor instans))
			   (setf (instans-query-output-processor instans) (create-query-output-processor query-output-name query-output-type)))
			 (instans-add-rules instans-iri (expand-iri directory value) :create-instans-p nil :base base)
			 (unless (instans-find-status instans 'instans-rule-translation-succeeded)
			   (let ((status (first (instans-status instans))))
			     (cond ((null status)
				    (inform "Something wrong!"))
				   (t
				    (inform "~%~A:~A~{~%~A~}~%" value (type-of status) (instans-status-messages status)))))
			   (return-from run-configuration nil)))
			(:triples
			 (when (null (instans-query-output-processor instans))
			   (setf (instans-query-output-processor instans) (create-query-output-processor query-output-name query-output-type)))
			 (instans-add-triples instans-iri (expand-iri directory value) :graph graph :base base)
			 (unless (instans-find-status instans 'instans-triples-parsing-succeeded)
			   (let ((status (first (instans-status instans))))
			     (inform "~%~A:~A~{~%~A~}~%" value (type-of status) (instans-status-messages status)))
			   (return-from run-configuration nil)))
		     ;;; "base=http://example.org/friends/&graph=http://instans.org/events/&file=tests/input/fnb.ttl&input-policy=triples-block&operations:add:execute:remove:execute"
			(:input
			 (let ((input-parameters (parse-parameters value :colon-expand-fields '(:triple-processing-operations))))
			   (inform "~S" input-parameters)
			   (loop for (key . value) in input-parameters
				 do (case key
				      (:base (setf base value))
				      (:graph (setf graph value))
				      (:triple-input-policy (setf (getf policies :triple-input-policy) value))
				      (:triple-processing-operations (setf (getf policies :triple-processing-operations) value))))
			   (instans-add-triple-processor instans-iri 
							 (expand-iri directory (or (getf input-parameters :file) (getf input-parameters :iri)))
							 :graph graph
							 :base base)))
			(:query-output
			 (setf query-output-name value)
					;			  (inform "query-output-name = ~S" query-output-name)
			 (setf query-output-type (intern (string-upcase (pathname-type (parse-namestring value))) :keyword)))
			(:expect (let ((spec (parse-spec-string value)))
				   (inform "expect spec = ~S" spec)
				   (setf expected spec)))
			(:reporting (setf reporting (parse-colon-separated-values value))
				    (if (member :all reporting)
					(setf reporting '(:select :construct :modify :all)))
				    (setf (rule-instance-queue-report-p (instans-rule-instance-queue instans)) reporting))
			(:debug (setf debug (parse-colon-separated-values value)))
			(:verbose (setf debug (parse-colon-separated-values value)))
			(:triple-input-policy (set-policy :triple-input-policy (intern value :keyword) (instans-available-triple-input-policies instans)))
			(:triple-processing-operations (set-policy :triple-processing-operations (parse-colon-separated-values value)
								   (instans-available-triple-processing-operations instans)
								   :test #'(lambda (values accepted) (every #'(lambda (v) (member v accepted :test #'equal)) values))))
			(:rule-instance-removal-policy (set-policy :rule-instance-removal-policy (intern value :keyword) (instans-available-rule-instance-removal-policies instans)))
			(:queue-execution-policy (set-policy :queue-execution-policy (intern value :keyword) (instans-available-queue-execution-policies instans)))
			(:rete-html-page-dir (setf rete-html-page-dir value))))
;	       (t (e) (inform "~A" e))
;	  )
	  (when (instans-query-output-processor instans) (close-query-output-processor (instans-query-output-processor instans))))))))

(defvar *test-argv*)

(defun command-line-argv ()
  (if (boundp '*test-argv*) *test-argv*  sb-ext:*posix-argv*))

(defun main-test (&rest args)
  (let ((*test-argv* (cons "instans" (cons "--end-toplevel-options" (if (= 1 (length args)) (split-string (first args) " ") args)))))
    (main)))

(defun main ()
  (let* ((args (command-line-argv))
	 (configuration nil)
	 (notifications *error-output*)
	 (info-options nil)
	 (configuration-options nil))
    (labels ((usage ()
	       (format notifications "Usage: instans [ ~{~A~^ | ~} | { <configuration-option> } ]~%~%" (loop for option in info-options append (second option)))
	       (format notifications "General options:~%")
	       (loop for option in info-options do (format notifications "~{~A~^ | ~}~40T~A~%" (second option) (format nil (third option))))
	       (format notifications "~%Configuration options:~%")
	       (loop for option in configuration-options
		  unless (getf (cdddr option) :hiddenp)
		  do (format notifications "~{~:[~A~;~{~A~^ ~}~]~^~14T| ~}~40T~A~%"
			     (loop for o in (second option) nconc (list (consp o) o))
			     (format nil (third option))))
	       (format notifications "~%See the documentation for description of the parameters of the options above."))
	     (parse-arg (options not-found-error-p)
	       (loop with arg = (first args)
		     for option in options
		     when (find-if #'(lambda (item) (string= arg (if (consp item) (first item) item))) (second option))
		     do (progn
			  (pop args)
			  (let ((value (if (not (consp (first (second option)))) t (if args (pop args) (usage)))))
			    (cond ((getf (cdddr option) :operation)
				   (funcall (getf (cdddr option) :operation) arg value))
				  (t
				   (push-to-end (list (first option) value) configuration)))
			    (return option)))
		     finally (cond (not-found-error-p
				    (format notifications "Illegal argument \"~A\"~%" arg)
				    (return-from main nil))
				   (t
				    (return nil)))))
	     (read-args-from-file (file)
	       (with-open-file (stream file)
		 (loop for line = (read-line stream nil nil)
		       while line
		       do (setf line (string-trim '(#\Space #\Tab) line))
		       nconc (let* ((key-end (position-if #'whitespace-char-p line)))
			       (if (null key-end) 
				   (list (format nil "--~(~A~)" line))
				   (list (format nil "--~(~A~)" (subseq line 0 key-end))
					 (string-left-trim '(#\Space #\Tab) (subseq line key-end)))))))))
    (setf info-options `((:help ("-h" "--help") "Print this message and exit." :operation ,#'(lambda (&rest ignore) (declare (ignore ignore)) (usage)))
			 (:version ("-v" "--version") "Print version information and exit."
				   ,#'(lambda (&rest ignore) (declare (ignore ignore)) (format t "INSTANS version ~A~%" (instans-version))))))
    (setf configuration-options `((:name          (("-n" "<string>") ("--name" "<string>")) "Use <string> as the name of the system.")
				  (:directory     (("-d" "<dir>") ("--directory" "<dir>")) "Use <dir> as the prefix for file lookup.~%~
                                                                                            ~40TYou can use a file or an URL as <dir>")
				  (:base          (("-b" "<url>") ("--base" "<url>")) "Use <url> as the base.")
				  (:graph         (("-g" "<graph>") ("--graph" "<graph>")) "If <graph> is 'default' add the following ~%~
                                                                                            ~40Tinputs to the default graph. If it is <url>~%~
                                                                                            ~40Tadd them to the graph named by <url>")
				  (:rules         (("-r" "<rules>") ("--rules" "<rules>")) "Use rules in <rules>.")
				  (:input         (("-i" "<input>")  ("--input" "<input>")) "Read input based on <input>.")
				  (:triples       (("-t" "<input>") ("--triples" "<input>")) "Same as -i.")
				  (:output        (("-o" "<output>") ("--output" "<output>")) "Redirect output to <output>.")
				  (:expect        (("-e" "<expect>") ("--expect" "<expect>")) "Compare the results to <expect>." :hiddenp t)
				  (:query-output (("--query-output" "<file>")) "Output querys to <file>.")
				  (:file          (("-f" "<commands>") ("--file" "<commands>")) "Read options from <commands>."
						  :operation ,#'(lambda (arg value) (declare (ignore arg)) (setf args (append (read-args-from-file value) args))))
				  (:reporting        (("--report" "<rules>")) "The kinds of rules you want to get reported; a ':'~%~
                                                                           ~40Tseparated list of (select|construct|modify|all)." :hiddenp t)
				  (:rete-html-page-dir           (("--rete-html-page-dir" "<dir>")) "Create an HTML page about the Rete network.")
				  (:triple-input-policy          (("--triple-input-policy" "<policy>")) "The triple input policy.")
				  (:triple-processing-operations     (("--triple-processing-operations" "<policy>")) "See the documentation.")
				  (:rule-instance-removal-policy (("--rule-instance-removal-policy" "<policy>")) "See the documentation.")
				  (:queue-execution-policy        (("--queue-execution-policy" "<policy>")) "See the documentation.")
				  (:execute       (("-e") ("--execute")) "Execute the system. This is done by default at the end of processing all arguments.")
				  (:debug       (("--debug" "<phases>")) "See the documentation.")
				  (:verbose       (("--verbose" "<phases>")) "Same ase --debug.")))
      (pop args) ; Program path
      (when (equalp (first args) "--end-toplevel-options") (pop args)) ; Inserted by wrapper script
      (when (null args) (usage))
      (let ((option (parse-arg info-options nil)))
	(when (null option)
	  (loop while args
;	       do (inform "args = ~S" args)
		unless (parse-arg configuration-options t)
	        do (pop args))
	  (when configuration
;	    (format notifications "Configuration:~%~{~{~S~^ ~}~^~%~}~%" configuration)
	    (run-configuration configuration)))))))
