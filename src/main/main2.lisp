;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defvar *instanssi*)

;(save-lisp-and-die "executable" :toplevel 'main :executable t)

(defun read-args-from-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil nil)
	  while line
	  do (setf line (string-trim '(#\Space #\Tab) line))
	  nconc (let* ((key-end (position-if #'whitespace-char-p line)))
		  (if (null key-end) 
		      (list (format nil "--~(~A~)" line))
		      (list (format nil "--~(~A~)" (subseq line 0 key-end))
			    (string-left-trim '(#\Space #\Tab) (subseq line key-end))))))))

;; (defun usage (commands)
;;   (declare (ignorable commands))
;;   nil)
;;   (format notifications "Usage: instans [ ~{~A~^ | ~} ]~%~%" (loop for option options))
;;   (loop for option in options
;;   	unless (getf (cdddr option) :hiddenp)
;;   	do (format notifications "~{~:[~A~;~{~A~^ ~}~]~^~14T| ~}~40T~A~%"
;;   		   (loop for o in (second option) nconc 
;;   			(format nil (third option))))))
	    
(defvar *doit* nil)

(defmacro parsing-commands (((key-var value-var) args-var &key program help usage) &body command-cases)
  (let* ((option-var (gensym "OPTION"))
	 (options-var (gensym "OPTIONS"))
	 (case-var (gensym "CASE"))
	 (arg-var (gensym "ARG"))
	 (outer (gensym "OUTER"))
	 (middle (gensym "MIDDLE"))
	 (max-option-length (loop for cc in command-cases unless (eq (first cc) t) maximize (loop for option in (getf (rest cc) :options) maximize (if (stringp option) (length option) (length (first option))))))
	 (usage-text-left-margin (+ 3 max-option-length))
)
    (flet ((option-text (option) (if (consp option) (format nil "~{~A~^ ~}" option) option)))
      `(block ,outer
	 (labels ((,usage ()
		    (format *error-output* "Usage: ~A { option }~%" ,program)
		    ,@(loop for cc in command-cases
			    for options = (getf (rest cc) :options)
			    for option-texts = (mapcar #'option-text options)
			    when (eq (first cc) t)
			    nconc (list `(format *error-output* "~%~A" ,(getf (rest cc) :short-help)))
			    else
			    nconc (cond ((< (apply #'+ (* 2 (length option-texts)) (mapcar #'length option-texts)) usage-text-left-margin)
					 (list `(format *error-output* "~A" ,(format nil "~%  ~{~A~^, ~}~VT~A" option-texts usage-text-left-margin (getf (rest cc) :short-help)))))
					(t
					 (loop for firstp = t then nil
					       for option in options
					       collect `(format *error-output* "~%  ~A" ,(option-text option))
					       when firstp
					       collect `(format *error-output* "~VT~A" ,usage-text-left-margin ,(getf (rest cc) :short-help))))))
		    (return-from ,outer nil))
		  (,help ()
		    (format *error-output* "Help") ; ,(help-text command-cases))
		    (return-from ,outer nil)))
	   (loop with ,key-var = nil
		 with ,value-var = nil
		 while ,args-var
		 for ,arg-var = (first ,args-var)
		 do (loop for ,case-var in ',(mapcar #'(lambda (cc) (list (first cc) :options (getf (rest cc) :options))) command-cases)
;			  do (inform "Testing ~S" ,case-var)
			  when (let ((,options-var (getf (rest ,case-var) :options)))
				 (loop named ,middle
				       for ,option-var in ,options-var
;				       do (inform "Checking ~S" ,option-var)
				       do (cond ((consp ,option-var)
						 (when (string= (first ,option-var) ,arg-var)
						   (setf ,key-var (first ,case-var))
						   (pop ,args-var)
						   (when (rest ,option-var)
						     (setf ,value-var ,args-var)
						     (pop ,args-var))
						   (return-from ,middle t)))
						(t
						 (let ((split-position (position #\= ,option-var)))
						   (cond (split-position
							  (when (< (length ,arg-var) (+ 2 split-position))
							    (,usage))
							  (loop for i from 0 to split-position
								unless (char= (char ,arg-var  i) (char ,option-var i))
								return nil
								finally (progn
									  (setf ,key-var (first ,case-var))
									  (setf ,value-var (subseq ,arg-var (1+ split-position)))
									  (pop ,args-var)
									  (return-from ,middle t))))
							 (t
							  (when (string= ,option-var ,arg-var)
							    (setf ,key-var (first ,case-var))
							    (pop ,args-var)
							    (return-from ,middle t)))))))))
			  do (progn (inform "key = ~S, value = ~S" ,key-var ,value-var)
				    (when *doit*
				      (case ,key-var
					,@(loop for case in command-cases
						unless (eq (first case) t)
						collect (cons (first case)
							      (loop for rest on (rest case) by #'cddr while (keywordp (first rest)) finally (return rest))))
					(t (,usage))))
				    (return)))))))))

(defun main2 (&rest args)
  (cond ((null args)
	 (setf args (command-line-argv)))
	(t
	 (setf args (cons "instans" (cons "--end-toplevel-options" (if (= 1 (length args)) (split-string (first args) " ") args))))))
  (multiple-value-bind (instans instans-iri) (create-instans)
    (let* ((executedp nil)
	   (policies (copy-list (instans-policies instans)))
	   (directory (parse-iri (format nil "file://~A" (expand-dirname "."))))
	   ;; (query-input-type nil)
	   (select-output-name nil)
	   (select-output-type :csv)
	   (construct-output-name nil)
	   (construct-output-type :trig)
	   time-output-name
	   time-output-stream
	   start-time-sec
	   start-time-usec
	   base graph
	   ;; expected
	   debug
	   reporting
	   rete-html-page-dir
	   ;; (notifications *error-output*)
)
      (labels ((valid-value-p (value accepted-values &key test)
		 (or (funcall test value accepted-values)
		     (error* "Value ~A not one of ~A" value accepted-values)))
	       (set-policy (key value accepted-values &key (test #'equal))
		 (when (valid-value-p value accepted-values :test test)
		   (setf (getf policies key) value)
		   (case key
		     (:query-input-policy (setf (instans-query-input-policy instans) value))
		     (:query-processing-operations (setf (instans-query-processing-operations instans) value))
		     (:rule-instance-removal-policy (setf (instans-rule-instance-removal-policy instans) value))
		     (:queue-execution-policy (setf (instans-queue-execution-policy instans) value))
		     (t (error* "Unknown policy ~A" key)))))
;	       (expand-iri-or-file-path (base iri-or-file-path input-type)
	       (parse-parameters (string &key colon-expand-fields)
		 (loop for param in (parse-spec-string string)
		       for (key value) = param
		       collect (if (member key colon-expand-fields) (list key (parse-colon-separated-values value)) param)))
	       (set-output-processors ()
		 (when (and select-output-type (null (instans-select-output-processor instans)))
		   (setf (instans-select-output-processor instans) (create-select-output-processor select-output-name select-output-type)))
		 (when (and construct-output-type (null (instans-construct-output-processor instans)))
		   (setf (instans-construct-output-processor instans) (create-construct-output-processor construct-output-name construct-output-type)))
		 )
	       (execute ()
		 (setf executedp t)
		 (instans-run instans-iri))
	       (output-time (fmt &rest args)
		 (multiple-value-bind (time-sec time-usec) (sb-unix::get-time-of-day)
		     (let* ((delta-sec (- time-sec start-time-sec))
			    (delta-usec (- time-usec start-time-usec)))
		       (when (< delta-usec 0)
			 (decf delta-sec)
			 (incf delta-usec 1000000))
		       (inform "At ~D.~6,'0D: ~A~%" delta-sec delta-usec  (apply #'format nil fmt args))))))
	(setf *instanssi* instans)
    (pop args) ; Program path
    (when (equalp (first args) "--end-toplevel-options") (pop args)) ; Inserted by wrapper script
    (unwind-protect
    	 (block command-loop
    	   (parsing-commands ((key value) args :program "instans" :help help :usage usage)
	     (t :short-help (format nil "~%General options:"))
	     (help :options ("-h" "--help")
		   :short-help  "Print help text."
		   :long-help
		   :operation
		   (help)
		   (return-from command-loop))
	     (version :options ("-v" "--version")
		      :short-help "Print version information and exit."
		      :long-help ""
		      (format t "INSTANS version ~A~%" (instans-version))
		      (return-from command-loop))
	     (commands :options (("-f" "FILE") "--file=FILE")
		       :short-help "Read options from FILE."
		       (setf args (append (read-args-from-file value) args)))
	     (t :short-help (format nil "~%Input options:"))
	     (rules :options (("-r" "RULES") "--rules=RULES")
		    :short-help "Use SPARQL rules in RULES. You can use a file or an URL as RULES."
		    (set-output-processors)
		    (instans-add-rules instans-iri (expand-iri directory value) :create-instans-p nil :base base)
		    (cond ((instans-find-status instans 'instans-rule-translation-succeeded)
			   (if rete-html-page-dir (output-rete-html-page instans value rete-html-page-dir)))
			  (t
			   (let ((status (first (instans-status instans))))
			     (cond ((null status)
				    (inform "Something wrong!"))
				   (t
				    (inform "~%~A:~A~{~%~A~}~%" value (type-of status) (instans-status-messages status)))))
			   (return-from command-loop nil))))
	     (input :options (("-t" "INPUT") ("-i" "INPUT") "--input=INPUT")
		    :short-help "Read INPUT based on its type. INPUT may be a file or an URL."
		    :long-help "The recognized file formats are TriG (type '.trig'), Turtle (type '.ttl' or '.turtle'), N-Triples (type '.nt' or '.n-triples'), and N-Quads (type '.nt' or '.n-quads').
                                If INPUT does not have a file type, use the type specific input options below."
		    (instans-add-query-input-processor
		     instans-iri value :graph graph :base base
		     :input-type (intern (string-upcase (pathname-type (parse-namestring value))) :keyword))
		    (execute))
	     (input-trig :options ("--input-trig=INPUT")
			 :short-help "Read TriG from INPUT."
			 :long-help "The '--input-<type>' options take a parameter INPUT, which can be a real file, a pseudo file like /dev/stdint, or a URI. The content format should be of the specified type, e.g., for '--input-ttl' INPUT should contain TriG format input. Even if INPUT is a file with a specific type, the type is not considered when parsing the file."
			 (instans-add-query-input-processor instans-iri value :graph graph :base base :input-type :trig)
			 (execute))
	     (input-turtle :options ("--input-turtle=INPUT")
			   :short-help "Read Turtle from INPUT."
			   :long-help "The '--input-<type>' options take a parameter INPUT, which can be a real file, a pseudo file like /dev/stdint, or a URI. The content format should be of the specified type, e.g., for '--input-ttl' INPUT should contain Turtle format input. Even if INPUT is a file with a specific type, the type is not considered when parsing the file."
			   (instans-add-query-input-processor instans-iri value :graph graph :base base :input-type :turtle)
			   (execute))
	     (input-nq :options ("--input-nq=INPUT")
		       :short-help "Read N-Quads from INPUT."
		       :long-help "The '--input-<type>' options take a parameter INPUT, which can be a real file, a pseudo file like /dev/stdint, or a URI. The content format should be of the specified type, e.g., for '--input-ttl' INPUT should contain N-Quads format input. Even if INPUT is a file with a specific type, the type is not considered when parsing the file."
		       (instans-add-query-input-processor instans-iri value :graph graph :base base :input-type :nq)
		       (execute))
	     (input-nt :options ("--input-nt=INPUT")
		       :short-help "Read N-Triples from INPUT."
		       :long-help "The '--input-<type>' options take a parameter INPUT, which can be a real file, a pseudo file like /dev/stdint, or a URI. The content format should be of the specified type, e.g., for '--input-ttl' INPUT should contain N-Triples format input. Even if INPUT is a file with a specific type, the type is not considered when parsing the file."
		       (instans-add-query-input-processor instans-iri value :graph graph :base base :input-type :nt)
		       (execute))
	     (base :options (("-b" "URL") "--base=URL")
		   :short-help "Use URL as the base."
		   (setf base (parse-iri value)))
	     (directory :options (("-d" "DIR") "--directory=DIR")
			:short-help "Use DIR as the prefix for file lookup. You can use a file or an URL as DIR."
			(setf directory (parse-iri (if (http-or-file-iri-string-p value) value (format nil "file://~A" (expand-dirname value))))))
	     (graph :options (("-g" "URL") "--graph=URL")
		    :short-help "Use URL as the graph."
		    (if (string= (string-downcase value) "default") nil (setf graph (parse-iri value))))
	     (t :short-help (format nil "~%Output options:"))
	     (select-output :options ("--select-output=FILE")
			    :short-help "Write select results to FILE. The type of the output is based on the file type."
			    :long-help ""
			    (setf select-output-name value)
			    (setf select-output-type (intern (string-upcase (pathname-type (parse-namestring value))) :keyword)))
	     (select-output-csv :options ("--select-output-csv=OUTPUT")
				:short-help "Write select results as CSV to OUTPUT."
				:long-help ""
				(setf select-output-name value)
				(setf select-output-type :csv))
	     (construct-output :options (("--construct-output" "FILE"))
			       :short-help "Write construct results to FILE. The type of the output is based on the file type."
			       :long-help ""
			       (setf construct-output-name value)
			       (setf construct-output-type (intern (string-upcase (pathname-type (parse-namestring value))) :keyword)))
	     (construct-output-trig :options ("--construct-output-trig=OUTPUT")
				    :short-help "Write construct results as TriG to OUTPUT."
				    :long-help ""
				    (setf construct-output-name value)
				    (setf construct-output-type :trig))
	     (construct-output-ttl :options ("--construct-output-ttl=OUTPUT" "--construct-output-turtle=OUTPUT")
				   :short-help "Write construct results as Turtle to OUTPUT."
				   :long-help ""
				   (setf construct-output-name value)
				   (setf construct-output-type :ttl))
	     (construct-output-nq :options ("--construct-output-nq=OUTPUT" "--construct-output-n-quads=OUTPUT")
				  :short-help "Write construct results as N-Quads to OUTPUT."
				  :long-help ""
				  (setf construct-output-name value)
				  (setf construct-output-type :nq))
	     (construct-output-nt :options ("--construct-output-nt=OUTPUT" "--construct-output-n-triples=OUTPUT")
				  :short-help "Write construct results as N-Triples to OUTPUT."
				  :long-help ""
				  (setf construct-output-name value)
				  (setf construct-output-type :nt))
	     (t :short-help (format nil "~%Policy options:"))
	     (query-input-policy :options ("--query-input-policy=POLICY")
				 :short-help "The triple input policy."
				 :long-help ""
				 (setf (instans-query-input-policy instans) value))
	     (query-processing-operations :options  ("--query-processing-operations=POLICY")
					  :short-help "See the documentation."
					  :long-help
					  (setf (instans-query-processing-operations instans) value))
	     (queue-execution-policy :options ("--queue-execution-policy=POLICY")
				     :short-help "See the documentation."
				     :long-help ""
				     (setf (instans-queue-execution-policy instans) value))
	     (rule-instance-removal-policy :options ("--rule-instance-removal-policy=POLICY")
					   :short-help "See the documentation."
					   :long-help ""
					   (setf (instans-rule-instance-removal-policy instans) value))
	     (t :short-help (format nil "~%Execution, debugging, and testing options:"))
	     (execute :options ("-e" "--execute")
		      :short-help "Execute the system. This is done by default at the end of processing all arguments.")
	     (debug :options ("--debug=PHASES" "--verbose=PHASES")
		    :short-help "See the documentation."
		    :long-help ""
		    (setf debug (parse-colon-separated-values value)))
	     (rete-html-page-dir :options (("--rete-html-page-dir" "DIR"))
				 :short-help "Create an HTML page about the Rete network."
				 :long-help "The HTML page contains the SPARQL query, a picture of the generate Rete network and other useful information"
				 (setf rete-html-page-dir value))
	     (name :options (("-n" "NAME") "--name=NAME")
		   :short-help "Use NAME as the name of the system."
		   :long-help "The name of system is used in generating various outputs and names during the execution of INSTANS, but the name does not bear any actual semantics."
		   (setf (instans-name instans) value))
	     (reporting :options (("--report=KINDS"))
			:short-help "The kinds of rules you want to get reported; a ':' separated list of (select|construct|modify|all)."
			:long-help ""
			:hiddenp t
			(setf reporting (parse-colon-separated-values value))
			(if (member :all reporting)
			    (setf reporting '(:select :construct :modify :all)))
			(setf (rule-instance-queue-report-p (instans-rule-instance-queue instans)) reporting))
	     (time :options ("--time=FILE")
		   :short-help "Output timing information to FILE. Use '-' for standard output."
		   :long-help ""
		   (setf time-output-name value)
		   (multiple-value-setq (start-time-sec start-time-usec) (sb-unix::get-time-of-day))
		   (setf time-output-stream
			 (if (string= value "-") *standard-output* (open value :direction :output :if-exists :supersede))))
	     )
	   (unless executedp (instans-run instans-iri))
	   instans)
      (when time-output-stream
    	(output-time "Done")
    	(close time-output-stream))
      (instans-close-open-streams instans))
    ))))


