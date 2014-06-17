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
;;   (format notifications "Usage: instans [ ~{~A~^ | ~} ]~%" (loop for option options))
;;   (loop for option in options
;;   	unless (getf (cdddr option) :hiddenp)
;;   	do (format notifications "~{~:[~A~;~{~A~^ ~}~]~^~14T| ~}~40T~A~%"
;;   		   (loop for o in (second option) nconc 
;;   			(format nil (third option))))))
	    
;(defvar *doit* nil)

(defmacro parsing-commands (((key-var value-var) args-var &key program html usage) &body command-cases)
  (declare (ignorable html))
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
		    (format *error-output* "Usage: ~A { option }" ,program)
		    ,@(loop for cc in command-cases
			    for options = (getf (rest cc) :options)
			    for option-texts = (mapcar #'option-text options)
			    for usage-texts = (getf (rest cc) :usage)
;			    do (inform "cc = ~A" cc)
			    when (eq (first cc) t)
			    nconc (list `(format *error-output* "~%~{~A~^~%~}" ,(if (consp usage-texts) `',usage-texts `'(,usage-texts))))
;			    nconc (list `(format *error-output* "~%~A" ,usage-texts))
			    else
			    nconc (cond ((< (apply #'+ (* 2 (length option-texts)) (mapcar #'length option-texts)) usage-text-left-margin)
					 (list `(format *error-output* "~A" ,(format nil "~%  ~{~A~^, ~}~{~VT~A~^~%~}"
										     option-texts (mapcan #'(lambda (l) (list usage-text-left-margin l))
													  (if (consp usage-texts) usage-texts (list usage-texts)))))))
					((consp usage-texts)
					 (loop for option in options
					       for usage-text = (pop usage-texts)
					       collect `(format *error-output* "~%  ~A~VT~{~A~}"
								,(option-text option) ,usage-text-left-margin ,(if usage-text `'(,usage-text)))))
					(t
					 (loop for option in options
					       for usage-text = usage-texts then nil
					       collect `(format *error-output* "~%  ~A~VT~{~A~}"
								,(option-text option) ,usage-text-left-margin ,(if usage-text `'(,usage-text)))))))
		    (format *error-output* "~%")
		    (return-from ,outer nil))
		  ;; (,html ()
		  ;;   (format *error-output* "Html") ; ,(html-text command-cases))
		  ;;   (return-from ,outer nil))
		  )
	   (when (null ,args-var)
	     (,usage))
	   (loop with ,key-var = nil
		 with ,value-var = nil
		 while ,args-var
		 for ,arg-var = (first ,args-var)
;		 do (inform "XXX ~A = ~A" ',arg-var ,arg-var)
		 do (loop for ,case-var in ',(mapcar #'(lambda (cc) (list (first cc) :options (getf (rest cc) :options))) command-cases)
;			  do (inform "Testing ~S" ,case-var)
			  when (let ((,options-var (getf (rest ,case-var) :options)))
;				 (inform "Before middle")
				 (loop named ,middle
				       for ,option-var in ,options-var
;				       do (inform "Checking ~S" ,option-var)
				       do (cond ((consp ,option-var)
						 (when (string= (first ,option-var) ,arg-var)
						   (setf ,key-var (first ,case-var))
						   (pop ,args-var)
						   (when (rest ,option-var)
						     (setf ,value-var (pop ,args-var)))
						   (return-from ,middle t)))
						(t
;						 (inform "B")
						 (let ((split-position (position #\= ,option-var)))
;						   (inform "C")
						   (cond (split-position
							  (when (and (> (length ,arg-var) split-position)
								     (string= ,arg-var ,option-var :end1 split-position :end2 split-position))
							    (loop for i from 0 to split-position
								  unless (char= (char ,arg-var  i) (char ,option-var i))
								  return nil
								  finally (progn
									    (setf ,key-var (first ,case-var))
									    (setf ,value-var (subseq ,arg-var (1+ split-position)))
									    (pop ,args-var)
									    (return-from ,middle t)))))
							 (t
							  (when (string= ,option-var ,arg-var)
							    (setf ,key-var (first ,case-var))
							    (pop ,args-var)
							    (return-from ,middle t))))))))
;				 (inform "After middle")
				 )
			  do (progn ;(inform "key = ~S, value = ~S, args = ~S" ,key-var ,value-var ,args-var)
				    (case ,key-var
				      ,@(loop for case in command-cases
					      unless (eq (first case) t)
					      collect (cons (first case)
							    (loop for rest on (rest case) by #'cddr while (keywordp (first rest)) finally (return rest)))))
				    (return))
			 finally (progn
				   (format *error-output* "~%Unrecognized option ~A~%" ,arg-var)
				   (,usage)))))))))

(defun main (&rest args)
  (cond ((null args)
	 (setf args (command-line-argv)))
	(t
	 (setf args (cons "instans" (cons "--end-toplevel-options" (if (= 1 (length args)) (split-string (first args) " ") args))))))
  (multiple-value-bind (instans instans-iri) (create-instans)
    (let* ((executedp nil)
	   (execute-immediately-p t)
	   (directory (parse-iri (format nil "file://~A" (expand-dirname "."))))
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
	   rete-html-output)
      (labels ((valid-value-p (value accepted-values &key test)
		 (or (funcall test value accepted-values)
		     (error* "Value ~A not one of ~A" value accepted-values)))
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
	       (maybe-execute ()
;		 (inform "maybe-execute?")
		 (when execute-immediately-p
;		   (inform "yes")
		   (setf executedp t)
		   (instans-run instans-iri
				:select-output-name select-output-name :select-output-type select-output-type
				:construct-output-name construct-output-name :construct-output-type construct-output-type)))
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
	       (parsing-commands ((key value) args :program "instans" :html html :usage usage)
		 (t :usage ("" "Options are of form '-o', '-o PARAM', or '--option=PARAM'." ""
			    "General options:" ""))
		 (usage
		  :options ("--help" "-h")
		  :usage "Print help text."
		  :html
		  :operation
		  (usage)
		  (return-from command-loop))
		 ;; (html
		 ;;  :options ("--html")
		 ;;  :usage  "Print html help text."
		 ;;  :html
		 ;;  :operation
		 ;;  (html)
		 ;;  (return-from command-loop))
		 (version
		  :options ("--version" "-v")
		  :usage "Print version information and exit."
		  :html ""
		  (format t "INSTANS version ~A~%" (instans-version))
		  (return-from command-loop))
		 (commands
		  :options ("--file=FILE" ("-f" "FILE"))
		  :usage "Read options from FILE."
		  (setf args (append (read-args-from-file value) args)))
		 (t :usage ("" "Input options:" ""))
		 (rules
		  :options ("--rules=RULES" ("-r" "RULES"))
		  :usage "Load SPARQL rules from a file or an URL."
		  (set-output-processors)
		  (instans-add-rules instans-iri (expand-iri directory value) :create-instans-p nil :base base)
		  (cond ((instans-find-status instans 'instans-rule-translation-succeeded)
			 (if rete-html-output (output-rete-html-page instans rete-html-output)))
			(t
			 (let ((status (first (instans-status instans))))
			   (cond ((null status)
				  (inform "Something wrong!"))
				 (t
				  (inform "~%~A:~A~{~%~A~}~%" value (type-of status) (instans-status-messages status)))))
			 (return-from command-loop nil))))
		 (input
		  :options ("--input=INPUT" ("-i" "INPUT") ("-t" "INPUT") )
		  :usage ("Read RDF from a file or an URL. The suffix of INPUT is used to"
                          "determine the type of the input.")
		  :html "The recognized file formats are TriG (type '.trig'), Turtle (type '.ttl' or '.turtle'), N-Triples (type '.nt' or '.n-triples'), and N-Quads (type '.nt' or '.n-quads').
                                If INPUT does not have a file type, use the type specific input options below."
		  (instans-add-query-input-processor instans-iri (expand-iri directory value)
						     :graph graph :base base
						     :input-type (intern (string-upcase (pathname-type (parse-namestring value))) :keyword))
		  (maybe-execute))
		 (input-trig
		  :options ("--input-trig=INPUT")
		  :usage "Read RDF in TriG format from INPUT."
		  :html "The '--input-<type>' options take a parameter INPUT, which can be a real file, a pseudo file like /dev/stdint, or a URI. The content format should be of the specified type, e.g., for '--input-ttl' INPUT should contain TriG format input. Even if INPUT is a file with a specific type, the type is not considered when parsing the file."
		  (instans-add-query-input-processor instans-iri (expand-iri directory value)
						     :graph graph :base base :input-type :trig)
		  (maybe-execute))
		 (input-turtle
		  :options ("--input-turtle=INPUT" "--input-ttl=INPUT")
		  :usage "Read RDF in Turtle format from INPUT."
		  :html "The '--input-<type>' options take a parameter INPUT, which can be a real file, a pseudo file like /dev/stdint, or a URI. The content format should be of the specified type, e.g., for '--input-ttl' INPUT should contain Turtle format input. Even if INPUT is a file with a specific type, the type is not considered when parsing the file."
		  (instans-add-query-input-processor instans-iri (expand-iri directory value)
						     :graph graph :base base :input-type :ttl)
		  (maybe-execute))
		 (input-nq
		  :options ("--input-nq=INPUT" "--input-n-quads=INPUT")
		  :usage "Read RDF in N-Quads format from INPUT."
		  :html "The '--input-<type>' options take a parameter INPUT, which can be a real file, a pseudo file like /dev/stdint, or a URI. The content format should be of the specified type, e.g., for '--input-ttl' INPUT should contain N-Quads format input. Even if INPUT is a file with a specific type, the type is not considered when parsing the file."
		  (instans-add-query-input-processor instans-iri (expand-iri directory value)
						     :graph graph :base base :input-type :nq)
		  (maybe-execute))
		 (input-nt
		  :options ("--input-nt=INPUT" "--input-n-triples=INPUT")
		  :usage "Read RDF in N-Triples format from INPUT."
		  :html "The '--input-<type>' options take a parameter INPUT, which can be a real file, a pseudo file like /dev/stdint, or a URI. The content format should be of the specified type, e.g., for '--input-ttl' INPUT should contain N-Triples format input. Even if INPUT is a file with a specific type, the type is not considered when parsing the file."
		  (instans-add-query-input-processor instans-iri (expand-iri directory value)
						     :graph graph :base base :input-type :nt)
		  (maybe-execute))
		 (base
		  :options (("-b" "URL") "--base=URL")
		  :usage "Use URL as the base."
		  (setf base (parse-iri value)))
		 (directory
		  :options (("-d" "DIR") "--directory=DIR")
		  :usage "Use DIR as the prefix for file lookup. You can use a file or an URL as DIR."
		  (setf directory (parse-iri (if (http-or-file-iri-string-p value) value (format nil "file://~A" (expand-dirname value))))))
		 (graph
		  :options (("-g" "URL") "--graph=URL")
		  :usage "Use URL as the graph."
		  (if (string= (string-downcase value) "default") nil (setf graph (parse-iri value))))
		 (t :usage ("" "Output options:" ""))
		 (select-output
		  :options ("--select-output=FILE")
		  :usage "Write SELECT results to FILE. Output is based on the file name suffix."
		  :html ""
		  (setf select-output-name value)
		  (setf select-output-type (intern (string-upcase (pathname-type (parse-namestring value))) :keyword)))
		 (select-output-csv
		  :options ("--select-output-csv=OUTPUT")
		  :usage "Write SELECT results as CSV to OUTPUT."
		  :html ""
		  (setf select-output-name value)
		  (setf select-output-type :csv))
		 (construct-output
		  :options ("--construct-output=FILE")
		  :usage "Write CONSTRUCT results to FILE. Output format is based on the file name suffix."
		  :html ""
		  (setf construct-output-name value)
		  (setf construct-output-type (intern (string-upcase (pathname-type (parse-namestring value))) :keyword)))
		 (construct-output-trig
		  :options ("--construct-output-trig=OUTPUT")
		  :usage "Write CONSTRUCT results as TriG to OUTPUT."
		  :html ""
		  (setf construct-output-name value)
		  (setf construct-output-type :trig))
		 (construct-output-ttl
		  :options ("--construct-output-ttl=OUTPUT" "--construct-output-turtle=OUTPUT")
		  :usage "Write CONSTRUCT results as Turtle to OUTPUT."
		  :html ""
		  (setf construct-output-name value)
		  (setf construct-output-type :ttl))
		 (construct-output-nq
		  :options ("--construct-output-nq=OUTPUT" "--construct-output-n-quads=OUTPUT")
		  :usage "Write CONSTRUCT results as N-Quads to OUTPUT."
		  :html ""
		  (setf construct-output-name value)
		  (setf construct-output-type :nq))
		 (construct-output-nt
		  :options ("--construct-output-nt=OUTPUT" "--construct-output-n-triples=OUTPUT")
		  :usage "Write CONSTRUCT results as N-Triples to OUTPUT."
		  :html ""
		  (setf construct-output-name value)
		  (setf construct-output-type :nt))
		 (t :usage ("" "Execution control options:" ""))
		 (execute
		  :options ("--execute" "-e")
		  :usage ("Changes the execution mode to immediate execution (the default). In the immediate"
			  "execution mode RDF input is processed immediately after each input parameter."
			  "If the mode was delayed execution, INSTANS processes the input parameters that"
			  "have been read after the mode was changed from immediate to delayed execution.")
		  (setf execute-immediately-p t)
		  (maybe-execute))
		 (noexecute
		  :options ("--noexecute")
		  :usage ("Changes the execution mode to delayed execution. In delayed execution mode"
			  "the input parameters are not processed after read. Instead, INSTANS waits until"
			  "the mode is switched back to immediate execution and only then processes these"
			  "input parameters.")
		  (setf execute-immediately-p nil)
		  (maybe-execute))
		 (rdf-input-unit
		  :options ("--rdf-input-unit=UNIT")
		  :usage ("Read RDF input in units of \"triple\", \"block\", or \"document\". \"Triple\" means"
			  "that the input is read and processed one triple (or quad in TriG or N-Quads input)"
			  "at a time. In N-Triples and N-Quads \"block\" has the same meaning as \"triple\"."
			  "In TriG it means that the input is read and processed based on the grammar rule [2g]"
			  "of the TriG grammar, and in Turtle it means that the input is read and processed"
			  "based on the grammar rule [6] of the Turtle grammar. The default is \"block\".")
		  :html ""
		  (setf (instans-rdf-input-unit instans) (intern (string-upcase value) :keyword)))
		 (rdf-operations
		  :options  ("--rdf-operations=LIST")
		  :usage ("Apply a colon separated list of operations to the unit of RDF input. Operations are"
			  "\"add\", \"remove\", and \"execute\". You can use \"event\" as a shorthand form"
			  "\"add:execute:remove:execute\". The default is \"add:execute\".")
		  :html ""
		  (setf (instans-rdf-operations instans) (parse-colon-separated-values value)))
		 (queue-execution-policy
		  :options ("--queue-execution-policy=POLICY")
		  :usage ("Execute the rules in the rule instance queue based on POLICY. Policies are \"first\","
			  "\"snapshot\", \"repeat-first\" (the default), and \"repeat-snapshot\". \"First\" executes"
			  "the first instance in the queue, \"repeat-first\" does this as long as the queue is not"
			  "empty. \"Snapshot\" takes the rules currently in the queue and executes them;"
			  "\"repeat-snapshot\" repeats this as long as the queue is not empty.")
		  :html ""
		  (setf (instans-queue-execution-policy instans) (intern (string-upcase value) :keyword)))
		 (allow-rule-instance-removal
		  :options ("--allow-rule-instance-removal=BOOL")
		  :usage ("If true (the default), adding or removing RDF input removes rule instances that have"
			  "not been executed yet from the rule instance queue, if they cease to be satisfied;"
			  "if false, rule instances are not removed from the queue even when they cease to be"
			  "satisfied when adding or removing RDF input.")
		  :html ""
		  (setf (instans-allow-rule-instance-removal-p instans)
			(cond ((string-equal value "true") t)
			      ((string-equal value "false") nil)
			      (t (usage)))))
		 (t :usage ("" "Miscelaneus debugging and testing options:" ""))
		 (verbose
		  :options ("--verbose=SITUATIONS")
		  :usage ("Print lots of information based on a comma-separated list of situations. Currently"
			  "the possible states are \"parser\", which prints information on the generated SPARQL,"
			  "TriG, Turtle, N-Quads, and N-Triples parsers, \"parse-operations\", which prints"
			  "operations of the parser, and \"token\", which prints the recognized input tokens.")
		  :html ""
		  (setf debug (parse-colon-separated-values value)))
		 (rete-html
		  :options ("--rete-html=FILE")
		  :usage ("Create an HTML page about the Rete network. The HTML page contains the SPARQL query,"
			  "a picture of the generate Rete network and other useful information.")
		  :html ""
		  (setf rete-html-output value))
		 (name
		  :options ("--name=NAME" ("-n" "NAME"))
		  :usage "Use NAME as the name of the system."
		  :html "The name of system is used in generating various outputs and names during the execution of INSTANS, but the name does not bear any actual semantics."
		  (setf (instans-name instans) value))
		 (reporting
		  :options ("--report=KINDS")
		  :usage "The kinds of rules you want to get reported; a ':' separated list of (select|construct|modify|all)."
		  :html ""
		  :hiddenp t
		  (setf reporting (parse-colon-separated-values value))
		  (if (member :all reporting)
		      (setf reporting '(:select :construct :modify :all)))
		  (setf (rule-instance-queue-report-p (instans-rule-instance-queue instans)) reporting))
		 (time
		  :options ("--time=FILE")
		  :usage "Output timing information to FILE. Use '-' for standard output."
		  :html ""
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

