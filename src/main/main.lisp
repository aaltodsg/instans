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

(defmacro parsing-commands (((key-var value-var) args-var &key program html usage before after) &body command-cases)
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
			  do (progn ,@(if before (list before))
				    (case ,key-var
				      ,@(loop for case in command-cases
					      unless (eq (first case) t)
					      collect (cons (first case)
							    (loop for rest on (rest case) by #'cddr while (keywordp (first rest)) finally (return rest)))))
				    ,@(if after (list after))
				    (return))
			 finally (progn
				   (format *error-output* "~%Unrecognized option ~A~%" ,arg-var)
				   (,usage)))))))))

(defun main (args)
  ;; (cond ((null args)
  ;; 	 (setf args sb-ext:*posix-argv*))
  ;; 	(t
  ;; 	 (setf args (cons "instans" (cons "--end-toplevel-options" (if (= 1 (length args)) (split-string (first args) " ") args))))))
  (cond ((null args)
	 (setf args sb-ext:*posix-argv*))
	((stringp args)
	 (setf args (cons "instans" (split-string args " ")))))
  (let* ((instans (create-instans))
	 (executedp nil)
	 (execute-immediately-p t)
	 (directory (parse-iri (format nil "file://~A" (expand-dirname (or *default-main-dir* ".")))))
	 (ask-output-name nil)
	 (ask-output-type nil)
	 (select-output-name nil)
	 (select-output-type :csv)
	 (select-output-append-p nil)
	 (construct-output-name nil)
	 (construct-output-type :trig)
	 (construct-output-append-p nil)
	 time-output-name
	 time-output-stream
	 start-time-sec
	 start-time-usec
	 base graph
	 ;; expected
	 debug
	 reporting
	 rete-html-file)
    (labels ((valid-value-p (value accepted-values &key test)
	       (or (funcall test value accepted-values)
		   (error* "Value ~A not one of ~A" value accepted-values)))
					;	       (expand-iri-or-file-path (base iri-or-file-path input-type)
	     (parse-parameters (string &key colon-expand-fields)
	       (loop for param in (parse-spec-string string)
		     for (key value) = param
		     collect (if (member key colon-expand-fields) (list key (parse-colon-separated-values value)) param)))
	     (set-output-processors ()
	       (when (and ask-output-type ask-output-name (null (instans-ask-output-processor instans)))
		 (inform "ask-output-type ~A ask-output-name ~A" ask-output-type ask-output-name)
		 (setf (instans-ask-output-processor instans) (create-ask-output-processor instans ask-output-name ask-output-type)))
	       (when (and select-output-type (null (instans-select-output-processor instans)))
		 (setf (instans-select-output-processor instans) (create-select-output-processor instans select-output-name select-output-type :appendp select-output-append-p)))
	       (when (and construct-output-type (null (instans-construct-output-processor instans)))
		 (setf (instans-construct-output-processor instans) (create-construct-output-processor instans construct-output-name construct-output-type :appendp construct-output-append-p))))
	     (execute ()
	       (instans-run instans
			    :ask-output-name ask-output-name :ask-output-type ask-output-type
			    :select-output-name select-output-name :select-output-type select-output-type
			    :construct-output-name construct-output-name :construct-output-type construct-output-type))
	     (maybe-execute ()
					;		 (inform "maybe-execute?")
	       (when execute-immediately-p
					;		   (inform "yes")
		 (setf executedp t)
		 (execute)))
	     (output-time (fmt &rest args)
	       (multiple-value-bind (time-sec time-usec) (sb-unix::get-time-of-day)
		 (let* ((delta-sec (- time-sec start-time-sec))
			(delta-usec (- time-usec start-time-usec)))
		   (when (< delta-usec 0)
		     (decf delta-sec)
		     (incf delta-usec 1000000))
		   (format time-output-stream "~%At ~D.~6,'0D: ~A~%" delta-sec delta-usec  (apply #'format nil fmt args))))))
      (setf *instanssi* instans)
      (pop args) ; Program path
;      (when (equalp (first args) "--end-toplevel-options") (pop args)) ; Inserted by wrapper script
      (unwind-protect
	   (block command-loop
	     (parsing-commands ((key value) args :program "instans" :html html :usage usage
				:before (when time-output-stream (output-time "Command: ~(~A~), Parameter: ~A" key value)))
	       (t :usage ("" "Options are of form '-o', '-o PARAM', or '--option=PARAM'." ""
			     "General options:" ""))
	       (usage
		:options ("--help" "-h")
		:usage "Print help text."
		(usage))
	       (version
		:options ("--version" "-v")
		:usage "Print version information and exit."
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
		(let ((rules (expand-iri directory value)))
		  (set-output-processors)
		  (instans-add-rules instans rules :base base)
		  (cond ((instans-find-status instans 'instans-rule-translation-succeeded)
			 (if rete-html-file (output-rete-html-page instans rules rete-html-file)))
			(t
			 (let ((status (first (instans-status instans))))
			   (cond ((null status)
				  (inform "Something wrong!"))
				 (t
				  (inform "~%~A:~A~{~%~A~}~%" value (type-of status) (instans-status-messages status)))))
			 (return-from command-loop nil)))))
	       (input
		:options ("--input=INPUT" ("-i" "INPUT") ("-t" "INPUT") )
		:usage ("Read RDF from a file or an URL. The suffix of INPUT is used to determine the"
			"type of the input. The recognized file formats are TriG (type '.trig'), Turtle"
			"(type '.ttl' or '.turtle'), N-Triples (type '.nt' or '.n-triples'), N-Quads"
			"(type '.nt' or '.n-quads'), N-Lisp (type '.nl', '.n-lisp'), or Lisp-Block (type '.lbl')"
                        ". If INPUT does not have a file type, use the type"
			"specific input options below.")
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base :subscribe debug
						    :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
		(maybe-execute))
	       (input-trig
		:options ("--input-trig=INPUT")
		:usage ("Read RDF in TriG format from INPUT. The '--input-<type>' options take a"
			"parameter INPUT, which can be a real file, a pseudo file like /dev/stdint,"
			"or a URI. The content format should be of the specified type, e.g., for "
			"'--input-ttl' INPUT should contain TriG format input. Even if INPUT is a file"
			"with a specific type, the type is not considered when parsing the file.")
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base :input-type :trig)
		(maybe-execute))
	       (input-turtle
		:options ("--input-turtle=INPUT" "--input-ttl=INPUT")
		:usage "Read RDF in Turtle format from INPUT."
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base :input-type :ttl)
		(maybe-execute))
	       (input-lisp-block
		:options ("--input-lisp-blocks=INPUT" "--input-lbl=INPUT")
		:usage "Read RDF in Turtle format from INPUT."
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base :input-type :lbl)
		(maybe-execute))
	       (input-nq
		:options ("--input-nq=INPUT" "--input-n-quads=INPUT")
		:usage "Read RDF in N-Quads format from INPUT."
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base :input-type :nq)
		(maybe-execute))
	       (input-nt
		:options ("--input-nt=INPUT" "--input-n-triples=INPUT")
		:usage "Read RDF in N-Triples format from INPUT."
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base :input-type :nt)
		(maybe-execute))
	       (input-nlisp
		:options ("--input-n-lisp=INPUT" "--input-nl=INPUT")
		:usage "Read RDF in N-Lisp format from INPUT."
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base :input-type :nl)
		(maybe-execute))
	       (base
		:options ("--base=URL" ("-b" "URL"))
		:usage "Use URL as the base."
		(setf base (parse-iri value)))
	       (directory
		:options ("--directory=DIR" ("-d" "DIR"))
		:usage "Use DIR as the prefix for file lookup. You can use a file or an URL as DIR."
		(setf directory (parse-iri (if (http-or-file-iri-string-p value) value (format nil "file://~A" (expand-dirname value))))))
	       (graph
		:options ("--graph=URL" ("-g" "URL"))
		:usage "Use URL as the graph."
		(if (string= (string-downcase value) "default") nil (setf graph (parse-iri value))))
	       (t :usage ("" "Output options:" ""))
	       (ask-output
		:options ("--ask-output=FILE")
		:usage "Write ASK results to FILE. Output is based on the file name suffix."
		(setf ask-output-name value)
		(setf ask-output-type (intern-keyword (string-upcase (pathname-type (parse-namestring value))))))
	       (ask-output-ttl
		:options ("--ask-output-ttl=OUTPUT")
		:usage "Write ASK results in SPARQL XML result set format to OUTPUT."
		(setf ask-output-name value)
		(setf ask-output-type :ttl))
	       (ask-output-srx
		:options ("--ask-output-srx=OUTPUT")
		:usage "Write ASK results in SPARQL XML result set format to OUTPUT."
		(setf ask-output-name value)
		(setf ask-output-type :srx))
	       (select-output
		:options ("--select-output=FILE")
		:usage "Write SELECT results to FILE. Output is based on the file name suffix."
		(setf select-output-name value)
		(setf select-output-type (intern-keyword (string-upcase (pathname-type (parse-namestring value))))))
	       (select-output-append
		:options ("--select-output-append=FILE")
		:usage "Write SELECT results to FILE. Output is based on the file name suffix."
		(setf select-output-append-p t)
		(setf select-output-name value)
		(setf select-output-type (intern-keyword (string-upcase (pathname-type (parse-namestring value))))))
	       (select-output-csv
		:options ("--select-output-csv=OUTPUT")
		:usage "Write SELECT results in CSV format to OUTPUT."
		(setf select-output-name value)
		(setf select-output-type :csv))
	       (select-output-srx
		:options ("--select-output-srx=OUTPUT")
		:usage "Write SELECT results in SPARQL XML result set format to OUTPUT."
		(setf select-output-name value)
		(setf select-output-type :srx))
	       (select-output-ttl
		:options ("--select-output-ttl=OUTPUT")
		:usage "Write SELECT results in TTL format to OUTPUT."
		(setf select-output-name value)
		(setf select-output-type :ttl))
	       (construct-output
		:options ("--construct-output=FILE")
		:usage "Write CONSTRUCT results to FILE. Output format is based on the file name suffix."
		(setf construct-output-name value)
		(setf construct-output-type (let ((type (pathname-type (parse-namestring value))))
					      (and type (intern-keyword (string-upcase type))))))
	       (construct-output-append
		:options ("--construct-output-append=FILE")
		:usage "Write CONSTRUCT results to FILE. Output format is based on the file name suffix."
		(setf construct-output-append-p t)
		(setf construct-output-name value)
		(setf construct-output-type (let ((type (pathname-type (parse-namestring value))))
					      (and type (intern-keyword (string-upcase type))))))
	       (construct-output-trig
		:options ("--construct-output-trig=OUTPUT")
		:usage "Write CONSTRUCT results as TriG to OUTPUT."
		(setf construct-output-name value)
		(setf construct-output-type :trig))
	       (construct-output-ttl
		:options ("--construct-output-ttl=OUTPUT" "--construct-output-turtle=OUTPUT")
		:usage "Write CONSTRUCT results as Turtle to OUTPUT."
		(setf construct-output-name value)
		(setf construct-output-type :ttl))
	       (construct-output-lisp-block
		:options ("--construct-output-lisp-block=OUTPUT")
		:usage "Write CONSTRUCT results as Lisp blocks to OUTPUT."
		(setf construct-output-name value)
		(setf construct-output-type :lbl))
	       (construct-output-nq
		:options ("--construct-output-nq=OUTPUT" "--construct-output-n-quads=OUTPUT")
		:usage "Write CONSTRUCT results as N-Quads to OUTPUT."
		(setf construct-output-name value)
		(setf construct-output-type :nq))
	       (construct-output-nt
		:options ("--construct-output-nt=OUTPUT" "--construct-output-n-triples=OUTPUT")
		:usage "Write CONSTRUCT results as N-Triples to OUTPUT."
		(setf construct-output-name value)
		(setf construct-output-type :nt))
	       (construct-output-nlisp
		:options ("--construct-output-nl=OUTPUT" "--construct-output-n-lisp=OUTPUT")
		:usage "Write CONSTRUCT results as N-Lisp to OUTPUT."
		(setf construct-output-name value)
		(setf construct-output-type :nl))
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
		:usage ("Read RDF input in units of \"single\", \"block\", or \"document\". \"Single\" means"
			"that the input is read and processed one triple (or quad in TriG or N-Quads input)"
			"at a time. In N-Triples and N-Quads \"block\" has the same meaning as \"single\"."
			"In TriG it means that the input is read and processed based on the grammar rule [2g]"
			"of the TriG grammar, and in Turtle it means that the input is read and processed"
			"based on the grammar rule [6] of the Turtle grammar. The default is \"block\".")
		(setf (instans-rdf-input-unit instans) (intern-keyword (string-upcase value))))
	       (rdf-operations
		:options  ("--rdf-operations=LIST")
		:usage ("Apply a colon separated list of operations to the unit of RDF input. Operations are"
			"\"add\", \"remove\", \"flush\", \"execute\", \"execute-first\", \"execute-snapshot\","
			"\"execute-repeat-first\", and \"execute-repeat-snapshot\". The last four operations"
			"use the specified execution policy. \"First\" means to execute only the first rule"
			"instance in the queue, \"snapshot\" to execute the the instances currently in the"
			"queue, but not the new instances that are added to the queue during these instances."
			"\"Repeat\" before \"first\" or \"snapshot\" means to execute the system using that"
			"policy until the queue is empty. Operation \"execute\" is the a synonym to"
			"\"execute-repeat-first\". Operation \"flush\" flushes all pending output."
			"You can use \"event\" as a shorthand form \"add:execute:remove:execute\"."
			"The default operations list is \"add:execute\".")
		(set-instans-rdf-operations instans (parse-colon-separated-values value)))
	       (allow-rule-instance-removal
		:options ("--allow-rule-instance-removal=BOOL")
		:usage ("If true (the default), adding or removing RDF input removes rule instances that have"
			"not been executed yet from the rule instance queue, if they cease to be satisfied;"
			"if false, rule instances are not removed from the queue even when they cease to be"
			"satisfied when adding or removing RDF input.")
		(setf (instans-allow-rule-instance-removal-p instans)
		      (cond ((string-equal value "true") t)
			    ((string-equal value "false") nil)
			    (t (usage)))))
	       (t :usage ("" "Combos:" ""))
	       (input-triples
		:options ("--input-single=FILE")
		:usage "Same as '--rdf-input-unit=triple --input=FILE'"
		(setf (instans-rdf-input-unit instans) :single)
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base
						    :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
		(maybe-execute))
	       (input-blocks
		:options ("--input-blocks=FILE")
		:usage "Same as '--rdf-input-unit=block --input=FILE'"
		(setf (instans-rdf-input-unit instans) :block)
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base
						    :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
		(maybe-execute))
	       (input-document
		:options ("--input-document=FILE")
		:usage "Same as '--rdf-input-unit=document --input=FILE'"
		(setf (instans-rdf-input-unit instans) :document)
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base
						    :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
		(maybe-execute))
	       (input-events
		:options ("--input-events=FILE")
		:usage "Same as '--rdf-operations=event --rdf-input-unit=block --input=FILE'"
		(set-instans-rdf-operations instans :event)
		(setf (instans-rdf-input-unit instans) :block)
		(instans-add-stream-input-processor instans (expand-iri directory value)
						    :graph graph :base base
						    :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
		(maybe-execute))
	       (t :usage ("" "Miscelaneus debugging and testing options:" ""))
	       (warnings
		:options ("--warn-on-errors=BOOL")
		:usage ("If true, prints warnings, when FILTER or BIND evaluation causes an error. If false (the default), produces no output.")
		(cond ((string-equal value "true") (sparql-inform-and-throw-on-errors))
		      ((string-equal value "false") (sparql-throw-on-errors))
		      (t (usage))))
	       (verbose
		:options ("--verbose=SITUATIONS")
		:usage ("Print lots of information based on a comma-separated list of situations. Currently"
			"the possible states are \"parser\", which prints information on the generated SPARQL,"
			"TriG, Turtle, N-Quads, and N-Triples parsers, \"parse-operations\", which prints"
			"operations of the parser, and \"token\", which prints the recognized input tokens.")
		(loop for kind in debug
		      unless (member kind '(:parser :token :parse-operations :phases :triples))
		      do (usage))
		(setf debug (parse-colon-separated-values value)))
	       (rete-html
		:options ("--rete-html=FILE")
		:usage ("Create an HTML page about the Rete network. The HTML page contains the SPARQL query,"
			"a picture of the generate Rete network and other useful information.")
		(setf rete-html-file value))
	       (name
		:options ("--name=NAME" ("-n" "NAME"))
		:usage ("Use NAME as the name of the system. The name of system is used in generating"
			"various outputs and names during the execution of INSTANS, but the name does"
			"not bear any actual semantics.")
		(setf (instans-name instans) value))
	       (report
		:options ("--report=KINDS")
		:usage ("The kinds of rules you want to get reported; a ':' separated list of"
			"(select|construct|modify|all|rete-add|rete-remove|queue|rdf-operations|execute|memoryN|memoriesN)."
			"Here memoryN means a string like 'memory100' having an integer after 'memory'. This means that the interval of reporting is 100 rounds"
			"of execution. MemoryN reports the changes in total sizes of memories and memoriesN reports (in csv format) the sizes of different memories.")
		:hiddenp t
		(setf reporting (loop for kind in (parse-colon-separated-values value)
				      when (eq kind :all)
				      append '(:select t :construct t :modify t :all t :rete-add t :rete-remove t :queue t :rdf-operations t :execute t)
				      else when (eql 0 (search "MEMORY" (string kind)))
				      append (prog1 (list :memory-summaries) (parse-integer (string kind) :start 6))
				      else when (eql 0 (search "MEMORIES" (string kind)))
				      append (prog1 (list :memory-sizes) (parse-integer (string kind) :start 8))
				      else append (list kind t)))
		(loop for tail on reporting by #'cddr
		      unless (member (first tail) '(:select :construct :modify :rete-add :rete-remove :queue :call-succ-nodes :all :memory :memories :rdf-operations :execute))
		      do (usage))
		(initialize-reporting instans reporting))
	       (prefix-encoding
		:options ("--prefix-encoding=BOOL")
		:usage ("If true, use known prefixes when printing IRIs. If false (the default), print IRIs as such.")
		(instans-encode-prefixes instans (cond ((string-equal value "true") t)
						       ((string-equal value "false") nil)
						       (t (usage)))))
	       (print-prefix-encodings
		:options ("--print-prefix-encodings=BOOL")
		:usage ("If true (the default), print prefix definitions, when outputting results and with prefix encoding is on.")
		(setf (instans-print-prefix-encodings-p instans)
		      (cond ((string-equal value "true") t)
			    ((string-equal value "false") nil)
			    (t (usage)))))
	       (time
		:options ("--time=FILE")
		:usage "Output timing information to FILE. Use '-' for standard output."
		(setf time-output-name value)
		(multiple-value-setq (start-time-sec start-time-usec) (sb-unix::get-time-of-day))
		(setf time-output-stream
		      (if (string= value "-") *standard-output* (open-file value :direction :output :if-exists :supersede :fmt "main: open ~{~A~^ ~}"))))
	       (pause
		:options ("--pause")
		:usage "Pause and wait for user to press enter"
		(format *standard-output* "~&INSTANS process ~D paused. Press enter to continue: " (sb-unix:unix-getpid))
		(read-char))
	       (system
		:options ("--system=PATH")
		:usage "Execute a system command. PATH should name an executable program."
		(sb-ext:run-program value nil :pty *error-output* :search t))
	       (run-sparql-conformance-tests
		:options ("--run-sparql-conformance-tests==TEST_DIR")
		:usage "Run sparql test suites. Test suites should be in TEST_DIR/suites. The result is written into TEST_DIR/suites/results.csv"
		(run-sparql-test-suites value))
	       )
	     (unless executedp (execute))
	     instans)
	(when time-output-stream
	  (output-time "Done")
	  (close-stream-not-stdout-stderr time-output-stream))
	(instans-close-open-streams instans)))))
