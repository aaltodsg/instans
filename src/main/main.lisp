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

(defmacro old-parsing-commands (((key-var value-var) args-var &key program html usage before after) &body command-cases)
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

(defun old-main (args)
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
	 report-sizes-file
	 rete-html-file)
    (labels ((valid-value-p (value accepted-values &key test)
	       (or (funcall test value accepted-values)
		   (error* "Value ~A not one of ~A" value accepted-values)))
					;	       (expand-iri-or-file-path (base iri-or-file-path input-type)
	     (parse-parameters (string &key colon-expand-fields)
	       (loop for param in (parse-spec-string string)
		     for (key value) = param
		     collect (if (member key colon-expand-fields) (list key (intern-colon-separated-keywords value)) param)))
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
	     (old-parsing-commands ((key value) args :program "instans" :html html :usage usage
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
			"(type '.ttl' or '.turtle'), N-Triples (type '.nt' or '.n-triples'), and N-Quads"
			"(type '.nt' or '.n-quads'). If INPUT does not have a file type, use the type"
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
		(set-instans-rdf-operations instans (intern-colon-separated-keywords value)))
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
		(setf debug (intern-colon-separated-keywords value)))
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
			"(select|construct|modify|all|rete-add|rete-remove|queue|rdf-operations|execute|summaryN|sizesN)."
			"Here 'summaryN' means a string like 'summary100' having an integer after 'summary'. This means that the interval of reporting is 100 rounds"
			"of execution. Option 'summaryN' reports the changes in total sizes of storages and 'sizesN' reports (in csv format) the sizes of different storages.")
		;; :hiddenp nil
		(setf reporting (loop for kind in (intern-colon-separated-keywords value)
				      when (eq kind :all)
				      append '(:select t :construct t :modify t :all t :rete-add t :rete-remove t :queue t :rdf-operations t :execute t)
				      else when (eql 0 (search "SUMMARY" (string kind)))
				      append (list :storage-summary (parse-integer (string kind) :start 6))
				      else when (eql 0 (search "SIZES" (string kind)))
				      append (list :sizes (parse-integer (string kind) :start 8))
				      else append (list kind t)))
		(loop for tail on reporting by #'cddr
		      unless (member (first tail) '(:select :construct :modify :rete-add :rete-remove :queue :call-succ-nodes :all :storage-summary :sizes :rdf-operations :execute))
		      do (usage))
		(initialize-reporting instans reporting))
	       (report-sizes-file
		:options ("--report-sizes-file=FILE")
		:usage ("The CSV file to contain the sizes")
		(setf report-sizes-file value)
		(unless (getf reporting :sizes)
		  (setf reporting (cons :sizes (cons 1 reporting))))
		(setf (instans-sizes-report-stream instans) (open-file report-sizes-file :direction :output :if-exists :supersede :fmt "main: open ~{~A~^ ~}"))
		(initialize-reporting instans reporting)
		(report-sizes-headers instans))
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
		:options ("--run-sparql-conformance-tests==TEST_DIR[[:suite][:collection][:name]]")
		:usage "Run sparql test suites. Test suites should be in TEST_DIR/suites. If suite, collection, and/or name is present, run only the matching tests. The result is written into TEST_DIR/results/results.csv. Execution time is written in TEST_DIR/results/execution-time.txt"
		(run-sparql-test-suites value))
	       (run-suite-collection-name
		:options ("--run-suite-collection-name=TEST_DIR[[:suite][:collection][:name]]")
		:usage "Run sparql test suites. Test suites should be in TEST_DIR/suites. If suite, collection, and/or name is present, run only the matching tests. The result is written into TEST_DIR/results/results.csv. Execution time is written in TEST_DIR/results/execution-time.txt"
		(multiple-value-bind (test-dir suite collection name) (values-list (parse-colon-separated-strings value))
		  (run-suite-collection-name :root-directory test-dir :suite suite :collection collection :name name :reporting reporting))))
	     (unless executedp (execute))
	     instans)
	(when time-output-stream
	  (output-time "Done")
	  (close-stream-not-stdout-stderr time-output-stream))
	(when report-sizes-file
	  (close-stream-not-stdout-stderr (instans-sizes-report-stream instans)))
	(instans-close-open-streams instans)))))

(defvar *instans-command-cases* nil)

(eval-when (:compile-toplevel :load-toplevel)
  (defun split-options (options)
    (loop for option = (pop options)
	  while (and option (or (stringp option) (characterp option)))
	  collect option into option-texts
	  finally (return (list option-texts
				(loop for option-type = option then (pop options)
				      while option-type
				      when (member option-type '(:one-of :colon-list-of :parse-special)) collect (list option-type (pop options))
				      else collect option-type)))))
  (defun expand-command-cases (cases)
    (loop for case in cases
	  for options = (getf (rest case) :options)
	  for (option-names option-types) = (split-options options)
	  for expanded-options = (loop for name in option-names
				       nconc (if (null option-types) (list (list name nil))
						 (loop for type in option-types collect (list name type))))
	  for option-texts = (loop for (name type) in expanded-options
				   for (name-text separator) = (if (stringp name) (list (format nil "~(~A~)" name) "=") (list (format nil "-~C" name) " "))
				   collect (cond ((consp type)
						  (case (first type)
						    (:one-of (format nil "~A~A(~{~(~A~)~^|~})" name-text separator(second type)))
						    (:colon-list-of (format nil "~A~A<A ':' separated list of (~{~(~A~)~^|~})>" name-text separator (second type)))
						    (:parse-special (format nil "~A~A~A" name-text separator (second type)))
						    (t (error* "Unexpected option type ~A" type))))
						 ((null type)
						  (format nil "~A" name-text))
						 (t
						  (format nil "~A~A<~A>" name-text separator type))))
	  for ut = (getf (rest case) :usage)
	  for usage-texts = (if (consp ut) ut (list ut))
	  collect (append (list (first case)
				:expanded-options expanded-options
				:option-texts option-texts
				:usage-texts usage-texts)
			  (rest case))))

  (defun instans-command-cases ()
    `((t :usage ("" "Options are of form '-o', '-o PARAM', or '--option=PARAM'." ""
		    "General options:" ""))
      (usage
       :options ("--help" #\h)
       :usage "Print help text."
       (usage))
      (version
       :options ("--version" #\v)
       :usage "Print version information and exit."
       (format t "INSTANS version ~A~%" (instans-version))
       (return-from command-loop))
      (commands
       :options ("--file" #\f :file)
       :usage "Read options from <FILE>."
       (setf args (append (read-args-from-file value) args)))
      (t :usage ("" "Input options:" ""))
      (rules
       :options ("--rules" #\r :file :url)
       :usage "Load SPARQL rules from <FILE> or <URL>."
       (let ((rules (expand-iri directory value)))
	 ;; (set-output-processors)
	 (instans-add-rules instans rules)
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
       :options ("--input" #\i #\t :file :url)
       :usage ("Read RDF from <FILE> or <URL>. The suffix of the file or URL is used to determine the"
	       "type of the input. The recognized file formats are TriG (type '.trig'), Turtle"
	       "(type '.ttl' or '.turtle'), N-Triples (type '.nt' or '.n-triples'), and N-Quads"
	       "(type '.nt' or '.n-quads'). If the file or URL does not have a file type, use the type"
	       "specific input options below.")
       (instans-add-stream-input-processor instans (expand-iri directory value) :subscribe debug
					   :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
       (maybe-execute))
      (input-trig
       :options ("--input-trig" :file :url)
       :usage ("Read RDF in TriG format from <FILE> or <URL>. This can be a real file, a pseudo"
	       "file like /dev/stdint, or a URL. The content format should be of the specified type,"
	       "e.g., for '--input-trig' it should contain TriG format input. Even if the parameter"
	       "is a file with a specific type, the type is not considered before parsing.")
       (instans-add-stream-input-processor instans (expand-iri directory value) :input-type :trig)
       (maybe-execute))
      (input-turtle
       :options ("--input-turtle" "--input-ttl" :file :url)
       :usage "Read RDF in Turtle format from <FILE> or <URL>."
       (instans-add-stream-input-processor instans (expand-iri directory value) :input-type :ttl)
       (maybe-execute))
      (input-nq
       :options ("--input-nq" "--input-n-quads" :file :url)
       :usage "Read RDF in N-Quads format from <FILE> or <URL>."
       (instans-add-stream-input-processor instans (expand-iri directory value) :input-type :nq)
       (maybe-execute))
      (input-nt
       :options ("--input-nt" "--input-n-triples" :file :url)
       :usage "Read RDF in N-Quads format from <FILE> or <URL>."
       (instans-add-stream-input-processor instans (expand-iri directory value) :input-type :nt)
       (maybe-execute))
      (base
       :options ("--base" #\b :url)
       :usage "Use URL as the base."
       (setf (instans-base instans) (parse-iri value)))
      (directory
       :options ("--directory" #\d :dir)
       :usage "Use <DIR> as the prefix for file lookup. You can use a file or URL as <DIR>."
       (setf directory (parse-iri (if (http-or-file-iri-string-p value) value (format nil "file://~A" (expand-dirname value))))))
      (graph
       :options ("--graph" #\g :url)
       :usage "Use URL as the graph."
       (if (string= (string-downcase value) "default") nil (setf (instans-graph instans) (parse-iri value))))
      (t :usage ("" "Output options:" ""))
      (ask-output
       :options ("--ask-output" :file)
       :usage "Write ASK results to <FILE>. Output is based on the file name suffix."
       (setf (instans-ask-output-name instans) value)
       (setf (instans-ask-output-type instans) (intern-keyword (string-upcase (pathname-type (parse-namestring value))))))
      (ask-output-ttl
       :options ("--ask-output-ttl" :file)
       :usage "Write ASK results in SPARQL XML result set format to <FILE>."
       (setf (instans-ask-output-name instans) value)
       (setf (instans-ask-output-type instans) :ttl))
      (ask-output-srx
       :options ("--ask-output-srx" :file)
       :usage "Write ASK results in SPARQL XML result set format to <FILE>."
       (setf (instans-ask-output-name instans) value)
       (setf (instans-ask-output-type instans) :srx))
      (select-output
       :options ("--select-output" :file)
       :usage "Write SELECT results to <FILE>. Output is based on the file name suffix."
       (setf (instans-select-output-name instans) value)
       (setf (instans-select-output-type instans) (intern-keyword (string-upcase (pathname-type (parse-namestring value))))))
      (select-output-append
       :options ("--select-output-append" :file)
       :usage "Write SELECT results to <FILE>. Output is based on the file name suffix."
       (setf (instans-select-output-append-p instans) t)
       (setf (instans-select-output-name instans) value)
       (setf (instans-select-output-type instans) (intern-keyword (string-upcase (pathname-type (parse-namestring value))))))
      (select-output-csv
       :options ("--select-output-csv" :file)
       :usage "Write SELECT results in CSV format to <FILE>."
       (setf (instans-select-output-name instans) value)
       (setf (instans-select-output-type instans) :csv))
      (select-output-srx
       :options ("--select-output-srx" :file)
       :usage "Write SELECT results in SPARQL XML result set format to <FILE>."
       (setf (instans-select-output-name instans) value)
       (setf (instans-select-output-type instans) :srx))
      (select-output-ttl
       :options ("--select-output-ttl" :file)
       :usage "Write SELECT results in TTL format to <FILE>."
       (setf (instans-select-output-name instans) value)
       (setf (instans-select-output-type instans) :ttl))
      (construct-output
       :options ("--construct-output" :file)
       :usage "Write CONSTRUCT results to <FILE>. Output format is based on the file name suffix."
       (setf (instans-construct-output-name instans) value)
       (setf (instans-construct-output-type instans) (let ((type (pathname-type (parse-namestring value))))
				     (and type (intern-keyword (string-upcase type))))))
      (construct-output-append
       :options ("--construct-output-append" :file)
       :usage "Write CONSTRUCT results to <FILE>. Output format is based on the file name suffix."
       (setf (instans-construct-output-append-p instans) t)
       (setf (instans-construct-output-name instans) value)
       (setf (instans-construct-output-type instans) (let ((type (pathname-type (parse-namestring value))))
				     (and type (intern-keyword (string-upcase type))))))
      (construct-output-trig
       :options ("--construct-output-trig" :file)
       :usage "Write CONSTRUCT results as TriG to <FILE>."
       (setf (instans-construct-output-name instans) value)
       (setf (instans-construct-output-type instans) :trig))
      (construct-output-ttl
       :options ("--construct-output-ttl" "--construct-output-turtle" :file)
       :usage "Write CONSTRUCT results as Turtle to <FILE>."
       (setf (instans-construct-output-name instans) value)
       (setf (instans-construct-output-type instans) :ttl))
      (construct-output-nq
       :options ("--construct-output-nq" "--construct-output-n-quads" :file)
       :usage "Write CONSTRUCT results as N-Quads to <FILE>."
       (setf (instans-construct-output-name instans) value)
       (setf (instans-construct-output-type instans) :nq))
      (construct-output-nt
       :options ("--construct-output-nt" "--construct-output-n-triples" :file)
       :usage "Write CONSTRUCT results as N-Triples to <FILE>."
       (setf (instans-construct-output-name instans) value)
       (setf (instans-construct-output-type instans) :nt))
      (t :usage ("" "Execution control options:" ""))
      (execute
       :options ("--execute" #\e)
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
       :options ("--rdf-input-unit" :one-of (:single :block :document))
       :usage ("Read RDF input in units of \"single\", \"block\", or \"document\". \"Single\" means"
	       "that the input is read and processed one triple (or quad in TriG or N-Quads input)"
	       "at a time. In N-Triples and N-Quads \"block\" has the same meaning as \"single\"."
	       "In TriG it means that the input is read and processed based on the grammar rule [2g]"
	       "of the TriG grammar, and in Turtle it means that the input is read and processed"
	       "based on the grammar rule [6] of the Turtle grammar. The default is \"block\".")
       (setf (instans-rdf-input-unit instans) (intern-keyword (string-upcase value))))
      (rdf-operations
       :options  ("--rdf-operations" :colon-list-of (:add :remove :flush :execute :execute-first :execute-snapshot :execute-repeat-first :execute-repeat-snapshot))
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
       (set-instans-rdf-operations instans (intern-colon-separated-keywords value)))
      (allow-rule-instance-removal
       :options ("--allow-rule-instance-removal" :bool)
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
       :options ("--input-single" :file :url)
       :usage "Same as '--rdf-input-unit=triple --input=(<FILE>|<URL>)'"
       (setf (instans-rdf-input-unit instans) :single)
       (instans-add-stream-input-processor instans (expand-iri directory value) :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
       (maybe-execute))
      (input-blocks
       :options ("--input-blocks" :file :url)
       :usage "Same as '--rdf-input-unit=block --input=(<FILE>|<URL>)'"
       (setf (instans-rdf-input-unit instans) :block)
       (instans-add-stream-input-processor instans (expand-iri directory value) :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
       (maybe-execute))
      (input-document
       :options ("--input-document" :file :url)
       :usage "Same as '--rdf-input-unit=document --input=(<FILE>|<URL>)'"
       (setf (instans-rdf-input-unit instans) :document)
       (instans-add-stream-input-processor instans (expand-iri directory value) :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
       (maybe-execute))
      (input-events
       :options ("--input-events" :file :url)
       :usage "Same as '--rdf-operations=event --rdf-input-unit=block --input=(<FILE>|<URL>)'"
       (set-instans-rdf-operations instans :event)
       (setf (instans-rdf-input-unit instans) :block)
       (instans-add-stream-input-processor instans (expand-iri directory value) :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring value)))))
       (maybe-execute))
      (t :usage ("" "Miscelaneus debugging and testing options:" ""))
      (warnings
       :options ("--warn-on-errors" :bool)
       :usage ("If true, prints warnings, when FILTER or BIND evaluation causes an error."
	       "If false (the default), produces no output.")
       (cond ((string-equal value "true") (sparql-inform-and-throw-on-errors))
	     ((string-equal value "false") (sparql-throw-on-errors))
	     (t (usage))))
      (verbose
       :options ("--verbose" :colon-list-of (:parser :parse-operations :token))
       :usage ("Print lots of information based on a comma-separated list of situations. Currently"
	       "the possible states are \"parser\", which prints information on the generated SPARQL,"
	       "TriG, Turtle, N-Quads, and N-Triples parsers, \"parse-operations\", which prints"
	       "operations of the parser, and \"token\", which prints the recognized input tokens.")
       (loop for kind in debug
	     unless (member kind '(:parser :token :parse-operations :phases :triples))
	     do (usage))
       (setf debug (intern-colon-separated-keywords value)))
      (rete-html
       :options ("--rete-html" :file)
       :usage ("Create an HTML page about the Rete network. The HTML page contains the SPARQL query,"
	       "a picture of the generate Rete network and other useful information.")
       (setf rete-html-file value))
      (name
       :options ("--name" #\n :string)
       :usage ("Use <STRING> as the name of the system. The name of system is used in generating"
	       "various outputs and names during the execution of INSTANS, but the name does"
	       "not bear any actual semantics.")
       (setf (instans-name instans) value))
      (report
       :options ("--report" :colon-list-of (:select :construct :modify :all :rete-add :rete-remove :queue :rdf-operations :execute :summary-n :sizes-n))
       :usage ("The kinds of rules you want to get reported. Here 'summaryN' means a string like"
	       "'summary100' having an integer after 'summary'. This means that the interval of"
	       "reporting is 100 rounds of execution. Option 'summaryN' reports the changes in"
	       "total sizes of storages and 'sizesN' reports (in csv format) the sizes of"
	       "different storages.")
       ;; :hiddenp nil
       (setf reporting (loop for kind in (intern-colon-separated-keywords value)
			     when (eq kind :all)
			     append '(:select t :construct t :modify t :all t :rete-add t :rete-remove t :queue t :rdf-operations t :execute t)
			     else when (eql 0 (search "SUMMARY" (string kind)))
			     append (list :storage-summary (parse-integer (string kind) :start 6))
			     else when (eql 0 (search "SIZES" (string kind)))
			     append (list :sizes (parse-integer (string kind) :start 8))
			     else append (list kind t)))
       (loop for tail on reporting by #'cddr
	     unless (member (first tail) '(:select :construct :modify :rete-add :rete-remove :queue :call-succ-nodes :all :storage-summary :sizes :rdf-operations :execute))
	     do (usage))
       (initialize-reporting instans reporting))
      (report-sizes-file
       :options ("--report-sizes-file" :file)
       :usage ("The CSV file to contain the sizes")
       (setf report-sizes-file value)
       (unless (getf reporting :sizes)
	 (setf reporting (cons :sizes (cons 1 reporting))))
       (setf (instans-sizes-report-stream instans) (open-file report-sizes-file :direction :output :if-exists :supersede :fmt "main: open ~{~A~^ ~}"))
       (initialize-reporting instans reporting)
       (report-sizes-headers instans))
      (prefix-encoding
       :options ("--prefix-encoding" :bool)
       :usage ("If true, use known prefixes when printing IRIs. If false (the default),"
	       "print IRIs as such.")
       (instans-encode-prefixes instans (cond ((string-equal value "true") t)
					      ((string-equal value "false") nil)
					      (t (usage)))))
      (print-prefix-encodings
       :options ("--print-prefix-encodings" :bool)
       :usage ("If true (the default), print prefix definitions, when outputting results"
	       "and with prefix encoding is on.")
       (setf (instans-print-prefix-encodings-p instans)
	     (cond ((string-equal value "true") t)
		   ((string-equal value "false") nil)
		   (t (usage)))))
      (time
       :options ("--time" :file)
       :usage "Output timing information to <FILE>. Use '-' for standard output."
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
       :options ("--system" :file)
       :usage "Execute a system command. <FILE> should name an executable program."
       (sb-ext:run-program value nil :pty *error-output* :search t))
      (run-sparql-conformance-tests
       :options ("--run-sparql-conformance-tests" :dir)
       :usage ("Run sparql test suites. The result is written into <DIR>/results/results.csv."
	       "Execution time is written in <DIR>/results/execution-time.txt")
       (run-sparql-test-suites value))
      (sparql-conformance-tests-dir
       :options ("--sparql-conformance-tests-dir" :dir)
       :usage "Set the SPARQL conformance tests root directory")
      (run-suite-collection-name
       :options ("--run-suite-collection-name" :parse-special "[<SUITE>[:<COLLECTION>[:<NAME>]]]")
       :usage ("Run sparql test suites from directory specified by option --sparql-conformance-tests-dir."
	       "Only tests matching <SUITE>, <COLLECTION>, and <NAME> are executed. Missing name"
	       "matches all tests in a collection, missing collection matches all collections"
	       "of a suite, and missing suite matches all suites.")
       (multiple-value-bind (test-dir suite collection name) (values-list (parse-colon-separated-strings value))
	 (run-suite-collection-name :root-directory test-dir :suite suite :collection collection :name name :reporting reporting))))
    )

  (defun split-command-case-parameters (case)
    (list (pop case)
	  (loop while (and case (keywordp (first case)))
		nconc (list (pop case) (pop case)))
	  case))

  (defun usage-string (expanded-cases)
    (apply #'concatenate 'string
	   (loop with max-usage-text-left-margin = 40
		 with two-column-max-option-width = (- max-usage-text-left-margin 3) 
		 with max-option-length = (loop for case in expanded-cases unless (eq (first case) t)
						maximize (loop for option-text in (getf (rest case) :option-texts)
							       maximize (min two-column-max-option-width (length option-text))))
		 for usage-text-left-margin = (+ 3 max-option-length)
		 for case in expanded-cases
		 for option-texts = (getf (rest case) :option-texts)
		 for usage-texts = (getf (rest case) :usage-texts)
		 for separatep = nil then t
					;        do (inform "~S" case)
		 nconc (cond ((eq (first case) t) (list (format nil "~:[~;~%~]~%~{~A~^~%~}" separatep usage-texts)))
			     (t
			      (loop for option-text in option-texts
				    collect (cond ((or (> (length option-text) two-column-max-option-width)
						       (null usage-texts))
						   (format nil "~%  ~A" option-text))
						  (t
						   (format nil "~%  ~A~VT~A" option-text usage-text-left-margin (pop usage-texts))))
				    into head
				    finally (let ((tail (loop for usage-text in usage-texts
							      collect (format nil "~%~VT~A" usage-text-left-margin usage-text))))
					      (return (if separatep (cons (format nil "~%") (append head tail)) (append head tail))))))))))

  (defun split-command-line-args (args)
    (logmsg "split-command-line-args ~S" args)
    (let ((args-string  (format nil "~{~A~^ ~}" args))
	  (index 0)
	  (result nil))
      (labels ((getchar () (and (< index (length args-string)) (prog1 (char args-string index) (incf index))))
	       (skip-whitespace ()
		 (loop while (and (< index (length args-string)) (spacing-char-p (char args-string index)))
		       do (incf index)
		       finally (return (getchar))))
	       (eat-long-option ()
		 (loop for char = (getchar)
		       while (and char (not (spacing-char-p char)) (not (char= char #\=)))
		       collect char into key-chars
		       finally (cond ((null key-chars)
				      (throw :error (format nil "~%Missing option name after '--' at index ~D of ~A" (1- index) args-string)))
				     ((char= char #\=)
				      (loop for char = (getchar)
					    while (and char (not (spacing-char-p char)))
					    collect char into value-chars
					    finally (return-from eat-long-option (list (coerce key-chars 'string) (coerce value-chars 'string)))))
				     (t
				      (return-from eat-long-option (list (coerce key-chars 'string)))))))
	       (eat-short-option ()
		 (let ((option-char (getchar)))
		   (cond ((null option-char)
			  (throw :error (format nil "~%Missing option char after '-' at the end of ~A" args-string)))
			 (t
			  (let ((next-char (getchar)))
			    (cond ((null next-char)
				   (list option-char))
				  ((not (spacing-char-p next-char))
				   (throw :error (format nil "~%Expecting a space ' ' after option -~C at position ~D in ~A" option-char (1- index) args-string)))
				  (t
				   (setf next-char (getchar))
				   (cond ((null next-char)
					  (list option-char))
					 ((char= next-char #\-)
					  (decf index)
					  (list option-char))
					 (t
					  (decf index)
					  (loop for char = (getchar)
						while (and char (not (spacing-char-p char)))
						collect char into value-chars
						finally (return-from eat-short-option (list option-char (coerce value-chars 'string))))))))))))))
	(loop for char = (skip-whitespace)
	      do (cond ((null char) (return (reverse result)))
		       ((char= char #\-)
			(let ((next-char (getchar)))
			  (cond ((null next-char)
				 (throw :error (format nil "~%Missing argument after '-' at the end of ~A" args-string)))
				((char= next-char #\-)
				 (push (eat-long-option) result))
				(t
				 (decf index)
				 (push (eat-short-option) result)))))
		       (t
			(throw :error (format nil "~%Expecting '-' at index ~D of ~A" (1- index) args-string)))))
	(reverse result))))

  (setf *instans-command-cases* (expand-command-cases (instans-command-cases)))
  )

(defmacro parsing-instans-commands ((key-var value-var) args-var &key before after)
  (let* ((expanded-command-cases *instans-command-cases*)
	 (split-args-var (gensym "SPLITARGS"))
	 (program "instans"))
    (setf *instans-command-cases* expanded-command-cases)
    `(flet ((usage () (format *error-output* "usage: ~A options~%~A~%" ,program ,(usage-string expanded-command-cases))))
       (let ((,split-args-var (split-command-line-args ,args-var)))
	 (cond ((null ,split-args-var) (usage))
	       (t
		(loop for (,key-var ,value-var) in ,split-args-var
		     ,@(if before (list 'do before))
		      do (cond ,@(loop for case in expanded-command-cases
				       for (casename properties body) = (split-command-case-parameters case)
				       for expanded-options = (getf properties :expanded-options)
				       unless (eq casename t)
				       nconc (loop for option in expanded-options
						   for name = (first option)
						   for type = (second option)
						   collect (if (stringp name)
							       `((equal ,(subseq name 2) ,key-var) ,@body)
							       `((equal ,name ,key-var) ,@body))))
			       (t
				(format *error-output* "~%Unrecognized option ~A~%" ,key-var)
				(usage)))
		     ,@(if after (list 'do after)))))))))

(defun logmsg (msg &rest args)
  (with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
    (apply #'format str (format nil "~%~A~%" msg) args)))

(defun logdescribe (object)
  (with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
    (let ((*standard-output* str))
      (describe object))))

(defun main (args &key instans (exit-after-processing-args-p (null instans)) (execute-immediately-p t))
  ;; (cond ((null args)
  ;; 	 (setf args sb-ext:*posix-argv*))
  ;; 	(t
  ;; 	 (setf args (cons "instans" (cons "--end-toplevel-options" (if (= 1 (length args)) (split-string (first args) " ") args))))))
  (let ((value
	 (catch :error
	   (logmsg "In main ~S" args)
	   (cond ((null args)
		  (setf args sb-ext:*posix-argv*))
		 ((stringp args)
		  (setf args (cons "instans" (split-string args " ")))))
	   (unless instans (setf instans (create-instans)))
	   (let* ((executedp nil)
		  (directory (parse-iri (format nil "file://~A" (expand-dirname (or *default-main-dir* ".")))))
		  time-output-name
		  time-output-stream
		  start-time-sec
		  start-time-usec
		  ;; expected
		  debug
		  reporting
		  report-sizes-file
		  rete-html-file)
	     (labels ((valid-value-p (value accepted-values &key test)
			(or (funcall test value accepted-values)
			    (error* "Value ~A not one of ~A" value accepted-values)))
					;	       (expand-iri-or-file-path (base iri-or-file-path input-type)
		      (parse-parameters (string &key colon-expand-fields)
			(loop for param in (parse-spec-string string)
			      for (key value) = param
			      collect (if (member key colon-expand-fields) (list key (intern-colon-separated-keywords value)) param)))
		      ;; (set-output-processors ()
		      ;; 	(when (and (instans-ask-output-type instans) (instans-ask-output-name instans) (null (instans-ask-output-processor instans)))
		      ;; 	  (inform "(instans-ask-output-type instans) ~A (instans-ask-output-name instans) ~A" (instans-ask-output-type instans) (instans-ask-output-name instans))
		      ;; 	  (setf (instans-ask-output-processor instans) (create-ask-output-processor instans (instans-ask-output-name instans) (instans-ask-output-type instans))))
		      ;; 	(when (and (instans-select-output-type instans) (null (instans-select-output-processor instans)))
		      ;; 	  (setf (instans-select-output-processor instans) (create-select-output-processor instans (instans-select-output-name instans) (instans-select-output-type instans) :appendp (instans-select-output-append-p instans))))
		      ;; 	(when (and (instans-construct-output-type instans) (null (instans-construct-output-processor instans)))
		      ;; 	  (setf (instans-construct-output-processor instans) (create-construct-output-processor instans (instans-construct-output-name instans) (instans-construct-output-type instans) :appendp (instans-construct-output-append-p instans)))))
		      (execute () (instans-run instans))
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
		      (parsing-instans-commands (key value) args :before (when time-output-stream (output-time "Command: ~(~A~), Parameter: ~A" key value)))
		      (unless executedp (execute))
		      instans)
		 (when exit-after-processing-args-p
		   (when time-output-stream
		     (output-time "Done")
		     (close-stream-not-stdout-stderr time-output-stream))
		   (when report-sizes-file
		     (close-stream-not-stdout-stderr (instans-sizes-report-stream instans)))
		   (instans-close-open-streams instans))))))))
    (logmsg "value=~A" value)
    value))
