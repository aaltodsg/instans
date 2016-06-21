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

(defvar *instans-command-cases* nil)

(eval-when (:compile-toplevel :load-toplevel)
  (defun set-output-file-name-and-type (instans default-directory value name-slot type-slot &key forced-type default-type)
    (cond ((equal value "-")
	   (setf (slot-value instans name-slot) "-")
	   (setf (slot-value instans type-slot) (or forced-type default-type)))
	  (t
	   (setf (slot-value instans name-slot) (expand-iri default-directory value))
	   (setf (slot-value instans type-slot)
		 (or forced-type
		     (let ((pnt (pathname-type (parse-namestring value)))) (and pnt (intern-keyword (string-upcase pnt))))
		     default-type)))))
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
;		(if rete-html-file (output-rete-html-page instans rules rete-html-file))
		)
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
       (set-output-file-name-and-type instans directory value 'ask-output-name 'ask-output-type :default-type :ttl))
      (ask-output-append
       :options ("--ask-output-append" :file)
       :usage "Append ASK results to <FILE>. Output is based on the file name suffix."
       (setf (instans-ask-output-append-p instans) t)
       (set-output-file-name-and-type instans directory value 'ask-output-name 'ask-output-type :default-type :ttl))
      (ask-output-ttl
       :options ("--ask-output-ttl" :file)
       :usage "Write ASK results in SPARQL XML result set format to <FILE>."
       (set-output-file-name-and-type instans directory value 'ask-output-name 'ask-output-type :forced-type :ttl))
      (ask-output-srx
       :options ("--ask-output-srx" :file)
       :usage "Write ASK results in SPARQL XML result set format to <FILE>."
       (set-output-file-name-and-type instans directory value 'ask-output-name 'ask-output-type :forced-type :srx))
      (select-output
       :options ("--select-output" :file)
       :usage "Write SELECT results to <FILE>. Output is based on the file name suffix."
       (set-output-file-name-and-type instans directory value 'select-output-name 'select-output-type :default-type :csv))
      (select-output-append
       :options ("--select-output-append" :file)
       :usage "Append SELECT results to <FILE>. Output is based on the file name suffix."
       (setf (instans-select-output-append-p instans) t)
       (set-output-file-name-and-type instans directory value 'select-output-name 'select-output-type :default-type :csv))
      (select-output-csv
       :options ("--select-output-csv" :file)
       :usage "Write SELECT results in CSV format to <FILE>."
       (set-output-file-name-and-type instans directory value 'select-output-name 'select-output-type :forced-type :csv))
      (select-output-srx
       :options ("--select-output-srx" :file)
       :usage "Write SELECT results in SPARQL XML result set format to <FILE>."
       (set-output-file-name-and-type instans directory value 'select-output-name 'select-output-type :forced-type :srx))
      (select-output-ttl
       :options ("--select-output-ttl" :file)
       :usage "Write SELECT results in SPARQL XML result set format to <FILE>."
       (set-output-file-name-and-type instans directory value 'select-output-name 'select-output-type :forced-type :ttl))
      (construct-output
       :options ("--construct-output" :file)
       :usage "Write CONSTRUCT results to <FILE>. Output format is based on the file name suffix."
       (set-output-file-name-and-type instans directory value 'construct-output-name 'construct-output-type :default-type :trig))
      (construct-output-append
       :options ("--construct-output-append" :file)
       :usage "Append CONSTRUCT results to <FILE>. Output format is based on the file name suffix."
       (setf (instans-construct-output-append-p instans) t)
       (set-output-file-name-and-type instans directory value 'construct-output-name 'construct-output-type :default-type :trig))
      (construct-output-trig
       :options ("--construct-output-trig" :file)
       :usage "Write CONSTRUCT results as TriG to <FILE>."
       (set-output-file-name-and-type instans directory value 'construct-output-name 'construct-output-type :forced-type :trig))
      (construct-output-ttl
       :options ("--construct-output-ttl" "--construct-output-turtle" :file)
       :usage "Write CONSTRUCT results as Turtle to <FILE>."
       (set-output-file-name-and-type instans directory value 'construct-output-name 'construct-output-type :forced-type :ttl))
      (construct-output-nq
       :options ("--construct-output-nq" "--construct-output-n-quads" :file)
       :usage "Write CONSTRUCT results as N-Quads to <FILE>."
       (set-output-file-name-and-type instans directory value 'construct-output-name 'construct-output-type :forced-type :nq))
      (construct-output-nt
       :options ("--construct-output-nt" "--construct-output-n-triples" :file)
       :usage "Write CONSTRUCT results as N-Triples to <FILE>."
       (set-output-file-name-and-type instans directory value 'construct-output-name 'construct-output-type :forced-type :nt))
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
      (optimize-filters
       :options ("--optimize-filters" :bool)
       :usage ("Tries to move filter nodes up in the RETE network to eliminate unnecessary token generation. Default is FALSE.")
       (setf (instans-optimize-filters-p instans)
	     (cond ((string-equal value "true") t)
		   ((string-equal value "false") nil)
		   (t (usage)))))
      (join-hash-index-type
       :options ("--join-hash-index-type" :one-of (:hash-token-index :hash-token-index2 :flexible-hash-token-index))
       :usage ("The type of hash index to use in join nodes. Type :hash-token-index maps keys to lists of tokens"
	       "using a hash table. Type :hash-token-index2 uses two-level hash tables: the first level maps the key"
	       "to another hash table, which contains the tokens with that key. Type :flexible-hash-taoken-index is"
	       "a combination of these. As long as a key maps to a small number of tokens, they are stored in a list,"
	       "but when a limit is reached, the tokens are put into a hash table")
       (setf (instans-join-hash-token-index-type instans) (intern (string-upcase (string value)) :instans))
       (inform "join-hash-index-type: ~S -> ~S" value (instans-join-hash-token-index-type instans)))
      (ordered-index
       :options ("--ordered-index" :string)
       :usage ("The named join node should have an ordered index that uses the given comparison"
	       "operator for joins. The parameters should be of form 'name:var1:op:var2', where op is one of"
	       "<, <=, >=, and > and var1 and var2 are SPARQL variable names")
       (let ((args (parse-colon-separated-strings value)))
	 (if (not (= 4 (length args)))
	     (usage)
	     (push (cons (intern (format nil "~@:(~A~)" (first args)))
			 (case (intern (third args))
			   (<= (list :alpha (list :var (fourth args) :equal-op #'%=% :order-op #'%>% :key-op #'%<=%)
				     :beta (list :var (second args) :equal-op #'%=% :order-op #'%<% :key-op #'%>=%)))
			   (< (list :alpha (list :var (fourth args)  :equal-op #'%=% :order-op #'%>% :key-op #'%<%)
				    :beta (list :var (second args) :equal-op #'%=% :order-op #'%<% :key-op #'%>%)))
			   (>= (list :alpha (list :var (fourth args)  :equal-op #'%=% :order-op #'%<% :key-op #'%>=%)
				     :beta (list :var (second args) :equal-op #'%=% :order-op #'%>% :key-op #'%<=%)))
			   (> (list :alpha (list :var (fourth args)  :equal-op #'%=% :order-op #'%<% :key-op #'%>%)
				    :beta (list :var (second args) :equal-op #'%=% :order-op #'%>% :key-op #'%<%)))
			   (t (usage))))
		   (instans-ordered-index-nodes instans)))
	 ;; (inform "Ordered-index-nodes = ~A" (instans-ordered-index-nodes instans))
	 ))
      (avl-index
       :options ("--avl-index" :string)
       :usage ("The named join node should have an avl index that uses the given comparison"
	       "operator for joins. The parameters should be of form 'name:var1:op:var2', where op is one of"
	       "<, <=, >=, and > and var1 and var2 are SPARQL variable names")
       (let ((args (parse-colon-separated-strings value)))
	 (if (not (= 4 (length args)))
	     (usage)
	     (push (cons (intern (format nil "~@:(~A~)" (first args)))
			 (case (intern (third args))
			   (< (list :alpha (list :var (fourth args) :range-getter #'(lambda (tree x) (avl-get-range tree :lower-bound x :lower-bound-inclusive-p nil)))
				    :beta (list :var (second args) :range-getter #'(lambda (tree x) (avl-get-range tree :upper-bound x :upper-bound-inclusive-p nil)))))
			   (<= (list :alpha (list :var (fourth args) :range-getter #'(lambda (tree x) (avl-get-range tree :lower-bound x :lower-bound-inclusive-p t)))
				     :beta (list :var (second args) :range-getter #'(lambda (tree x) (avl-get-range tree :upper-bound x :upper-bound-inclusive-p t)))))
			   (>= (list :alpha (list :var (fourth args) :range-getter #'(lambda (tree x) (avl-get-range tree :upper-bound x :upper-bound-inclusive-p t)))
				     :beta (list :var (second args) :range-getter #'(lambda (tree x) (avl-get-range tree :lower-bound x :lower-bound-inclusive-p t)))))
			   (> (list :alpha (list :var (fourth args) :range-getter #'(lambda (tree x) (avl-get-range tree :upper-bound x :upper-bound-inclusive-p nil)))
				    :beta (list :var (second args) :range-getter #'(lambda (tree x) (avl-get-range tree :lower-bound x :lower-bound-inclusive-p nil)))))
			   (t (usage))))
		   (instans-avl-index-nodes instans)))
	 ;; (inform "Avl-index-nodes = ~A" (instans-avl-index-nodes instans))
	 ))
      (comment
       :options ("--comment" :string)
       :usage "A comment to be printed"
       (inform "~A~%" value))
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
       (setf (instans-sizes-report-stream instans) (open-file report-sizes-file :direction :output :if-exists :supersede :message "main: open ~{~A~^ ~}"))
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
      (profile
       :options ("--profile" :file)
       :usage "Output profile information to <FILE>. Use '-' for standard output."
       (setf profile-report-file value)
       (sb-profile:unprofile)
       (profile-rete))
      (profile-functions
       :options ("--profile-functions" :file)
       :usage "Read a LISP list containing the names of functions to profile from <FILE>."
       (setf *rete-profiled-functions* (with-open-file (input value :direction :input) (read input)))
       (inform "Profiling functions ~A" *rete-profiled-functions*)
       (unless profile-report-file (setf profile-report-file "-"))
       (sb-profile:unprofile)
       (profile-rete))
      (time
       :options ("--time" :file)
       :usage "Output timing information to <FILE>. Use '-' for standard output."
       (setf time-output-name value)
       (multiple-value-setq (start-time-sec start-time-usec) (sb-unix::get-time-of-day))
       (setf time-output-stream
	     (if (string= value "-") *standard-output* (open-file value :direction :output :if-exists :supersede :message "main: open ~{~A~^ ~}"))))
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
       (run-sparql-test-suites :suites-dir value :output-test-options-to-file "test-options"))
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
    (let ((args-string  (format nil "~{~A~^ ~}" args))
	  (index 0)
	  (result nil))
      (labels ((getchar () (and (< index (length args-string)) (prog1 (char args-string index) (incf index))))
	       (skip-whitespace ()
		 (loop while (and (< index (length args-string)) (spacing-char-p (char args-string index)))
		       do (incf index)
		       finally (return (getchar))))
	       (eat-long-option ()
		 ;; (inform "eat-long-option ~A, ~A" args-string index)
		 (loop for char = (getchar)
		       while (and char (not (spacing-char-p char)) (not (char= char #\=)))
		       collect char into key-chars
		       finally (cond ((null key-chars)
				      (throw :error (format nil "~%Missing option name after '--' at index ~D of ~A" (1- index) args-string)))
				     ((and char (char= char #\=))
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

(defun parsed-options-to-string (parsed-options)
  (format nil "~{~A~^ ~}"
	  (loop for (key value) in parsed-options
		collect (cond ((characterp key)
			       (cond ((null value) (format nil "-~C" key))
				     (t (format nil "-~C ~A" key value))))
			      ((null value)
			       (format nil "--~A" key))
			      (t
			       (format nil "--~A=~A" key value))))))

(defun extract-options (args options)
  (loop for (key value) in (split-command-line-args args)
        when (member key options :test #'equal)
        collect (list key value) into extracted
        else collect (list key value) into remaining
        finally (return (values extracted remaining))))

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
	   (logmsg "In main ~S ~%       :exit-after-processing-args-p ~A :execute-immediately-p ~A" args exit-after-processing-args-p execute-immediately-p)
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
		  profile-report-file
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
		      (execute () (logmsg "Calling instans-run")
			       (instans-run instans))
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
		      (when (and execute-immediately-p (not executedp)) (execute))
		      instans)
		 (logmsg "At the end of main")
		 (when exit-after-processing-args-p
		   (logmsg "Final exit with ~A" *stream-open-close-report-output*)
		   (when time-output-stream
		     (output-time "Done")
		     (close-stream-not-stdout-stderr time-output-stream))
		   (when report-sizes-file
		     (close-stream-not-stdout-stderr (instans-sizes-report-stream instans)))
		   (cond ((equal profile-report-file "-")
			  (let ((*trace-output* *standard-output*))
			    (sb-profile:report)))
			 ((not (null profile-report-file))
			  (with-open-file (output profile-report-file :direction :output :if-exists :supersede)
			    (let ((*trace-output* output))
			      (sb-profile:report)))))
		   (if rete-html-file (output-rete-html-page instans (expand-iri directory rete-html-file)))
		   (instans-close-open-streams instans))))))))
    (logmsg "value=~A" value)
    value))
