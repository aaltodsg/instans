;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class abstract-sparql-turtle-lexer (abstract-lexer)
  ((base :accessor lexer-base :initarg :base :initform (parse-iri "http://"))
   (prefix-table :accessor lexer-prefix-table :initarg :prefix-table :initform (make-binding-table))
   (string-table :accessor lexer-string-table :initarg :string-table :initform (make-binding-table :weakness :value))
   (keyword-table :accessor lexer-keyword-table :initarg :keyword-table :initform nil)
   (instans :accessor lexer-instans :initarg :instans)))

(defgeneric make-keyword-table (abstract-sparql-turtle-lexer))

(defmethod initialize-instance :after ((this abstract-sparql-turtle-lexer) &key &allow-other-keys)
  (setf (lexer-keyword-table this) (make-keyword-table this))
  (bind-prefix this "xsd" (parse-iri "http://www.w3.org/2001/XMLSchema#"))
  (when (lexer-base this)
    (set-lexer-base this (lexer-base this))))

(defgeneric set-lexer-base (lexer iri)
  (:method ((this abstract-sparql-turtle-lexer) iri)
    (when (null (rdf-iri-scheme iri))
      (warn "Base does not define a scheme: ~A" iri)
      (describe iri)
      (lexer-error this "Base does not define a scheme: ~A" iri))
    ;; (inform "Lexer base:")
    ;; (describe iri)
    (setf (lexer-base this) iri)))

(defgeneric lexer-expand-iri (lexer iri-string)
  (:method ((this abstract-sparql-turtle-lexer) iri-string)
    (multiple-value-bind (expanded error-msg)
	(expand-iri (lexer-base this) iri-string)
      (cond ((null error-msg) expanded)
	    (t
	     (lexer-error this error-msg))))))

(defun expand-iri (base iri-string)
;  (inform "expand-iri ~S ~S" base iri-string)
  (let ((iri (parse-iri iri-string)))
    (cond ((rdf-iri-scheme iri)
;	   (inform "~S has scheme ~S, we use it" iri (rdf-iri-scheme iri))
	   iri)
	  ;; (cond ((rdf-iri-had-dot-segments-p iri)
	  ;; 	    (recompose-iri iri))
	  ;; 	   (t iri)))
	  (t
	   (let ((cp (rdf-iri-path iri)))
	     (cond ((null base)
		    (values nil (format nil "Base not defined for relative IRI ~S" iri-string)))
		   (t
		    (setf (rdf-iri-scheme iri) (rdf-iri-scheme base))
		    (when (not (rdf-iri-authority iri))
		      (setf (rdf-iri-authority iri) (rdf-iri-authority base))
		      (cond ((string= cp "")
			     (setf (rdf-iri-path iri) (rdf-iri-path base))
			     (unless (rdf-iri-query iri)
			       (setf (rdf-iri-query iri) (rdf-iri-query base))))
			    (t
			     (when (or (zerop (length cp)) (not (char= (char cp 0) #\/)))
			       (setf (rdf-iri-path iri)
				     (cond ((and (rdf-iri-authority base) (string= (rdf-iri-path base) ""))
					    (concatenate 'string "/" (rdf-iri-path iri)))
					   (t
					    (let ((lsp (position #\/ (rdf-iri-path base) :from-end t)))
					      (cond ((null lsp)
						     (rdf-iri-path iri))
						    (t
						     (coerce (remove-dot-segments (concatenate 'list (subseq (rdf-iri-path base) 0 (1+ lsp)) (rdf-iri-path iri))) 'string)))))))))))
		    (recompose-iri iri))))))))

(defgeneric resolve-prefix (lexer buf &optional start end)
  (:method ((this abstract-sparql-turtle-lexer) buf &optional (start 0) (end (chbuf-index buf)))
    (find-binding (lexer-prefix-table this) (chbuf-contents buf) :start start :end end :case-sensitive-p t :updatep t)))

(defgeneric pname-to-iri (lexer prefix-binding &optional suffix-string)
  (:method ((this abstract-sparql-turtle-lexer) prefix-binding &optional suffix-string)
    (lexer-expand-iri this (cond ((null (cdr prefix-binding))
;			    (error* "Unbound prefix binding ~A" (car prefix-binding)))
;			    (lexer-error this "Unbound prefix binding ~A" (car prefix-binding)))
			    nil)
			   ((null suffix-string)
			    (cdr prefix-binding))
			   (t
			    (concatenate 'string (rdf-iri-string (second prefix-binding)) suffix-string))))))

(defgeneric rebind-prefix (lexer binding expansion)
  (:method ((this abstract-sparql-turtle-lexer) binding expansion)
    (declare (ignorable this))
    (setf (cdr binding) (list expansion))))

(defgeneric bind-prefix (lexer prefix expansion)
  (:method ((this abstract-sparql-turtle-lexer) prefix expansion)
    (let ((binding (find-binding (lexer-prefix-table this) prefix :updatep t)))
      (rebind-prefix this binding expansion))))

(defgeneric canonize-string (lexer buf &optional start end)
  (:method ((this abstract-sparql-turtle-lexer) buf &optional (start 0) (end (chbuf-index buf)))
    (first (find-binding (lexer-string-table this) (chbuf-contents buf) :start start :end end :case-sensitive-p t :updatep t))))

(defgeneric resolve-keyword (lexer buf &optional start end)
  (:method ((this abstract-sparql-turtle-lexer) buf &optional (start 0) (end (chbuf-index buf)))
    (find-binding (lexer-keyword-table this) (chbuf-contents buf) :start start :end end :case-sensitive-p nil :updatep nil :case-sensitive-p nil)))

(define-class sparql-lexer (abstract-sparql-turtle-lexer)
  ((buffered-input-token :accessor lexer-buffered-input-token :initform nil)))

(define-class turtle-lexer (abstract-sparql-turtle-lexer) ())

(defmethod make-keyword-table ((this sparql-lexer))
  (declare (ignorable this))
  (let ((table (make-binding-table)))
    (loop for (keyword . terminal) in '(("A" . A-TERMINAL)
					("ABS" . ABS-TERMINAL)
					("ADD" . ADD-TERMINAL)
					("ALL" . ALL-TERMINAL)
					("AS" . AS-TERMINAL)
					("ASC" . ASC-TERMINAL)
					("ASK" . ASK-TERMINAL)
					("AVG" . AVG-TERMINAL)
					("BASE" . BASE-TERMINAL)
					("BIND" . BIND-TERMINAL)
					("BNODE" . BNODE-TERMINAL)
					("BOUND" . BOUND-TERMINAL)
					("BY" . BY-TERMINAL)
					("CEIL" . CEIL-TERMINAL)
					("CLEAR" . CLEAR-TERMINAL)
					("COALESCE" . COALESCE-TERMINAL)
					("CONCAT" . CONCAT-TERMINAL)
					("CONSTRUCT" . CONSTRUCT-TERMINAL)
					("CONTAINS" . CONTAINS-TERMINAL)
					("COPY" . COPY-TERMINAL)
					("COUNT" . COUNT-TERMINAL)
					("CREATE" . CREATE-TERMINAL)
					("DATA" . DATA-TERMINAL)
					("DATATYPE" . DATATYPE-TERMINAL)
					("DAY" . DAY-TERMINAL)
					("DEFAULT" . DEFAULT-TERMINAL)
					("DELETE" . DELETE-TERMINAL)
					("DELETE DATA" . DELETE-DATA-TERMINAL)
					("DELETE WHERE" . DELETE-WHERE-TERMINAL)
					("DESC" . DESC-TERMINAL)
					("DESCRIBE" . DESCRIBE-TERMINAL)
					("DISTINCT" . DISTINCT-TERMINAL)
					("DROP" . DROP-TERMINAL)
					("ENCODE_FOR_URI" . ENCODE_FOR_URI-TERMINAL)
					("EXISTS" . EXISTS-TERMINAL)
					("FALSE" . FALSE-TERMINAL)
					("FILTER" . FILTER-TERMINAL)
					("FLOOR" . FLOOR-TERMINAL)
					("FROM" . FROM-TERMINAL)
					("GRAPH" . GRAPH-TERMINAL)
					("GROUP" . GROUP-TERMINAL)
					("GROUP_CONCAT" . GROUP_CONCAT-TERMINAL)
					("HAVING" . HAVING-TERMINAL)
					("HOURS" . HOURS-TERMINAL)
					("IF" . IF-TERMINAL)
					("IN" . IN-TERMINAL)
					("INSERT" . INSERT-TERMINAL)
					("INSERT DATA" . INSERT-DATA-TERMINAL)
					("INTO" . INTO-TERMINAL)
					("IRI" . IRI-TERMINAL)
					("ISBLANK" . ISBLANK-TERMINAL)
					("ISIRI" . ISIRI-TERMINAL)
					("ISLITERAL" . ISLITERAL-TERMINAL)
					("ISNUMERIC" . ISNUMERIC-TERMINAL)
					("ISURI" . ISURI-TERMINAL)
					("LANG" . LANG-TERMINAL)
					("LANGMATCHES" . LANGMATCHES-TERMINAL)
					("LCASE" . LCASE-TERMINAL)
					("LIMIT" . LIMIT-TERMINAL)
					("LOAD" . LOAD-TERMINAL)
					("MAX" . MAX-TERMINAL)
					("MD5" . MD5-TERMINAL)
					("MIN" . MIN-TERMINAL)
					("MINUS" . MINUS-TERMINAL)
					("MINUTES" . MINUTES-TERMINAL)
					("MONTH" . MONTH-TERMINAL)
					("MOVE" . MOVE-TERMINAL)
					("NAMED" . NAMED-TERMINAL)
					("NOT" . NOT-TERMINAL)
					("NOW" . NOW-TERMINAL)
					("OFFSET" . OFFSET-TERMINAL)
					("OPTIONAL" . OPTIONAL-TERMINAL)
					("ORDER" . ORDER-TERMINAL)
					("PREFIX" . PREFIX-TERMINAL)
					("RAND" . RAND-TERMINAL)
					("REDUCED" . REDUCED-TERMINAL)
					("REGEX" . REGEX-TERMINAL)
					("REPLACE" . REPLACE-TERMINAL)
					("ROUND" . ROUND-TERMINAL)
					("SAMETERM" . SAMETERM-TERMINAL)
					("SAMPLE" . SAMPLE-TERMINAL)
					("SECONDS" . SECONDS-TERMINAL)
					("SELECT" . SELECT-TERMINAL)
					("SEPARATOR" . SEPARATOR-TERMINAL)
					("SERVICE" . SERVICE-TERMINAL)
					("SHA1" . SHA1-TERMINAL)
					("SHA256" . SHA256-TERMINAL)
					("SHA384" . SHA384-TERMINAL)
					("SHA512" . SHA512-TERMINAL)
					("SILENT" . SILENT-TERMINAL)
					("STR" . STR-TERMINAL)
					("STRAFTER" . STRAFTER-TERMINAL)
					("STRBEFORE" . STRBEFORE-TERMINAL)
					("STRDT" . STRDT-TERMINAL)
					("STRENDS" . STRENDS-TERMINAL)
					("STRLANG" . STRLANG-TERMINAL)
					("STRLEN" . STRLEN-TERMINAL)
					("STRSTARTS" . STRSTARTS-TERMINAL)
					("STRUUID" . STRUUID-TERMINAL)
					("SUBSTR" . SUBSTR-TERMINAL)
					("SUM" . SUM-TERMINAL)
					("TIMEZONE" . TIMEZONE-TERMINAL)
					("TO" . TO-TERMINAL)
					("TRUE" . TRUE-TERMINAL)
					("TZ" . TZ-TERMINAL)
					("UCASE" . UCASE-TERMINAL)
					("UNDEF" . UNDEF-TERMINAL)
					("UNION" . UNION-TERMINAL)
					("URI" . URI-TERMINAL)
					("USING" . USING-TERMINAL)
					("UUID" . UUID-TERMINAL)
					("VALUES" . VALUES-TERMINAL)
					("WHERE" . WHERE-TERMINAL)
					("WITH" . WITH-TERMINAL)
					("YEAR" . YEAR-TERMINAL)
					;;; Begin Extension
					("ASSERT" . ASSERT-TERMINAL)
					("ERROR" . ERROR-TERMINAL)
					("INPUT" . INPUT-TERMINAL)
					("OUTPUT" . OUTPUT-TERMINAL)
					("SOLUTIONS" . SOLUTIONS-TERMINAL)
					("TRIPLES" . TRIPLES-TERMINAL)
					("TEST" . TESTS-TERMINAL)
					;;; End Extension
					)
	  for binding = (find-binding table keyword :case-sensitive-p nil :updatep t)
	  do (setf (cdr binding) terminal))
    table))

(defmethod make-keyword-table ((this turtle-lexer))
  (declare (ignorable this))
  (let ((table (make-binding-table)))
    (loop for (keyword . terminal) in '(("a" . A-TERMINAL)
					("BASE" . BASE-TERMINAL)
					("FALSE" . FALSE-TERMINAL)
					("PREFIX" . PREFIX-TERMINAL)
					("TRUE" . TRUE-TERMINAL)
					("@base" . @BASE-TERMINAL)
					("@prefix" . @PREFIX-TERMINAL))
	  for binding = (find-binding table keyword :case-sensitive-p nil)
	  do (setf (cdr binding) terminal))
    table));;; Use these to get an apropriate lexer

(defmethod get-input-token ((lexer turtle-lexer))
  (next-input-token lexer))

(defmethod get-input-token ((lexer sparql-lexer))
  (let* ((input-token1 (if (lexer-buffered-input-token lexer)
			   (prog1 (lexer-buffered-input-token lexer)
			     (setf (lexer-buffered-input-token lexer) nil))
			   (next-input-token lexer)))
	 (type1 (input-token-type input-token1)))
;    (inform "get-input-token i1=~S, t1=~S" input-token1 type1)
    (cond ((member type1 '(INSERT-TERMINAL DELETE-TERMINAL |(-TERMINAL| |[-TERMINAL|))
	   (let* ((input-token2 (next-input-token lexer))
		  (type2 (input-token-type input-token2)))
;	     (inform "get-input-token i2=~S, t2=~S" input-token2 type2)
	     (cond ((and (eq type1 'INSERT-TERMINAL) (eq type2 'DATA-TERMINAL))
		    (make-input-token :type 'INSERT-DATA-TERMINAL :value "INSERT DATA"))
		   ((and (eq type1 'DELETE-TERMINAL) (eq type2 'DATA-TERMINAL))
		    (make-input-token :type 'DELETE-DATA-TERMINAL :value "DELETE DATA"))
		   ((and (eq type1 'DELETE-TERMINAL) (eq type2 'WHERE-TERMINAL))
		    (make-input-token :type 'DELETE-WHERE-TERMINAL :value "DELETE WHERE"))
		   ((and (eq type1 '|(-TERMINAL|) (eq type2 '|)-TERMINAL|))
		    (make-input-token :type 'NIL-TERMINAL :value (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")))
		   ((and (eq type1 '|[-TERMINAL|) (eq type2 '|]-TERMINAL|))
		    (make-input-token :type 'ANON-TERMINAL :value "ANON"))
		   (t
		    (setf (lexer-buffered-input-token lexer) input-token2)
		    input-token1))))
	  (t input-token1))))

(defun turtlep (lexer)
  (typep lexer 'turtle-lexer))

(defun sparqlp (lexer)
  (typep lexer 'sparql-lexer))

(defun return-input-token (lexer type value)
  (throw 'input-token (make-input-token :type type :value value :position (lexer-current-position lexer))))

(defun lexer-error (lexer fmt &rest args)
  (declare (ignorable lexer))
  (let ((msg (apply #'format nil fmt args)))
    (return-input-token lexer :error msg)))

(defun skip-whitespace-and-comments (lexer)
  (loop for ch = (peekch lexer)
	do (cond ((null ch) (return-from skip-whitespace-and-comments nil))
		 ((whitespace-char-p ch) (get-char lexer))
		 ((char= ch #\#)
		  (get-char lexer)
		  (loop for ch2 = (peekch lexer)
			while (not (char=* ch2 #\newline))
			do (get-char lexer)))
		 (t (return-from skip-whitespace-and-comments)))))

(defun get-char-if-looking-at (lexer ch) (and (char=* ch (peekch lexer)) (get-char lexer) t))


(defun select-keyword (lexer buf)
  (let ((binding (resolve-keyword lexer buf)))
    (cond ((null binding)
	   (lexer-error lexer "Unrecognized keyword ~S" (chbuf-string buf)))
	  (t
	   (return-input-token lexer (cdr binding) (car binding))))))

;;; Eating different kind of input-tokens

(defun eat-number (lexer buf) ; We saw a digit (not .). There may be a preceding '+' or '-' in buf
  (chbuf-put-char buf (get-char lexer))
  (loop for ch = (peekch lexer)
	while (digit-char-p* ch)
	do (chbuf-put-char buf (get-char lexer))
	finally (cond ((char=* ch #\.)
		       (chbuf-put-char buf (get-char lexer))
		       (eat-fraction lexer buf))
		      (t (return-input-token lexer (cond ((turtlep lexer) 'INTEGER-TERMINAL)
							 ((char= #\+ (elt (chbuf-contents buf) 0)) 'INTEGER_POSITIVE-TERMINAL)
							 ((char= #\- (elt (chbuf-contents buf) 0)) 'INTEGER_NEGATIVE-TERMINAL)
							 (t 'INTEGER-TERMINAL))
					     (instans::parse-xsd-integer (chbuf-contents buf) :start 0 :end (chbuf-index buf)))))))

(defun eat-fraction (lexer buf) ; There may be a sign and integral part. Last of buf is #\., which we have already consumed. We are looking at a digit.
  (loop for ch = (peekch lexer)
	while (digit-char-p* ch)
	do (chbuf-put-char buf (get-char lexer))
	finally (cond ((char-in-set-p* (peekch lexer) "Ee")
		       (return (eat-exponent lexer buf)))
		      (t (return-input-token lexer (cond ((turtlep lexer) 'DECIMAL-TERMINAL)
							 ((char= #\+ (elt (chbuf-contents buf) 0)) 'DECIMAL_POSITIVE-TERMINAL)
							 ((char= #\- (elt (chbuf-contents buf) 0)) 'DECIMAL_NEGATIVE-TERMINAL)
							 (t 'DECIMAL-TERMINAL))
					     (instans::parse-xsd-decimal (chbuf-contents buf) :start 0 :end (chbuf-index buf)))))))

(defun eat-exponent (lexer buf) ;; (peekch lexer) in "eE"
  (chbuf-put-char buf (get-char lexer))
  (when (char-in-set-p* (peekch lexer) "+-")
    (chbuf-put-char buf (get-char lexer)))
  (unless (digit-char-p* (peekch lexer))
    (lexer-error lexer "Malformed double ~A~A" (chbuf-string buf) (peekch lexer)))
  (loop for ch = (peekch lexer)
	while (digit-char-p* ch)
	do (chbuf-put-char buf (get-char lexer))
	finally (return-input-token lexer (cond ((turtlep lexer) 'DOUBLE-TERMINAL)
						((char= #\+ (elt (chbuf-contents buf) 0)) 'DOUBLE_POSITIVE-TERMINAL)
						((char= #\- (elt (chbuf-contents buf) 0)) 'DOUBLE_NEGATIVE-TERMINAL)
						(t 'DOUBLE-TERMINAL))
				    (instans::parse-xsd-double (chbuf-contents buf) :start 0 :end (chbuf-index buf)))))

(defun eat-blank-node-label (lexer) ; We saw _:, must get-char : first
  (let* ((first-char (get-char lexer))
	 (buf (empty-chbuf #\_ #\: first-char)))
    (loop for prev-ch = first-char then ch
	  for ch = (peekch lexer)
	  while (or (pn-chars-p ch) (char=* ch #\.))
	  do (chbuf-put-char buf (get-char lexer))
	  finally (progn
;		    (inform "Lexer detected blank node with text ~S. Prev-ch = ~C" (subseq (chbuf-contents buf) 0 (chbuf-index buf)) prev-ch)
		    (when (char=* prev-ch #\.)
;		      (inform "Dropping trailin .")
		      (unget-char lexer prev-ch)
		      (chbuf-drop-last-char buf))
		    (return-input-token lexer 'BLANK_NODE_LABEL-TERMINAL (canonize-string lexer buf))))))

(defun eat-identifier (lexer buf)
  (let ((first-char (get-char lexer)))
    (chbuf-put-char buf first-char) ; PN_CHARS_BASE, get-char that first
    (loop for prev-ch = first-char then ch
	  for ch = (peekch lexer)
	  while (or (pn-chars-p ch) (char=* ch #\.))
	  do (chbuf-put-char buf (get-char lexer))
	  finally (return (cond ((char=* prev-ch #\.)
				 (unget-char lexer prev-ch)
				 (chbuf-drop-last-char buf)
				 (select-keyword lexer buf))
				((get-char-if-looking-at lexer #\:)
				 (eat-pn-local lexer (resolve-prefix lexer buf)))
				(t
				 (select-keyword lexer buf)))))))

(defun eat-pn-local (lexer prefix-binding) ; prefix and ':'
  (let ((buf (empty-chbuf)))
    (loop do (cond ((get-char-if-looking-at lexer #\%)
		    (slurp-hex lexer (chbuf-put-char buf #\%)))
		   ((get-char-if-looking-at lexer #\\)
		    (slurp-local-esc lexer (chbuf-put-char buf #\\)))
		   (t
		    (let ((ch (peekch lexer)))
		      (cond ((or (pn-chars-p ch) (char-in-set-p* ch ".:"))
			     (chbuf-put-char buf (get-char lexer)))
			    ((zerop (chbuf-index buf))
			     (return-input-token lexer 'PNAME_NS-TERMINAL prefix-binding))
			    (t
			     (return-input-token lexer 'PNAME_LN-TERMINAL
						 (progn
						   (when (and (char=* (aref (chbuf-contents buf) (1- (chbuf-index buf))) #\.)
							      (or (= (chbuf-index buf) 1)
								  (not (char=* (aref (chbuf-contents buf) (- (chbuf-index buf) 2)) #\\))))
						     (unget-char lexer #\.)
						     (chbuf-drop-last-char buf))
						   (list prefix-binding (canonize-string lexer buf))))))))))))

(defun slurp-hex (lexer buf)
  (let ((ch1 (peekch lexer)))
    (cond ((digit-char-p* ch1 16)
	   (get-char lexer)
	   (let ((ch2 (peekch lexer)))
	     (cond ((digit-char-p* ch2 16)
		    (chbuf-put-chars buf ch1 (get-char lexer))
		    buf)
		   (t (lexer-error lexer "Expected a hex digit")))))
	  (t (lexer-error lexer "Expected a hex digit")))))

(defun slurp-local-esc (lexer buf)
  (cond ((and (peekch lexer) (char-in-set-p* (peekch lexer) "_~.-!$&'()*+,;=/?#@%"))
	 (chbuf-put-char buf (get-char lexer)))
	(t (lexer-error lexer "Unexpected escape char '~C'" (get-char lexer)))))

(defun eat-at (lexer)
  (loop with buf = (empty-chbuf #\@)
	for ch = (peekch lexer)
	while (alpha-char-p ch)
	do (chbuf-put-char buf (get-char lexer))
	finally (cond ((turtlep lexer)
		       (let ((binding (resolve-keyword lexer buf)))
			 (cond ((null binding)
				(return-input-token lexer 'LANGTAG-TERMINAL (canonize-string lexer buf)))
			       (t
				(return-input-token lexer (cdr binding) (car binding))))))
		      (t
		       (return-input-token lexer 'LANGTAG-TERMINAL (chbuf-string buf))))))

(defun eat-iri (lexer) ; we saw '<'
  (loop with buf = ;(if (turtlep lexer) 
       (empty-chbuf)
	for ch = (peekch lexer)
	while (and ch (iri-char-p ch))
	do (chbuf-put-char buf (get-char lexer))
	finally (cond ((char=* ch #\>)
		       (get-char lexer)
		       (return-input-token lexer 'IRIREF-TERMINAL (lexer-expand-iri lexer (chbuf-string buf))))
		      (t ; we assume, that the original '<' was actually the operator less-than.
		       (loop for i from (1- (chbuf-index buf)) downto 0
			     do (unget-char lexer (elt (chbuf-contents buf) i)))
		       (return-input-token lexer '<-TERMINAL "<")))))

(defun eat-var-name (lexer terminal-type buf)  ; we are looking at a var-name-start-char, just get-char first-char away
  (chbuf-put-char buf (get-char lexer))
  (loop for ch = (peekch lexer)
	while (var-name-char-p ch)
	do (chbuf-put-char buf (get-char lexer))
	finally (return-input-token lexer terminal-type (string-upcase (canonize-string lexer buf)))))

(defun eat-string-literal (lexer quote-char short-banned-chars short-terminal long-terminal)
  (let ((buf (empty-chbuf)))
    (cond ((get-char-if-looking-at lexer quote-char)
	   (cond ((get-char-if-looking-at lexer quote-char)
		  (loop for ch = (peekch lexer)
			do (cond ((get-char-if-looking-at lexer quote-char)
				  (cond ((get-char-if-looking-at lexer quote-char)
					 (cond ((get-char-if-looking-at lexer quote-char)
						(return-from eat-string-literal (return-input-token lexer long-terminal (canonize-string lexer buf))))
					       (t (chbuf-put-chars buf quote-char quote-char))))
					(t (chbuf-put-char buf quote-char))))
				 ((char=* ch #\\)
				  (get-char lexer)
				  (cond ((and (peekch lexer) (char-in-set-p* (peekch lexer) "tbnrf\\\"'"))
					 (chbuf-put-chars buf ch (get-char lexer)))
					(t (lexer-error lexer "Illegal escape char '~C'" (get-char lexer)))))
				 (t
				  (chbuf-put-char buf (get-char lexer))))))
		 (t (return-input-token lexer short-terminal (canonize-string lexer buf)))))
	  (t
	   (loop do (cond ((get-char-if-looking-at lexer quote-char)
			   (return-input-token lexer short-terminal (canonize-string lexer buf)))
			  (t
			   (let ((ch (peekch lexer)))
			     (cond ((char=* ch #\\)
				    (get-char lexer)
				    (cond ((and (peekch lexer) (char-in-set-p* (peekch lexer) "tbnrf\\\"'"))
					   (chbuf-put-chars buf ch (get-char lexer)))
					  (t (lexer-error lexer "Illegal escape char '~C'" (get-char lexer)))))
				   ((and ch (not (char-in-set-p* ch short-banned-chars)))
				    (chbuf-put-char buf (get-char lexer)))
				   (t (lexer-error lexer "Malformed string literal, missing ~C" quote-char)))))))))))

;;; Wrap-up
(defun next-input-token (lexer)
  (multiple-value-bind (type lexem)
      (catch 'input-token
	(progn
	  (skip-whitespace-and-comments lexer)
	  (cond ((null (peekch lexer))
		 (close (lexer-input-stream lexer))
		 (return-input-token lexer :eof "End of input"))
		((digit-char-p (peekch lexer))
		 (eat-number lexer (empty-chbuf)))
		((pn-chars-base-p (peekch lexer))
		 (eat-identifier lexer (empty-chbuf)))
		((get-char-if-looking-at lexer #\:)
		 (eat-pn-local lexer (resolve-prefix lexer (empty-chbuf))))
		((get-char-if-looking-at lexer #\')
		 (if (turtlep lexer)
		     (eat-string-literal lexer #\' '(#x27 #x5C #xA #xD) 'STRING_LITERAL_SINGLE_QUOTE-TERMINAL 'STRING_LITERAL_LONG_SINGLE_QUOTE-TERMINAL)
		     (eat-string-literal lexer #\' '(#x27 #x5C #xA #xD) 'STRING_LITERAL1-TERMINAL 'STRING_LITERAL_LONG1-TERMINAL)))
		((get-char-if-looking-at lexer #\")
		 (if (turtlep lexer)
		     (eat-string-literal lexer #\" '(#x22 #x5C #xA #xD) 'STRING_LITERAL_QUOTE-TERMINAL 'STRING_LITERAL_LONG_QUOTE-TERMINAL)
		     (eat-string-literal lexer #\" '(#x22 #x5C #xA #xD) 'STRING_LITERAL2-TERMINAL 'STRING_LITERAL_LONG2-TERMINAL)))
		((get-char-if-looking-at lexer #\!)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\!))
		       ((get-char-if-looking-at lexer #\=) (return-input-token lexer '!=-TERMINAL "!="))
		       (t (return-input-token lexer '!-TERMINAL "!"))))
		((get-char-if-looking-at lexer #\&)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\&))
		       ((get-char-if-looking-at lexer #\&) (return-input-token lexer '&&-TERMINAL "&&"))
		       (t (lexer-error lexer "Unrecognized input-token ~C" #\&))))
		((get-char-if-looking-at lexer #\()
		 (return-input-token lexer '|(-TERMINAL| "("))
		((get-char-if-looking-at lexer #\)) (return-input-token lexer '|)-TERMINAL| ")"))
		((get-char-if-looking-at lexer #\*)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\*))
		       (t (return-input-token lexer '*-TERMINAL "*"))))
		((get-char-if-looking-at lexer #\+)
		 (cond ((get-char-if-looking-at lexer #\.)
			(cond ((digit-char-p* (peekch lexer)) (eat-fraction lexer (empty-chbuf #\.)))
			      ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
			      (t (unget-char lexer #\.) (return-input-token lexer '+-TERMINAL "+"))))
		       ((digit-char-p* (peekch lexer)) (eat-number lexer (empty-chbuf #\+)))
		       ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
		       (t (return-input-token lexer '+-TERMINAL "+"))))
		((get-char-if-looking-at lexer #\,) (return-input-token lexer '|,-TERMINAL| ","))
		((get-char-if-looking-at lexer #\-)
		 (cond ((get-char-if-looking-at lexer #\.)
			(cond ((digit-char-p* (peekch lexer)) (eat-fraction lexer (empty-chbuf #\- #\.)))
			      ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
			      (t (unget-char lexer #\.) (return-input-token lexer '--TERMINAL "-"))))
		       ((digit-char-p* (peekch lexer)) (eat-number lexer (empty-chbuf #\-)))
		       ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
		       (t (return-input-token lexer '--TERMINAL "-"))))
		((get-char-if-looking-at lexer #\_)
		 (cond ((get-char-if-looking-at lexer #\:)
			(eat-blank-node-label lexer))
		       (t (lexer-error lexer "Expected ':' after '_'"))))
		((get-char-if-looking-at lexer #\.)
		 (cond ((digit-char-p* (peekch lexer)) (eat-fraction lexer (empty-chbuf #\.)))
		       (t (return-input-token lexer '.-TERMINAL "."))))
		((get-char-if-looking-at lexer #\/)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\/))
		       (t (return-input-token lexer '/-TERMINAL "/"))))
		((get-char-if-looking-at lexer #\;) (return-input-token lexer '|;-TERMINAL| ";"))
		((get-char-if-looking-at lexer #\<)
		 (cond ((get-char-if-looking-at lexer #\=) (return-input-token lexer '<=-TERMINAL "<="))
		       ((and (peekch lexer) (or (char= (peekch lexer) #\>) (iri-char-p (peekch lexer)))) (eat-iri lexer))
		       ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
		       (t (return-input-token lexer '<-TERMINAL "<"))))
		((get-char-if-looking-at lexer #\=)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\/))
		       (t (return-input-token lexer '=-TERMINAL "="))))
		((get-char-if-looking-at lexer #\>)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\>))
		       ((get-char-if-looking-at lexer #\=) (return-input-token lexer '>=-TERMINAL ">="))
		       (t (return-input-token lexer '>-TERMINAL ">"))))
		((get-char-if-looking-at lexer #\?)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\?))
		       ((var-name-start-char-p (peekch lexer)) (eat-var-name lexer 'VAR1-TERMINAL (empty-chbuf #\?)))
		       (t (return-input-token lexer '?-TERMINAL "?"))))
		((get-char-if-looking-at lexer #\$)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\$))
		       ((and (peekch lexer) (var-name-start-char-p (peekch lexer))) (eat-var-name lexer 'VAR2-TERMINAL (empty-chbuf #\$)))
		       (t (lexer-error lexer "Expecting a varname"))))
		((get-char-if-looking-at lexer #\[)
		 (return-input-token lexer '[-TERMINAL "["))
		((get-char-if-looking-at lexer #\]) (return-input-token lexer ']-TERMINAL "]"))
		((get-char-if-looking-at lexer #\^)
		 (cond ((get-char-if-looking-at lexer #\^) (return-input-token lexer '^^-TERMINAL "^^"))
		       ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
		       (t (return-input-token lexer '^-TERMINAL "^"))))
		((get-char-if-looking-at lexer #\{)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\{))
		       (t (return-input-token lexer '{-TERMINAL "{"))))
		((get-char-if-looking-at lexer #\|)
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\|))
		       ((get-char-if-looking-at lexer #\|) (return-input-token lexer '|\|\|-TERMINAL| "\|\|"))
		       (t (return-input-token lexer '|\|-TERMINAL| "\|"))))
		((get-char-if-looking-at lexer #\})
		 (cond ((turtlep lexer) (lexer-error lexer "Unrecognized input-token ~C" #\}))
		       (t (return-input-token lexer '}-TERMINAL "}"))))
		((get-char-if-looking-at lexer #\@)
		 (eat-at lexer))
		(t (lexer-error lexer "Unrecognized input-token ~C" (get-char lexer))))))
    (values type lexem (lexer-current-position lexer))))

