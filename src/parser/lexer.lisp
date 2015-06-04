;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun return-input-token (lexer type value)
  (throw 'input-token (make-input-token :type type :value value :position (lexer-current-position lexer))))

(defun lexer-error (lexer fmt &rest args)
  (declare (ignorable lexer))
  (let ((msg (apply #'format nil fmt args)))
    (return-input-token lexer :error msg)))

(defun skip-whitespace-and-comments (lexer)
  (loop with saw-eol-p = nil
	with save-comment-p = (lexer-save-previous-comment-p lexer)
	with comments = nil
	for ch = (peekch lexer)
	do (cond ((null ch) (return saw-eol-p))
		 ((eol-char-p ch)
		  (setf saw-eol-p t)
		  (get-char lexer))
		 ((spacing-char-p ch)
		  (get-char lexer))
		 ((char= ch #\#)
		  (get-char lexer)
		  (loop for ch2 = (peekch lexer)
			while (not (char=* ch2 #\newline))
		        when save-comment-p collect ch2 into comment
			do (get-char lexer)
		        finally (when save-comment-p 
				  (push-to-end (apply #'append '(#\#) comment (if (char=* ch2 #\newline) '((#\newline)))) comments))))

		 (t (when (and save-comment-p comments)
		      (setf (lexer-previous-comment lexer) (coerce (apply #'append comments) 'string))
;		      (inform "Got comment ~A" (lexer-previous-comment lexer))
		      )
		    (return saw-eol-p)))))

(defun get-char-if-looking-at (lexer ch) (and (char=* ch (peekch lexer)) (get-char lexer)))

;;; Eating different kind of input-tokens

(defun eat-number (lexer buf) ; We saw a digit (not .). There may be a preceding '+' or '-' in buf
  (chbuf-put-char buf (get-char lexer))
  (loop for ch = (peekch lexer)
	while (digit-char-p* ch)
	do (chbuf-put-char buf (get-char lexer))
	finally (cond ((char=* ch #\.)
		       (chbuf-put-char buf (get-char lexer))
		       (eat-fraction lexer buf))
		      ((char-in-set-p* ch "eE")
		       (eat-exponent lexer buf))
		      (t (return-input-token lexer (cond ((trig-lexer-p lexer) 'INTEGER-TERMINAL)
							 ((char= #\+ (elt (chbuf-contents buf) 0)) 'INTEGER_POSITIVE-TERMINAL)
							 ((char= #\- (elt (chbuf-contents buf) 0)) 'INTEGER_NEGATIVE-TERMINAL)
							 (t 'INTEGER-TERMINAL))
					     (parse-xsd-integer (chbuf-contents buf) :start 0 :end (chbuf-index buf)))))))

(defun eat-fraction (lexer buf) ; There may be a sign and integral part. Last of buf is #\., which we have already consumed. We are looking at a digit.
  (loop for ch = (peekch lexer)
	while (digit-char-p* ch)
	do (chbuf-put-char buf (get-char lexer))
	finally (cond ((char-in-set-p* (peekch lexer) "Ee")
		       (return (eat-exponent lexer buf)))
		      (t
		       (cond ((char=* (aref (chbuf-contents buf) (1- (chbuf-index buf))) #\.)
			      (unget-char lexer #\.)
			      (chbuf-drop-last-char buf)
			      (return-input-token lexer (cond ((trig-lexer-p lexer) 'INTEGER-TERMINAL)
							      ((char= #\+ (elt (chbuf-contents buf) 0)) 'INTEGER_POSITIVE-TERMINAL)
							      ((char= #\- (elt (chbuf-contents buf) 0)) 'INTEGER_NEGATIVE-TERMINAL)
							      (t 'INTEGER-TERMINAL))
						  (parse-xsd-integer (chbuf-contents buf) :start 0 :end (chbuf-index buf))))
			     (t
			      (return-input-token lexer (cond ((trig-lexer-p lexer) 'DECIMAL-TERMINAL)
							      ((char= #\+ (elt (chbuf-contents buf) 0)) 'DECIMAL_POSITIVE-TERMINAL)
							      ((char= #\- (elt (chbuf-contents buf) 0)) 'DECIMAL_NEGATIVE-TERMINAL)
							 (t 'DECIMAL-TERMINAL))
						  (parse-xsd-decimal (chbuf-contents buf) :start 0 :end (chbuf-index buf)))))))))

(defun eat-exponent (lexer buf) ;; (peekch lexer) in "eE"
  (chbuf-put-char buf (get-char lexer))
  (when (char-in-set-p* (peekch lexer) "+-")
    (chbuf-put-char buf (get-char lexer)))
  (unless (digit-char-p* (peekch lexer))
    (lexer-error lexer "Malformed double ~A~A" (chbuf-string buf) (peekch lexer)))
  (loop for ch = (peekch lexer)
	while (digit-char-p* ch)
	do (chbuf-put-char buf (get-char lexer))
	finally (return-input-token lexer (cond ((trig-lexer-p lexer) 'DOUBLE-TERMINAL)
						((char= #\+ (elt (chbuf-contents buf) 0)) 'DOUBLE_POSITIVE-TERMINAL)
						((char= #\- (elt (chbuf-contents buf) 0)) 'DOUBLE_NEGATIVE-TERMINAL)
						(t 'DOUBLE-TERMINAL))
				    (parse-xsd-double (chbuf-contents buf) :start 0 :end (chbuf-index buf)))))

(defun eat-blank-node-label (lexer) ; We saw _, must see : and PN_CHARS_U
  (unless (get-char-if-looking-at lexer #\:) 
    (lexer-error lexer "Expected ':' after '_'"))
  (let* ((first-char (get-char lexer))
	 (buf (empty-chbuf #\_ #\: first-char)))
    (unless (pn-chars-u-digit-p first-char)
      (lexer-error lexer "Expected a PN_CHAR_U or DIGIT after '_:'"))
    (loop for prev-ch = first-char then ch
	  for ch = (peekch lexer)
	  while (pn-chars-dot-p ch)
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
				 (eat-pn-local lexer (chbuf-string buf)))
				(t
				 (select-keyword lexer buf)))))))

(defun eat-pn-local (lexer prefix) ; prefix and ':' seen
  (let ((buf (empty-chbuf)))
    (cond ((get-char-if-looking-at lexer #\%)
	   (slurp-hex lexer buf))
	  ((get-char-if-looking-at lexer #\\)
	   (slurp-local-esc lexer buf))
	  ((or (pn-chars-u-digit-p (peekch lexer)) (char=* (peekch lexer) #\:))
	   (chbuf-put-char buf (get-char lexer)))
	  ((null (peekch lexer))
	   (lexer-error lexer "Unexpected EOF inside PN_LOCAL"))
	  (t
	   (return-input-token lexer 'PNAME_NS-TERMINAL prefix)))
    (loop do (cond ((get-char-if-looking-at lexer #\%)
		    (slurp-hex lexer buf))
		   ((get-char-if-looking-at lexer #\\)
		    (slurp-local-esc lexer buf))
		   ((or (pn-chars-p (peekch lexer)) (char-in-set-p* (peekch lexer) ".:"))
		    (chbuf-put-char buf (get-char lexer)))
		   (t
		    (return-input-token lexer 'PNAME_LN-TERMINAL
					(progn
					  (when (and (char=* (aref (chbuf-contents buf) (1- (chbuf-index buf))) #\.)
						     (or (= (chbuf-index buf) 1)
							 (not (char=* (aref (chbuf-contents buf) (- (chbuf-index buf) 2)) #\\))))
					    (unget-char lexer #\.)
					    (chbuf-drop-last-char buf))
					  (list prefix (canonize-string lexer buf)))))))))

(defun slurp-hex (lexer buf)
  (let ((ch1 (peekch lexer)))
    (cond ((digit-char-p* ch1 16)
	   (get-char lexer)
	   (let ((ch2 (peekch lexer)))
	     (cond ((digit-char-p* ch2 16)
		    (chbuf-put-chars buf #\% ch1 (get-char lexer))
		    buf)
		   (t (lexer-error lexer "Expected a hex digit")))))
	  (t (lexer-error lexer "Expected a hex digit")))))

(defun get-uhex (lexer)
  ;;; Assuming we have eaten \ and seed u or U
  (loop with code = 0
	repeat (if (char= (get-char lexer) #\u) 4 8)
        do (cond ((null (peekch lexer))
		  (lexer-error lexer "Unexpected EOF while scanning Unicode escape sequence"))
		 ((not (digit-char-p (peekch lexer) 16))
		  (lexer-error lexer "A non hex digit char ~S in a Unicode escape sequence" (peekch lexer)))
		 (t
		  (setf code (+ (* 16 code) (hex-char-to-int (get-char lexer))))))
	finally (return (handler-case (code-char code)
			  (t (e) (declare (ignore e)) (lexer-error lexer "Cannot convert \\u escape sequence ~A" code))))))

(defun slurp-local-esc (lexer buf)
  (cond ((and (peekch lexer) (char-in-set-p* (peekch lexer) "_~.-!$&'()*+,;=/?#@%"))
	 (chbuf-put-chars buf (get-char lexer)))
	(t (lexer-error lexer "Unexpected escape char '~C'" (get-char lexer)))))

(defun eat-at (lexer)
  ;;; Saw @
  (let ((first-ch (peekch lexer)))
    (cond ((null first-ch)
	   (lexer-error lexer "Unexpected EOF"))
	  ((not (alpha-char-p first-ch))
	   (lexer-error lexer "Unexpected char ~S in langtag" first-ch))
	  (t
	   (loop with buf = (empty-chbuf #\@ (get-char lexer))
		 for ch = (peekch lexer)
		 while (and ch (or (alphanumericp ch) (char= ch #\-)))
		 do (chbuf-put-char buf (get-char lexer))
		 finally (cond ((trig-lexer-p lexer)
				(or (select-keyword lexer buf :not-exists-error-p nil)
				    (return-input-token lexer 'LANGTAG-TERMINAL (canonize-string lexer buf))))
			       (t
				(return-input-token lexer 'LANGTAG-TERMINAL (chbuf-string buf)))))))))

(defun eat-iri (lexer) ; we saw '<'
  (loop with buf = ;(if (trig-lexer-p lexer) 
       (empty-chbuf)
	for ch = (peekch lexer)
	while ch
        do (cond ((iri-char-p ch)
		  (chbuf-put-char buf (get-char lexer)))
		 ((get-char-if-looking-at lexer #\\)
		  (cond ((char-in-set-p* (peekch lexer) "uU")
			 (let ((uch (get-uhex lexer)))
			   (cond ((iri-char-p uch)
				  (chbuf-put-char buf uch))
				 (t
				  (lexer-error lexer "Char ~C (~D) not allowed in an IRI" uch (char-code uch))))))
			(t
			 (lexer-error lexer "Unexpected escape char '~C'" (get-char lexer)))))
		 (t (loop-finish)))
	finally (cond ((char=* ch #\>)
		       (get-char lexer)
		       (return-input-token lexer 'IRIREF-TERMINAL (lexer-expand-iri lexer (chbuf-string buf))))
		      ((not (sparql-lexer-p lexer))
		       (lexer-error lexer "Malformed IRI"))
		      (t ; we assume, that the original '<' was actually the operator less-than.
		       (loop for i from (1- (chbuf-index buf)) downto 0
			     do (unget-char lexer (elt (chbuf-contents buf) i)))
		       (return-input-token lexer '<-TERMINAL "<")))))

(defun eat-var-name (lexer terminal-type buf)  ; we are looking at a var-name-start-char, just get-char first-char away
  (chbuf-put-char buf (get-char lexer))
  (loop for ch = (peekch lexer)
	while (var-name-other-char-p ch)
	do (chbuf-put-char buf (get-char lexer))
	finally (return-input-token lexer terminal-type (canonize-string lexer buf))))

(defun get-char-if-looking-at-with-eof-error (lexer ch eof-fmt &rest args)
  (cond ((null (peekch lexer))
	 (apply #'lexer-error lexer eof-fmt args))
	(t
	 (and (char=* ch (peekch lexer)) (get-char lexer)))))

(defun eat-string-literal (lexer quote-char short-banned-chars short-terminal long-terminal)
  (block inner
    (let ((buf (empty-chbuf)))
      (flet ((maybe-get-quote () (get-char-if-looking-at-with-eof-error lexer quote-char "Eof while scanning string literal"))
	     (handle-escape ()
	       (get-char lexer)
	       (cond ((not (peekch lexer))
		      (lexer-error lexer "Unexpected EOF"))
		     ((char-in-set-p* (peekch lexer) "tbnrf\\\"'")
		      (chbuf-put-chars buf (decode-string-escape-char (get-char lexer))))
		     ((char-in-set-p* (peekch lexer) "uU")
		      (chbuf-put-char buf (get-uhex lexer)))
		     (t (lexer-error lexer "Illegal escape char '~C'" (get-char lexer))))))
	(cond ((and long-terminal (maybe-get-quote))
	       (cond ((maybe-get-quote)
		      (loop for ch = (peekch lexer)
			    do (cond ((maybe-get-quote)
				      (cond ((maybe-get-quote)
					     (cond ((maybe-get-quote)
						    (return-from inner (return-input-token lexer long-terminal (canonize-string lexer buf))))
						   (t (chbuf-put-chars buf quote-char quote-char))))
					    (t (chbuf-put-char buf quote-char))))
				     ((char=* ch #\\)
				      (handle-escape))
				     (t
				      (chbuf-put-char buf (get-char lexer))))))
		     (t (return-input-token lexer short-terminal (canonize-string lexer buf)))))
	      (t
	       (loop do (cond ((maybe-get-quote)
			       (return-input-token lexer short-terminal (canonize-string lexer buf)))
			      (t
			       (let ((ch (peekch lexer)))
				 (cond ((char=* ch #\\)
					(handle-escape))
				       ((and ch (not (char-in-set-p* ch short-banned-chars)))
					(chbuf-put-char buf (get-char lexer)))
				       (t (lexer-error lexer "Malformed string literal, missing ~C" quote-char)))))))))))))

;;; Wrap-up
(defun next-input-token (lexer)
  (catch 'input-token
    (progn
      (skip-whitespace-and-comments lexer)
      (cond ((null (peekch lexer))
	     (close-stream (lexer-input-stream lexer) "next-input-token: close ~A")
	     (return-input-token lexer :eof "End of input"))
	    ((digit-char-p (peekch lexer))
	     (eat-number lexer (empty-chbuf)))
	    ((pn-chars-base-p (peekch lexer))
	     (eat-identifier lexer (empty-chbuf)))
	    ((get-char-if-looking-at lexer #\:)
	     (eat-pn-local lexer ""))
	    ((get-char-if-looking-at lexer #\')
	     (if (trig-lexer-p lexer)
		 (eat-string-literal lexer #\' '(#x27 #x5C #xA #xD) 'STRING_LITERAL_SINGLE_QUOTE-TERMINAL 'STRING_LITERAL_LONG_SINGLE_QUOTE-TERMINAL)
		 (eat-string-literal lexer #\' '(#x27 #x5C #xA #xD) 'STRING_LITERAL1-TERMINAL 'STRING_LITERAL_LONG1-TERMINAL)))
	    ((get-char-if-looking-at lexer #\")
	     (if (trig-lexer-p lexer)
		 (eat-string-literal lexer #\" '(#x22 #x5C #xA #xD) 'STRING_LITERAL_QUOTE-TERMINAL 'STRING_LITERAL_LONG_QUOTE-TERMINAL)
		 (eat-string-literal lexer #\" '(#x22 #x5C #xA #xD) 'STRING_LITERAL2-TERMINAL 'STRING_LITERAL_LONG2-TERMINAL)))
	    ((get-char-if-looking-at lexer #\!)
	     (cond ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\!))
		   ((get-char-if-looking-at lexer #\=) (return-input-token lexer '!=-TERMINAL "!="))
		   (t (return-input-token lexer '!-TERMINAL "!"))))
	    ((get-char-if-looking-at lexer #\&)
	     (cond ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\&))
		   ((get-char-if-looking-at lexer #\&) (return-input-token lexer '&&-TERMINAL "&&"))
		   (t (lexer-error lexer "Unrecognized input-token ~C" #\&))))
	    ((get-char-if-looking-at lexer #\()
	     (return-input-token lexer '|(-TERMINAL| "("))
	    ((get-char-if-looking-at lexer #\)) (return-input-token lexer '|)-TERMINAL| ")"))
	    ((get-char-if-looking-at lexer #\*)
	     (cond ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\*))
		   (t (return-input-token lexer '*-TERMINAL "*"))))
	    ((get-char-if-looking-at lexer #\+)
	     (cond ((get-char-if-looking-at lexer #\.)
		    (cond ((digit-char-p* (peekch lexer)) (eat-fraction lexer (empty-chbuf #\.)))
			  ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
			  (t (unget-char lexer #\.) (return-input-token lexer '+-TERMINAL "+"))))
		   ((digit-char-p* (peekch lexer)) (eat-number lexer (empty-chbuf #\+)))
		   ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
		   (t (return-input-token lexer '+-TERMINAL "+"))))
	    ((get-char-if-looking-at lexer #\,) (return-input-token lexer '|,-TERMINAL| ","))
	    ((get-char-if-looking-at lexer #\-)
	     (cond ((get-char-if-looking-at lexer #\.)
		    (cond ((digit-char-p* (peekch lexer)) (eat-fraction lexer (empty-chbuf #\- #\.)))
			  ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
			  (t (unget-char lexer #\.) (return-input-token lexer '--TERMINAL "-"))))
		   ((digit-char-p* (peekch lexer)) (eat-number lexer (empty-chbuf #\-)))
		   ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
		   (t (return-input-token lexer '--TERMINAL "-"))))
	    ((get-char-if-looking-at lexer #\_)
	     (eat-blank-node-label lexer))
	    ((get-char-if-looking-at lexer #\.)
	     (cond ((digit-char-p* (peekch lexer)) (eat-fraction lexer (empty-chbuf #\.)))
		   (t (return-input-token lexer '.-TERMINAL "."))))
	    ((get-char-if-looking-at lexer #\/)
	     (cond ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\/))
		   (t (return-input-token lexer '/-TERMINAL "/"))))
	    ((get-char-if-looking-at lexer #\;) (return-input-token lexer '|;-TERMINAL| ";"))
	    ((get-char-if-looking-at lexer #\<)
	     (cond ((trig-lexer-p lexer)
		    (cond ((and (peekch lexer) (or (char= (peekch lexer) #\>) (iri-char-p (peekch lexer)))) (eat-iri lexer))
			  (t (lexer-error lexer "Unrecognized input-token <~C" (peekch lexer)))))
		   ((get-char-if-looking-at lexer #\=)
		    (return-input-token lexer '<=-TERMINAL "<="))
		   ((and (peekch lexer) (or (char= (peekch lexer) #\>) (iri-char-p (peekch lexer)))) (eat-iri lexer))
		   (t (return-input-token lexer '<-TERMINAL "<"))))
	    ((get-char-if-looking-at lexer #\=)
	     (cond ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\/))
		   (t (return-input-token lexer '=-TERMINAL "="))))
	    ((get-char-if-looking-at lexer #\>)
	     (cond ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\>))
		   ((get-char-if-looking-at lexer #\=) (return-input-token lexer '>=-TERMINAL ">="))
		   (t (return-input-token lexer '>-TERMINAL ">"))))
	    ((get-char-if-looking-at lexer #\?)
	     (cond ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\?))
		   ((var-name-start-char-p (peekch lexer)) (eat-var-name lexer 'VAR1-TERMINAL (empty-chbuf #\?)))
		   (t (return-input-token lexer '?-TERMINAL "?"))))
	    ((get-char-if-looking-at lexer #\$)
	     (cond ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\$))
		   ((and (peekch lexer) (var-name-start-char-p (peekch lexer))) (eat-var-name lexer 'VAR2-TERMINAL (empty-chbuf #\$)))
		   (t (lexer-error lexer "Expecting a varname"))))
	    ((get-char-if-looking-at lexer #\[)
	     (return-input-token lexer '[-TERMINAL "["))
	    ((get-char-if-looking-at lexer #\]) (return-input-token lexer ']-TERMINAL "]"))
	    ((get-char-if-looking-at lexer #\^)
	     (cond ((get-char-if-looking-at lexer #\^) (return-input-token lexer '^^-TERMINAL "^^"))
		   ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" (peekch lexer)))
		   (t (return-input-token lexer '^-TERMINAL "^"))))
	    ((get-char-if-looking-at lexer #\{)
	     (cond ((turtle-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\{))
		   (t (return-input-token lexer '{-TERMINAL "{"))))
	    ((get-char-if-looking-at lexer #\|)
	     (cond ((trig-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\|))
		   ((get-char-if-looking-at lexer #\|) (return-input-token lexer '|\|\|-TERMINAL| "\|\|"))
		   (t (return-input-token lexer '|\|-TERMINAL| "\|"))))
	    ((get-char-if-looking-at lexer #\})
	     (cond ((turtle-lexer-p lexer) (lexer-error lexer "Unrecognized input-token ~C" #\}))
		   (t (return-input-token lexer '}-TERMINAL "}"))))
	    ((get-char-if-looking-at lexer #\@)
	     (eat-at lexer))
	    (t (lexer-error lexer "Unrecognized input-token ~C" (get-char lexer)))))))

