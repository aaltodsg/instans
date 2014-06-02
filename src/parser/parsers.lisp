;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun make-rdf-parser (instans input-stream file-type &key subscribe base graph triple-callback block-callback document-callback)
  (let* ((kind (intern (string-upcase file-type) :keyword))
	 (make-parser (cond ((member kind '(:ttl :turtle)) #'make-turtle-parser)
			    ((eq kind ':trig) #'make-trig-parser)
			    ((member kind '(:nt :ntriples :n-triples)) #'make-n-triples-parser)
			    ((member kind '(:nq :nquads :n-quads)) #'make-n-quads-parser)
			    (t (error* "Cannot parse files of type ~S" kind)))))
    (apply make-parser instans input-stream
	   (append (if base (list :base base))
		   (if graph (list :graph graph))
		   (if subscribe (list :subscribe subscribe))
		   (if triple-callback (list :triple-callback triple-callback))
		   (if block-callback (list :block-callback block-callback))
		   (if document-callback (list :document-callback document-callback))))))

(defun parse-rdf-file (file &rest keys &key subscribe base triple-callback block-callback document-callback)
  (declare (ignorable subscribe base triple-callback block-callback document-callback))
  (inform "~%parse-triple-file ~A:~%" file)
;  (time
   (with-open-file (stream file)
     (let* ((*triple-count* 0)
	    (instans (make-instance 'instans :name file))
	    (parser (apply #'make-rdf-parser instans stream (pathname-type (parse-namestring file)) keys))
	    (lexer (ll-parser-lexer parser))
	    (result (parse parser)))
       (cond ((ll-parser-succeeded-p result)
	      ;; (inform "~%triple-count = ~D, triple-sizes = ~D~%strings: ~D elems, prefixes: ~D elems, keywords: ~D elems"
	      ;; 	*triple-count* *triple-sizes* (hash-table-count (lexer-string-table lexer))
	      ;; 	(hash-table-count (lexer-prefix-table lexer)) (hash-table-count (lexer-keyword-table lexer)))
	      (inform "~%  ~A: triple-count = ~D, strings: ~D elems, prefixes: ~D elems, keywords: ~D elems"
		      (pathname-name (pathname file)) *triple-count* (hash-table-count (lexer-string-table lexer))
		      (hash-table-count (lexer-prefix-table lexer)) (hash-table-count (lexer-keyword-table lexer)))
	      )
	     (t
	      (inform "Parsing failed")))
       result))
;   )
)

(defun parse-triple-file (file &key subscribe base triple-callback block-callback document-callback)
  (let* ((*triple-count* 0)
	 (kind (intern (string-upcase (pathname-type (parse-namestring file))) :keyword))
	 ;; (*triple-sizes* 0)
	 (make-parser (cond ((member kind '(:ttl :turtle)) #'make-turtle-parser)
			    ((eq kind ':trig) #'make-trig-parser)
			    ((member kind '(:nt :ntriples :n-triples)) #'make-n-triples-parser)
			    ((member kind '(:nq :nquads :n-quads)) #'make-n-quads-parser)
			    (t (error* "Cannot parse files of type ~S" kind)))))
    (when (and (or block-callback base) (not (member kind '(:trig :ttl :turtle))))
      (error* "You cannot use block-callback with ~A input" kind))
;    (time
    (inform "~%parse-triple-file ~A:~%" file)
     (with-open-file (stream file)
       (let* ((instans (make-instance 'instans :name file))
	      (parser (apply make-parser instans stream
			     (append (if base (list :base base))
				     (if subscribe (list :subscribe subscribe))
				     (if triple-callback (list :triple-callback triple-callback))
				     (if block-callback (list :block-callback block-callback))
				     (if document-callback (list :document-callback document-callback)))))
	      (lexer (ll-parser-lexer parser))
	      (result (parse parser)))
	 (cond ((ll-parser-succeeded-p result)
		;; (inform "~%triple-count = ~D, triple-sizes = ~D~%strings: ~D elems, prefixes: ~D elems, keywords: ~D elems"
		;; 	*triple-count* *triple-sizes* (hash-table-count (lexer-string-table lexer))
		;; 	(hash-table-count (lexer-prefix-table lexer)) (hash-table-count (lexer-keyword-table lexer)))
		(inform "~%  ~A: triple-count = ~D, strings: ~D elems, prefixes: ~D elems, keywords: ~D elems"
			(pathname-name (pathname file)) *triple-count* (hash-table-count (lexer-string-table lexer))
			(hash-table-count (lexer-prefix-table lexer)) (hash-table-count (lexer-keyword-table lexer)))
		)
	       (t
		(inform "Parsing failed")))
	 result))
;)
     ))

(defun show-statement (&rest statement)
  (inform "    ~{~S~^ ~}" statement))

(defun show-statements (statements)
  (inform "got")
  (loop for s in statements do (inform "  ~{~S~^ ~}" s)))

(defun parse-sparql-stream (instans input-stream &key base subscribe)
  (let* ((input-string (replace-codepoint-escape-sequences (stream-contents-to-string input-stream)))
	 (parser (make-sparql-parser instans (make-string-input-stream input-string) :base base :subscribe subscribe)))
    (parse parser)))

(defun parse-sparql-file (instans query-file &key base subscribe)
  (with-open-file (input-stream query-file)
    (parse-sparql-stream instans input-stream :base base :subscribe subscribe)))

(defun replace-codepoint-escape-sequences (str)
  (loop with i = -1
     with max = (1- (length str))
     with result = (list nil)
     with tail = result
     while (< i max)
     for ch = (char str (incf i))
					;        do (inform "str = ~S, max = ~D, i = ~D, ch = ~C, result = ~S, tail = ~S" str max i ch result tail)
     do (cond ((not (char= ch #\\))
	       (setf (cdr tail) (list ch))
	       (setf tail (cdr tail)))
	      ((< i max)
	       (setf ch (char str (incf i)))
	       (let (ch1 ch2 ch3 ch4)
		 (cond ((or (not (or (char= ch #\u) (char= ch #\U)))
			    (>= (+ i 4) max)
			    (not (and (setf ch1 (digit-char-p (char str (+ i 1)) 16))
				      (setf ch2 (digit-char-p (char str (+ i 2)) 16))
				      (setf ch3 (digit-char-p (char str (+ i 3)) 16))
				      (setf ch4 (digit-char-p (char str (+ i 4)) 16)))))
			(setf (cdr tail) (list #\\ ch))
			(setf tail (cddr tail)))
		       (t
			(let ((num1234 (+ ch4 (* 16 (+ ch3 (* 16 (+ ch2 (* 16 ch1)))))))
			      ch5 ch6)
			  (incf i 4)
			  (cond ((or (> (+ i 2) max)
				     (not (and (setf ch5 (digit-char-p (char str (+ i 1)) 16))
					       (setf ch6 (digit-char-p (char str (+ i 2)) 16)))))
				 (setf (cdr tail) (list (code-char num1234)))
				 (setf tail (cdr tail)))
				(t
				 (incf i 2)
				 (setf (cdr tail) (list (code-char (+ ch6 (* 16 (+ ch5 (* 16 num1234)))))))
				 (setf tail (cdr tail))))))))))
     finally (return (coerce (cdr result) 'string))))

(defun parse-sparql-files (instans directory-path &key subscribe print-input-p print-result-p)
  (loop for file in (directory directory-path)
     do (progn
	  (when subscribe
	    (inform "File ~A:" file))
	  (when print-input-p
	    (with-open-file (input file)
	      (loop for line = (read-line input nil nil)
		 while line
		 do (inform "~A" line))))
	  (let ((result (parse-sparql-file instans file :subscribe subscribe)))
	    (cond ((not (ll-parser-succeeded-p result))
		   (loop for msg in (ll-parser-error-messages result)
		      do (inform "~A:~A" file msg)))
		  (print-result-p
		   (loop for item in (car (ll-parser-result-stack result))
		      do (inform "~S" item))))))))
