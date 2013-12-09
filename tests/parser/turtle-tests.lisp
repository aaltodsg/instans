;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun test-turtle (&optional show-parse-p)
 (let ((input 
	 (tokenify-input
	  '("@prefix" (PNAME_NS "foaf") (IRIREF "http://xmlns.com/foaf/0.1/") "."
	   "[" (PNAME_LN ("foaf" "name")) (STRING_LITERAL_QUOTE "Jussi Pussinen") "]"
	   (PNAME_LN ("foaf" "knows")) (IRIREF "a") "."
	   (IRIREF "a") (PNAME_LN ("foaf" "knows")) (IRIREF "b")
	   "," (IRIREF "c") "," (ANON)
	   ";" (PNAME_LN ("foaf" "mbox")) (IRIREF "a@example.com")
	   ";" (PNAME_LN ("foaf" "age")) (INTEGER 25)
	   ";" (PNAME_LN ("foaf" "name")) (STRING_LITERAL_QUOTE "Antero Pantero")
	   "."
	   (IRIREF "a") (PNAME_LN ("foaf" "friends")) "(" (INTEGER 1) (INTEGER 2) (INTEGER 3) ")" "."
	   )))
	(turtle-parser (make-turtle-parser)))
    (funcall turtle-parser #'(lambda () (pop input)) show-parse-p)))

(defun directory-files (dirpath)
  (directory dirpath))

(defun turtle-open-files-test (dirpath)
  (time 
   (loop for file in (directory-files dirpath)
      do (with-open-file (stream file)
	   nil))))

(defun turtle-readch-files-test (dirpath)
  (time 
   (let ((file-count 0))
     (loop for file in (directory-files dirpath)
	do (when (zerop (mod (incf file-count) 5000)) (format *error-output* "."))
	do (with-open-file (stream file)
	     (loop for ch = (read-char stream nil nil) while ch finally (return ch))))
     (format *error-output* "~%~D files" file-count))))

(defun turtle-read-sequence-files-test (dirpath)
  (time 
   (let ((file-count 0)
	 (read-count 0)
	 (total-char-count 0)
	 (buffer (make-array 32768 :element-type 'character)))
     (loop for file in (directory-files dirpath)
	do (when (zerop (mod (incf file-count) 5000)) (format *error-output* "."))
	do (with-open-file (stream file)
	     (let ((length (file-length stream)))
	       (when (> length (length buffer))
		 (setf buffer (make-array length :element-type 'character))))
	     (loop for char-count = (read-sequence buffer stream)
		while (not (zerop char-count))
		do (progn
		     (incf read-count)
		     (incf total-char-count char-count)))))
     (format *error-output* "~%Read ~D files containing ~D characters in ~D read-sequence calls" file-count total-char-count read-count))))

(defun turtle-tokenize-files-test (dirpath)
  (time 
   (let ((*chbuf* nil)
	 (input-token-count 0)
	 (prefix-table (make-binding-table))
	 (file-count 0))
     (declare (special input-token-count file-count prefix-table))
     (loop for file in (directory-files dirpath)
	do (incf file-count)
	do (tt-one-file file prefix-table))
     (format *error-output* "~%~D input-tokens in ~d files" input-token-count file-count))))

(defun turtle-parse-files-test (dirpath)
  (let ((fcount 0)
	(*chbuf* nil)
	(*triple-count* 0)
	(*triple-sizes* 0)
	(prefix-table (make-binding-table))
	(lexer nil))
    (time
     (let ((parser (make-turtle-parser)))
       (loop for file in (directory dirpath)
	  when (zerop (mod (incf fcount) 100))
	  do (format *error-output* ".")
	  do (with-open-file (stream file)
	       (setf lexer (make-instance 'turtle-lexer :input-stream stream :prefix-table prefix-table)); :string-table string-table)))
		 (funcall parser lexer)))))
    (format *error-output* "~%file-count = ~D, triple-count = ~D, triple-sizes = ~D~%strings: ~D elems, prefixes: ~D elems, keywords: ~D elems" fcount *triple-count* *triple-sizes* (hash-table-count (lexer-string-table lexer)) (hash-table-count (lexer-prefix-table lexer)) (hash-table-count (lexer-keyword-table lexer)))))

(defun turtle-files-test (dirpath)
  (turtle-open-files-test dirpath)
  (turtle-readch-files-test dirpath)
  (turtle-tokenize-files-test dirpath)
  (turtle-parse-files-test dirpath))

(defun tt-one-file (file prefix-table)
  (declare (special input-token-count file-count))
;  (format *error-output* "~%~S" file)
  (with-open-file (stream file)
    (let ((lexer (make-instance 'turtle-lexer :input-stream stream :prefix-table prefix-table)))
      (loop for input-token = (funcall lexer) while input-token do (incf input-token-count) finally (return input-token)))))

(defun turtle-one (filepath &optional show-parse-p)
  (let ((result nil)
	(*triple-count* 0)
	(*triple-sizes* 0))
    (time 
     (let ((parser (make-turtle-parser))
	   (file (first (directory filepath))))
       (with-open-file (stream file)
	 (let* ((lexer (make-turtle-lexer stream)))
	   (setf result (multiple-value-list (funcall parser lexer)))))))
    (format *error-output* "~%triple-count = ~D, triple-sizes = ~D" *triple-count* *triple-sizes*)
    result))

(defun tokenize-turtle-file (file)
  (with-open-file (stream file)
    (let ((lexer (make-turtle-lexer stream)))
      (loop for input-token = (funcall lexer)
	    while input-token
	    collect input-token))))

