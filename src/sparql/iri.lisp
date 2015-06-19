;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package :instans)

;;; From http://tools.ietf.org/html/rfc3987.html#section-2


;;; ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
;;;  12            3  4          5       6  7        8 9
;;; scheme    = $2
;;; authority = $4
;;; path      = $5
;;; query     = $7
;;; fragment  = $9

(defun expand-iri (base iri-or-string)
;  (inform "expand-iri ~S ~S" base iri-string)
  (let ((iri (if (rdf-iri-p iri-or-string) iri-or-string (parse-iri iri-or-string))))
    (cond ((rdf-iri-scheme iri)
;	   (inform "~S has scheme ~S, we use it" iri (rdf-iri-scheme iri))
	   iri)
	  ;; (cond ((rdf-iri-had-dot-segments-p iri)
	  ;; 	    (recompose-iri iri))
	  ;; 	   (t iri)))
	  (t
	   (let ((cp (rdf-iri-path iri)))
	     (cond ((null base)
		    (values nil (format nil "Base not defined for relative IRI ~S" iri)))
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


(defun parse-iri (string)
  (let ((chars (coerce string 'list))
	(result (make-instance 'rdf-iri :string string)))
    (flet ((result-component (x) (coerce x 'string)))
      (unless (null chars)
	(when (alpha-char-p (first chars))
	  (loop for rest on (rest chars)
		for char = (first rest)
		while (or (alpha-char-p char) (digit-char-p char) (char= char #\+) (char= char #\-) (char= char #\.))
		collect char into scheme
		finally (when (char=* char #\:)
			  (setf (rdf-iri-scheme result) (result-component (cons (first chars) scheme)))
			  (setf chars (cdr rest)))))
	(when (and (char=* (first chars) #\/) (char=* (second chars) #\/))
	  (loop for rest on (cddr chars)
		for char = (car rest)
		while (not (char-in-set-p* char "/?#")) collect char into authority
		finally (progn
			  (setf (rdf-iri-authority result) (result-component authority))
			  (setf chars rest))))
	(loop for rest on chars
	      for char = (car rest)
	      while (not (char-in-set-p* char "?#")) collect char into path
	      finally (setf (rdf-iri-path result) (result-component path)
			    chars rest))
	;; finally (multiple-value-bind (cleaned dotsp) (remove-dot-segments path)
	;; 	      (setf (rdf-iri-path result) (result-component cleaned))
	;; 	      (setf (rdf-iri-had-dot-segments-p result) dotsp)
	;; 	      (setf chars rest)))
	(when (char=* (first chars) #\?)
	  (loop for rest on (cdr chars)
		for char = (car rest)
		while (not (char-in-set-p* char "#")) collect char into query
		finally (progn
			  (setf (rdf-iri-query result) (result-component query))
			  (setf chars rest))))
	(when (char=* (first chars) #\#)
	  (setf (rdf-iri-fragment result) (result-component (cdr chars))))
	;; (when (rdf-iri-had-dot-segments-p result)
	;; 	(recompose-iri result))
	)
	result)))

(defun remove-dot-segments (list)
  (flet ((prefixp (prefix list)
	   (and (<= (length prefix) (length list))
		(loop for x in list
		      for y in prefix
		      when (not (char= x y)) do (return nil)
		      finally (return t)))))
    (loop with output = nil
	  with dotsp = nil
	  while list
	  do (cond ((prefixp '(#\. #\/) list)
		    (setf dotsp t)
		    (setf list (cddr list)))
		   ((prefixp '(#\. #\. #\/) list)
		    (setf dotsp t)
		    (setf list (cdddr list)))
		   ((prefixp '(#\/ #\. #\/) list)
		    (setf dotsp t)
		    (setf list (cddr list)))
		   ((equal '(#\/ #\.) list)
		    (setf dotsp t)
		    (setf list (list #\/)))
		   ((prefixp '(#\/ #\. #\. #\/) list)
		    (setf dotsp t)
		    (setf list (cdddr list))
		    (pop output))
		   ((equal '(#\/ #\. #\.) list)
		    (setf dotsp t)
		    (setf list (list #\/))
		    (pop output))
		   ((or (equal '(#\. #\.) list) (equal '(#\.) list))
		    (setf dotsp t)
		    (setf list nil))
		   (t
		    (let ((segment (if (char= (car list) #\/) (list (pop list)))))
		      (loop while (and list (not (char= (car list) #\/)))
			    do (push (pop list) segment))
		      (push (nreverse segment) output))))
	  finally (return (values (apply #'append (nreverse output)) dotsp)))))

(defun recompose-iri (iri)
  (let ((parts nil))
    ;; (inform "enter recompose-iri")
    ;; (describe iri)
    (when (rdf-iri-fragment iri)
      (push (rdf-iri-fragment iri) parts)
      (push "#" parts))
    (when (rdf-iri-query iri)
      (push (rdf-iri-query iri) parts)
      (push "?" parts))
    (push (rdf-iri-path iri) parts)
    (when (rdf-iri-authority iri)
      (push (rdf-iri-authority iri) parts)
      (push "//" parts))
    (when (rdf-iri-scheme iri)
      (push ":" parts)
      (push (rdf-iri-scheme iri) parts))
    (setf (rdf-iri-string iri) (apply #'concatenate 'string parts))
    (setf (hashkeyed-hashkey iri) nil)
    ;; (describe iri)
    ;; (inform "recompose-iri (parts ~S) -> ~S" parts iri)
    iri))

(defun iri-to-string (iri &optional prefixes no-brackets-p)
  (cond ((null prefixes)
	 (with-output-to-string (str)
	   (unless no-brackets-p
	     (format str "<"))
	   (when (rdf-iri-scheme iri)
	     (format str "~A:" (rdf-iri-scheme iri)))
	   (when (rdf-iri-authority iri)
	     (format str "//~A" (rdf-iri-authority iri)))
	   (when (rdf-iri-path iri)
	     (format str "~A" (rdf-iri-path iri)))
	   (when (rdf-iri-query iri)
	     (format str "?~A" (rdf-iri-query iri)))
	   (when (rdf-iri-fragment iri)
	     (format str "#~A" (rdf-iri-fragment iri)))
	   (unless no-brackets-p
	     (format str ">"))))
	(t
	 (let ((str (with-output-to-string (str)
		      (when (rdf-iri-scheme iri)
			(format str "~A:" (rdf-iri-scheme iri)))
		      (when (rdf-iri-authority iri)
			(format str "//~A" (rdf-iri-authority iri)))
		      (when (rdf-iri-path iri)
			(format str "~A" (rdf-iri-path iri)))
		      (when (rdf-iri-query iri)
			(format str "?~A" (rdf-iri-query iri)))
		      (when (rdf-iri-fragment iri)
			(format str "#~A" (rdf-iri-fragment iri))))))
	   (loop for (k . v) in prefixes
;		 do (inform "testing, k = ~S, v = ~S, str = ~S" k v str)
		 do (when (and (<= (length v) (length str)) (every #'char= v str))
;		      (inform "Hit: k = ~S, v = ~S, str = ~S" k v str)
		      (cond ((string-equal k "BASE")
			     (return-from iri-to-string (format nil "<~A>" (subseq str (length v)))))
			    (t
			     (return-from iri-to-string (format nil "~A:~A" k (subseq str (length v)))))))
		 finally (return (format nil "<~A>" str)))))))
