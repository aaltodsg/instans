;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package :instans)

(defvar *char-buffer* nil)

;;; ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
;;;  12            3  4          5       6  7        8 9
;;; scheme    = $2
;;; authority = $4
;;; path      = $5
;;; query     = $7
;;; fragment  = $9

(defun parse-iri (string)
  (let ((chars (coerce string 'list))
	(result (make-instance 'rdf-iri :string string)))
    (flet ((result-component (x) (coerce x 'string)))
      (loop for rest on chars
	    for char = (car rest)
	    while (not (char-in-set-p* char ":/?#")) collect char into scheme
	    finally (when (char=* char #\:)
		      (setf (rdf-iri-scheme result) (result-component scheme))
		      (setf chars (cdr rest))))
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
	  finally (multiple-value-bind (cleaned dotsp) (remove-dot-segments path)
		    (setf (rdf-iri-path result) (result-component cleaned))
		    (setf (rdf-iri-had-dot-segments-p result) dotsp)
		    (setf chars rest)))
    (when (char=* (first chars) #\?)
      (loop for rest on (cdr chars)
	    for char = (car rest)
	    while (not (char-in-set-p* char "#")) collect char into query
	    finally (progn
		      (setf (rdf-iri-query result) (result-component query))
		      (setf chars rest))))
    (when (char=* (first chars) #\#)
      (setf (rdf-iri-fragment result) (result-component (cdr chars))))
    result)))

(defun remove-dot-segments (list)
  (flet ((prefixp (prefix list)
	   (and (<= (length prefix) (length list))
		(loop for x in list
		      for y in prefix
		      when (not (char= x y)) do (return nil)
		      finally (return t)))))
    (loop with output = nil
	  while list
;	  do (barf "list = ~S, output = ~S~%" (coerce list 'string) (mapcar #'(lambda (x) (coerce x 'string)) output))
	  do (cond ((prefixp '(#\. #\/) list)
		    (setf list (cddr list)))
		   ((prefixp '(#\. #\. #\/) list)
		    (setf list (cdddr list)))
		   ((prefixp '(#\/ #\. #\/) list)
		    (setf list (cddr list)))
		   ((equal '(#\/ #\.) list)
		    (setf list (list #\/)))
		   ((prefixp '(#\/ #\. #\. #\/) list)
		    (setf list (cdddr list))
		    (pop output))
		   ((equal '(#\/ #\. #\.) list)
		    (setf list (list #\/))
		    (pop output))
		   ((or (equal '(#\. #\.) list) (equal '(#\.) list))
		    (setf list nil))
		   (t
		    (let ((segment (if (char= (car list) #\/) (list (pop list)))))
		      (loop while (and list (not (char= (car list) #\/)))
			    do (push (pop list) segment))
		      (push (nreverse segment) output))))
	 finally (return (apply #'append (nreverse output))))))

(defun recompose-iri (iri)
  (let ((parts nil))
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
    iri))


