;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun hash-characters (characters &key (start 0) (end (length characters)) (case-sensitive-p t))
  (loop with hv = (sxhash nil)
	for i from start below end
	for ch = (if case-sensitive-p (elt characters i) (char-upcase (elt characters i)))
	do (setf hv (mix (sxhash ch) hv))
	finally (return hv)))

(defun characters-to-string (characters &optional (start 0) (end (length characters)))
  (let* ((length (- end start))
	 (string (make-string length)))
    (loop for i from 0 below length
	  do (setf (aref string i) (elt characters (+ start i))))
    string))

(defun make-binding-table (&key (weakness nil))
  (make-hash-table :weakness weakness))

(defun find-binding (table characters &key (start 0) (end (length characters)) (case-sensitive-p t) (updatep t))
  (let* ((hv (hash-characters characters :start start :end end :case-sensitive-p case-sensitive-p))
	 (hash-table-item (gethash hv table)))
    (cond ((not (null hash-table-item))
	   (loop with length = (- end start)
		 for binding in hash-table-item
		 for key = (car binding)
		 when (= (length key) length)
		 do (loop for i from 0 below length
			  for ch = (if case-sensitive-p (elt characters (+ start i)) (char-upcase (elt characters (+ start i))))
			  unless (char= ch (if case-sensitive-p (char key i) (char-upcase (char key i))))
			  return nil
			  finally (return-from find-binding binding))
		 finally (let ((new-binding (list (characters-to-string characters start end))))
			   (setf (cdr (last hash-table-item)) (list new-binding))
			   (return new-binding))))
	  (updatep
	   (let ((new-binding (list (characters-to-string characters start end))))
	     (setf (gethash hv table) (list new-binding))
	     new-binding))
	  (t nil))))

