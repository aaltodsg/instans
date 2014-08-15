;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class trie ()
  ((level :accessor trie-level :initform nil)
   (level-tail :accessor trie-level-tail :initform nil)))

(defun trie-level-add-tail (trie item)
  (let ((tail (list item)))
    (cond ((null (trie-level trie))
	   (setf (trie-level trie) tail)
	   (setf (trie-level-tail trie) tail))
	  (t
	   (setf (cdr (trie-level-tail trie)) tail)
	   (setf (trie-level-tail trie) tail)))))

(defun trie-add-path (trie path &optional (test #'equal))
  (unless (null path)
    (cond ((null (rest path))
	   (unless (assoc (first path) (trie-level trie) :test test)
	     (trie-level-add-tail trie (cons (first path) nil))))
	  (t
	   (let ((item (assoc (first path) (trie-level trie) :test test)))
	     (cond ((null item)
		    (trie-level-add-tail trie (cons (first path) (trie-add-path (make-instance 'trie) (rest path)))))
		   (t
		    (trie-add-path (cdr item) (rest path))))))))
  trie)

(defun trie-paths (trie &optional result)
  (loop for (item . subtrie) in (trie-level trie)
	append (if (null subtrie) (list (reverse (cons item result))) (trie-paths subtrie (cons item result)))))

(defun trie-map (trie function &optional result)
  (loop for (item . subtrie) in (trie-level trie)
	when (null subtrie)
	do (funcall function (reverse (cons item result)))
	else
	do (trie-map subtrie function (cons item result))))

(defun trie-print (trie &optional (stream *standard-output*) (indent 0))
  (format stream "~%~VT~A { " indent trie)
  (loop for (key . value) in (trie-level trie)
        do (format stream "~A:" key)
        when value
        do (trie-print value stream (+ indent 2 (length (format nil "~A" key))))
	else do (format stream ",~%~VT" indent))
  (format stream "~VT}~%" indent))
  

