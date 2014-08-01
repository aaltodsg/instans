;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class trie ()
  ((level :accessor trie-level :initform nil)
   (level-tail :accessor trie-level-tail :initform nil)))

(defun add-path (trie path &optional (test #'eql))
  (cond ((null path) trie)
	((null (trie-level trie))
	 (let ((next-level (make-instance 'trie)))
	   (setf (trie-level trie) (list (cons (car path) next-level)))
	   (setf (trie-level-tail trie) (trie-level trie)))
	 (add-path next-level (cdr path)))
	(t
	 (let ((item (assoc (first path) (trie-level trie) :test test)))
	   (cond ((null item)
		  (let ((next-level (make-instance 'trie)))
		    (setf (cdr (trie-level-tail trie)) (list (cons (car path) next-level)))
		    (setf (trie-level-tail trie) (cdr (trie-level-tail trie))))
		  (add-path next-level (cdr path)))
		 (t
		  (add-path (cdr item) (cdr path))))))))





