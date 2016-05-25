;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defclass binary-tree-node ()
  ((key :accessor binary-tree-node-key :initarg :key)
   (value :accessor binary-tree-node-value :initarg :value)
   (left :accessor binary-tree-node-left :initarg :left :initform nil)))
   (right :accessor binary-tree-node-right :initarg :right :initform nil)))

(defun binary-tree-add (tree key value &key (cmp #'-) same-key-handler)
  (cond ((null tree)
	 (make-instance 'binary-tree :key key :value value))
	(t
	 (let ((cmp (funcall cmp key (binary-tree-key tree))))
	   (cond ((minusp cmp)
		  (binary-tree-add (binary-tree-left tree) key value compare))
		 ((plusp cmp)
		  (binary-tree-add (binary-tree-right tree) key value compare))
		 ((null same-key-hanlder)
		  tree)
		 (t
		  (funcall same-key-handler tree key value)))))))
	 
(defun binary-tree-remove (tree key value &key (cmp #'-) same-key-handler)
  (cond ((null tree)
	 (make-instance 'binary-tree :key key :value value))
	(t
	 (let ((cmp (funcall cmp key (binary-tree-key tree))))
	   (cond ((minusp cmp)
		  (binary-tree-add (binary-tree-left tree) key value compare))
		 ((plusp cmp)
		  (binary-tree-add (binary-tree-right tree) key value compare))
		 ((null same-key-hanlder)
		  tree)
		 (t
		  (funcall same-key-handler tree key value)))))))
	 
(defun binary-tree-nodes (current &optional (accessor #'(lambda (x) x)))
  (let ((result nil)
  (labels ((walk (c
  (
  (if (null tree) nil
      (values (funcall accessor current)
	      (cons (binary-tree-left current)
		    (cons (binary-tree-right current)
			  remaining)))))

(defun binary-tree-node-iterator (current remaining &optional (accessor #'(lambda (x) x)))
  (if (null tree) nil
      (values (funcall accessor current)
	      (cons (binary-tree-left current)
		    (cons (binary-tree-right current)
			  remaining)))))

(defun 
	    

