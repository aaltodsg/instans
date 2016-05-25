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

(defun add-same-key-add-values-to-list (tree key value &key (test equal))
  (pushnew (binary-tree-value tree) :test test))

(defun remove-same-key-add-values-to-list (tree key value &key (test equal))
  (let ((new-values (remove value (binary-tree-value tree :test test))))
    (cond ((null new-values)
	   (let ((left (binary-tree-left tree))
		 (right (binary-tree-right tree)))
	     (cond ((not (null left))
		    ;;; tree.left replaces tree. tree.right goes to the rightmost descedent of left
		    (loop for node = left then child
			  for child = (binary-tree-right node)
			  while child
			  finally (progn
				    ;;; node.right is nil, node is the rightmost descedent
				    (setf (binary-tree-right node) (binary-tree-right tree))
				    (return left))))
		   ((not (null right))
		    ;;; tree.right replaces tree. tree.left goes to the leftmost descedent of right
		    (loop for node = right then child
			  for child = (binary-tree-left node)
			  while child
			  finally (progn
				    ;;; node.left is nil, node is the leftmost descedent
				    (setf (binary-tree-left node) (binary-tree-left tree))
				    (return right))))
		   (t nil))))
	  (t
	   (setf (binary-tree-value tree) new-values)
	   tree))))

;;; Remember to store the result of binary-tree-add somewhere. Otherwise you may lose its effect.
(defun binary-tree-add (tree key value &key (cmp #'-) same-key-handler)
  (cond ((null tree)
	 (make-instance 'binary-tree :key key :value value))
	(t
	 (let ((cmp (funcall cmp key (binary-tree-key tree))))
	   (cond ((minusp cmp)
		  (setf (binary-tree-left tree) (binary-tree-add (binary-tree-left tree) key value compare)))
		 ((plusp cmp)
		  (setf (binary-tree-right tree) (binary-tree-add (binary-tree-right tree) key value compare)))
		 (t
		  (funcall same-key-handler tree key value)))))))
	 
;;; Remember to store the result of binary-tree-remove somewhere. Otherwise you may lose its effect.
(defun binary-tree-remove (tree key value &key (cmp #'-) same-key-handler)
  (cond ((null tree)
	 nil)
	(t
	 (let ((cmp (funcall cmp key (binary-tree-key tree))))
	   (cond ((minusp cmp)
		  (setf (binary-tree-left tree) (binary-tree-add (binary-tree-left tree) key value compare)))
		 ((plusp cmp)
		  (setf (binary-tree-right tree) (binary-tree-add (binary-tree-right tree) key value compare)))
		 (t
		  (funcall same-key-handler tree key value)))))))
	 
(defun binary-tree-node-iterator (current remaining &optional (accessor #'(lambda (x) x)))
  (if (null tree) nil
      (values (funcall accessor current)
	      (cons (binary-tree-left current)
		    (cons (binary-tree-right current)
			  remaining)))))

	    

