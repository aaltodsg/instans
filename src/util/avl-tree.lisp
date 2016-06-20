;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

;;;
;;; Adapted from http://www.geeksforgeeks.org/avl-tree-set-2-deletion/
;;;

(in-package #:instans)

(defclass avl-tree ()
  ((key :accessor avl-tree-key :initarg :key :initform nil)
   (value :accessor avl-tree-value :initarg :value :initform nil)
   (height :accessor avl-tree-height :initarg :height :initform 1)
   (left :accessor avl-tree-left :initarg :left :initform nil)
   (right :accessor avl-tree-right :initarg :right :initform nil)))

(defun get-avl-tree-height (node)
  (if node (avl-tree-height node) 0))

(defun get-avl-tree-balance (node)
  (if node (- (get-avl-tree-height (avl-tree-left node)) (get-avl-tree-height (avl-tree-right node))) 0))

(defun add-same-key-values-in-list (tree value &key (test #'equal))
  (pushnew value (avl-tree-value tree) :test test)
  tree)

(defun delete-same-key-values-in-list (tree value &key (test #'equal))
  (setf (avl-tree-value tree) (delete value (avl-tree-value tree) :test test))
  tree)

(defun avl-tree-right-rotate (y)
  (let* ((x (avl-tree-left y))
	 (t2 (avl-tree-right x)))
    (setf (avl-tree-right x) y)
    (setf (avl-tree-left y) t2)
    (setf (avl-tree-height y) (1+ (max (get-avl-tree-height (avl-tree-left y)) (get-avl-tree-height (avl-tree-right y)))))
    (setf (avl-tree-height x) (1+ (max (get-avl-tree-height (avl-tree-left x)) (get-avl-tree-height (avl-tree-right x)))))
    x))

(defun avl-tree-left-rotate (x)
  (let* ((y (avl-tree-right x))
	 (t2 (avl-tree-left y)))
    (setf (avl-tree-left y) x)
    (setf (avl-tree-right x) t2)
    (setf (avl-tree-height x) (1+ (max (get-avl-tree-height (avl-tree-left x)) (get-avl-tree-height (avl-tree-right x)))))
    (setf (avl-tree-height y) (1+ (max (get-avl-tree-height (avl-tree-left y)) (get-avl-tree-height (avl-tree-right y)))))
    y))

(defun rebalance-avl-tree-after-insert (node key compare)
  (setf (avl-tree-height node) (1+ (max (get-avl-tree-height (avl-tree-left node)) (get-avl-tree-height (avl-tree-right node)))))
  (let ((balance (get-avl-tree-balance node)))
    (cond ((> balance 1)
	   (cond ((minusp (funcall compare key (avl-tree-key (avl-tree-left node))))
					; Left Left Case
		  (avl-tree-right-rotate node))
		 (t
					; Left Right Case
		  (setf (avl-tree-left node) (avl-tree-left-rotate (avl-tree-left node)))
		  (avl-tree-right-rotate node))))
	  ((< balance -1)
	   (cond ((minusp (funcall compare key (avl-tree-key (avl-tree-right node))))
					; Right Left Case
		  (setf (avl-tree-right node) (avl-tree-right-rotate (avl-tree-right node)))
		  (avl-tree-left-rotate node))
		 (t
					; Right Right Case
		  (avl-tree-left-rotate node))))
	  (t node))))

(defun avl-tree-insert (node key value &key (compare #'-) (same-key-handler #'add-same-key-values-in-list))
  (cond ((null node)
	 (funcall same-key-handler (make-instance 'avl-tree :key key :height 1) value))
	(t
	 (let ((cmp (funcall compare key (avl-tree-key node))))
	   (cond ((minusp cmp)
		  (setf (avl-tree-left node) (avl-tree-insert (avl-tree-left node) key value :compare compare :same-key-handler same-key-handler))
		  (rebalance-avl-tree-after-insert node key compare))
		 ((plusp cmp)
		  (setf (avl-tree-right node) (avl-tree-insert (avl-tree-right node) key value :compare compare :same-key-handler same-key-handler))
		  (rebalance-avl-tree-after-insert node key compare))
		 (t
		  (funcall same-key-handler node value)))))))

(defun avl-tree-min-value-node (node)
  (loop for current = node then (avl-tree-left current)
        while (avl-tree-left current)
        finally (return current)))

(defun rebalance-avl-tree-after-delete (root)
  (setf (avl-tree-height root) (1+ (max (get-avl-tree-height (avl-tree-left root)) (get-avl-tree-height (avl-tree-right root)))))
  (let ((balance (get-avl-tree-balance root)))
    (cond ((> balance 1)
	   (cond ((>= (get-avl-tree-balance (avl-tree-left root)) 0)
					; Left Left Case
		  (avl-tree-right-rotate root))
		 (t
					; Left Right Case
		  (setf (avl-tree-left root) (avl-tree-left-rotate (avl-tree-left root)))
		  (avl-tree-right-rotate root))))
	  ((< balance -1)
	   (cond ((> (get-avl-tree-balance (avl-tree-right root)) 0)
					; Right Left Case
		  (setf (avl-tree-right root) (avl-tree-right-rotate (avl-tree-right root)))
		  (avl-tree-left-rotate root))
		 (t
					; Right Right Case
		  (avl-tree-left-rotate root))))
	  (t
	   root))))

(defun avl-tree-delete (root key &key (value :all) (compare #'-) (same-key-handler #'delete-same-key-values-in-list))
  (cond ((null root) nil)
	(t
	 (let ((cmp (funcall compare key (avl-tree-key root))))
	   (cond ((minusp cmp)
		  (setf (avl-tree-left root) (avl-tree-delete (avl-tree-left root) key :value value :compare compare :same-key-handler same-key-handler)))
		 ((plusp cmp)
		  (setf (avl-tree-right root) (avl-tree-delete (avl-tree-right root) key :value value :compare compare :same-key-handler same-key-handler)))
		 (t
		  (unless (eq value :all)
		    (funcall same-key-handler root value))
		  ;; (inform "value ~S~%" value)
		  (cond ((and (not (eq value :all)) (avl-tree-value root)) root)
			((and (avl-tree-left root) (avl-tree-right root))
			 (let ((temp (avl-tree-min-value-node (avl-tree-right root))))
			   (setf (avl-tree-key root) (avl-tree-key temp))
			   (setf (avl-tree-value root) (avl-tree-value temp))
			   (setf (avl-tree-right root) (avl-tree-delete (avl-tree-right root) (avl-tree-key temp) :compare compare :same-key-handler same-key-handler))))
			(t
			 (let ((temp (or (avl-tree-left root) (avl-tree-right root))))
			   ;; (inform "Root ~S, temp ~S~%" root temp)
			   (cond ((null temp)
				  (setf root nil))
				 (t
				  (setf (avl-tree-key root) (avl-tree-key temp))
				  (setf (avl-tree-value root) (avl-tree-value temp))
				  (setf (avl-tree-left root) (avl-tree-left temp))
				  (setf (avl-tree-right root) (avl-tree-right temp))
				  (setf (avl-tree-height root) (avl-tree-height temp))))))))))
	 (and root (rebalance-avl-tree-after-delete root)))))

(defun avl-tree-depth-first-traversal (tree func)
  (cond ((null tree)
	 nil)
	(t
	 (avl-tree-depth-first-traversal (avl-tree-left tree) func)
	 (funcall func tree)
	 (avl-tree-depth-first-traversal (avl-tree-right tree) func))))

(defun print-avl-tree (tree &optional (stream t) (indent 0))
  (cond ((null tree)
	 (format stream "~&~VT()" indent))
	(t
	 (print-avl-tree (avl-tree-right tree) stream (+ 4 indent))
	 (format stream "~&~VT~A" indent tree)
	 (print-avl-tree (avl-tree-left tree) stream (+ 4 indent)))))

(defmethod print-object ((this avl-tree) stream)
  (format stream "<avl-tree ~A: height: ~D, balance: ~D, value: ~A>" (avl-tree-key this) (avl-tree-height this) (get-avl-tree-balance this) (avl-tree-value this)))

(defun check-avl-tree-balances (tree)
  (cond ((null tree) t)
	(t
	 (check-avl-tree-balances (avl-tree-left tree))
	 (assert** (< -2 (get-avl-tree-balance tree) 2) "Wrong balance in node ~S" tree)
	 (check-avl-tree-balances (avl-tree-right tree)))))
				  
(defun check-avl-tree-heights (tree)
  (cond ((null tree) 0)
	(t
	 (let* ((lh (check-avl-tree-heights (avl-tree-left tree)))
		(rh (check-avl-tree-heights (avl-tree-right tree)))
		(ch (1+ (max lh rh))))
	   (assert** (= (avl-tree-height tree) ch) "Wrong height in ~S, should be ~S" tree ch)
	   ch))))

(defun avl-add (tree v)
  (let ((new (avl-tree-insert tree v v)))
    (print-avl-tree new)
    new))

(defun avl-rem (tree v)
  (let ((new (avl-tree-delete tree v :value v)))
    (print-avl-tree new)
    new))

(defun avl-add-list (tree vl)
  (loop for v in vl
	do (setf tree (avl-add tree v)))
  tree)

(defun avl-test1 (&optional (data '(9 5 10 0 6 11 -1 1 2)))
  (let ((tree nil))
    (inform "Adding ~S to ~S~%" data tree)
    (loop for x in data 
	  do (inform "Adding ~S~%" x)
	  do (setf tree (avl-tree-insert tree x x))
	  do (print-avl-tree tree)
	  do (check-avl-tree-heights tree)
	  do (check-avl-tree-balances tree))
    (let ((y 10))
      (inform "Removing ~S to ~S~%" y tree)
      (setf tree (avl-tree-delete tree y :value y))
      (check-avl-tree-heights tree)
      (check-avl-tree-balances tree)
      (inform "After removing value from ~S~%" tree)
      (print-avl-tree tree))))

(defun avl-tree-key-value-pairs (tree)
  (cond ((null tree) nil)
	(t
	 (append (avl-tree-key-value-pairs (avl-tree-left tree)) (list (cons (avl-tree-key tree) (avl-tree-value tree)))  (avl-tree-key-value-pairs (avl-tree-right tree))))))

(defun avl-test2 (&optional verbosep)
  (labels ((sorted-pairs (l) (mapcar #'(lambda (x) (list x x)) (sort (copy-list l) #'<=)))
	   (check-contents (numbers tree)
	     (let ((contents (avl-tree-key-value-pairs tree))
		   (sp (sorted-pairs numbers)))
	       (assert** (equal contents sp) "~%~S is different from~%~S" contents sp))))
    (let ((*random-state* (make-random-state t)))
      (loop with tree = nil
	    with numbers = nil
	    for i from 1 to 100
	    for n = (random (1+ i))
	    do (unless (member n numbers)
		 (push n numbers)
		 (when verbosep (inform "Adding ~S, numbers ~S~%" n numbers))
		 (setf tree (avl-tree-insert tree n n))
		 (check-avl-tree-heights tree)
		 (check-avl-tree-balances tree)
		 (check-contents numbers tree))
	    finally (loop while numbers
			  for x = (nth (random (length numbers)) numbers)
			  do (progn
			       (setf numbers (delete x numbers))
			       (when verbosep (print-avl-tree tree))
			       (when verbosep (inform "Deleting ~S~%" x))
			       (setf tree (avl-tree-delete tree x :value x))
			       (when verbosep (print-avl-tree tree))
			       (check-avl-tree-heights tree)
			       (check-avl-tree-balances tree)
			       (check-contents numbers tree)))))))
			     
		 
