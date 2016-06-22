;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

;;;
;;; Adapted from http://www.geeksforgeeks.org/avl-tree-set-2-deletion/
;;;

(in-package #:instans)

(define-class avl-node ()
  ((key :accessor avl-node-key :initarg :key :initform nil)
   (values :accessor avl-node-values :initarg :values :initform nil)
   (height :accessor avl-node-height :initarg :height :initform 1)
   (left :accessor avl-node-left :initarg :left :initform nil)
   (right :accessor avl-node-right :initarg :right :initform nil)))

(define-class avl-tree ()
  ((root :accessor avl-tree-root :initform nil)
   (key-count :accessor avl-tree-key-count :initform 0)
   (value-count :accessor avl-tree-value-count :initform 0)
   (key-compare :accessor avl-tree-key-compare :initarg :key-compare :initform #'-)
   (value-equal :accessor avl-tree-value-equal :initarg :value-equal :initform #'equal)))

(defun avl-get-range (tree &key lower-bound lower-bound-inclusive-p upper-bound upper-bound-inclusive-p)
  (let* ((result (list nil))
	 (tail result)
	 (key-compare (avl-tree-key-compare tree)))
    (labels ((add-to-result (node)
	       (let ((new (copy-list (avl-node-values node))))
		 (setf (cdr tail) new)
		 (setf tail (last new))))
	     (inclusion-test (node)
	       (let ((key (avl-node-key node)))
		 (and (or (null lower-bound) (if lower-bound-inclusive-p (<= lower-bound key) (< lower-bound key)))
		      (or (null upper-bound) (if upper-bound-inclusive-p (>= upper-bound key) (> upper-bound key))))))
	     (visit (node)
	       (when (and (avl-node-left node) (or (null lower-bound) (plusp (funcall key-compare (avl-node-key node) lower-bound))))
		 (visit (avl-node-left node)))
	       (when (inclusion-test node)
		 (add-to-result node))
	       (when (and (avl-node-right node) (or (null upper-bound) (plusp (funcall key-compare upper-bound (avl-node-key node)))))
		 (visit (avl-node-right node)))))
      (visit (avl-tree-root tree))
      (cdr result))))

(defun get-avl-node-height (node)
  (if node (avl-node-height node) 0))

(defun get-avl-node-balance (node)
  (if node (- (get-avl-node-height (avl-node-left node)) (get-avl-node-height (avl-node-right node))) 0))

(defun avl-right-rotate (y)
  (let* ((x (avl-node-left y))
	 (t2 (avl-node-right x)))
    (setf (avl-node-right x) y)
    (setf (avl-node-left y) t2)
    (setf (avl-node-height y) (1+ (max (get-avl-node-height (avl-node-left y)) (get-avl-node-height (avl-node-right y)))))
    (setf (avl-node-height x) (1+ (max (get-avl-node-height (avl-node-left x)) (get-avl-node-height (avl-node-right x)))))
    x))

(defun avl-left-rotate (x)
  (let* ((y (avl-node-right x))
	 (t2 (avl-node-left y)))
    (setf (avl-node-left y) x)
    (setf (avl-node-right x) t2)
    (setf (avl-node-height x) (1+ (max (get-avl-node-height (avl-node-left x)) (get-avl-node-height (avl-node-right x)))))
    (setf (avl-node-height y) (1+ (max (get-avl-node-height (avl-node-left y)) (get-avl-node-height (avl-node-right y)))))
    y))

(defun rebalance-avl-after-insert (node key key-compare)
  (setf (avl-node-height node) (1+ (max (get-avl-node-height (avl-node-left node)) (get-avl-node-height (avl-node-right node)))))
  (let ((balance (get-avl-node-balance node)))
    (cond ((> balance 1)
	   (cond ((minusp (funcall key-compare key (avl-node-key (avl-node-left node))))
					; Left Left Case
		  (avl-right-rotate node))
		 (t
					; Left Right Case
		  (setf (avl-node-left node) (avl-left-rotate (avl-node-left node)))
		  (avl-right-rotate node))))
	  ((< balance -1)
	   (cond ((minusp (funcall key-compare key (avl-node-key (avl-node-right node))))
					; Right Left Case
		  (setf (avl-node-right node) (avl-right-rotate (avl-node-right node)))
		  (avl-left-rotate node))
		 (t
					; Right Right Case
		  (avl-left-rotate node))))
	  (t node))))

;(defun avl-insert (node key value &key (value-equal #'equal) (key-compare #'-))
(defun avl-insert (tree key value)
  (let ((key-compare (avl-tree-key-compare tree))
	(value-equal (avl-tree-value-equal tree)))
    (labels ((visit (node key value)
	       (cond ((null node)
		      (incf (avl-tree-key-count tree))
		      (incf (avl-tree-value-count tree))
		      (make-instance 'avl-node :key key :height 1 :values (list value)))
		     (t
		      (let ((cmp (funcall key-compare key (avl-node-key node))))
			(cond ((minusp cmp)
			       (setf (avl-node-left node) (visit (avl-node-left node) key value))
			       (rebalance-avl-after-insert node key key-compare))
			      ((plusp cmp)
			       (setf (avl-node-right node) (visit (avl-node-right node) key value))
			       (rebalance-avl-after-insert node key key-compare))
			      (t
			       (let ((prev-count (length (avl-node-values node))))
				 (pushnew value (avl-node-values node) :test value-equal)
				 (when (> (length (avl-node-values node)) prev-count)
				   (incf (avl-tree-value-count tree))))
			       node)))))))
      (let ((prev-count (avl-tree-key-count tree)))
	(setf (avl-tree-root tree) (visit (avl-tree-root tree) key value))
	(> (avl-tree-key-count tree) prev-count)))))

(defun avl-min-value-node (node)
  (loop for current = node then (avl-node-left current)
        while (avl-node-left current)
        finally (return current)))

(defun rebalance-avl-after-delete (root)
  (setf (avl-node-height root) (1+ (max (get-avl-node-height (avl-node-left root)) (get-avl-node-height (avl-node-right root)))))
  (let ((balance (get-avl-node-balance root)))
    (cond ((> balance 1)
	   (cond ((>= (get-avl-node-balance (avl-node-left root)) 0)
					; Left Left Case
		  (avl-right-rotate root))
		 (t
					; Left Right Case
		  (setf (avl-node-left root) (avl-left-rotate (avl-node-left root)))
		  (avl-right-rotate root))))
	  ((< balance -1)
	   (cond ((> (get-avl-node-balance (avl-node-right root)) 0)
					; Right Left Case
		  (setf (avl-node-right root) (avl-right-rotate (avl-node-right root)))
		  (avl-left-rotate root))
		 (t
					; Right Right Case
		  (avl-left-rotate root))))
	  (t
	   root))))

(defun avl-delete (tree key value)
  (let ((key-compare (avl-tree-key-compare tree))
	(value-equal (avl-tree-value-equal tree))
	(moving-node-p nil))
    (labels ((visit (node key value)
	       (cond ((null node) nil)
		     (t
		      (let ((cmp (funcall key-compare key (avl-node-key node))))
			(cond ((minusp cmp)
			       (setf (avl-node-left node) (visit (avl-node-left node) key value)))
			      ((plusp cmp)
			       (setf (avl-node-right node) (visit (avl-node-right node) key value)))
			      (t
			       (unless moving-node-p
				 (let ((prev-count (length (avl-node-values node))))
				   (setf (avl-node-values node) (delete value (avl-node-values node) :test value-equal))
				   (when (< (length (avl-node-values node)) prev-count)
				     (decf (avl-tree-value-count tree))))
				 (when (null (avl-node-values node))
				   (decf (avl-tree-key-count tree))))
			       ;; (inform "value ~S~%" value)
			       (cond ((and (not moving-node-p) (avl-node-values node)) node)
				     ((and (avl-node-left node) (avl-node-right node))
				      (let ((temp (avl-min-value-node (avl-node-right node))))
					(setf (avl-node-key node) (avl-node-key temp))
					(setf (avl-node-values node) (avl-node-values temp))
					(setf moving-node-p t)
					(setf (avl-node-right node) (visit (avl-node-right node) (avl-node-key temp) :all))))
				     (t
				      (let ((temp (or (avl-node-left node) (avl-node-right node))))
					;; (inform "Node ~S, temp ~S~%" node temp)
					(cond ((null temp)
					       (setf node nil))
					      (t
					       (setf (avl-node-key node) (avl-node-key temp))
					       (setf (avl-node-values node) (avl-node-values temp))
					       (setf (avl-node-left node) (avl-node-left temp))
					       (setf (avl-node-right node) (avl-node-right temp))
					       (setf (avl-node-height node) (avl-node-height temp))))))))))
	       (and node (rebalance-avl-after-delete node))))))
      (let ((prev-key-count (avl-tree-key-count tree)))
	(setf (avl-tree-root tree) (visit (avl-tree-root tree) key value))
	(< (avl-tree-key-count tree) prev-key-count)))))

;;; Traversal


;;; Printing

(defun print-avl (tree &optional (stream t))
  (labels ((visit (node indent)
	     (cond ((null node)
		    (format stream "~&~VT()" indent))
		   (t
		    (visit (avl-node-right node) (+ 4 indent))
		    (format stream "~&~VT~A" indent node)
		    (visit (avl-node-left node) (+ 4 indent))))))
    (visit (avl-tree-root tree) 0)))

(defmethod print-object ((this avl-node) stream)
  (format stream "<avl-node ~A: height: ~D, balance: ~D, value: ~A>" (avl-node-key this) (avl-node-height this) (get-avl-node-balance this) (avl-node-values this)))

(defmethod print-object ((this avl-tree) stream)
  (format stream "<avl-tree: key-count: ~D, value-count: ~D, root: ~A>" (avl-tree-key-count this) (avl-tree-value-count this) (avl-tree-root this)))

;;; Testing

(defun check-contents (tree numbers)
  (let ((contents (avl-node-key-value-pairs tree))
	(sp (mapcar #'(lambda (x) (list x x)) (sort (copy-list numbers) #'<=))))
      (assert** (equal contents sp) "~%~S is different from~%~S" contents sp)))

(defun check-avl-balances (tree)
  (labels ((visit (node)
	     (cond ((null node) t)
		   (t
		    (visit (avl-node-left node))
		    (assert** (< -2 (get-avl-node-balance node) 2) "Wrong balance in node ~S" node)
		    (visit (avl-node-right node))))))
    (visit (avl-tree-root tree))))
				  
(defun check-avl-node-heights (tree)
  (labels ((visit (node)
	     (cond ((null node) 0)
		   (t
		    (let* ((lh (visit (avl-node-left node)))
			   (rh (visit (avl-node-right node)))
			   (ch (1+ (max lh rh))))
		      (assert** (= (avl-node-height node) ch) "Wrong height in ~S, should be ~S" node ch)
		      ch)))))
    (visit (avl-tree-root tree))))

(defun avl-add (tree v)
  (let ((new (avl-insert tree v v)))
    (print-avl new)
    new))

(defun avl-rem (tree v)
  (let ((new (avl-delete tree v v)))
    (print-avl new)
    new))

(defun avl-add-list (tree vl)
  (loop for v in vl
	do (avl-add tree v))
  tree)

(defun avl-test1 (&key (data '(9 5 10 0 6 11 -1 1 2)) verbosep)
  (let ((tree (make-instance 'avl-tree)))
    (when verbosep (inform "Adding ~S to ~S~%" data tree))
    (loop for x in data 
	  collect x into added
	  when verbosep do (inform "Adding ~S~%" x)
	  do (avl-insert tree x x)
	  when verbosep do (print-avl tree)
	  do (check-contents tree added)
	  do (check-avl-node-heights tree)
	  do (check-avl-balances tree))
    (let ((y 10))
      (when verbosep (inform "Removing ~S to ~S~%" y tree))
      (avl-delete tree y y)
      (check-contents tree (remove 10 data))
      (check-avl-node-heights tree)
      (check-avl-balances tree)
      (when verbosep
	(inform "After removing value from ~S~%" tree)
	(print-avl tree)))))

(defun avl-node-key-value-pairs (tree)
  (labels ((visit (node)
	     (cond ((null node) nil)
		   (t
		    (append (visit (avl-node-left node)) (list (cons (avl-node-key node) (avl-node-values node)))  (visit (avl-node-right node)))))))
    (visit (avl-tree-root tree))))

(defun avl-test2 (&optional verbosep)
  (let ((*random-state* (make-random-state t)))
    (loop with tree = (make-instance 'avl-tree)
	  with numbers = nil
	  for i from 1 to 100
	  for n = (random (1+ i))
	  do (unless (member n numbers)
	       (push n numbers)
	       (when verbosep (inform "Adding ~S, numbers ~S~%" n numbers))
	       (avl-insert tree n n)
	       (check-avl-node-heights tree)
	       (check-avl-balances tree)
	       (check-contents tree numbers))
	  finally (loop while numbers
			for x = (nth (random (length numbers)) numbers)
			do (progn
			     (setf numbers (delete x numbers))
			     (when verbosep (print-avl tree))
			     (when verbosep (inform "Deleting ~S~%" x))
			     (avl-delete tree x x)
			     (when verbosep (print-avl tree))
			     (check-avl-node-heights tree)
			     (check-avl-balances tree)
			     (check-contents tree numbers))))))
		 
(defun avl-range-test (low include-low-p high include-high-p keys)
  (let ((tree (make-instance 'avl-tree)))
    (loop for k in keys
	  do (avl-insert tree k k))
    (avl-get-range tree :lower-bound low :lower-bound-inclusive-p include-low-p :upper-bound high :upper-bound-inclusive-p include-high-p)))
