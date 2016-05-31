;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defclass binary-tree ()
  ((key :accessor binary-tree-key :initarg :key :initform nil)
   (value :accessor binary-tree-value :initarg :value :initform nil)
   (height :accessor binary-tree-height :initarg :height :initform 1)
   (balance :accessor binary-tree-balance :initarg :balance :initform 0)
   (left :accessor binary-tree-left :initarg :left :initform nil)
   (right :accessor binary-tree-right :initarg :right :initform nil)))

(defun add-same-key-add-values-to-list (tree value &key (test #'equal))
  (pushnew value (binary-tree-value tree) :test test)
  tree)

(defun remove-same-key-add-values-to-list (tree value &key (test #'equal))
  (let ((new-values (remove value (binary-tree-value tree) :test test)))
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
(defun binary-tree-add (tree key value &key (compare #'-) (same-key-handler #'add-same-key-add-values-to-list))
  (cond ((null tree)
	 (funcall same-key-handler (make-instance 'binary-tree :key key) value))
	(t
	 (let ((cmp (funcall compare key (binary-tree-key tree))))
	   (cond ((minusp cmp)
		  (setf (binary-tree-left tree) (binary-tree-add (binary-tree-left tree) key value :compare compare :same-key-handler same-key-handler))
		  (rebalance tree))
		 ((plusp cmp)
		  (setf (binary-tree-right tree) (binary-tree-add (binary-tree-right tree) key value :compare compare :same-key-handler same-key-handler))
		  (rebalance tree))
		 (t
		  (funcall same-key-handler tree value)))))))
	 
(defun height-and-balance (tree)
  (let* ((left (binary-tree-left tree))
	 (left-height (if left (binary-tree-height left) 0))
	 (right (binary-tree-right tree))
	 (right-height (if right (binary-tree-height right) 0)))
  (values (1+ (max left-height right-height))
	  (- left-height right-height))))

(defun update-height-and-balance (tree)
  (multiple-value-bind (h b)
      (height-and-balance tree)
    (setf (binary-tree-height tree) h)
    (setf (binary-tree-balance tree) b))
  tree)

(defmacro rebalance-branch (tree x y balance-sign)
  (let ((xy (intern (format nil "~A-~A" x y)))
	(xyx (intern (format nil "~A-~A-~A" x y x)))
	(xyy (intern (format nil "~A-~A-~A" x y y)))
	(binary-tree-x (intern (format nil "BINARY-TREE-~A" x)))
	(binary-tree-y (intern (format nil "BINARY-TREE-~A" y))))
    `(cond ((= (binary-tree-balance ,x) (,balance-sign 1))
	    (let ((,xy (,binary-tree-y ,x)))
	      (setf (,binary-tree-x tree) ,xy)
	      (update-height-and-balance ,tree)
	      (assert* (binary-tree-balance tree) "Expected balance to be zero in ~A" ,tree)
	      (setf (,binary-tree-y ,x) ,tree)
	      (update-height-and-balance ,x)
	      (assert* (zerop (binary-tree-balance ,x)) "Expected balance to be zero in ~A" ,x))
	    ,x)
	   (t
	    (assert* (= (binary-tree-balance ,x) (,balance-sign -1)) "Expected balance to be -1 in ~A" ,x)
	    (let* ((,xy (,binary-tree-y ,x))
		   (,xyx (,binary-tree-x ,xy))
		   (,xyy (,binary-tree-y ,xy)))
	      (setf (,binary-tree-y ,x) ,xyx)
	      (update-height-and-balance ,x)
	      (setf (,binary-tree-x ,tree) ,xyy)
	      (update-height-and-balance ,tree)
	      (setf (,binary-tree-x ,xy) ,x)
	      (setf (,binary-tree-y ,xy) ,tree)
	      (update-height-and-balance ,xy)
	      ,xy)))))

(defun rebalance (tree)
  (let ((left (binary-tree-left tree))
	(right (binary-tree-right tree)))
    (multiple-value-bind (new-height new-balance)
	(height-and-balance tree)
      (case new-balance
	((-1 0 1)
	 (setf (binary-tree-height tree) new-height
	       (binary-tree-balance tree) new-balance)
	 tree)
	(2 (rebalance-branch tree left right +))
	(-2 (rebalance-branch tree right left -))
	(t (error* "Unexpected balance in ~A" tree))))))

;; (defun rebalance (tree)
;;   (let ((left (binary-tree-left tree))
;; 	(right (binary-tree-right tree)))
;;     (multiple-value-bind (new-height new-balance)
;; 	(height-and-balance tree)
;;       (case new-balance
;; 	((-1 0 1)
;; 	 (setf (binary-tree-height tree) new-height
;; 	       (binary-tree-balance tree) new-balance)
;; 	 tree)
;; 	(2
;; 	 (cond ((= (binary-tree-balance left) 1)
;; 		(let ((lr (binary-tree-right left)))
;; 		  (setf (binary-tree-left tree) lr)
;; 		  (update-height-and-balance tree)
;; 		  (assert* (binary-tree-balance tree) "Expected balance to be zero in ~A" tree)
;; 		  (setf (binary-tree-right left) tree)
;; 		  (update-height-and-balance left)
;; 		  (assert* (zerop (binary-tree-balance left)) "Expected balance to be zero in ~A" left))
;; 		left)
;; 	       (t
;; 		(assert* (= (binary-tree-balance left) -1) "Expected balance to be -1 in ~A" left)
;; 		(let* ((lr (binary-tree-right left))
;; 		       (lrl (binary-tree-left lr))
;; 		       (lrr (binary-tree-right lr)))
;; 		  (setf (binary-tree-right left) lrl)
;; 		  (update-height-and-balance left)
;; 		  (setf (binary-tree-left tree) lrr)
;; 		  (update-height-and-balance tree)
;; 		  (setf (binary-tree-left lr) left)
;; 		  (setf (binary-tree-right lr) tree)
;; 		  (update-height-and-balance lr)
;; 		  lr))))
;; 	(-2
;; 	 (cond ((= (binary-tree-balance right) -1)
;; 		(let ((rl (binary-tree-left right)))
;; 		  (setf (binary-tree-right tree) rl)
;; 		  (update-height-and-balance tree)
;; 		  (assert* (binary-tree-balance tree) "Expected balance to be zero in ~A" tree)
;; 		  (setf (binary-tree-left right) tree)
;; 		  (update-height-and-balance right)
;; 		  (assert* (zerop (binary-tree-balance right)) "Expected balance to be zero in ~A" right))
;; 		right)
;; 	       (t
;; 		(assert* (= (binary-tree-balance right) 1) "Expected balance to be -1 in ~A" right)
;; 		(let* ((rl (binary-tree-left right))
;; 		       (rlr (binary-tree-right rl))
;; 		       (rll (binary-tree-left rl)))
;; 		  (setf (binary-tree-left right) rlr)
;; 		  (update-height-and-balance right)
;; 		  (setf (binary-tree-right tree) rll)
;; 		  (update-height-and-balance tree)
;; 		  (setf (binary-tree-right rl) right)
;; 		  (setf (binary-tree-left rl) tree)
;; 		  (update-height-and-balance rl)
;; 		  rl))))
;; 	(t (error* "Unexpected balance in ~A" tree))))))

;;; Remember to store the result of binary-tree-remove somewhere. Otherwise you may lose its effect.
(defun binary-tree-remove (tree key value &key (compare #'-) (same-key-handler #'remove-same-key-add-values-to-list))
  (cond ((null tree) nil)
	(t
	 (let ((cmp (funcall compare key (binary-tree-key tree))))
	   (cond ((minusp cmp)
		  (setf (binary-tree-left tree) (binary-tree-remove (binary-tree-left tree) key value :compare compare :same-key-handler same-key-handler))
		  tree)
		 ((plusp cmp)
		  (setf (binary-tree-right tree) (binary-tree-remove (binary-tree-right tree) key value :compare compare :same-key-handler same-key-handler))
		  tree)
		 (t
		  (funcall same-key-handler tree value)))))))
	 
(defun binary-tree-iterator (current remaining &optional (accessor #'(lambda (x) x)))
  (if (null current) nil
      (values (funcall accessor current)
	      (cons (binary-tree-left current)
		    (cons (binary-tree-right current)
			  remaining)))))

	    

(defun print-binary-tree (tree &optional (stream t) (indent 0))
  (cond ((null tree)
	 (format stream "~&~VT()" indent))
	(t
	 (print-binary-tree (binary-tree-right tree) stream (+ 4 indent))
	 (format stream "~&~VT~A" indent tree)
	 (print-binary-tree (binary-tree-left tree) stream (+ 4 indent)))))
(defmethod print-object ((this binary-tree) stream)
  (format stream "<binary-tree ~A: height: ~D, balance: ~D, value: ~A>" (binary-tree-key this) (binary-tree-height this) (binary-tree-balance this) (binary-tree-value this)))

(defun bta (tree v)
  (let ((new (binary-tree-add tree v v)))
    (print-binary-tree new)
    new))

(defun btr (tree v)
  (let ((new (binary-tree-remove tree v v)))
    (print-binary-tree new)
    new))

(defun btal (tree vl)
  (loop for v in vl
	do (setf tree (bta tree v)))
  tree)
