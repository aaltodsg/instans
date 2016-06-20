;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

;;;
;;; Adapted from http://www.geeksforgeeks.org/avl-tree-set-2-deletion/
;;;

(in-package #:instans)

(define-class avl ()
  ((key :accessor avl-key :initarg :key :initform nil)
   (value :accessor avl-value :initarg :value :initform nil)
   (height :accessor avl-height :initarg :height :initform 1)
   (left :accessor avl-left :initarg :left :initform nil)
   (right :accessor avl-right :initarg :right :initform nil)))

;; (defun get-range (tree &key lower-bound lower-bound-op upper-bound upper-bound-op (node-value-getter #'(lambda (n) (copy-list (avl-value n)))))
;;   (let* ((result (list nil))
;; 	 (tail result)
;; 	 (lowest-found nil))
;;     (labels ((add-results (x)
;; 	       (setf (cdr tail) x)
;; 	       (setf tail (last x)))
;; 	     (visit (node)
;; 	       (cond ((null node) nil)
;; 		     ((not lowest-found)
;; 		      (cond ((not lower-bound)
;; 			     (cond ((null (avl-left node))
;; 				    (setf lowest-found t)
;; 				    (add-result (funcall node-value-getter node)))
;; 				   (t
;; 				    (visit (avl-left node)))))
;; 			    ((funcall lower-bound-op lower-bound (avl-key node))
;; 			     (

(defun get-avl-height (node)
  (if node (avl-height node) 0))

(defun get-avl-balance (node)
  (if node (- (get-avl-height (avl-left node)) (get-avl-height (avl-right node))) 0))

(defun add-same-key-values-in-list (tree value &key (test #'equal))
  (pushnew value (avl-value tree) :test test)
  tree)

(defun delete-same-key-values-in-list (tree value &key (test #'equal))
  (setf (avl-value tree) (delete value (avl-value tree) :test test))
  tree)

(defun avl-right-rotate (y)
  (let* ((x (avl-left y))
	 (t2 (avl-right x)))
    (setf (avl-right x) y)
    (setf (avl-left y) t2)
    (setf (avl-height y) (1+ (max (get-avl-height (avl-left y)) (get-avl-height (avl-right y)))))
    (setf (avl-height x) (1+ (max (get-avl-height (avl-left x)) (get-avl-height (avl-right x)))))
    x))

(defun avl-left-rotate (x)
  (let* ((y (avl-right x))
	 (t2 (avl-left y)))
    (setf (avl-left y) x)
    (setf (avl-right x) t2)
    (setf (avl-height x) (1+ (max (get-avl-height (avl-left x)) (get-avl-height (avl-right x)))))
    (setf (avl-height y) (1+ (max (get-avl-height (avl-left y)) (get-avl-height (avl-right y)))))
    y))

(defun rebalance-avl-after-insert (node key compare)
  (setf (avl-height node) (1+ (max (get-avl-height (avl-left node)) (get-avl-height (avl-right node)))))
  (let ((balance (get-avl-balance node)))
    (cond ((> balance 1)
	   (cond ((minusp (funcall compare key (avl-key (avl-left node))))
					; Left Left Case
		  (avl-right-rotate node))
		 (t
					; Left Right Case
		  (setf (avl-left node) (avl-left-rotate (avl-left node)))
		  (avl-right-rotate node))))
	  ((< balance -1)
	   (cond ((minusp (funcall compare key (avl-key (avl-right node))))
					; Right Left Case
		  (setf (avl-right node) (avl-right-rotate (avl-right node)))
		  (avl-left-rotate node))
		 (t
					; Right Right Case
		  (avl-left-rotate node))))
	  (t node))))

(defun avl-insert (node key value &key (compare #'-) (same-key-handler #'add-same-key-values-in-list))
  (cond ((null node)
	 (funcall same-key-handler (make-instance 'avl :key key :height 1) value))
	(t
	 (let ((cmp (funcall compare key (avl-key node))))
	   (cond ((minusp cmp)
		  (setf (avl-left node) (avl-insert (avl-left node) key value :compare compare :same-key-handler same-key-handler))
		  (rebalance-avl-after-insert node key compare))
		 ((plusp cmp)
		  (setf (avl-right node) (avl-insert (avl-right node) key value :compare compare :same-key-handler same-key-handler))
		  (rebalance-avl-after-insert node key compare))
		 (t
		  (funcall same-key-handler node value)))))))

(defun avl-min-value-node (node)
  (loop for current = node then (avl-left current)
        while (avl-left current)
        finally (return current)))

(defun rebalance-avl-after-delete (root)
  (setf (avl-height root) (1+ (max (get-avl-height (avl-left root)) (get-avl-height (avl-right root)))))
  (let ((balance (get-avl-balance root)))
    (cond ((> balance 1)
	   (cond ((>= (get-avl-balance (avl-left root)) 0)
					; Left Left Case
		  (avl-right-rotate root))
		 (t
					; Left Right Case
		  (setf (avl-left root) (avl-left-rotate (avl-left root)))
		  (avl-right-rotate root))))
	  ((< balance -1)
	   (cond ((> (get-avl-balance (avl-right root)) 0)
					; Right Left Case
		  (setf (avl-right root) (avl-right-rotate (avl-right root)))
		  (avl-left-rotate root))
		 (t
					; Right Right Case
		  (avl-left-rotate root))))
	  (t
	   root))))

(defun avl-delete (root key &key (value :all) (compare #'-) (same-key-handler #'delete-same-key-values-in-list))
  (cond ((null root) nil)
	(t
	 (let ((cmp (funcall compare key (avl-key root))))
	   (cond ((minusp cmp)
		  (setf (avl-left root) (avl-delete (avl-left root) key :value value :compare compare :same-key-handler same-key-handler)))
		 ((plusp cmp)
		  (setf (avl-right root) (avl-delete (avl-right root) key :value value :compare compare :same-key-handler same-key-handler)))
		 (t
		  (unless (eq value :all)
		    (funcall same-key-handler root value))
		  ;; (inform "value ~S~%" value)
		  (cond ((and (not (eq value :all)) (avl-value root)) root)
			((and (avl-left root) (avl-right root))
			 (let ((temp (avl-min-value-node (avl-right root))))
			   (setf (avl-key root) (avl-key temp))
			   (setf (avl-value root) (avl-value temp))
			   (setf (avl-right root) (avl-delete (avl-right root) (avl-key temp) :compare compare :same-key-handler same-key-handler))))
			(t
			 (let ((temp (or (avl-left root) (avl-right root))))
			   ;; (inform "Root ~S, temp ~S~%" root temp)
			   (cond ((null temp)
				  (setf root nil))
				 (t
				  (setf (avl-key root) (avl-key temp))
				  (setf (avl-value root) (avl-value temp))
				  (setf (avl-left root) (avl-left temp))
				  (setf (avl-right root) (avl-right temp))
				  (setf (avl-height root) (avl-height temp))))))))))
	 (and root (rebalance-avl-after-delete root)))))

(defun avl-depth-first-traversal (tree func)
  (cond ((null tree)
	 nil)
	(t
	 (avl-depth-first-traversal (avl-left tree) func)
	 (funcall func tree)
	 (avl-depth-first-traversal (avl-right tree) func))))

(defun print-avl (tree &optional (stream t) (indent 0))
  (cond ((null tree)
	 (format stream "~&~VT()" indent))
	(t
	 (print-avl (avl-right tree) stream (+ 4 indent))
	 (format stream "~&~VT~A" indent tree)
	 (print-avl (avl-left tree) stream (+ 4 indent)))))

(defmethod print-object ((this avl) stream)
  (format stream "<avl ~A: height: ~D, balance: ~D, value: ~A>" (avl-key this) (avl-height this) (get-avl-balance this) (avl-value this)))

(defun check-avl-balances (tree)
  (cond ((null tree) t)
	(t
	 (check-avl-balances (avl-left tree))
	 (assert** (< -2 (get-avl-balance tree) 2) "Wrong balance in node ~S" tree)
	 (check-avl-balances (avl-right tree)))))
				  
(defun check-avl-heights (tree)
  (cond ((null tree) 0)
	(t
	 (let* ((lh (check-avl-heights (avl-left tree)))
		(rh (check-avl-heights (avl-right tree)))
		(ch (1+ (max lh rh))))
	   (assert** (= (avl-height tree) ch) "Wrong height in ~S, should be ~S" tree ch)
	   ch))))

(defun avl-add (tree v)
  (let ((new (avl-insert tree v v)))
    (print-avl new)
    new))

(defun avl-rem (tree v)
  (let ((new (avl-delete tree v :value v)))
    (print-avl new)
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
	  do (setf tree (avl-insert tree x x))
	  do (print-avl tree)
	  do (check-avl-heights tree)
	  do (check-avl-balances tree))
    (let ((y 10))
      (inform "Removing ~S to ~S~%" y tree)
      (setf tree (avl-delete tree y :value y))
      (check-avl-heights tree)
      (check-avl-balances tree)
      (inform "After removing value from ~S~%" tree)
      (print-avl tree))))

(defun avl-key-value-pairs (tree)
  (cond ((null tree) nil)
	(t
	 (append (avl-key-value-pairs (avl-left tree)) (list (cons (avl-key tree) (avl-value tree)))  (avl-key-value-pairs (avl-right tree))))))

(defun avl-test2 (&optional verbosep)
  (labels ((sorted-pairs (l) (mapcar #'(lambda (x) (list x x)) (sort (copy-list l) #'<=)))
	   (check-contents (numbers tree)
	     (let ((contents (avl-key-value-pairs tree))
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
		 (setf tree (avl-insert tree n n))
		 (check-avl-heights tree)
		 (check-avl-balances tree)
		 (check-contents numbers tree))
	    finally (loop while numbers
			  for x = (nth (random (length numbers)) numbers)
			  do (progn
			       (setf numbers (delete x numbers))
			       (when verbosep (print-avl tree))
			       (when verbosep (inform "Deleting ~S~%" x))
			       (setf tree (avl-delete tree x :value x))
			       (when verbosep (print-avl tree))
			       (check-avl-heights tree)
			       (check-avl-balances tree)
			       (check-contents numbers tree)))))))
			     
		 
