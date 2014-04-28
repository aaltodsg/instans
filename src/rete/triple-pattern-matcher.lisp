;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun print-triple-pattern-matcher (tm &optional (stream *standard-output*))
  (labels ((print-table-contents (table indent)
	     (let ((firstp t))
	       (maphash #'(lambda (key value)
			    (cond ((hash-table-p value)
				   (cond (firstp
					  (setf firstp nil)
					  (format stream "~A->~%~V@T{" key (+ indent 4)))
					 (t
					  (format stream "~%~V@T~A->~%~V@T{" indent key (+ indent 4))))
				   (print-table-contents value (+ indent 5))
				   (format stream "};"))
				  (t
				   (cond (firstp
					  (setf firstp nil)
					  (format stream "{~A -> ~A}" key value))
					 (t
					  (format stream "~%~V@T{~A -> ~A}" indent key value))))))
		      table)))
	   (print-triple-pattern-matcher-hash-table (name table &optional (indent 4))
	     (unless (null table)
	       (format stream "~%~V@T~A: {" indent name)
	       (print-table-contents table (+ indent 6))
	       (format stream "};"))))
    (format stream "~%#<TRIPLE-PATTERN-MATCHER ~A: " (instans-name (triple-pattern-matcher-instans tm)))
    (format stream "~%    xxx: ~A; " (triple-pattern-matcher-xxx tm))
    (print-triple-pattern-matcher-hash-table "sxx" (triple-pattern-matcher-sxx tm))
    (print-triple-pattern-matcher-hash-table "xpx" (triple-pattern-matcher-xpx tm))
    (print-triple-pattern-matcher-hash-table "xxo" (triple-pattern-matcher-xxo tm))
    (print-triple-pattern-matcher-hash-table "spx" (triple-pattern-matcher-spx tm))
    (print-triple-pattern-matcher-hash-table "sxo" (triple-pattern-matcher-sxo tm))
    (print-triple-pattern-matcher-hash-table "xpo" (triple-pattern-matcher-xpo tm))
    (print-triple-pattern-matcher-hash-table "spo" (triple-pattern-matcher-spo tm))
  (format stream ">")))

(defun hashable-key (term)
  (cond ((rdf-term-p term)
	 (get-hashkey term))
	((atom term)
	 (sxhash term))
	(t (error* "Trying to get hashable-key from a non-atom ~S" term))))

(defmacro gethash-or-else-update (table key update)
  (let ((table-var (gensym "TABLE"))
	(key-var (gensym "KEY"))
	(update-var (gensym "UPDATE")))
  `(let ((,table-var ,table)
	 (,key-var (hashable-key ,key)))
     (or (gethash ,key-var ,table-var)
	 (let ((,update-var ,update))
	   (setf (gethash ,key-var ,table-var) ,update-var)
	   ,update-var)))))

(defmacro hashtable-push-new (table key node)
  (let ((table-var (gensym "TABLE"))
	(key-var (gensym "KEY")))
  `(let ((,table-var ,table)
	 (,key-var (hashable-key ,key)))
     (or (member ,node (gethash ,key-var ,table-var))
	 (push-to-end ,node (gethash ,key-var ,table-var))))))

(defmacro get-or-create-table (accessor)
  `(or ,accessor (progn (setf ,accessor (make-hash-table :test #'equal)) ,accessor)))

(defun add-triple-pattern-node (tm triple-pattern-node)
  (let* ((triple-pattern (triple-pattern-node-triple-pattern triple-pattern-node))
	 (subj (first triple-pattern))
	 (pred (second triple-pattern))
	 (obj (third triple-pattern))
	 (existsp t))
    (cond ((sparql-var-p subj)
	   (cond ((sparql-var-p pred)
		  (cond ((sparql-var-p obj) ;;; xxx
			 (setf existsp nil)
			 (push-to-end-new triple-pattern-node (triple-pattern-matcher-xxx tm)))
			(t ;;; xxo
			 (hashtable-push-new (get-or-create-table (triple-pattern-matcher-xxo tm)) obj triple-pattern-node))))
		 ((sparql-var-p obj) ;;; xpx
		  (hashtable-push-new (get-or-create-table (triple-pattern-matcher-xpx tm)) pred triple-pattern-node))
		 (t ;;; xpo
		  (hashtable-push-new (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-xpo tm)) pred (make-hash-table :test #'equal)) obj triple-pattern-node))))
	  ((sparql-var-p pred)
	   (cond ((sparql-var-p obj) ;;; sxx
		  (hashtable-push-new (get-or-create-table (triple-pattern-matcher-sxx tm)) subj triple-pattern-node))
		 (t ;;; sxo
		  (hashtable-push-new (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-sxo tm)) subj (make-hash-table :test #'equal)) obj triple-pattern-node))))
	  (t
	   (cond ((sparql-var-p obj) ;;; spx
		  (hashtable-push-new (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-spx tm)) subj (make-hash-table :test #'equal)) pred triple-pattern-node))
		 (t ;;; spo
		  (hashtable-push-new (gethash-or-else-update (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-spo tm)) subj (make-hash-table :test #'equal)) pred (make-hash-table :test #'equal)) obj triple-pattern-node)))))))

(defun match-quad (tm subj pred obj graph)
  (let ((result nil))
    (flet ((match? (triple-nodes &rest args) (loop for node in triple-nodes do (push (cons node (cons graph args)) result)))
	   (gethash* (term table) (gethash (hashable-key term) table)))
      (match? (triple-pattern-matcher-xxx tm) subj pred obj)
      ;; (loop for node in (triple-pattern-matcher-xxx tm)
      ;; 	    do (match? node subj pred obj))
      (let ((table (triple-pattern-matcher-sxx tm))) (if table (match? (gethash* subj table) pred obj)))
      (let ((table (triple-pattern-matcher-xpx tm))) (if table (match? (gethash* pred table) subj obj)))
      (let ((table (triple-pattern-matcher-xxo tm))) (if table (match? (gethash* obj table) subj pred )))
      (let ((table (triple-pattern-matcher-spx tm))) (if table (let ((px (gethash* subj table))) (if px (match? (gethash* pred px) obj)))))
      (let ((table (triple-pattern-matcher-sxo tm))) (if table (let ((xo (gethash* subj table))) (if xo (match? (gethash* obj xo) pred)))))
      (let ((table (triple-pattern-matcher-xpo tm))) (if table (let ((xo (gethash* pred table))) (if xo (match? (gethash* obj xo) subj)))))
      (let ((table (triple-pattern-matcher-spo tm))) (if table (let ((po (gethash* subj table))) (if po (let ((o (gethash* pred po))) (if o (match? (gethash* obj o))))))))
    result)))

;; (defun add-triple-pattern-node (tm triple-pattern-node)
;;   (let* ((triple-pattern (triple-pattern-node-triple-pattern triple-pattern-node))
;; 	 (subj (first triple-pattern))
;; 	 (pred (second triple-pattern))
;; 	 (obj (third triple-pattern))
;; 	 (existsp t))
;; ;    (inform "add-triple-pattern-node ~S, (~S, ~S, ~S)" triple-pattern-node subj pred obj)
;; ;    (flet ((add-node () (setf existsp nil) (if graph (cons triple-pattern-node graph) triple-pattern-node)))
;;     (flet ((add-node () (setf existsp nil) triple-pattern-node))
;;       (let ((result (cond ((sparql-var-p subj)
;; 			   (cond ((sparql-var-p pred)
;; 				  (cond ((sparql-var-p obj) ;;; xxx
;; 					 (setf existsp nil)
;; 					 (push-to-end triple-pattern-node (triple-pattern-matcher-xxx tm)))
;; 					(t ;;; xxo
;; 					 (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-xxo tm)) obj (add-node)))))
;; 				 ((sparql-var-p obj) ;;; xpx
;; 				  (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-xpx tm)) pred (add-node)))
;; 				 (t ;;; xpo
;; 				  (gethash-or-else-update (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-xpo tm)) pred (make-hash-table :test #'equal)) obj (add-node)))))
;; 			  ((sparql-var-p pred)
;; 			   (cond ((sparql-var-p obj) ;;; sxx
;; 				  (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-sxx tm)) subj (add-node)))
;; 				 (t ;;; sxo
;; 				  (gethash-or-else-update (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-sxo tm)) subj (make-hash-table :test #'equal)) obj (add-node)))))
;; 			  (t
;; 			   (cond ((sparql-var-p obj) ;;; spx
;; 				  (gethash-or-else-update (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-spx tm)) subj (make-hash-table :test #'equal)) pred (add-node)))
;; 				 (t ;;; spo
;; 				  (gethash-or-else-update (gethash-or-else-update (gethash-or-else-update (get-or-create-table (triple-pattern-matcher-spo tm)) subj (make-hash-table :test #'equal)) pred (make-hash-table :test #'equal)) obj (add-node))))))))
;; 	(values result existsp)))))

;; (defun match-quad (tm subj pred obj graph)
;;   (let ((result nil))
;;     (flet ((match? (triple-node &rest args) (if triple-node (push (cons triple-node (cons graph args)) result)))
;; 	   (gethash* (term table) (gethash (hashable-key term) table)))
;;       (loop for node in (triple-pattern-matcher-xxx tm)
;; 	    do (match? node subj pred obj))
;;       (let ((table (triple-pattern-matcher-sxx tm))) (if table (match? (gethash* subj table) pred obj)))
;;       (let ((table (triple-pattern-matcher-xpx tm))) (if table (match? (gethash* pred table) subj obj)))
;;       (let ((table (triple-pattern-matcher-xxo tm))) (if table (match? (gethash* obj table) subj pred )))
;;       (let ((table (triple-pattern-matcher-spx tm))) (if table (let ((px (gethash* subj table))) (if px (match? (gethash* pred px) obj)))))
;;       (let ((table (triple-pattern-matcher-sxo tm))) (if table (let ((xo (gethash* subj table))) (if xo (match? (gethash* obj xo) pred)))))
;;       (let ((table (triple-pattern-matcher-xpo tm))) (if table (let ((xo (gethash* pred table))) (if xo (match? (gethash* obj xo) subj)))))
;;       (let ((table (triple-pattern-matcher-spo tm))) (if table (let ((po (gethash* subj table))) (if po (let ((o (gethash* pred po))) (if o (match? (gethash* obj o))))))))
;;     result)))

