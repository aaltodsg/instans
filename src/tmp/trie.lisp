;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class trie ()
  ((level :accessor trie-level :initform nil)
   (level-tail :accessor trie-level-tail :initform nil)))

(defun trie-add-path (trie path &optional (test #'eql))
  (cond ((null path) trie)
	((null trie)
	 (setf trie (make-instance 'trie))
	 (setf (trie-level trie) (list (cons (car path) (trie-add-path nil (cdr path)))))
	 (setf (trie-level-tail trie) (trie-level trie))
	 trie)
	(t
	 (let ((item (assoc (first path) (trie-level trie) :test test)))
	   (cond ((null item)
		  (setf (cdr (trie-level-tail trie)) (list (cons (car path) (trie-add-path nil (cdr path)))))
		  (setf (trie-level-tail trie) (cdr (trie-level-tail trie)))
		  trie)
		 (t
		  (trie-add-path (cdr item) (cdr path))
		  trie))))))

(defun trie-paths (trie &optional result)
  (cond ((null trie) (list (reverse result)))
	(t (loop for (item . subtrie) in (trie-level trie)
		 append (trie-paths subtrie (cons item result))))))

(defun trie-map (trie function &optional result)
  (cond ((null trie) (funcall function (reverse result)))
	(t (loop for (item . subtrie) in (trie-level trie)
		 do (trie-map subtrie function (cons item result))))))

;; (defun add-path (trie path &optional (test #'eql))
;;   (cond ((null path) trie)
;; 	((null (trie-level trie))
;; 	 (let ((next-level (make-instance 'trie)))
;; 	   (setf (trie-level trie) (list (cons (car path) next-level)))
;; 	   (setf (trie-level-tail trie) (trie-level trie))
;; 	   (add-path next-level (cdr path))))
;; 	(t
;; 	 (let ((item (assoc (first path) (trie-level trie) :test test)))
;; 	   (cond ((null item)
;; 		  (let ((next-level (make-instance 'trie)))
;; 		    (setf (cdr (trie-level-tail trie)) (list (cons (car path) next-level)))
;; 		    (setf (trie-level-tail trie) (cdr (trie-level-tail trie)))
;; 		    (add-path next-level (cdr path))))
;; 		 (t
;; 		  (add-path (cdr item) (cdr path))))))))

;; (defun trie-paths (trie &optional result)
;;   (cond ((null (trie-level trie)) (list (reverse result)))
;; 	(t (loop for (item . subtrie) in (trie-level trie)
;; 		 append (trie-paths subtrie (cons item result))))))

;; (defun trie-map (trie function &optional result)
;;   (cond ((null (trie-level trie)) (funcall function (reverse result)))
;; 	(t (loop for (item . subtrie) in (trie-level trie)
;; 		 do (trie-map subtrie function (cons item result))))))

