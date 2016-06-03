;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun optimize-rete-network (instans)
  (declare (ignorable instans))
;  (optimize-filters instans)
  )

;; (defun optimize-filters (instans)
;;   (loop for node in (instans-nodes instans)
;; 	when (filter-node-p node)
;; 	do (optimize-filter node)))

;; (defun split-conjunction (test)
;;   (let ((op (first test)))
;;     (cond ((eq op (find-sparql-op "AND"))
;; 	   (mapcan #'split-conjunction (rest test)))
;; 	  (t
;; 	   (list test)))))

;; (defun filter-node-optimizable-p (node expr)
;;   nil)

;; (defun find-optimizable-join (node expr)
;;   (cond ((= 1 (length (node-parents node)))
;; 	 (let ((parent (first (node-parents node))))
;; 	   (cond ((filter-node-p parent)
;; 		  (filter-node-optimizable-p parent expr))
;; 		 (t
;; 		  (find-optimizable-join parent expr)))))
;; 	(t
;; 	 nil)))

;; (defun optimize-test-part (node expr)
;;   (let* ((op (first expr))
;; 	 (op-name (sparql-op-name op)))
;;     (cond ((not (member op-name '("<" "<=" ">=" ">") :test #'string=))
;; 	   nil)
;; 	  ((not (every #'linear-expression-p (rest expr)))
;; 	   nil)
;; 	  (t
;; 	   (find-optimizable-join node expr)))))

;; (defun compare-to-n-iris (iri n prefix)
;;   (let ((iris (loop for i from 0 below n
;; 		    collect (parse-iri (format nil "~A-~D" prefix i)))))
;;     (loop for iri1 in iris
;; 	  do (sparql-value-equal iri1 iri))))
	      

;; (defun optimize-filter (node)
;;   (inform "Checking filter node ~S" node)
;;   (let* ((test (filter-test node))
;; 	 (test-parts (split-conjunction test))
;; 	 (remaining-test-parts nil))
;;     (loop for test-part in test-parts
;; 	  for matching-join = (optimize-test-part node test-part)
;; 	  do (inform "Test part ~S -> ~S" test-part matching-join)
;; 	  do (when (null matching-join)
;; 	       (inform "Cannot optimize test part ~S" test-part)
;; 	       (push test-part remaining-test-parts)))
;;     (cond ((null remaining-test-parts)
;; 	   (inform "Filter node ~S can be eliminated" node))
;; 	  ((= (length remaining-test-parts) (length test-parts))
;; 	   (inform "Filter node ~S cannot be optimized" node))
;; 	  (t
;; 	   (inform "Filter test can be simplified in filter node ~S" node)))))
