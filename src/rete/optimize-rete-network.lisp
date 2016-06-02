;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun optimize-rete-network (instans)
  (optimize-filters instans))

(defun optimize-filters (instans)
  (loop for node in (instans-nodes instans)
	when (filter-node-p node)
	do (optimize-filter node)))

(defun split-conjunction (test)
  (let ((op (first test)))
    (cond ((eq op (find-sparql-op "AND"))
	   (mapcan #'split-conjunction (rest test)))
	  (t
	   (list test)))))

(defun optimize-test-part (node expr)
  (let* ((op (first expr))
	 (op-name (sparql-op-name op)))
    (cond ((not (member op-name '("<" "<=" ">=" ">") :test #'string=))
	   nil)
	  (t
	   nil
))))

;; (defun compare-to-n-iris (iri n prefix)
;;   (let ((iris (loop for i from 0 below n
;; 		    collect (parse-iri (format nil "~A-~D" prefix i)))))
;;     (loop for iri1 in iris
;; 	  do (sparql-value-equal iri1 iri))))
	      

(defun optimize-filter (node)
  (inform "Checking filter node ~S" node)
  (let* ((test (filter-test node))
	 (test-parts (split-conjunction test))
	 (remaining-test-parts nil))
    (loop for test-part in test-parts
	  for optimized-test = (optimize-test-part node test-part)
	  do (inform "Test part ~S -> ~S" test-part optimized-test)
	  do (cond ((null optimized-test)
		    (inform "Can optimize test part ~S" test-part))
		   (t
		    (inform "Cannot optimize test part ~S" test-part)
		    (push test-part remaining-test-parts))))
    (cond ((null remaining-test-parts)
	   (inform "Filter node ~S can be eliminated" node))
	  ((= (length remaining-test-parts) (length test-parts))
	   (inform "Filter node ~S cannot be optimized" node))
	  (t
	   (inform "Filter test can be simplified in filter node ~S" node)))))
