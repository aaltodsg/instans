;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun test-dirs (test-set)
  (unless (member test-set '(:r2 :sparql11))
    (error* "Unknown test set ~A" test-set))
  (directory (format nil "~A/tests/data-~A/*/" (namestring (find-instans-root-directory)) (string-downcase (string test-set)))))

(defun manifests (test-set)
  (mapcan #'(lambda (d) (directory (format nil "~A/manifest.ttl" (namestring d)))) (test-dirs test-set)))

(defun manifest-tests (manifest &optional (query (concatenate 'string (namestring (find-instans-root-directory)) "tests/input/syntax-test.rq")))
  (let* ((base (format nil "file://~A" (directory-namestring (truename manifest))))
	 (instans (main-test (format nil "--output-type solution-set -b ~A -r ~A -t ~A" base query manifest)))
	 (p (instans-query-output-processor instans)))
    (cond ((query-output-solution-set-processor-p p)
	   (if (slot-boundp p 'variables)
	       (values (query-output-solution-set-processor-variables p)
		       (query-output-solution-set-processor-bindings p))
	       (values nil nil)))
	  (t nil))))
    

(defvar *r2-dirs*)
(defvar *sparql11-dirs*)
(defvar *all-dirs*)
(defvar *r2-manifests*)
(defvar *sparql11-manifests*)
(defvar *all-manifests*)

(defun test-init ()
  (setf *r2-dirs* (test-dirs :r2))
  (setf *sparql11-dirs* (test-dirs :sparql11))
  (setf *all-dirs* (append *r2-dirs* *sparql11-dirs*))
  (setf *r2-manifests* (manifests :r2))
  (setf *sparql11-manifests* (manifests  :sparql11))
  (setf *all-manifests* (append *r2-manifests* *sparql11-manifests*)))

(defun test-counts (&rest args)
  (flet ((testlist (name list)
	   (inform "~A:" name)
	   (loop for m in list for nvals = (length (second (multiple-value-list (apply #'manifest-tests m args))))
	      do (inform "~2d tests in ~A" nvals m) sum nvals into count finally (inform "~4D tests" count))))
    (testlist "DATA-R2" *r2-manifests*)
    (testlist "DATA-SPARQL11" *sparql11-manifests*)))

(defun test-files (&rest args)
  (flet ((testlist (name list)
	   (inform "~A:" name)
	   (loop for m in list for solution = (multiple-value-list (apply #'manifest-tests m args))
	      for vars = (first solution)
	      for results = (second solution)
	      for nvals = (length results)
	      do (inform "~2d tests in ~A:" nvals m)
	      do (loop for result in results do (inform "~{  ~{~12A: ~A~}~^~%~}" (mapcar #'list vars result))
		      when (and (second result) (not (third result))) do (inform "!!!"))
	      sum nvals into count finally (inform "~4D tests" count))))
    (testlist "DATA-R2" *r2-manifests*)
    (testlist "DATA-SPARQL11" *sparql11-manifests*)))
