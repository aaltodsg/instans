;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun collect-expression-variables (p)
  (let ((variables nil))
    (labels ((collect (x)
	       (cond ((consp x)
		      (collect (car x))
		      (collect (cdr x)))
		     ((sparql-var-p x) (push-to-end-new x variables :test #'uniquely-named-object-equal))
		     (t nil))))
      (collect p)
      variables)))

;;; Kesken
(defun pattern-variables-consistent-p (e1 e2)
  (error* "Not implemented yet: pattern-variables-consistent-p ~S, ~S" e1 e2))

;;; Bindings and variables
(defun resolve-binding (instans from)
  (cdr (assoc from (instans-bindings instans) :test #'uniquely-named-object-equal)))

(defun reverse-resolve-binding (instans to)
  (car (rassoc to (instans-bindings instans))))

(defun add-binding (instans from to-name)
  (let ((to (make-sparql-var instans to-name)))
    (push-to-end (cons from to) (instans-bindings instans))
  to))

(defun resolve-or-add-binding (instans from)
  (or (resolve-binding instans from)
      (let ((to (fmt-intern "?~D" (length (instans-bindings instans)))))
	(add-binding instans from to))))

(defun canonize-sparql-var (instans v)
  (resolve-or-add-binding instans v))

(defun canonize-sparql-algebra-variables (instans expr)
  (cond ((or (sparql-var-p expr) (rdf-blank-node-p expr))
	 (canonize-sparql-var instans expr))
	((atom expr) expr)
	(t
	 (cons (canonize-sparql-algebra-variables instans (car expr)) (canonize-sparql-algebra-variables instans (cdr expr))))))
