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
(defun resolve-binding (bindings from)
  (cdr (assoc from (bindings-alist bindings) :test #'uniquely-named-object-equal)))

(defun reverse-resolve-binding (bindings to)
  (car (rassoc to (bindings-alist bindings))))

(defun add-binding (bindings from to-name)
  (let ((to (make-sparql-var to-name)))
    (push-to-end (cons from to) (bindings-alist bindings))
  to))

(defun resolve-or-add-binding (bindings from)
  (or (resolve-binding bindings from)
      (let ((to (fmt-intern "?~D" (length (bindings-alist bindings)))))
	(add-binding bindings from to))))

(defvar *gen-var-counter* nil)

(defun canonize-sparql-var (v bindings)
  (resolve-or-add-binding bindings v))

(defun canonize-sparql-algebra-variables (expr bindings)
  (cond ((or (sparql-var-p expr) (rdf-blank-node-p expr))
	 (canonize-sparql-var expr bindings))
	((atom expr) expr)
	(t
	 (cons (canonize-sparql-algebra-variables (car expr) bindings) (canonize-sparql-algebra-variables (cdr expr) bindings)))))

(defun new-var (prefix bindings)
  (let* ((n (incf *gen-var-counter*))
	 (v (make-sparql-var (fmt-intern "~A~D" prefix n))))
    (canonize-sparql-var v bindings)))
