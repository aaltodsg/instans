;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun flatten-outermost-ands (expr &optional (and-op (find-sparql-op "LOGICAL-AND")))
  (cond ((consp expr)
	 (cond ((eq (first expr) and-op)
		(cons and-op
		      (loop for x in (mapcar #'flatten-outermost-ands (rest expr))
			 nconc (cond ((and (consp x) (eq (first x) and-op)) (copy-list (rest x)))
				     (t (list x))))))
	       (t expr)))
	(t expr)))

;; (defun reorder-linear-inequality(expr beta-vars alpha-vars)
;;   (let ((relop (first expr)))
;;     (cond ((member relop '("<" "<=" ">=" ">") :test #'string=)
;; 	   (let* ((lexprlist (flatten-sum (second expr)))
;; 		  (rexprlist (flatten-sum (third expr)))
;; 		  (exprlist (append lexprlist (mapcar #'(lambda (x)
;; 							  (if (and (listp x) (string= (first x) "-"))))
	     
;; 	  (t
;; 	   nil))))

(defun find-targets (node vars)
  (cond ((and (= 1 (length (node-succ node))) (subsetp vars (node-def-prec node)))
	 (cond ((minus-node-p node) nil)
	       ((join-node-p node)
		(list-union (find-targets (join-beta node) vars) (find-targets (join-alpha node) vars)))
	       ((null (node-prev node))
		nil)
	       (t
		(let ((prev-targets (find-targets (node-prev node) vars)))
		  (or prev-targets (list node))))))))

(defun add-filter-before (node test)
  ;; (inform "add filter before ~S ~S" node test)
  (let* ((prev (node-prev node))
	 (new-filter (make-instance 'filter-node :instans (node-instans node) :prev prev :test test :test-parameters (collect-expression-variables test))))
    (setf (node-succ prev) (remove node (node-succ prev)))
    (push new-filter (node-succ prev))
    (setf (node-prev node) new-filter)
    (push node (node-succ new-filter))
    new-filter))

(defun find-filter-non-relational-expression-move-targets (filter-node)
  (let* ((flattened (flatten-outermost-ands (filter-test filter-node)))
	 (hits (loop for test-expr in (if (and (consp flattened) (eq (first flattened) (find-sparql-op "LOGICAL-AND"))) (rest flattened) (list flattened))
		     for vars = (collect-expression-variables test-expr)
		     for targets = (find-targets (node-prev filter-node) vars)
		     ;; do (inform "Checking ~S" test-expr)
		     collect (list filter-node test-expr targets))))
    (loop for (filter-node test-expr targets) in hits
	  ;; do (inform "For ~S~%found targets ~S~%" test-expr targets)
	  nconc (loop for target in targets collect (add-filter-before target test-expr)) into new-nodes
	  finally (return new-nodes))))

(defun find-ordered-index-join-nodes (node expr)
  ;; (let ((vars (collect-expression-variables expr)))
  ;;   (cond ((and (= 1 (node-succ node)) (list-subset vars (node-def-prec node)))
  ;; 	   (cond ((join-node-p node)
  ;; 		  (multiple-value-bind (splittablep relop beta-expr alpha-expr)
  ;; 		      (reorder-linear-inequality expr (node-all-vars-out (join-beta node)) (node-all-vars-out (join-alpha node)))
  ;; 		    (if splittablep
  ;; 			(list (list node relop beta-expr alpha-expr))
  ;; 			(union (find-ordered-index-join-node (join-beta node) expr) (find-ordered-index-join-node (join-alpha node) expr) :test #'equal))))
  ;; 		 (t
  ;; 		  (find-ordered-index-join-node (node-prev node) expr))))
  ;; 	  (t nil)))
  )

(defun optimize-filter (node)
  (inform "Checking filter node ~S" node)
  (let* ((test (filter-test node))
  	 (flattened (flatten-outermost-ands test)))
    (inform "Test is ~S~%" (pretty-sparql-expr test))
    (cond ((eq (first flattened) (find-sparql-op "LOGICAL-AND"))
  	   (inform "Can be split into:~{~%~A~}~%" (mapcar #'pretty-sparql-expr (rest flattened)))
	   (loop for expr in (rest flattened)
		 do (find-ordered-index-join-nodes (node-prev node) expr)))
  	  (t
  	   (inform "Cannot be split: ~A~%" (pretty-sparql-expr flattened)))))
  (find-filter-non-relational-expression-move-targets node))

(defun optimize-filters (instans)
  (loop for node in (instans-nodes instans)
	when (filter-node-p node)
	nconc (optimize-filter node) into new-nodes
        finally (when new-nodes
		  (compute-node-vars (instans-nodes instans))
		  (lisp-compile-nodes new-nodes))))

(defun optimize-rete-network (instans)
  (declare (ignorable instans))
  ;; (inform "Nodes ~S" (instans-nodes instans))
  (when (instans-optimize-filters-p instans)
    (optimize-filters instans))
  )



