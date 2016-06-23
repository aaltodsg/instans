;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun flatten-outermost-ands (expr)
  (cond ((consp expr)
	 (cond ((eq (first expr) (find-sparql-op "LOGICAL-AND"))
		(cons (find-sparql-op "LOGICAL-AND")
		      (loop for x in (mapcar #'flatten-outermost-ands (rest expr))
			 nconc (cond ((and (consp x) (eq (first x) (find-sparql-op "LOGICAL-AND"))) (copy-list (rest x)))
				     (t (list x))))))
	       (t expr)))
	(t expr)))

(defun negate-term (x)
  (if (and (consp x) (eq (first x) (find-sparql-op "NUMERIC-UNARY-MINUS"))) (second x) (list (find-sparql-op "NUMERIC-UNARY-MINUS") x)))

(defun flatten-sum (expr)
  (labels ((visit (expr)
	     (cond ((consp expr)
		    (cond ((eq (first expr) (find-sparql-op "+"))
			   (assert* (= 3 (length expr)) "Malformed + expr ~A" expr)
			   (append (visit (second expr)) (visit (third expr))))
			  ((eq (first expr) (find-sparql-op "-"))
			   (assert* (= 3 (length expr)) "Malformed - expr ~A" expr)
			   (append (visit (second expr)) (mapcar #'negate-term (visit (third expr)))))
			  ((eq (first expr) (find-sparql-op "NUMERIC-UNARY-MINUS"))
			   (assert* (= 2 (length expr)) "Malformed unary-minus expr ~A" expr)
			   (mapcar #'negate-term (visit (second expr))))
			  ((eq (first expr) (find-sparql-op "NUMERIC-UNARY-PLUS"))
			   (assert* (= 2 (length expr)) "Malformed unary-minus expr ~A" expr)
			   (visit (second expr)))
			  (t
			   (list expr))))
		   (t (list expr)))))
    (let ((terms (visit expr)))
      (cond ((= 1 (length terms)) (first terms))
	    (t (cons (find-sparql-op "+") terms))))))

(defun reorder-linear-inequality (expr beta-vars alpha-vars)
  (let ((relop (first expr)))
    (cond ((member relop (list (find-sparql-op "<") (find-sparql-op "<=") (find-sparql-op ">") (find-sparql-op ">=")))
	   (let* ((diff (create-sparql-call "-" (second expr) (third expr)))
		  (flattened (flatten-sum diff)))
	     (inform "  flattened ~A" (pretty-sparql-expr flattened))
	     (loop for term in (rest flattened)
		   for term-vars = (collect-expression-variables term)
		   do (inform "  testing term ~A with vars ~A" term term-vars)
		   do (inform "  beta-vars ~A" beta-vars)
		   do (inform "  alpha-vars ~A" alpha-vars)
		   do (inform "  (list-subset term-vars beta-vars) = ~A" (list-subset term-vars beta-vars))
		   do (inform "  (list-subset term-vars alpha-vars) = ~A" (list-subset term-vars alpha-vars))
		   when (list-subset term-vars beta-vars)
		   collect term into beta-expr-list
		   else when (list-subset term-vars alpha-vars)
		   collect term into alpha-expr-list
		   else do (return nil)
		   do (inform "  beta-expr-list ~A" (mapcar #'pretty-sparql-expr beta-expr-list))
		   do (inform "  alpha-expr-list ~A" (mapcar #'pretty-sparql-expr alpha-expr-list))
		   finally (return (and beta-expr-list alpha-expr-list
					(values relop
						(if (= 1 (length beta-expr-list))
						    (first beta-expr-list)
						    (cons (find-sparql-op "+") beta-expr-list))
						(if (= 1 (length alpha-expr-list))
						    (negate-term (first alpha-expr-list))
						    (cons (find-sparql-op "+") (mapcar #'negate-term alpha-expr-list)))))))))
	  (t
	   nil))))

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
  ;; (inform "add filter before ~A ~A" node test)
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
		     ;; do (inform "Checking ~A" test-expr)
		     collect (list filter-node test-expr targets))))
    (loop for (filter-node test-expr targets) in hits
	  ;; do (inform "For ~A~%found targets ~A~%" test-expr targets)
	  nconc (loop for target in targets collect (add-filter-before target test-expr)) into new-nodes
	  finally (return new-nodes))))

(defun find-ordered-index-join-nodes (node expr)
  (let ((vars (collect-expression-variables expr))
	(instans (node-instans node)))
    (inform "  node ~A, succ ~A, def-prec ~A, vars ~A~%" node (node-succ node) (pretty-sparql-vars (node-def-prec node) instans) (pretty-sparql-var vars instans))
    (cond ((and (= 1 (length (node-succ node))) (list-subset vars (node-def-prec node)))
  	   (cond ((join-node-p node)
  		  (multiple-value-bind (relop beta-expr alpha-expr)
  		      (reorder-linear-inequality expr (node-all-vars-out (join-beta node)) (node-all-vars-out (join-alpha node)))
  		    (if relop
  			(list (list node relop beta-expr alpha-expr))
  			(union (find-ordered-index-join-nodes (join-beta node) expr) (find-ordered-index-join-nodes (join-alpha node) expr) :test #'equal))))
  		 (t
  		  (find-ordered-index-join-nodes (node-prev node) expr))))
  	  (t nil)))
  )

(defun optimize-filter (node)
  (let* ((test (filter-test node))
  	 (flattened (flatten-outermost-ands test)))
    (inform "Checking filter node ~A~%with test ~A:~%" node (pretty-sparql-expr test (node-instans node)))
    (let ((targets (cond ((eq (first flattened) (find-sparql-op "LOGICAL-AND"))
			  (inform "can be split into:~{~%~A~}~%" (mapcar #'pretty-sparql-expr (rest flattened)))
			  (loop for expr in (rest flattened)
			        do (inform "  finding ordered index join node above ~A~%  for expr ~A" node (pretty-sparql-expr expr (node-instans node)))
				nconc (find-ordered-index-join-nodes (node-prev node) expr)))
			 (t
			  (inform "Cannot be split: ~A~%" (pretty-sparql-expr flattened))
			  (find-ordered-index-join-nodes (node-prev node) flattened)))))
      (when targets
	(inform "Ordered indices for nodes:")
	(loop for (node relop beta-expr alpha-expr) in targets
	      do (inform "~%--ordered-index=~A:~A:~A:~A~%"
			 (node-name node)
			 (if (sparql-var-p beta-expr) (sparql-var-name (reverse-resolve-binding (node-instans node) beta-expr)) beta-expr)
			 (sparql-op-name relop)
			 (if (sparql-var-p alpha-expr) (sparql-var-name (reverse-resolve-binding (node-instans node) alpha-expr)) alpha-expr))))))
  (find-filter-non-relational-expression-move-targets node)
  (inform "END checking filter node ~A~%" node))

(defun optimize-filters (instans)
  (loop for node in (instans-nodes instans)
	when (filter-node-p node)
	nconc (optimize-filter node) into new-nodes
        finally (when new-nodes
		  (compute-node-vars (instans-nodes instans))
		  (lisp-compile-nodes new-nodes))))

(defun optimize-rete-network (instans)
  (declare (ignorable instans))
  ;; (inform "Nodes ~A" (instans-nodes instans))
  (when (instans-optimize-filters-p instans)
    (optimize-filters instans))
  )

;;; Testing

(defun test-rli ()
  (let ((instans (create-instans)))
    (flet ((cpc (op &rest args) (apply #'create-sparql-call op args))
	   (mv (n) (make-sparql-var instans n)))
      (let* ((bv1 (mv "b1"))
	     (bv2 (mv "b2"))
	     (av1 (mv "a1"))
	     (av2 (mv "a2"))
	     (av3 (mv "a3"))
	     (bvl (list bv1 bv2))
	     (avl (list av1 av2 av3))
	     (expr (cpc ">" (cpc "-" av1 av2) (cpc "+" bv1 (cpc "numeric-unary-minus" bv2)))))
	(inform "expr = ~A~%" expr)
	(multiple-value-bind (op bel ael)
	    (reorder-linear-inequality expr bvl avl)
	  (cond ((null op)
		 (inform "Not a linear inequality"))
		(t
		 (inform "op: ~A~%beta:  ~A~%alpha: ~A~%" op bel ael))))))))
