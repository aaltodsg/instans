;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Recursively builds a RETE network from the given expr.
;;; Returns the bottom-most node(s) of the created network.
;;; Adds newly created nodes to new-nodes and to (network-nodes network).
;;; Some variables are created during the translation. This is a bit cludge, since we both create the variables and then immediately
;;; replace them with canonic variables.
;;; !NOTE! Level is updated never. What is its purpose?
(defun translate-sparql-algebra-to-rete (sae network)
  (let ((new-nodes nil)
					;	(level 0)
	(bindings (network-bindings network)))
    (labels ((replace-exists-by-vars (e)
	       (let ((exists-list nil)
		     (counter-var-list nil))
		 (labels ((walk (x)
			    (cond ((consp x)
				   (cond ((member (car x) '(EXISTS NOT-EXISTS))
					  (push-to-end x exists-list)
					  (let ((v (new-var "_C" bindings)))
					    (push-to-end v counter-var-list)
					    (if (eq (car x) 'EXISTS) (create-sparql-call ">" v 0) (create-sparql-call "<=" v 0))))
					 (t
					  (cons (car x) (mapcar #'walk (cdr x))))))
				  (t x))))
		   (let ((new-expr (walk e)))
		     (values new-expr exists-list counter-var-list)))))
	     (generate-and-canonize-var (prefix) (canonize-sparql-var (generate-sparql-var prefix) bindings))
	     (make-or-share-instance (type &rest args)
               ;;; Finds a node with equal parameters, which is either a child of 'prev' argument in args (if non-null) or is an existing node.
               ;;; If not found, creates a new node, adds it to new-nodes and links it to node-succ of 'prev' (if non-null).
               ;;; Sharing of exists, optional, and union structures is not allowed.
	       (flet ((node-matches-constructor-args-p (n type args)
			(and (eq (type-of n) type)
			     (loop for rest on args by #'cddr for key = (first rest) for value = (second rest)
				   unless (or (member key '(:bindings :algebra-expr))
					      (equal (slot-value n (intern (string key))) value)) do (return nil)
				   finally (return t)))))
		 (let ((prev (or (getf args :prev) (getf args :beta))))
		   (let ((result (and (not (member type '(exists-start-node exists-end-node optional-start-node optional-end-node union-start-node union-end-node)))
					; Check this			      (or (zerop level) (member type '(triple-pattern-node datablock-node alpha-memory)))
				      (find-if #'(lambda (n) (node-matches-constructor-args-p n type args)) (if prev (node-succ prev) (network-nodes network))))))
		     (when (null result)
		       (setf result (apply #'make-instance type :network network args))
		       (push-to-end result new-nodes)
		       (when prev (push-to-end-new result (node-succ prev))))
		     result))))
	     (translate (expr prev dataset)
	       (let ((op (first expr))
		     (args (rest expr)))
		 (case op
		   (ZERO-PATTERN
		    (assert (null prev))
		    (make-or-share-instance 'beta-memory :prev prev))
		   (BGP (loop for triple-pattern in args
			      do (progn
				   (assert* (not (member 'PATH triple-pattern)) "Cannot handle paths yet ~S" triple-pattern)
				   (let* ((beta-memory (cond ((typep prev 'beta-memory) prev)
							     (t (make-or-share-instance 'beta-memory :prev prev))))
					  (triple-pattern-node (make-or-share-instance 'triple-pattern-node :triple-pattern triple-pattern :dataset dataset))
					  (alpha-memory (make-or-share-instance 'alpha-memory :prev triple-pattern-node)))
				     (setf prev (make-or-share-instance 'join-node :beta beta-memory :alpha alpha-memory)))))
			prev)
		   (EXTEND (setf prev (translate (first args) prev dataset))
			   (let ((var (second args))
				 (form (third args)))
			     (cond ((eq (first form) 'agg)
				    (assert* (typep prev 'aggregate-join-node) "AGG not inside an aggregate-join-node: ~S inside ~S" form prev)
				    (let ((var-aggr-list (aggregate-join-var-aggr-list prev)))
				      (setf (first (nth (- (second form) 1) var-aggr-list)) var)))
				   (t
				    (setf prev (make-or-share-instance 'bind-node :prev prev :variable var
								       :form form :form-parameters (collect-expression-variables form))))))
			   prev)
		   (FILTER (setf prev (translate (second args) prev dataset))
			   (let ((f (first args)))
			     (cond ((eq f T)
				    prev)
				   ((eq (first f) 'EXISTS)
				    (let* ((counter-var (generate-and-canonize-var "!COUNTER"))
					   (active-p-var (generate-and-canonize-var "!ACTIVEP"))
					   (exists-start (make-or-share-instance 'exists-start-node :prev prev :counter-var counter-var :active-p-var active-p-var
										 :kind :simple-exists))
					   (exists-end (make-or-share-instance 'exists-end-node :prev (translate (second f) exists-start dataset)
									       :start-node exists-start :kind :simple-exists)))
				      (setf (subgraph-end-node exists-start) exists-end)
				      exists-end))
				   ((eq (first f) 'NOT-EXISTS)
				    (let* ((counter-var (generate-and-canonize-var "!COUNTER"))
					   (active-p-var (generate-and-canonize-var "!ACTIVEP"))
					   (exists-start (make-or-share-instance 'exists-start-node :prev prev :counter-var counter-var :active-p-var active-p-var
										 :kind :simple-not-exists))
					   (exists-end (make-or-share-instance 'exists-end-node :prev (translate (second f) exists-start dataset)
									       :start-node exists-start :kind :simple-not-exists)))
				      (setf (subgraph-end-node exists-start) exists-end)
				      exists-end))
				   (t
				    (multiple-value-bind (modified-expr exists-list counter-var-list)
					(replace-exists-by-vars f)
				      (cond ((null exists-list)
					     (make-or-share-instance 'filter-node :prev prev :test f :test-parameters (collect-expression-variables f)))
					    (t
					     (setf expr modified-expr)
					     (loop for exist in exists-list
						   for counter-var in counter-var-list
						   for active-p-var = (generate-and-canonize-var "!ACTIVEP")
						   for exists-start = (make-or-share-instance 'exists-start-node :prev prev
											      :counter-var counter-var :active-p-var active-p-var :kind :embedded-exists)
						   for exists-end = (make-or-share-instance 'exists-end-node :prev (translate (second exist) exists-start dataset)
											    :start-node exists-start :kind :embedded-exists)
						   do (setf (subgraph-end-node exists-start) exists-end)
						   do (setf prev exists-end)
						   finally (return (make-or-share-instance 'filter-memory :prev prev :prev-value-var (generate-and-canonize-var "!PREVVAL")
											   :test modified-expr
											   :test-parameters (collect-expression-variables modified-expr)))))))))))
		   (JOIN (translate (second args) (translate (first args) prev dataset) dataset))
		   (LEFTJOIN (setf prev (translate (first args) prev dataset))
			     (let* ((counter-var (generate-and-canonize-var "!COUNTER"))
				    (active-p-var (generate-and-canonize-var "!ACTIVEP"))
				    (optional-expr `(FILTER ,(third args) ,(second args)))
				    (optional-start (make-or-share-instance 'optional-start-node :prev prev :counter-var counter-var :active-p-var active-p-var))
				    (optional-end (make-or-share-instance 'optional-end-node
									  :start-node optional-start
									  :prev (translate optional-expr optional-start dataset))))
			       (setf (subgraph-end-node optional-start) optional-end)
			       optional-end))
		   (MINUS (let ((e1 (first args))
				(e2 (second args)))
			    (cond ((or (null (list-intersection (collect-expression-variables e1) (collect-expression-variables e2)))
				       (not (pattern-variables-consistent-p e1 e2)))
				   (translate e1 prev dataset))
				  (t
				   (translate `(FILTER ,e1 ,(create-sparql-call "!" `(EXISTS ,e2))) prev dataset)))))
		   (UNION (let* ((start (if (and prev (typep prev 'beta-memory) (null (node-prev prev))) prev (make-or-share-instance 'union-start-node :prev prev)))
				 (arg1-end (translate (first args) start dataset))
				 (arg2-end (translate (second args) start dataset)))
			    (make-or-share-instance 'union-end-node :prev1 arg1-end :prev2 arg2-end)))
		   (AGGREGATEJOIN (let* ((aggregations (first args))
					 (aggregation-specs (mapcar #'(lambda (a) (list (generate-and-canonize-var "!AGG") (second a) (third a) (fourth a))) aggregations))
					 (agg1 (first aggregations))
					 (group (fifth agg1))
					 (group-expr (second group))
					 (pattern (third group)))
				    (make-or-share-instance 'aggregate-join-node :prev (translate pattern prev dataset)
							    :group group-expr :group-form group-expr :var-aggr-list aggregation-specs)))
		   (TO-LIST (translate (first args) prev dataset))
		   (ORDER-BY (setf prev (translate (first args) prev dataset))
			     (unless (typep prev 'solution-modifiers-node)
			       (setf prev (make-or-share-instance 'solution-modifiers-node :prev prev :order-by nil :project-vars nil :distinctp nil :start nil :length nil)))
			     (setf (solution-modifiers-order-by prev) (second args))
			     prev)
		   (PROJECT (setf prev (translate (first args) prev dataset))
			    (unless (typep prev 'solution-modifiers-node)
			      (setf prev (make-or-share-instance 'solution-modifiers-node :prev prev :order-by nil :project-vars nil :distinctp nil :start nil :length nil)))
			    (setf (solution-modifiers-project-vars prev) (second args))
			    prev)
		   ((DISTINCT REDUCED) (setf prev (translate (first args) prev dataset))
		    (unless (typep prev 'solution-modifiers-node)
		      (setf prev (make-or-share-instance 'solution-modifiers-node :prev prev :order-by nil :project-vars nil :distinctp nil :start nil :length nil)))
		    (setf (solution-modifiers-distinct-p prev) t)
		    prev)
		   (SLICE (setf prev (translate (first args) prev dataset))
			  (unless (typep prev 'solution-modifiers-node)
			    (setf prev (make-or-share-instance `solution-modifiers-node :prev prev :order-by nil :project-vars nil :distinctp nil :start nil :length nil)))
			  (setf (solution-modifiers-start prev) (second args))
			  (setf (solution-modifiers-length prev) (third args))
			  prev)
		   (TO-MULTI-SET (translate (first args) prev dataset))
		   (GRAPH (translate (second args) prev (first args)))
		   (SELECT (let ((where-clause (getf args :where)))
			     (make-or-share-instance 'select-node :prev (translate where-clause prev dataset))))
		   (CONSTRUCT (let ((where-clause (getf args :where))
				    (construct-clause (getf args :construct-template)))
				(make-or-share-instance 'construct-node :prev (translate where-clause prev dataset) :construct-template construct-clause)))
		   (DELETE-INSERT (let ((where-clause (getf args :where))
					(delete-clause (getf args :deleteclause))
					(insert-clause (getf args :insertclause)))
				    (make-or-share-instance 'modify-node :prev (translate where-clause prev dataset) 
							    :delete-template (let ((netvar (gensym "NET")))
									       (cons netvar (translate-template netvar delete-clause 'rete-remove)))
							    :delete-parameters (collect-expression-variables delete-clause)
							    :insert-template (let ((netvar (gensym "NET")))
									       (cons netvar (translate-template netvar insert-clause 'rete-add)))
							    :insert-parameters (collect-expression-variables insert-clause))))
		   ;; ((|INSERT DATA| |DELETE DATA| LOAD CLEAR)
		   ;;  (assert* nil "Don't know how to translate ~S" expr))
		   ;; (SERVICE
		   ;;  (assert* nil "SERVICE not implemented properly yet ~S" expr)
		   ;;  nil)
		   (INLINEDATA
		    (let* ((beta-memory (cond ((typep prev 'beta-memory) prev)
					      (t (make-or-share-instance 'beta-memory :prev prev))))
			   (datablock-node (make-or-share-instance 'datablock-node :variables (first args) :values (second args)))
			   (alpha-memory (make-or-share-instance 'alpha-memory :prev datablock-node)))
		      (setf prev (make-or-share-instance 'join-node
							 :beta beta-memory :alpha alpha-memory))
		      prev))
		   (t
		    (assert* nil "Cannot translate ~S" expr)
		    nil)))))
      (translate sae nil nil)
      new-nodes)))

(defun translate-template (netvar template op)
  (multiple-value-bind (graph triple-patterns)
      (cond ((eq (car template) 'GRAPH)
	     (values `',(second template) (second (third template))))
	    ((eq (car template) 'BGP)
	     (values nil (second template))))
    (loop for triple-pattern in triple-patterns
	  collect `(,op ,netvar ,@(loop for term in triple-pattern collect (cond ((atom term) term)
										 ((sparql-var-p term) (second term))
										 (t `',term))) ,graph))))
