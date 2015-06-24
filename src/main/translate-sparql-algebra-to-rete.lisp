;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Recursively builds a RETE network from the given expr.
;;; Returns the bottom-most node(s) of the created network.
;;; Adds newly created nodes to new-nodes and to (instans-nodes instans).
;;; Some variables are created during the translation. This is a bit cludge, since we both create the variables and then immediately
;;; replace them with canonic variables.
;;; !NOTE! Level is updated never. What is its purpose?
(defun translate-sparql-algebra-to-rete (instans sae)
;  (inform "translate-sparql-algebra-to-rete ~S" sae)
  (let ((new-nodes nil)
	;; (level 0)
	(color (instans-next-color instans)))
    (labels ((translate-not-implemented-yet (fmt &rest args)
	       (let ((msg (apply #'format nil fmt args)))
;		 (inform "(translate-not-implemented-yet ~S ~S) -> ~S" fmt args msg)
		 (instans-add-status instans 'instans-feature-not-implemented-yet)
		 (instans-add-status instans 'instans-rule-translation-failed (list msg))
		 (return-from translate-sparql-algebra-to-rete nil)))
	     (replace-exists-by-vars (e)
	       (let ((exists-list nil)
		     (counter-var-list nil))
		 (labels ((walk (x)
			    (cond ((consp x)
				   (cond ((member (car x) '(EXISTS NOT-EXISTS))
					  (push-to-end x exists-list)
					  (let ((v (generate-and-canonize-var "!COUNTER")))
					    (push-to-end v counter-var-list)
					    (if (eq (car x) 'EXISTS) (create-sparql-call ">" v 0) (create-sparql-call "<=" v 0))))
					 (t
					  (cons (car x) (mapcar #'walk (cdr x))))))
				  (t x))))
		   (let ((new-expr (walk e)))
		     (values new-expr exists-list counter-var-list)))))
	     (generate-and-canonize-var (prefix) (canonize-sparql-var instans (generate-sparql-var instans prefix)))
	     (equal-value (v1 v2)
	       (let ((r 
	       (or (equal v1 v2)
		   (cond ((consp v1)
			  (and (consp v2) (equal-value (car v1) (car v2)) (equal-value (cdr v1) (cdr v2))))
			 ((uniquely-named-object-p v1)
			  (and (uniquely-named-object-p v2) (uniquely-named-object-equal v1 v2)))
			 ((or (rdf-term-p v1) (typep v1 'xsd-value))
			  (and (or (rdf-term-p v2) (typep v2 'xsd-value)) (sparql-call "sameTerm" v1 v2))))))
		     )
;		 (inform "equal-value ~S ~S -> ~S" v1 v2 r)
		 r))
	     (make-or-share-instance (type &rest args)
               ;;; Finds a node with equal parameters, which is either a child of 'prev' argument in args (if non-null) or is an existing node.
               ;;; If not found, creates a new node, adds it to new-nodes and links it to node-succ of 'prev' (if non-null).
               ;;; Sharing of exists, optional, and union structures is not allowed.
	       (flet ((node-matches-constructor-args-p (n type args)
			(and (equal (type-of n) type)
			     (loop for rest on args by #'cddr for key = (first rest) for value = (second rest)
				   unless (or (member key '(:bindings :algebra-expr))
					      (equal-value (slot-value n (intern-instans (string key))) value))
				   do (return nil)
				   finally (return t)))))
		 (let ((prev (or (getf args :prev) (getf args :beta))))
		   (let ((result (and (not (member type '(exists-start-node exists-end-node optional-start-node optional-end-node union-start-node union-end-node)))
					; Check this			      (or (zerop level) (member type '(triple-pattern-node datablock-node alpha-memory)))
				      (find-if #'(lambda (n) (node-matches-constructor-args-p n type args)) (if prev (node-succ prev) (instans-nodes instans))))))
;		     (inform "make-or-share-instance ~A ~A" type args)
		     (when (null result)
;		       (inform "creating new" type args)
		       (setf result (apply #'make-instance type :instans instans args))
		       (push-to-end (cons result color) (instans-node-color-alist instans))
		       (push-to-end result new-nodes)
		       (when prev (push-to-end-new result (node-succ prev))))
		     result))))
	     (translate (expr prev dataset)
;	       (inform "translate ~S, prev = ~A" expr prev)
	       (let ((op (first expr))
		     (args (rest expr)))
		 (case op
		   (ZERO-PATTERN
		    (or prev (make-or-share-instance 'beta-memory :prev nil)))
		   (GGP (translate (getf (rest expr) :form) prev dataset))
		   (BGP (loop for triple-pattern in args
			      do (progn
				   (when (member 'PATH triple-pattern)
				     (translate-not-implemented-yet "Paths not fully implemented yet ~S" triple-pattern))
				   (let* ((beta-memory (cond ((typep prev 'beta-memory) prev)
							     (t (make-or-share-instance 'beta-memory :prev prev))))
					  (triple-pattern-node (make-or-share-instance 'triple-pattern-node :triple-pattern triple-pattern :dataset dataset))
					  (alpha-memory (make-or-share-instance 'alpha-memory :prev triple-pattern-node)))
				     (setf prev (make-or-share-instance 'join-node :beta beta-memory :alpha alpha-memory))
				     (push-to-end-new prev (node-succ alpha-memory)))))
			prev)
		   (EXTEND (setf prev (translate (first args) prev dataset))
			   (let ((var (second args))
				 (form (third args)))
			     ;; (cond ((and (consp form) (eq (first form) 'agg))
			     ;; 	    (assert* (typep prev 'aggregate-join-node) "AGG not inside an aggregate-join-node: ~S inside ~S" form prev)
			     ;; 	    (let ((var-aggr-list (aggregate-join-var-aggr-list prev)))
			     ;; 	      (setf (first (nth (- (second form) 1) var-aggr-list)) var)))
			     ;; 	   (t
;			     (inform "translate-satr: bind-form ~A = ~A" expr form)
			     (setf prev (make-or-share-instance 'bind-node :prev prev :variable var :form form :form-parameters (collect-expression-variables form))))
			   prev)
		   (FILTER (setf prev (translate (second args) prev dataset))
			   (let ((f (first args)))
			     (cond ((eq f T)
				    prev)
				   ((and (consp f) (eq (first f) 'EXISTS))
				    (let* ((counter-var (generate-and-canonize-var "!COUNTER"))
					   (active-p-var (generate-and-canonize-var "!ACTIVEP"))
					   (exists-start (make-or-share-instance 'exists-start-node :prev prev :counter-var counter-var :active-p-var active-p-var
										 :kind :simple-exists))
					   (exists-end (make-or-share-instance 'exists-end-node :prev (translate (second f) exists-start dataset)
									       :kill (list counter-var active-p-var)
									       :start-node exists-start :kind :simple-exists)))
				      (setf (subgraph-end-node exists-start) exists-end)
				      exists-end))
				   ((and (consp f) (eq (first f) 'NOT-EXISTS))
				    (let* ((counter-var (generate-and-canonize-var "!COUNTER"))
					   (active-p-var (generate-and-canonize-var "!ACTIVEP"))
					   (exists-start (make-or-share-instance 'exists-start-node :prev prev :counter-var counter-var :active-p-var active-p-var
										 :kind :simple-not-exists))
					   (exists-end (make-or-share-instance 'exists-end-node :prev (translate (second f) exists-start dataset)
									       :kill (list counter-var active-p-var)
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
					     (loop with kill-vars = nil
						   for exist in exists-list
						   for counter-var in counter-var-list
						   for active-p-var = (generate-and-canonize-var "!ACTIVEP")
						   for exists-start = (make-or-share-instance 'exists-start-node :prev prev
											      :counter-var counter-var :active-p-var active-p-var :kind :embedded-exists)
						   for exists-end = (make-or-share-instance 'exists-end-node :prev (translate (second exist) exists-start dataset)
											    :start-node exists-start :kind :embedded-exists)
						   do (progn
							(push counter-var kill-vars)
							(push active-p-var kill-vars)
							(setf (subgraph-end-node exists-start) exists-end)
							(setf prev exists-end))
						   finally (return (make-or-share-instance 'filter-memory :prev prev :prev-value-var (generate-and-canonize-var "!PREVVAL")
											   :kill (nreverse kill-vars)
											   :test modified-expr
											   :test-parameters (collect-expression-variables modified-expr)))))))))))
		   (JOIN (cond ((eq 'UNION (car (second args)))
				(let* ((beta (translate (first args) prev dataset))
				       (beta-memory (cond ((typep beta 'beta-memory) beta)
							  (t (make-or-share-instance 'beta-memory :prev beta))))
				       (union-args (rest (second args)))
				       (arg1-end (translate (first union-args) beta-memory dataset))
				       (arg2-end (translate (second union-args) beta-memory dataset))
				       (end (make-or-share-instance 'union-end-node :prev1 arg1-end :prev2 arg2-end :start-node nil))
				       (alpha-memory (make-or-share-instance 'alpha-memory :prev end)))
				  (push-to-end-new end (node-succ arg1-end))
				  (push-to-end-new end (node-succ arg2-end))
				  (setf prev (make-or-share-instance 'join-node :beta beta-memory :alpha alpha-memory))
				  (push-to-end-new prev (node-succ alpha-memory))))
			       (t
				(let* ((beta (translate (first args) prev dataset))
				       (beta-memory (cond ((typep beta 'beta-memory) beta)
							  (t (make-or-share-instance 'beta-memory :prev beta))))
				       (alpha (translate (second args) nil dataset))
				       (alpha-memory (make-or-share-instance 'alpha-memory :prev alpha)))
				  (setf prev (make-or-share-instance 'join-node :beta beta-memory :alpha alpha-memory))
				  (push-to-end-new prev (node-succ alpha-memory)))))
			 prev)
		   (LEFTJOIN (setf prev (translate (first args) prev dataset))
			     (let* ((counter-var (generate-and-canonize-var "!COUNTER"))
				    (active-p-var (generate-and-canonize-var "!ACTIVEP"))
				    (optional-expr `(FILTER ,(third args) ,(second args)))
				    (optional-start (make-or-share-instance 'optional-start-node :prev prev :counter-var counter-var :active-p-var active-p-var))
				    (optional-end (make-or-share-instance 'optional-end-node
									  :start-node optional-start
									  :kill (list counter-var active-p-var)
									  :prev (translate optional-expr optional-start dataset))))
			       (setf (subgraph-end-node optional-start) optional-end)
			       optional-end))
		   (MINUS (let ((e1 (first args))
				(e2 (second args)))
			    (cond ((null (sparql-var-list-intersection (collect-expression-variables e1) (collect-expression-variables e2)))
				   (translate e1 prev dataset))
				  (t
				   (let* ((positive-tr (translate e1 prev dataset))
					  (positive (if (typep positive-tr 'beta-memory) positive-tr (make-or-share-instance 'beta-memory :prev positive-tr)))
					  (negative-tr (translate e2 nil dataset))
					  (negative (if (typep negative-tr 'alpha-memory) negative-tr (make-or-share-instance 'alpha-memory :prev negative-tr))))
				     (setf prev (make-or-share-instance 'minus-node :beta positive :alpha negative))
				     (push-to-end-new prev (node-succ positive))
				     (push-to-end-new prev (node-succ negative))
				     ;; (inform "MINUS")
				     ;; (describe positive)
				     ;; (describe negative)
				     ;; (describe prev)
				     )))
			    prev))
		   (UNION (let* ((start (make-or-share-instance 'union-start-node :prev (or prev (make-or-share-instance 'beta-memory :prev nil))))
				 (arg1-end (translate (first args) start dataset))
				 (arg2-end (translate (second args) start dataset))
				 (end (make-or-share-instance 'union-end-node :prev1 arg1-end :prev2 arg2-end :start-node start)))
			    (push-to-end-new end (node-succ arg1-end))
			    (push-to-end-new end (node-succ arg2-end))
			    (setf (subgraph-end-node start) end)))
		   (AGGREGATE-JOIN (make-or-share-instance 'aggregate-join-node :prev (translate (first args) prev dataset)
							   :aggr-exprs (second args)
							   :group (third args)))
		   (TO-LIST (translate (first args) prev dataset))
		   (TO-MULTI-SET (translate (first args) prev dataset))
		   (GRAPH (translate (second args) prev (first args)))
		   (SELECT (let* ((comment (getf args :comment))
				  (ggp (getf args :where)))
			     (remf args :where)
			     (apply #'make-or-share-instance 'select-node :comment comment :prev (translate ggp prev dataset) args)))
		   (ASK (let* ((comment (getf args :comment))
			       (ggp (getf args :where)))
			  (remf args :where)
			  (apply #'make-or-share-instance 'ask-node :comment comment :prev (translate ggp prev dataset) args)))
		   (DESCRIBE (let* ((comment (getf args :comment))
				    (ggp (getf args :where)))
			       (remf args :where)
			       (apply #'make-or-share-instance 'describe-node :comment comment :prev (if ggp (translate ggp prev dataset) (make-or-share-instance 'beta-memory :prev nil)) args)))
		   (CONSTRUCT (let* ((comment (getf args :comment))
				     (where-clause (getf args :where))
				     (construct-clause (getf args :construct-template)))
				(make-or-share-instance 'construct-node :prev (translate where-clause prev dataset)
							:comment comment
							:construct-template construct-clause
							:construct-parameters (collect-expression-variables construct-clause))))
		   (DELETE-INSERT ;(inform "translating ~S" expr)
				  (let* ((comment (getf args  :comment))
					 (where-clause (getf args :where))
					 (delete-clause (getf args :delete-clause))
					 (insert-clause (getf args :insert-clause)))
				    (make-or-share-instance 'modify-node :prev (translate where-clause prev dataset) 
							    :comment comment
							    :delete-template delete-clause
							    :delete-parameters (collect-expression-variables delete-clause)
							    :insert-template insert-clause
							    :insert-parameters (collect-expression-variables insert-clause))))
		   (INLINEDATA
		    (let* ((beta-memory (cond ((typep prev 'beta-memory) prev)
					      (t (make-or-share-instance 'beta-memory :prev prev))))
			   (datablock-node (make-or-share-instance 'datablock-node :variables (first args) :values (second args)))
			   (alpha-memory (make-or-share-instance 'alpha-memory :prev datablock-node)))
		      (setf prev (make-or-share-instance 'join-node
							 :beta beta-memory :alpha alpha-memory))
		      prev))
		   (LOAD
		    (translate-not-implemented-yet "~A not implemented yet (in ~S)" op expr))
		   (CLEAR
		    (translate-not-implemented-yet "~A not implemented yet (in ~S)" op expr))
		   (DROP
		    (translate-not-implemented-yet "~A not implemented yet (in ~S)" op expr))
		   (CREATE
		    (translate-not-implemented-yet "~A not implemented yet (in ~S)" op expr))
		   (ADD
		    (translate-not-implemented-yet "~A not implemented yet (in ~S)" op expr))
		   (MOVE
		    (translate-not-implemented-yet "~A not implemented yet (in ~S)" op expr))
		   (COPY
		    (translate-not-implemented-yet "~A not implemented yet (in ~S)" op expr))
		   (SERVICE
		    (setf prev (translate (first args) prev dataset))
		    (make-or-share-instance 'service-node :prev prev
					    :silentp (not (null (second args)))
					    :endpoint (third args)
					    :query-vars (collect-expression-variables (fourth args))
					    :query-token-strings (fifth args)))
		   ;; (SERVICE
		   ;;  (setf prev (translate (first args) prev dataset))
		   ;;  (let ((token-strings (loop for ts in (fifth args)
		   ;; 			       do (inform "ts = ~S" ts)
		   ;; 			       when (and (> (length ts) 0) (member (char ts 0) '(#\? #\$) :test #'char=))
		   ;; 			       collect (uniquely-named-object-name (cdr (find-if #'(lambda (binding) (equal (uniquely-named-object-name (first binding)) ts)) (instans-bindings instans))))
		   ;; 			       else collect ts)))
		   ;;    (make-or-share-instance 'service-node :prev prev
		   ;; 			      :silentp (not (null (second args)))
		   ;; 			      :endpoint (third args)
		   ;; 			      :query-vars (collect-expression-variables (fourth args))
		   ;; 			      :query-string (format nil "SELECT * { ~{~A~^ ~} }" token-strings))))
		   ((INSERT-DATA DELETE-DATA)
		    (error* "INSERT-DATA and DELETE-DATA should be handled as DELETE-INSERT: ~A" expr))
		   (t
		    (error* "Cannot translate ~S within ~S" expr sae)
		    nil)))))
      (translate sae nil nil)
      (compute-node-vars new-nodes)
      (instans-add-status instans 'instans-rule-translation-succeeded)
      new-nodes)))

(defun collect-blanks (expr &optional blanks)
  (cond ((consp expr)
	 (collect-blanks (cdr expr) (collect-blanks (car expr) blanks)))
	((rdf-blank-node-p expr)
	 (if (member expr blanks :test #'uniquely-named-object-equal) blanks (cons expr blanks)))
	(t blanks)))

(defun translate-triples-op-template (instans template op instans-var allow-blanks-p)
  (let* ((blanks (collect-blanks template))
	 (blank-var-alist (loop for blank in blanks collect (cons blank (gensym (string (uniquely-named-object-name blank))))))
	 (iri-vars nil)
	 (literal-vars nil)
	 (triple-op-forms nil))
;    (inform "blanks = ~S~%" blanks)
    (loop for item in template
;       do (inform "item = ~S" item)
       do (multiple-value-bind (graph triple-patterns)
	      (cond ((eq (car item) 'GRAPH)
		     (values (second item) (rest (third (third item)))))
		    ((eq (car item) 'BGP)
		     (values nil (rest item)))
		    (t (error* "Malformed template pattern ~S" item)))
	    (loop for triple-pattern in triple-patterns
	       for pattern-vars = (collect-expression-variables triple-pattern)
;	       do (inform "Triple-pattern = ~S" triple-pattern)
	       do (push-to-end `(unless (or ,@(mapcar #'(lambda (var) `(sparql-unbound-p ,(sparql-var-lisp-name var))) pattern-vars))
				  (,op ,instans-var
				       ,@(loop for term in triple-pattern
;					       do (inform "translate-triples-op-template, term = ~A" term)
					       collect (cond ((sparql-var-p term) (sparql-var-lisp-name term))
							     ((rdf-iri-p term) ;(inform "~A iri" term)
							      (let ((var (get-constant-iri instans term))) (push-to-end-new var iri-vars) var))
							     ((rdf-literal-p term) (let ((var (get-constant-literal instans term))) (push-to-end-new var literal-vars) var))
							     ((rdf-blank-node-p term) (cdr (assoc term blank-var-alist)))
							     ((datetimep term) `(create-datetime ,(datetime-canonic-string term)))
							     (t term)))
				       ,(cond ((rdf-iri-p graph)
					       (setf graph (get-constant-iri instans graph))
					       (setf iri-vars (cons graph iri-vars))
					       graph)
					      ((sparql-var-p graph) (sparql-var-lisp-name graph))
					      (t graph))))
			       triple-op-forms))))
    (let ((specials (append iri-vars literal-vars)))
      (cond ((and blanks (not allow-blanks-p))
	     (signal-sparql-error "Blank nodes not allowed in ~S" template))
	    (t
	     `(,@(if specials `((declare (special ,@specials))))
		 ,@(cond ((null blanks) triple-op-forms)
			 (t `((let (,@(loop for (blank . var) in blank-var-alist collect `(,var (generate-anonymous-blank-node ,instans-var))))
				,@triple-op-forms))))))))))


