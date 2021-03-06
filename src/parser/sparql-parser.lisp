;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;; 18.2.4 Converting Groups, Aggregates, HAVING, final VALUES clause and SELECT Expressions
;;
;; In this step, we process clauses on the query level in the following order:
;;
;;     Grouping
;;     Aggregates
;;     HAVING
;;     VALUES
;;     Select expressions
;;
;; 18.2.4.1 Grouping and Aggregation
;;
;; Step: GROUP BY
;;
;; If the GROUP BY keyword is used, or there is implicit grouping due to the use of aggregates in the projection, then grouping is
;; performed by the Group function. It divides the solution set into groups of one or more solutions, with the same overall
;; cardinality. In case of implicit grouping, a fixed constant (1) is used to group all solutions into a single group.
;;
;; Step: Aggregates
;;
;; The aggregation step is applied as a transformation on the query level, replacing aggregate expressions in the query level with
;; Aggregation() algebraic expressions.
;;
;; The transformation for query levels that use any aggregates is given below:
;;
;; Let A := the empty sequence
;; Let Q := the query level being evaluated
;; Let P := the algebra translation of the GroupGraphPattern of the query level
;; Let E := [], a list of pairs of the form (variable, expression)
;;
;; If Q contains GROUP BY exprlist
;;    Let G := Group(exprlist, P)
;; Else If Q contains an aggregate in SELECT, HAVING, ORDER BY
;;    Let G := Group((1), P)
;; Else
;;    skip the rest of the aggregate step
;;    End
;;
;; Global i := 1   # Initially 1 for each query processed
;;
;; For each (X AS Var) in SELECT, each HAVING(X), and each ORDER BY X in Q
;;   For each unaggregated variable V in X
;;       Replace V with Sample(V)
;;       End
;;   For each aggregate R(args ; scalarvals) now in X
;;       # note scalarvals may be omitted, then it's equivalent to the empty set
;;       Ai := Aggregation(args, R, scalarvals, G)
;;       Replace R(...) with aggi in Q
;;       i := i + 1
;;       End
;;   End
;;
;; For each variable V appearing outside of an aggregate
;;    Ai := Aggregation(V, Sample, {}, G)
;;    E := E append (V, aggi)
;;    i := i + 1
;;    End
;;
;; A := Ai, ..., Ai-1
;; P := AggregateJoin(A)
;;
;; Note: aggi is a temporary variable. E is then used in 18.2.4.4 for the processing of select expressions.

(defun as-form-p (x)
  (and (consp x) (eq (car x) 'AS)))

(defun aggregate-function-name-p (name)
  (member name '(COUNT SUM MIN MAX AVG GROUP_CONCAT SAMPLE)))

(defun translate-algebra (instans clauses)
  (flet ((pop-property (indicator) (prog1 (getf clauses indicator) (remf clauses indicator))))
    (let* ((ggp (getf clauses :where))
	   (scope-vars (getf (rest ggp) (if (eq (car ggp) 'SELECT) :project-vars :scope-vars)))
	   (project (pop-property :project))
	   (project-vars nil)
	   (having (pop-property :having))
	   (order-by (pop-property :order-by))
	   (group-by (pop-property :group-by))
	   (limit (pop-property :limit))
	   (offset (pop-property :offset))
	   (values (pop-property :values))
	   (query-form (pop-property :query-form))
	   (aggregate-op (find-sparql-op "aggregate"))
	   (aggregate-call-forms nil)
	   (aggr-var-counter -1)
	   (group-expr nil)
	   (group-var nil)
	   (expr-var-list nil))
      (when (eq project '*) (setf project (filter #'(lambda (x) (char= (char (uniquely-named-object-name x) 0) #\?)) scope-vars)))
      (labels ((contains-aggregates-p (form) (and (consp form) (or (aggregate-function-name-p (first form)) (some #'contains-aggregates-p (rest form)))))
	       (samplify (expr)
					;	       (inform "samplify ~S" expr)
		 (cond ((sparql-var-p expr) (list 'SAMPLE expr))
		       ((or (not (consp expr)) (aggregate-function-name-p (first expr))) expr)
		       (t
			(mapcar #'samplify expr))))
	       (aggregatify (expr)
					;	       (inform "aggregatify ~S" expr)
		 (cond ((not (consp expr)) expr)
		     ;;; Note: Nested aggregates not allowed!
		       ((aggregate-function-name-p (first expr))
			;; (let ((aggr-var (generate-sparql-var instans (format nil "!AGG~D" (incf aggr-var-counter)))))
			;; 	(push (cons (apply #'create-sparql-aggregate-call expr) aggr-var) aggregate-aggr-var-list)
			;; 	aggr-var)
			(let ((form (append (list aggregate-op (first expr) group-var (incf aggr-var-counter)) (rest expr))))
			  (push form aggregate-call-forms)
			  form))
		       (t
			(mapcar #'aggregatify expr))))
	       (translate-aggregates ()
		 (setf group-var (generate-sparql-var instans "!GROUP"))
		 (loop with valid-project-vars = (loop for item in group-by when (sparql-var-p item) collect item else when (as-form-p item) collect (second item))
		    for pr in project
		    when (sparql-var-p pr) 
		    do (when (not (member pr valid-project-vars :test #'uniquely-named-object-equal))
			 (sparql-parse-error "Cannot project non-group key var ~A" pr))
		    else when (not (let ((e (third pr)))
				     (if (sparql-var-p e)
					 (member e valid-project-vars :test #'uniquely-named-object-equal)
					 (and (consp e) (aggregate-function-name-p (first e))))))
		    do (sparql-parse-error "Cannot project ~A" pr))
		 (loop for item in project
		    when (as-form-p item)
		    do (progn
					;		       (inform "translate-aggregates:loop1 item = ~A" item)
			 (setf (third item) (aggregatify (samplify (third item))))
					;		       (inform "item now = ~A" item)
			 ))
		 (when having
		   (setf having (aggregatify (samplify having))))
		 (when order-by
		   (setf order-by (aggregatify (samplify order-by))))
		 ;; (loop for item in project
		 ;; 	  do (inform "translate-aggregates:loop2 item = ~A" item)
		 ;; 	  when (not (consp item))
		 ;; 	  do (let ((var (generate-sparql-var instans (format nil "!AGG~D" (incf aggr-var-counter))))
		 ;; 		   (aggregate (create-sparql-aggregate-call 'SAMPLE item)))
		 ;; 	       (push (cons aggregate var) aggregate-call-forms)
		 ;; 	       (push (cons var item) expr-var-list)))
		 (setf ggp (list 'AGGREGATE-JOIN ggp (reverse aggregate-call-forms) (list 'GROUP group-expr group-var)))))
	(loop for item in group-by
	   when (as-form-p item)
	   do (let ((var (second item)))
		(cond ((find-sparql-var var scope-vars)
		       (sparql-parse-error "Variable ~S already bound" var))
		      (t
		       (push-to-end var scope-vars)))))
	(loop for item in project
	   when (sparql-var-p item)
	   ;; do (cond ((not (find-sparql-var item scope-vars))
	   ;; 	    (sparql-parse-error "Variable ~S not in SELECT" (uniquely-named-object-name item)))
	   ;; 	   (t
	   ;; 	    (push-to-end item project-vars)))
	   ;; Note! Variables not declared inside SELECT are OK!
	   do (push-to-end-new item project-vars)
	   else
	   do (let ((var (second item)))
;		(inform "AS expression ~S, var = ~S, scope-vars ~S" item var scope-vars)
		(cond ((find-sparql-var var project-vars)
		       (sparql-parse-error "Variable ~S already bound" var))
		      (t
		       (push-to-end var scope-vars)
		       (push-to-end var project-vars)))))
					;      (inform "h3")
	(cond ((not (null group-by))
	       (loop for condition in group-by
		  collect (cond ((as-form-p condition)
					;			       (inform "EXTEND expr ~A AS var ~A" (third condition) (second condition))
				 (setf ggp (list 'EXTEND ggp (second condition) (third condition)))
				 (second condition))
				(t
				 condition)) into conditions
		  finally (setf group-expr conditions))
	       (translate-aggregates))
	      ((or (and (consp project)
			(some #'(lambda (ve)
				  (and (as-form-p ve) (contains-aggregates-p (third ve))))
			      project))
		   (some #'contains-aggregates-p having)
		   (some #'(lambda (oc) (contains-aggregates-p (second oc))) order-by))
	       (setf group-expr '(1))
	       (translate-aggregates)))
					;      (inform "h4")
	(when having
	  (setf ggp (list 'FILTER () ggp)))
	(when values 
	  (setf ggp (list 'DATABLOCK () ggp )))
					;      (inform "h7")
	(loop with vars = nil
	   for item in project
					;	 do (inform "t: item = ~A" item)
	   when (as-form-p item)
	   do (progn
		(pushnew (second item) vars)
		(push (cons (third item) (second item)) expr-var-list))
	   else do (pushnew item vars))
					;      (inform "expr-var-list = ~S" expr-var-list)
	(loop for (expr . var) in expr-var-list
					;	 do (inform "extend expr ~A AS var ~A" expr var)
	   when (and (consp expr) (eq (first expr) aggregate-op))
	   do (setf ggp (list 'EXTEND ggp var (list aggregate-op (third expr) (fourth expr))))
	   else
	   do (setf ggp (list 'EXTEND ggp var expr)))
					;      (inform "haa")
	(when (or limit offset)
	  (setf (getf clauses :start) offset)
	  (setf (getf clauses :length) limit))
	(setf (getf clauses :where) ggp)
	(append (list query-form :project-vars project-vars) clauses)))))

(defun build-query-expression-new (instans clauses)
;  (inform "build-query-expression clauses = ~S" clauses)
  (let ((query-form (getf clauses :query-form)))
    (case query-form
      ((SELECT ASK DESCRIBE CONSTRUCT DELETE-INSERT)
       (translate-algebra instans clauses))
      (DELETE-WHERE
       (setf (getf clauses :query-form) 'DELETE-INSERT)
       (translate-algebra instans clauses))
      ((INSERT-DATA DELETE-DATA)
;       (inform "~S: ~S" query-form clauses)
       (flet ((check-triple (triple) (when (some #'sparql-var-p triple) (sparql-parse-error "Variables not allowed in ~A: ~A" query-form triple))))
	 (loop for item in (getf clauses (if (eq query-form 'INSERT-DATA) :insert-clause :delete-clause))
	       when (eq (first item) 'GRAPH)
	       do (let ((graph (second item)))
		    (when (sparql-var-p graph)
		      (sparql-parse-error "Variables not allowed in ~A: ~A" query-form graph))
		    (assert* (and (eq 'GGP (first (third item))) (eq :FORM (second (third item)))) "Malformed expr ~A" item)
		    (loop for triple in (rest (third (third item))) do (check-triple triple)))
	       else do (loop for triple in (rest item) do (check-triple triple)))
	 (setf (getf clauses :query-form) 'DELETE-INSERT)
	 (translate-algebra instans clauses)))
      ((LOAD CREATE DROP CLEAR ADD MOVE COPY SERVICE) (remf clauses :query-form) (cons query-form clauses))
      (t
       (inform "~A not implemented yet" (and (symbolp query-form) (substitute #\space #\- (string query-form))))
       (instans-add-status instans 'instans-feature-not-implemented-yet)
       (sparql-parse-error "~A not implemented yet" (and (symbolp query-form) (substitute #\space #\- (string query-form))))))))

(defun sparql-parse-error (fmt &rest args)
  (apply #'ll-parser-failure fmt args))

(defun build-query-expression (instans clauses)
; (build-query-expression-old instans clauses)
  (build-query-expression-new instans clauses)
  )

(defun make-sparql-parser (instans input-stream &key base subscribe)
  (when (null base) (setf base (parse-iri "http://")))
  (let* ((lexer (make-instance 'sparql-lexer :input-stream input-stream :instans instans :base base :show-parses-p subscribe))
	 (triples (list nil))
	 (triples-last triples)
	 (replace-blank-nodes-by-vars-p t)
	 (blank-nodes-allowed-p t)
	 (indent 0)
	 (parser nil)
	 (blanks nil)
	 (bgp-blanks nil)
	 (other-bgp-blanks nil)
	 )
    (labels ((set-prefix (prefix-binding expansion)
	       (instans-store-prefix-binding instans prefix-binding expansion)
	       (bind-prefix lexer prefix-binding expansion))
	     (set-base (b)
	       (instans-store-prefix-binding instans "BASE" b)
	       (set-lexer-base lexer b) (values))
	     (pname2iri (prefix suffix)
	       (or (pname-to-iri lexer prefix suffix) (sparql-parse-error "Unbound prefix ~S" prefix)))
	     (clear-triples ()
	       (setf triples (list nil))
	       (setf triples-last triples))
	     (get-triples ()
	       (prog1 (translate-paths (cdr triples))
		 (clear-triples)))
	     (emit (s p o)
	       (setf (cdr triples-last) (list (list s p o)))
	       (setf triples-last (cdr triples-last)))
	     (emit-subj-pred-obj-list (s pol)
	       (loop for (p . ol) in pol do (loop for o in ol do (emit s p o)))
	       s)
	     (expand-collection (col)
	       (cond ((null col) *rdf-nil*)
		     (t
		      (let ((subj (generate-anonymous-blank-node-or-var))
			    (rest (expand-collection (cdr col))))
			(emit subj *rdf-rest* rest)
			(emit subj *rdf-first* (car col))
			subj))))
	     (make-var (name) (make-sparql-var instans name))
	     (generate-var (name) (make-sparql-var instans name))
	     (blank-translation-settings (&key (allowedp t) (replacep t))
	       (setf replace-blank-nodes-by-vars-p replacep)
	       (setf blank-nodes-allowed-p allowedp))
	     ;; (reset-bgp-blanks () (setf bgp-blanks nil))
	     (ban-blanks ()
	       (setf other-bgp-blanks (append bgp-blanks other-bgp-blanks))
	       ;; (inform "ban-blanks, blanks = ~S, other-bgp-blanks = ~S" blanks other-bgp-blanks)
	       (setf bgp-blanks nil))
	     (make-named-blank-node-or-var (name)
	       (cond ((not blank-nodes-allowed-p)
		      (sparql-parse-error "Blank node (~A) not allowed here" name))
		     ((not replace-blank-nodes-by-vars-p)
		      (or (find-if #'(lambda (var) (string= (uniquely-named-object-name var) name)) blanks)
			  (let ((blank (make-named-blank-node instans name)))
;			    (inform "Not replacing blanks by vars, created ~S" blank)
			    (push blank blanks)
			    blank)))
		     (t
		      (or (find-if #'(lambda (var) (string= (uniquely-named-object-name var) name)) blanks)
			  (let ((var (make-var name)))
			    ;; (inform "Created ~S" var)
			    (push var blanks)
			    var)))))
	     (replace-blank-nodes-by-vars (expr)
	       (cond ((consp expr)
		      (cons (replace-blank-nodes-by-vars (car expr)) (replace-blank-nodes-by-vars (cdr expr))))
		     ((rdf-blank-node-p expr)
		      (or (find-if #'(lambda (var) (string= (uniquely-named-object-name var) (uniquely-named-object-name expr)) blanks)
			  (let ((var (generate-sparql-var instans)))
			    ;; (inform "Created ~S" var)
			    (push var blanks)
			    var))))
		     (t expr)))
	     (generate-anonymous-blank-node-or-var ()
	       (cond ((not blank-nodes-allowed-p)
		      (sparql-parse-error "Blank node not allowed here"))
		     ((not replace-blank-nodes-by-vars-p) (generate-anonymous-blank-node instans))
		     (t (generate-sparql-var instans "!"))))
	     (fold-left-binary (value funcs)
	       (loop for func in funcs
		  do (setf value (funcall func value)))
	       value)
	     (translate-path (triple)
	       (incf indent)
	       (let ((v
		      (let ((path (second triple)))
			(cond ((or (rdf-iri-p path) (sparql-var-p path))
			       (list triple))
			      ((eq (car path) 'INV)
			       (translate-path (list (third triple) (second path) (first triple))))
			      ((eq (car path) 'SEQ)
			       (let* ((preds (cdr path))
				      (objs (append (loop repeat (1- (length preds)) collect (generate-var "!PATH")) (list (third triple)))))
				 (loop for pred in preds
				    for subj = (first triple) then obj
				    for obj in objs
				    append (translate-path (list subj pred obj)))))
			      (t
			       (list (cons 'PATH triple)))))
		       ))
		 (decf indent)
		 v))
	     (translate-paths (triple-list)
	       (loop for triple in triple-list
		  append (translate-path triple)))
	     (zero-pattern-p (x) (equal x '(ZERO-PATTERN)))
	     (simplify-joins (e)
	       (case (first e)
		 (JOIN
		  (let ((e1 (simplify-joins (second e)))
			(e2 (simplify-joins (third e))))
		    (cond ((zero-pattern-p e1) e2)
			  ((zero-pattern-p e2) e1)
			  (t (list 'JOIN e1 e2)))))
		 (LEFTJOIN
		  (list 'LEFTJOIN (simplify-joins (second e)) (simplify-joins (third e)) (fourth e)))
		 (EXTEND
		  (list 'EXTEND (simplify-joins (second e)) (third e) (fourth e)))
		 (GRAPH
		  (list 'GRAPH (second e) (simplify-joins (third e))))
		 (SERVICE
		  (cons 'SERVICE (cons (simplify-joins (second e)) (cddr e))))
		 ((MINUS UNION)
		  (cons (first e) (mapcar #'simplify-joins (rest e))))
		 (t e)))
	     (translate-group-graph-pattern (ggp)
;	       (inform "enter translate-group-graph-pattern ~A" ggp)
	       (let ((g '(ZERO-PATTERN))
		     (vars nil))
		 (flet ((ggp-scope-vars (ggp) (getf (rest ggp) :scope-vars))
			(add-vars (vlist)
;			  (inform "add-vars ~A, vars before = ~A" vlist vars)
			  (setf vars (append vars vlist))
;			  (inform "add-vars ~A, vars after = ~A" vlist vars)
			  )
			(join (x) (setf g (list 'JOIN g x))))
		   ;;; Blanks must be distinct from those in other GGPs
		   (ban-blanks)
		   (loop with fs = nil
		      for e in ggp
;		      do (inform "loop ~A" e)
		      do (case (first e)
			   (GGP
			    (add-vars (ggp-scope-vars e))
			    (join e))
			   (SELECT (add-vars (getf (rest e) :project-vars)) ; this is taken care of elsewhere
				   (join e))
			   (BGP
			    (let ((bgp-vars (collect-expression-variables (rest e))))
			      (unless (and (eq (first g) 'JOIN) (consp (third g)) (eq (first (third g)) 'BGP))
		                 ;;; Blanks must be distinct from those in BGPs that are not immediately next to this, e.g.,
				 ;;; only separated by FILTERs
				(ban-blanks))
			      (loop for var in bgp-vars
				 when (and (member var blanks) (member var other-bgp-blanks))
				 do (sparql-parse-error "Blank node ~S used in separate basic graph patterns" (uniquely-named-object-name var))
				 else do (push var bgp-blanks))
			      (add-vars bgp-vars)
			      (join e)))
			   (UNION
			    (add-vars (ggp-scope-vars (second e)))
			    (add-vars (ggp-scope-vars (third e)))
			    (join e))
			   (OPTIONAL
			    (let ((a (second e)))
			      (add-vars (ggp-scope-vars (second e)))
			      (setf g (cond ((eq (first a) 'FILTER) (list 'LEFTJOIN g (second a) (third a)))
					    (t (list 'LEFTJOIN g a t))))))
			   (MINUS
			    (setf g (list 'MINUS g (second e))))
			   (GRAPH
			    (let ((var-or-iri (second e))
				  (subggp-vars (ggp-scope-vars (third e))))
			      (add-vars (if (sparql-var-p var-or-iri) (cons var-or-iri subggp-vars) subggp-vars))
			      (join e)))
			   (SERVICE
			      (add-vars (ggp-scope-vars (fourth e)))
			      ;; (inform "SERVICE ggp-vars = ~S" (ggp-scope-vars (fourth e)))
			      (setf g (cons 'SERVICE (cons g (cdr e)))))
			   (FILTER
			    (setf fs (if (null fs) (second e) (create-sparql-call "logical-and" fs (second e)))))
			   (BIND
			    (let ((var (third e))
				  (expr (second e)))
			      (cond ((find-sparql-var var vars)
				     (sparql-parse-error "Variable ~S already defined in scope" var))
				    (t
				     (push-to-end var vars)))
			      (setf g (list 'EXTEND g var expr))))
			   (INLINEDATA
			    (add-vars (second e))
			    (join e))
			   (t (error* "Illegal form ~S" e)))
		      finally (progn
				(ban-blanks)
				(setf g (simplify-joins g))
				(when (not (null fs))
				  (setf g (list 'FILTER fs g)))
				(return (list 'GGP :form g :scope-vars (remove-duplicates vars :test #'sparql-var-equal))))))))
	     (create-call (op-name &rest args)
	       (or (apply #'create-sparql-call op-name args)
		   (sparql-parse-error "~A does not name a Sparql function or form" op-name)))
	     (create-call-through-iri (iri arglist)
	       (let ((sparql-op (find-sparql-op (rdf-iri-string iri))))
		 (cond ((null sparql-op)
			(let* ((dynamic-call-op-name "instans:dynamic_call")
			       (dynamic-call-op (find-sparql-op dynamic-call-op-name)))
			  (cond ((null dynamic-call-op)
				 (error* "Dynamic call operation ~A not found!" dynamic-call-op))
				(t
				 (cons dynamic-call-op (cons iri arglist))))))
		       (t
			(cons sparql-op arglist)))))
	     (get-preceding-comment ()
	       (and (lexer-previous-comment lexer) (list :comment (lexer-previous-comment lexer)))))
      (setf
       parser
       (generate-ll1-parser sparql-parser ()
	 (Rules ::= (Prologue (:OPT ((:OR Query1 Update1) ((:OPT (|;-TERMINAL| Rules :RESULT $1)) :RESULT (opt-value $0)) :RESULT (cons $0 $1)))
			      :RESULT (append $0 (opt-value $1))))
	 (Query1 ::= ((:OR SelectQuery ConstructQuery DescribeQuery AskQuery) ValuesClause :RESULT (build-query-expression instans (append $0 (opt-value $1)))))
	 (Prologue ::= (:REP0 (:OR BaseDecl PrefixDecl)))
	 (BaseDecl ::= (BASE-TERMINAL IRIREF-TERMINAL :RESULT (progn (set-base $1) (list 'BASE $1))))
	 (PrefixDecl ::= (PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL :RESULT (progn (set-prefix $1 $2) (list 'PREFIX $1 $2))))
	 (SelectQuery ::= (SelectClause ((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0))) WhereClause SolutionModifier
					:RESULT (let ((result (append '(:query-form SELECT) $0 $1 $2 $3)))
					;						    (inform "Scope vars in ~S~%~S" result (scope-vars result))
						  result)))
	 (SubSelect ::= (SelectClause WhereClause SolutionModifier ValuesClause
				      :RESULT (build-query-expression instans (append '(:query-form SELECT) $0 $1 $2 (opt-value $3)))))
					; `(SELECT ,@$0 :where ,$1 ,@$2 ,@(if (opt-yes-p $3) (opt-value $3)))))
	 (SelectClause ::= ((SELECT-TERMINAL :RESULT (get-preceding-comment))
			    ((:OPT (:OR (DISTINCT-TERMINAL :RESULT :distinctp) (REDUCED-TERMINAL :RESULT :distinctp))) ;;; distinct == reduced!
					     :RESULT (if (opt-yes-p $0) (list (opt-value $0) t)))
					    (:OR (:REP1 (:OR Var (|(-TERMINAL| Expression AS-TERMINAL Var |)-TERMINAL| :RESULT (list 'AS $3 $1))))
						 (|*-TERMINAL| :RESULT '*))
					    :RESULT (append $0 $1 (list :project $2))))
	 ;;; Note: blanks should be reinstantiated when running the template. See 16.2.1 Templates with Blank Nodes in the SPecs
	 (ConstructQuery ::= ((CONSTRUCT-TERMINAL :RESULT (progn  (blank-translation-settings :allowedp t :replacep nil) (get-preceding-comment)))
			      (:OR ((; ConstructTemplate
				     QuadPattern
				     :RESULT (progn  (blank-translation-settings :allowedp t :replacep t) (list :construct-template $0)))
				    ((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0))) WhereClause SolutionModifier
				    :RESULT (append '(:query-form CONSTRUCT) $0 $1 $2 $3))
				   (((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0)))
				    WHERE-TERMINAL |{-TERMINAL|
				    (:OPT (TriplesTemplate :RESULT (list (cons 'BGP (get-triples)))))
				    |}-TERMINAL| SolutionModifier
				    :RESULT (progn (blank-translation-settings :allowedp t :replacep t)
						   (append '(:query-form CONSTRUCT) (list :where (translate-group-graph-pattern (replace-blank-nodes-by-vars (opt-value $3)))
											  :construct-template (opt-value $3)) $0 $5))))
			      :RESULT (append $0 $1)))
	 (DescribeQuery ::= ((DESCRIBE-TERMINAL :RESULT (get-preceding-comment))
			     ((:OR (:REP1 VarOrIri) (|*-TERMINAL| :RESULT '*)) :RESULT (list :var-or-iri-list $0))
			     ((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0)))
			     (:OPT WhereClause) SolutionModifier
			     :RESULT (append $0 '(:query-form DESCRIBE) $1 $2 (opt-value $3) $4)))
	 (AskQuery ::= ((ASK-TERMINAL :RESULT (get-preceding-comment))
			((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0))) WhereClause SolutionModifier
			:RESULT (append $0 '(:query-form ASK) $1 $2 $3)))
	 (DatasetClause ::= (FROM-TERMINAL (:OR DefaultGraphClause NamedGraphClause) :RESULT $1))
	 (DefaultGraphClause ::= (SourceSelector :RESULT (list :default $0)))
	 (NamedGraphClause ::= (NAMED-TERMINAL SourceSelector :RESULT (list :named $1)))
	 (SourceSelector ::= iri)
	 (WhereClause ::= ((:OPT WHERE-TERMINAL) GroupGraphPattern :RESULT (list :where $1)))
	 (SolutionModifier ::= ((:OPT GroupClause) (:OPT HavingClause) (:OPT OrderClause) (:OPT LimitOffsetClauses)
				:RESULT (append (opt-value $0) (opt-value $1) (opt-value $2) (opt-value $3))))
	 (GroupClause ::= (GROUP-TERMINAL BY-TERMINAL (:REP1 GroupCondition) :RESULT (list :group-by $2)))
	 (GroupCondition ::= (:OR BuiltInCall
				  FunctionCall
				  (|(-TERMINAL| Expression (:OPT (AS-TERMINAL Var :RESULT $1)) |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'AS (opt-value $2) $1) $1)) Var))
	 (HavingClause ::= (HAVING-TERMINAL (:REP1 HavingCondition) :RESULT (list :having $1)))
	 (HavingCondition ::= Constraint)
	 (OrderClause ::= (ORDER-TERMINAL BY-TERMINAL (:REP1 OrderCondition) :RESULT (list :order-by $2)))
	 (OrderCondition ::= (:OR ((:OR (ASC-TERMINAL :RESULT 'ASC) (DESC-TERMINAL :RESULT 'DESC)) BrackettedExpression :RESULT (list $0 $1))
				  (Constraint :RESULT (list 'ASC $0))
				  (Var :RESULT (list 'ASC $0))))
	 (LimitOffsetClauses ::= (:OR (LimitClause (:OPT OffsetClause) :RESULT (append (list :limit $0) (and (opt-yes-p $1) (list :offset (opt-value $1)))))
				      (OffsetClause (:OPT LimitClause) :RESULT (append (list :offset $0) (and (opt-yes-p $1) (list :limit (opt-value $1)))))))
	 (LimitClause ::= (LIMIT-TERMINAL INTEGER-TERMINAL :RESULT $1))
	 (OffsetClause ::= (OFFSET-TERMINAL INTEGER-TERMINAL :RESULT $1))
	 (ValuesClause ::= (:OPT (VALUES-TERMINAL DataBlock :RESULT (list :datablock $1))))
	 ;; (Update ::= (Prologue (:OPT (Update1 (:OPT (|;-TERMINAL| Update))))))
	 (Update1 ::= (:OR Load Clear Drop Add Move Copy Create InsertData DeleteData DeleteWhere Modify) :RESULT (build-query-expression instans $0))
	 (_OptSilent ::= (:OPT SILENT-TERMINAL) :RESULT (if (opt-yes-p $0) (list :silent t)))
	 (Load ::= ((LOAD-TERMINAL :RESULT (get-preceding-comment))
		    _OptSilent iri (:OPT (INTO-TERMINAL GraphRef :RESULT $1)) :RESULT (append '(:query-form LOAD) $1 (list :from $2) (if (opt-yes-p $3) (list :to (opt-value $3))))))
	 (Clear ::= ((CLEAR-TERMINAL :RESULT (get-preceding-comment))
		     _OptSilent GraphRefAll :RESULT (append '(:query-form CLEAR) $1 $2)))
	 (Drop ::= ((DROP-TERMINAL :RESULT (get-preceding-comment))
		    _OptSilent GraphRefAll :RESULT (append '(:query-form DROP) $1 $2)))
	 (Create ::= ((CREATE-TERMINAL :RESULT (get-preceding-comment))
		      _OptSilent GraphRef :RESULT (append '(:query-form CREATE) $1 $2)))
	 (Add ::= ((ADD-TERMINAL :RESULT (get-preceding-comment))
		   _OptSilent GraphOrDefault TO-TERMINAL GraphOrDefault :RESULT (append '(:query-form ADD) $1 (list :graph $2 :to $4))))
	 (Move ::= ((MOVE-TERMINAL :RESULT (get-preceding-comment))
		    _OptSilent GraphOrDefault TO-TERMINAL GraphOrDefault :RESULT (append '(:query-form MOVE) $1 (list :graph $2 :to $4))))
	 (Copy ::= ((COPY-TERMINAL :RESULT (get-preceding-comment))
		    _OptSilent GraphOrDefault TO-TERMINAL GraphOrDefault :RESULT (append '(:query-form COPY) $1 (list :graph $2 :to $4))))
	 (InsertData ::= ((INSERT-DATA-TERMINAL :result (progn (blank-translation-settings :allowedp t :replacep nil) (get-preceding-comment)))
			  QuadData :RESULT (progn (blank-translation-settings :allowedp t :replacep t)
						  (list :query-form 'INSERT-DATA :insert-clause $1 :where (translate-group-graph-pattern nil)))))
	 (DeleteData ::= ((DELETE-DATA-TERMINAL :result (blank-translation-settings :allowedp nil :replacep nil))
			  QuadData :RESULT (progn (blank-translation-settings :allowedp t :replacep t)
						  (list :query-form 'DELETE-DATA :delete-clause $1 :where (translate-group-graph-pattern nil)))))
	 (DeleteWhere ::= ((DELETE-WHERE-TERMINAL :result (progn (blank-translation-settings :allowedp nil) (get-preceding-comment)))
			   QuadPattern
			   :RESULT (progn (blank-translation-settings :allowedp t :replacep t)
					  (list :query-form 'DELETE-WHERE :delete-clause $1 :where (translate-group-graph-pattern $1)))))
	 (Modify ::= (((:OPT (WITH-TERMINAL iri :RESULT $1)) :RESULT (append (if (opt-yes-p $0) (list :with (opt-value $0))) (get-preceding-comment)))
		      (:OR (DeleteClause (:OPT InsertClause) :RESULT (append $0 (opt-value $1)))
			   (InsertClause))
		      ((:REP0 UsingClause) :RESULT (if $0 (list :using $0)))
		      (WHERE-TERMINAL GroupGraphPattern :result (list :where $1))
		      :RESULT (append '(:query-form DELETE-INSERT) $0 $1 $2 $3)))
	 (DeleteClause ::= ((DELETE-TERMINAL :result (blank-translation-settings :allowedp nil)) QuadPattern
			    :RESULT (progn (blank-translation-settings :allowedp t :replacep t) (list :delete-clause $1))))
	 (InsertClause ::= ((INSERT-TERMINAL :result (blank-translation-settings :allowedp t :replacep nil)) QuadPattern
			    :RESULT (progn (blank-translation-settings :allowedp t :replacep t)
					   (list :insert-clause $1))))
	 (UsingClause ::= (USING-TERMINAL (:OR iri (NAMED-TERMINAL iri :RESULT $1))))
	 (GraphOrDefault ::= (:OR (DEFAULT-TERMINAL :RESULT :default) ((:OPT GRAPH-TERMINAL) iri :RESULT $1)))
	 (GraphRef ::= (GRAPH-TERMINAL iri :RESULT (list :graph $1)))
	 (GraphRefAll ::= (:OR GraphRef (DEFAULT-TERMINAL :RESULT :default) (NAMED-TERMINAL :RESULT :named) (ALL-TERMINAL :RESULT :all)) :RESULT (list :graph $0))
	 (QuadPattern ::= (|{-TERMINAL| Quads |}-TERMINAL| :RESULT $1))
	 (QuadData ::= (|{-TERMINAL| Quads |}-TERMINAL| :RESULT $1))
	 (Quads ::= (((:OPT TriplesTemplate) :RESULT (if (opt-yes-p $0) (list (cons 'BGP (get-triples)))))
		     (:REP0 (QuadsNotTriples (:OPT |.-TERMINAL|) ((:OPT TriplesTemplate) :RESULT (if (opt-yes-p $0) (list (cons 'BGP (get-triples))))) :RESULT (append $0 $2)))
		     :RESULT (apply #'append $0 $1)))
	 (QuadsNotTriples ::= (GRAPH-TERMINAL VarOrIri |{-TERMINAL| (:OPT TriplesTemplate) |}-TERMINAL|
					      :RESULT (list (list 'GRAPH $1 (translate-group-graph-pattern (list (cons 'BGP (get-triples))))))))
	 (TriplesTemplate ::= (TriplesSameSubject (:OPT (|.-TERMINAL| (:OPT TriplesTemplate)))))
	 (GroupGraphPattern ::= (|{-TERMINAL| (:OR SubSelect GroupGraphPatternSub) |}-TERMINAL| :RESULT $1))
	 (GroupGraphPatternSub ::= ((:OPT TriplesBlock) ((:REP0 (GraphPatternNotTriples (:OPT |.-TERMINAL|) (:OPT TriplesBlock)
											:RESULT (cons $0 (and (opt-yes-p $2) (list (opt-value $2))))))
							 :RESULT (apply #'append $0))
				    :RESULT (translate-group-graph-pattern (if (opt-yes-p $0) (cons (opt-value $0) $1) $1))))
	 (TriplesBlock ::= (;(:RESULT (reset-bgp-blanks))
			    TriplesBlockInner :RESULT (progn ;(ban-blanks)
							(cons 'BGP (get-triples)))))
	 (TriplesBlockInner ::= (TriplesSameSubjectPath (:OPT (|.-TERMINAL| (:OPT TriplesBlockInner)))))
	 (GraphPatternNotTriples ::= (:OR GroupOrUnionGraphPattern OptionalGraphPattern MinusGraphPattern GraphGraphPattern ServiceGraphPattern Filter Bind InlineData))
	 (OptionalGraphPattern ::= (OPTIONAL-TERMINAL GroupGraphPattern) :RESULT (list 'OPTIONAL $1))
	 (GraphGraphPattern ::= (GRAPH-TERMINAL VarOrIri GroupGraphPattern :RESULT (list 'GRAPH $1 $2)))
	 (ServiceGraphPattern ::= ((SERVICE-TERMINAL :RESULT (cdr (ll-parser-stored-token-stack parser))) _OptSilent VarOrIri GroupGraphPattern
				   :RESULT (let ((ggp-tokens (loop with result = nil
								   for tokens on (cddr (ll-parser-stored-token-stack parser))
								   while (not (eq tokens $0))
								   do (push (input-token-to-string lexer (car tokens)) result)
								   finally (return (cddr result)))))
					     (list 'SERVICE $1 $2 $3 ggp-tokens))))
	 (Bind ::= (BIND-TERMINAL |(-TERMINAL| Expression AS-TERMINAL Var |)-TERMINAL|) :RESULT (list 'BIND $2 $4))
	 (InlineData ::= (VALUES-TERMINAL DataBlock :RESULT $1))
	 (DataBlock ::= (:OR InlineDataOneVar InlineDataFull))
	 (InlineDataOneVar ::= (Var |{-TERMINAL| (:REP0 DataBlockValue) |}-TERMINAL|) :RESULT (list 'INLINEDATA (list $0) (list $2)))
	 (InlineDataFull ::= ((:OR (NIL-TERMINAL :RESULT (progn nil)) (|(-TERMINAL| (:REP0 Var) |)-TERMINAL| :RESULT $1)) |{-TERMINAL|
			      (:REP0 (:OR (|(-TERMINAL| (:REP0 DataBlockValue) |)-TERMINAL| :RESULT $1) (NIL-TERMINAL :RESULT (progn nil)))) |}-TERMINAL|
			      :RESULT (list 'INLINEDATA $0 $2)))
	 (DataBlockValue ::= (:OR iri RDFLiteral NumericLiteral BooleanLiteral (UNDEF-TERMINAL :RESULT (sparql-unbound))))
	 (MinusGraphPattern ::= (MINUS-TERMINAL GroupGraphPattern :RESULT (list 'MINUS $1)))
	 (GroupOrUnionGraphPattern ::= (GroupGraphPattern (:REP0 (UNION-TERMINAL GroupGraphPattern :RESULT $1)) :RESULT (if (null $1) $0 (cons 'UNION (cons $0 $1)))))
	 (Filter ::= (FILTER-TERMINAL Constraint :RESULT (list 'FILTER $1)))
	 (Constraint ::= (:OR BrackettedExpression BuiltInCall FunctionCall))
	 (FunctionCall ::= (iri ArgList) :RESULT (create-call-through-iri $0 $1))
	 (ArgList ::= (:OR (NIL-TERMINAL :RESULT (progn nil))
			   (|(-TERMINAL| (:OPT DISTINCT-TERMINAL) (Expression (:REP0 (|,-TERMINAL| Expression :RESULT $1)) :RESULT (cons $0 $1)) |)-TERMINAL|
					 :RESULT (if (opt-yes-p $1) (cons (sparql-distinct) $2) $2))))
	 (ExpressionList ::= (:OR (NIL-TERMINAL :RESULT (progn nil)) (|(-TERMINAL| (Expression (:REP0 (|,-TERMINAL| Expression :RESULT $1)) :RESULT (cons $0 $1)) |)-TERMINAL| :RESULT $1)))
	 (ConstructTemplate ::= ((|{-TERMINAL| :RESULT (blank-translation-settings :allowedp t :replacep nil)) (:OPT ConstructTriples) |}-TERMINAL|
				 :RESULT (progn (blank-translation-settings :allowedp t :replacep t) (list (cons 'BGP (get-triples))))))
	 (ConstructTriples ::= (TriplesSameSubject (:OPT (|.-TERMINAL| (:OPT ConstructTriples)))))
	 (TriplesSameSubject ::= (:OR (VarOrTerm PropertyListNotEmpty :RESULT (emit-subj-pred-obj-list $0 $1))
				      (TriplesNode PropertyList :RESULT (emit-subj-pred-obj-list $0 (opt-value $1)))))
	 (PropertyList ::= (:OPT PropertyListNotEmpty))
	 (PropertyListNotEmpty ::= ((Verb ObjectList :RESULT (cons $0 $1)) (:REP0 (|;-TERMINAL| (:OPT (Verb ObjectList :RESULT (cons $0 $1))) :RESULT (opt-value $1))))
			       :RESULT (cons $0 $1))
	 (Verb ::= (:OR VarOrIri (a-TERMINAL :RESULT *rdf-type*)))
	 (ObjectList ::= ((Object (:REP0 (|,-TERMINAL| Object :RESULT $1)) :RESULT (cons $0 $1))))
	 (Object ::= GraphNode)
	 (TriplesSameSubjectPath ::= (:OR (VarOrTerm PropertyListPathNotEmpty :RESULT (emit-subj-pred-obj-list $0 $1))
					  (TriplesNodePath PropertyListPath :RESULT (emit-subj-pred-obj-list $0 (opt-value $1)))))
	 (PropertyListPath ::= (:OPT PropertyListPathNotEmpty))
	 (PropertyListPathNotEmpty ::= (((:OR VerbPath VerbSimple) ObjectListPath :RESULT (cons $0 $1))
					(:REP0 (|;-TERMINAL| (:OPT ((:OR VerbPath VerbSimple) ObjectListPath :RESULT (cons $0 $1))) :RESULT (opt-value $1))))
				   :RESULT (cons $0 $1))
	 (VerbPath ::= Path)
	 (VerbSimple ::= Var)
	 (ObjectListPath ::= ((ObjectPath (:REP0 (|,-TERMINAL| ObjectPath :RESULT $1)) :RESULT (cons $0 $1))))
	 (ObjectPath ::= GraphNodePath)
	 (Path ::= PathAlternative)
	 (PathAlternative ::= (PathSequence (:REP0 (|\|-TERMINAL| PathSequence :RESULT $1)) :RESULT (if $1 (cons 'ALT (cons $0 $1)) $0)))
	 (PathSequence ::= (PathEltOrInverse (:REP0 (|/-TERMINAL| PathEltOrInverse :RESULT $1)) :RESULT (if $1 (cons 'SEQ (cons $0 $1)) $0)))
	 (PathElt ::= (PathPrimary (:OPT PathMod) :RESULT (if (opt-yes-p $1) (list (opt-value $1) $0) $0)))
	 (PathEltOrInverse ::= (:OR PathElt (|^-TERMINAL| PathElt :RESULT (list 'INV $1))))
	 (PathMod ::= (:OR (|?-TERMINAL| :RESULT 'ZERO-OR-ONE-PATH) (|*-TERMINAL| :RESULT 'ZERO-OR-MORE-PATH) (|+-TERMINAL| :RESULT 'ONE-OR-MORE-PATH)))
	 (PathPrimary ::= (:OR iri (a-TERMINAL :RESULT *rdf-type*)
			       (|!-TERMINAL| PathNegatedPropertySet
					     :RESULT (loop for x in $1 when (and (consp x) (eq (car x) 'INV)) collect (second x) into inv else collect x into noinv
							finally (return (cond ((null inv) (cons 'NPS noinv))
									      ((null noinv) (list 'INV (cons 'NPS inv)))
									      (t (list 'ALT (cons 'NPS noinv) (list 'INV (cons 'NPS inv))))))))
			       (|(-TERMINAL| Path |)-TERMINAL| :RESULT $1)))
	 (PathNegatedPropertySet ::= (:OR (PathOneInPropertySet :RESULT (list $0))
					  (|(-TERMINAL| (:OPT (PathOneInPropertySet (:REP0 (|\|-TERMINAL| PathOneInPropertySet :RESULT $1))
										    :RESULT (cons $0 $1)))
							|)-TERMINAL| :RESULT (opt-value $1))))
	 (PathOneInPropertySet ::= (:OR iri (a-TERMINAL :RESULT *rdf-type*)
					(|^-TERMINAL| (:OR iri a-TERMINAL) :RESULT (list 'INV $1))))
	 (Integer ::= INTEGER-TERMINAL)
	 (TriplesNode ::= (:OR Collection BlankNodePropertyList))
	 (BlankNodePropertyList ::= (|[-TERMINAL| PropertyListNotEmpty |]-TERMINAL| :RESULT (emit-subj-pred-obj-list (generate-anonymous-blank-node-or-var) $1)))
	 (TriplesNodePath ::= (:OR CollectionPath BlankNodePropertyListPath))
	 (BlankNodePropertyListPath ::= (|[-TERMINAL| PropertyListPathNotEmpty |]-TERMINAL| :RESULT (emit-subj-pred-obj-list (generate-anonymous-blank-node-or-var) $1)))
	 (Collection ::= (|(-TERMINAL| (:REP1 GraphNode) |)-TERMINAL| :RESULT (expand-collection $1)))
	 (CollectionPath ::= (|(-TERMINAL| (:REP1 GraphNodePath) |)-TERMINAL| :RESULT (expand-collection $1)))
	 (GraphNode ::= (:OR VarOrTerm TriplesNode))
	 (GraphNodePath ::= (:OR VarOrTerm TriplesNodePath))
	 (VarOrTerm ::= (:OR Var GraphTerm) :RESULT $0)
	 (VarOrIri ::= (:OR Var iri))
	 (Var ::= (:OR VAR1-TERMINAL VAR2-TERMINAL) :RESULT (make-var (concatenate 'string "?" (subseq $0 1))))
	 (GraphTerm ::= (:OR iri RDFLiteral NumericLiteral BooleanLiteral BlankNode NIL-TERMINAL))
	 (Expression ::= ConditionalOrExpression)
	 (ConditionalOrExpression ::= (ConditionalAndExpression (:REP0 (|\|\|-TERMINAL| ConditionalAndExpression :RESULT #'(lambda (a) (create-call "logical-or" a $1))))
								:RESULT (fold-left-binary $0 $1)))
	 (ConditionalAndExpression ::= (ValueLogical (:REP0 (|&&-TERMINAL| ValueLogical :RESULT #'(lambda (a) (create-call "logical-and" a $1))))
						     :RESULT (fold-left-binary $0 $1)))
	 (ValueLogical ::= RelationalExpression)
	 (RelationalExpression ::= (NumericExpression (:OPT (:OR (|=-TERMINAL| NumericExpression :RESULT #'(lambda (a) (create-call "=" a $1)))
								 (|!=-TERMINAL| NumericExpression :RESULT #'(lambda (a) (create-call "!=" a $1)))
								 (|<-TERMINAL| NumericExpression :RESULT #'(lambda (a) (create-call "<" a $1)))
								 (|>-TERMINAL| NumericExpression :RESULT #'(lambda (a) (create-call ">" a $1)))
								 (|<=-TERMINAL| NumericExpression :RESULT #'(lambda (a) (create-call "<=" a $1)))
								 (|>=-TERMINAL| NumericExpression :RESULT #'(lambda (a) (create-call ">=" a $1)))
								 (IN-TERMINAL ExpressionList :RESULT #'(lambda (a) (apply #'create-call "in" a $1)))
								 (NOT-TERMINAL IN-TERMINAL ExpressionList :RESULT #'(lambda (a) (apply #'create-call "not in" a $2))))))
			       :RESULT (if (opt-yes-p $1) (fold-left-binary $0 (list (opt-value $1))) $0))
	 (NumericExpression ::= AdditiveExpression)
	 (AdditiveExpression ::= (MultiplicativeExpression (:REP0 (:OR (|+-TERMINAL| MultiplicativeExpression :RESULT #'(lambda (a) (create-call "+" a $1)))
								       (|--TERMINAL| MultiplicativeExpression :RESULT #'(lambda (a) (create-call "-" a $1)))
								       ((:OR NumericLiteralPositive NumericLiteralNegative)
									(:REP0 (:OR (|*-TERMINAL| UnaryExpression :RESULT #'(lambda (x) (create-call "*" x $1)))
										    (|/-TERMINAL| UnaryExpression :RESULT #'(lambda (x) (create-call "/" x $1)))))
									:RESULT #'(lambda (a) (create-call "+" a (fold-left-binary $0 $1))))))
							   :RESULT (fold-left-binary $0 $1)))
	 (MultiplicativeExpression ::= (UnaryExpression (:REP0 (:OR (|*-TERMINAL| UnaryExpression :RESULT #'(lambda (x) (create-call "*" x $1)))
								    (|/-TERMINAL| UnaryExpression :RESULT #'(lambda (x) (create-call "/" x $1)))))
							:RESULT (fold-left-binary $0 $1)))
	 (UnaryExpression ::= (:OR (|!-TERMINAL| PrimaryExpression :RESULT (create-call "not" $1))
				   (|+-TERMINAL| PrimaryExpression :RESULT (create-call "numeric-unary-plus" $1))
				   (|--TERMINAL| PrimaryExpression :RESULT (create-call "numeric-unary-minus" $1))
				   PrimaryExpression))
	 (PrimaryExpression ::= (:OR BrackettedExpression BuiltInCall iriOrFunction RDFLiteral NumericLiteral BooleanLiteral Var))
	 (BrackettedExpression ::= (|(-TERMINAL| Expression |)-TERMINAL| :RESULT $1))
	 (BuiltInCall ::= (:OR Aggregate
			       (STR-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (LANG-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (LANGMATCHES-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL|  :RESULT (create-call $0 $2 $4))
			       (DATATYPE-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (BOUND-TERMINAL |(-TERMINAL| Var |)-TERMINAL| :RESULT (create-call $0 $2))
			       (IRI-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (URI-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (BNODE-TERMINAL (:OR (|(-TERMINAL| Expression |)-TERMINAL| :RESULT $1) (NIL-TERMINAL :RESULT (progn nil))) :RESULT (create-call $0 $1))
			       (RAND-TERMINAL NIL-TERMINAL :RESULT (create-call $0))
			       (ABS-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (CEIL-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (FLOOR-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (ROUND-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (CONCAT-TERMINAL ExpressionList :RESULT (apply #'create-call $0 $1))
			       SubstringExpression
			       (STRLEN-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       StrReplaceExpression
			       (UCASE-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (LCASE-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (ENCODE_FOR_URI-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (CONTAINS-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2 $4))
			       (STRSTARTS-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2 $4))
			       (STRENDS-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2 $4))
			       (STRBEFORE-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2 $4))
			       (STRAFTER-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2 $4))
			       (YEAR-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (MONTH-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (DAY-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (HOURS-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (MINUTES-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (SECONDS-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (TIMEZONE-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (TZ-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (NOW-TERMINAL NIL-TERMINAL :RESULT (create-call $0))
			       (UUID-TERMINAL NIL-TERMINAL :RESULT (create-call $0))
			       (STRUUID-TERMINAL NIL-TERMINAL :RESULT (create-call $0))
			       (MD5-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (SHA1-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (SHA256-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (SHA384-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (SHA512-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (COALESCE-TERMINAL ExpressionList :RESULT (apply #'create-call $0 $1))
			       (IF-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2 $4 $6))
			       (STRLANG-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2 $4))
			       (STRDT-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2 $4))
			       (sameTerm-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2 $4))
			       (isIRI-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (isURI-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (isBLANK-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (isLITERAL-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       (isNUMERIC-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
			       RegexExpression
			       ExistsFunc
			       NotExistsFunc))
	 (RegexExpression ::= (REGEX-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression (:OPT (|,-TERMINAL| Expression :RESULT $1)) |)-TERMINAL|
					      :RESULT (create-call $0 $2 $4 (opt-value $5))))
	 (SubstringExpression ::= (SUBSTR-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression (:OPT (|,-TERMINAL| Expression :RESULT $1)) |)-TERMINAL|
						   :RESULT (create-call $0 $2 $4 (opt-value $5))))
	 (StrReplaceExpression ::= (REPLACE-TERMINAL |(-TERMINAL| Expression |,-TERMINAL| Expression |,-TERMINAL| Expression
						     (:OPT (|,-TERMINAL| Expression :RESULT $1)) |)-TERMINAL|
						     :RESULT (create-call $0 $2 $4 $6 (opt-value $7))))
	 (ExistsFunc ::= (EXISTS-TERMINAL GroupGraphPattern :RESULT (list 'EXISTS $1)))
	 (NotExistsFunc ::= (NOT-TERMINAL EXISTS-TERMINAL GroupGraphPattern :RESULT (list 'NOT-EXISTS $2)))
	 (Aggregate ::= (:OR (COUNT-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) (:OR (|*-TERMINAL| :RESULT '*) Expression) |)-TERMINAL|
					     :RESULT (if (opt-yes-p $2) (list 'COUNT :distinctp t $3) (list 'COUNT $3)))
			     (SUM-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'SUM :distinctp t $3) (list 'SUM $3)))
			     (MIN-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'MIN :distinctp t $3) (list 'MIN $3)))
			     (MAX-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'MAX :distinctp t $3) (list 'MAX $3)))
			     (AVG-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'AVG :distinctp t $3) (list 'AVG $3)))
			     (SAMPLE-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'SAMPLE :distinctp t $3) (list 'SAMPLE $3)))
			     (GROUP_CONCAT-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression
						    (:OPT (|;-TERMINAL| SEPARATOR-TERMINAL |=-TERMINAL| String :RESULT $3)) |)-TERMINAL|
						    :RESULT (if (opt-yes-p $2) (list 'GROUP_CONCAT :distinctp t $3 :separator (opt-value $4)) (list 'GROUP_CONCAT $3 :separator (opt-value $4))))))
	 (iriOrFunction ::= (iri (:OPT ArgList) :RESULT (if (opt-no-p $1) $0 (create-call-through-iri $0 (opt-value $1)))))
	 (RDFLiteral		  ::= (String (:OPT (:OR (LANGTAG-TERMINAL :RESULT #'(lambda (s) (create-rdf-literal-with-lang s (subseq $0 1))))
							 (^^-TERMINAL iri :RESULT #'(lambda (s) (nth-value 0 (create-rdf-literal-with-type s $1))))))
					      :RESULT (if (opt-yes-p $1) (funcall (opt-value $1) $0) $0)))
	 (NumericLiteral ::= (:OR NumericLiteralUnsigned NumericLiteralPositive NumericLiteralNegative))
	 (NumericLiteralUnsigned ::= (:OR INTEGER-TERMINAL DECIMAL-TERMINAL DOUBLE-TERMINAL))
	 (NumericLiteralPositive ::= (:OR INTEGER_POSITIVE-TERMINAL DECIMAL_POSITIVE-TERMINAL DOUBLE_POSITIVE-TERMINAL))
	 (NumericLiteralNegative ::= (:OR INTEGER_NEGATIVE-TERMINAL DECIMAL_NEGATIVE-TERMINAL DOUBLE_NEGATIVE-TERMINAL))
	 (BooleanLiteral ::= (:OR (TRUE-TERMINAL :RESULT t) (FALSE-TERMINAL :RESULT (progn nil))))
	 (String ::= (:OR STRING_LITERAL1-TERMINAL STRING_LITERAL2-TERMINAL STRING_LITERAL_LONG1-TERMINAL STRING_LITERAL_LONG2-TERMINAL))
	 (iri ::= (:OR IRIREF-TERMINAL PrefixedName))
	 (PrefixedName ::= (:OR (PNAME_LN-TERMINAL :RESULT (pname2iri (first $0) (second $0)))
				(PNAME_NS-TERMINAL :RESULT (or (pname2iri $0 "") (ll-parser-failure "Unbound prefix ~A" $0))))) 
	 (BlankNode ::= (:OR (BLANK_NODE_LABEL-TERMINAL :RESULT (make-named-blank-node-or-var $0)) (ANON-TERMINAL :RESULT (generate-anonymous-blank-node-or-var))))
	 ))
      (setf (ll-parser-lexer parser) lexer)
      (setf (ll-parser-subscribe parser) subscribe)
      (setf (ll-parser-store-tokens-p parser) t)
      parser)))

