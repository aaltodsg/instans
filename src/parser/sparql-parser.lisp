;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Todo

(defun handle-aggregates (ggp clauses)
  (labels ((level-has-aggregates-p (clauses)
	     (let ((project (getf clauses :project))
		   (having (getf clauses :having))
		   (order-by (getf clauses :order-by)))
	       (or (and (consp project)
			(some #'(lambda (ve) (and (consp ve) (eq (first ve) 'AS) (contains-aggregates-p (third ve)))) project))
		   (some #'contains-aggregates-p having)
		   (some #'(lambda (oc) (contains-aggregates-p (second oc))) order-by))))
	   (contains-aggregates-p (form)
	     (cond ((not (consp form)) nil)
		   ((member (first form) '(COUNT SUM MIN MAX AVG GROUP_CONCAT SAMPLE)) t)
		   (t (some #'contains-aggregates-p (rest form))))))
    (let* ((has-aggregates-p (level-has-aggregates-p clauses))
	   (group-by (getf clauses :group-by))
					;	 (dataset (getf clauses :dataset))
					;	 (having (getf clauses :having))
	   (group (cond ((not (null group-by)) (list 'GROUP group-by ggp))
			(has-aggregates-p (list 'GROUP '(1) ggp)))))
      (when group
	(parsing-failure "Aggregation not implemented yet!")
	;;       (let ((aggregates (list nil))
	;; 	    (expr-var-list (list nil)))
	;; 	(when (consp project)
	;; 	  (setf project (loop for ve in project
	;; 			      collect (cond ((eq (first ve) 'AS)
	;; 					     (destructuring-bind (as var expr) ve
	;; 					       (list as var (replace-aggregates expr aggregates expr-var-list group))))
	;; 					    (t ve)))))
	;; 	(setf having (loop for expr in having collect (replace-aggregates expr aggegrates expr-var-list group)))
	;; 	(setf order-by (loop for (order expr) in order-by collect (list order (replace-aggregates expr aggregates expr-var-list group))))
	;; 	(loop for expr in having do (setf ggp (list 'FILTER expr ggp)))
	;; ;kesken	(loop 
	;; 	(setf ggp (cons 'AGGREGATE-JOIN (cdr (reverse aggregates))))
	)
      ggp)))

(defun build-query-expression (clauses)
  (labels ((wrap-solution-modifiers (ggp clauses)
	     (let ((distinct (getf clauses :distinct))
		   (reduced (getf clauses :reduced))
		   (project (getf clauses :project))
		   (order-by (getf clauses :order-by))
		   (limit (getf clauses :limit))
		   (offset (getf clauses :offset)))
	       (setf ggp (list 'TO-LIST ggp))
	       (when order-by (setf ggp (list 'ORDER ggp order-by)))
	       (when project (setf ggp (list 'PROJECT ggp project)))
	       (when distinct (setf ggp (list 'DISTINCT ggp)))
	       (when reduced (setf ggp (list 'REDUCED ggp)))
	       (when (or limit offset) (setf ggp (list 'slice ggp limit offset)))
	       ggp))
	   (get-existing-property-and-value (property list)
	     (if (find property list) (list property (getf list property))))
	   (get-existing-properties-and-values (properties list)
	     (apply #'append (mapcar #'(lambda (property) (get-existing-property-and-value property list)) properties))))
    (let ((form (getf clauses :query-form)))
      (case form
	(SELECT
	 (let ((ggp (getf clauses :where)))
	   (setf ggp (handle-aggregates ggp clauses))
	   (list form :where (wrap-solution-modifiers ggp clauses))))
	(CONSTRUCT
	 (let* ((template (getf clauses :construct-template))
		(ggp (if (find :where clauses) (getf clauses :where) template)))
	   (setf ggp (handle-aggregates ggp clauses))
	   (list form :where (wrap-solution-modifiers ggp clauses) :construct-template template)))
	(DESCRIBE (parsing-failure "DESCRIBE query not implemented yet"))
	(ASK (parsing-failure "ASK query not implemented yet"))
	(LOAD (parsing-failure "LOAD not implemented yet"))
	(CLEAR (parsing-failure "CLEAR not implemented yet"))
	(ADD (parsing-failure "ADD not implemented yet"))
	(MOVE (parsing-failure "MOVE not implemented yet"))
	(COPY (parsing-failure "COPY not implemented yet"))
	(INSERT-DATA (parsing-failure "INSERT not implemented yet"))
	(DELETE-DATA (parsing-failure "DELETE not implemented yet"))
	(DELETE-WHERE (parsing-failure "DELETE not implemented yet"))
	(DELETE-INSERT
	 (cons form (get-existing-properties-and-values '(:with :delete-clause :insert-clause :using :where) clauses)))
	))))

(defun make-sparql-parser (&optional testingp)
  (let* ((triples (list nil))
	 (triples-last triples)
	 (replace-blank-nodes-by-vars-p t)
	 (indent 0)
	 (lexer nil)
	 (instans nil)
	 (mode :query))
    (labels ((init () 
	       (setf instans (lexer-instans lexer))
	       (clear-triples))
	     (set-prefix (prefix-binding expansion) (rebind-prefix lexer prefix-binding expansion))
	     (set-base (b) (set-lexer-base lexer b) (values))
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
		      (let ((subj (generate-blank-node-or-var))
			    (rest (expand-collection (cdr col))))
			(emit subj *rdf-rest* rest)
			(emit subj *rdf-first* (car col))
			subj))))
	     (make-var (name) (make-sparql-var instans name))
	     (generate-var (name) (make-sparql-var instans name))
	     (generate-blank-node-or-var ()
	       (if replace-blank-nodes-by-vars-p (generate-sparql-var instans "!BLANK") (generate-rdf-blank-node instans)))
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
				      (objs (nconc (loop repeat (1- (length preds)) collect (generate-var "!PATH")) (list (third triple)))))
				 (loop for pred in preds
				       for subj = (first triple) then obj
				       for obj in objs
				       nconc (translate-path (list subj pred obj)))))
			      (t
			       (list (cons 'PATH triple)))))
		       ))
		 (decf indent)
		 v))
	     (translate-paths (triple-list)
	       (loop for triple in triple-list
		     nconc (translate-path triple)))
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
		  (list 'SERVICE (second e) (third e) (simplify-joins (fourth e))))
		 ((MINUS UNION)
		  (list (first e) (mapcar #'simplify-joins (rest e))))
		 (t e)))
	     (translate-group-graph-pattern (ggp)
	       (loop with fs = nil
		     with g = '(ZERO-PATTERN)
		     for e in ggp
		     do (case (first e)
			  (OPTIONAL
			   (let ((a (second e)))
			     (setf g (cond ((eq (first a) 'FILTER) (list 'LEFTJOIN g (second a) (third a)))
					   (t (list 'LEFTJOIN g a t))))))
			  (MINUS
			   (setf g (list 'MINUS g (second e))))
			  (BIND
			   (setf g (list 'EXTEND g (third e) (second e))))
			  (FILTER
			   (setf fs (if (null fs) (second e) (create-sparql-call "logical-and" fs (second e)))))
			  (t
			   (setf g (list 'JOIN g e))))
		     finally (progn
			       (setf g (simplify-joins g))
			       (when (not (null fs))
				 (setf g (list 'FILTER fs g)))
			       (return g))))
	     (create-call (op-name &rest args)
	       (or (apply #' create-sparql-call op-name args)
		   (parsing-failure "~A does not name a Sparql function or form" op-name)))
	     (create-call-through-iri (iri arglist)
	       (let ((sparql-op (find-sparql-op (rdf-iri-string iri))))
		 (cond ((null sparql-op)
			(parsing-failure "~A does not name a Sparql function or form" iri))
		       (t
			(inform "create-call-through-iri ~A ~A" sparql-op arglist)
			(cons sparql-op arglist))))))
      (with-ll1-parser (sparql-parser)
	  ;; ((a ::= (:rep1 b)))
	  ((Rules ::= (:OR (Tests :RESULT (if (eq mode :testing) $0 (parsing-failure "Not in test mode")))
			   (Prologue (:OPT ((:OR Query1 Update1) ((:OPT (|;-TERMINAL| Rules :RESULT $1)) :RESULT (opt-value $0)) :RESULT (cons $0 $1)))
				     :RESULT (if (eq mode :testing) (parsing-failure "Test mode does not accept queries") (append $0 (opt-value $1))))))
	   (Tests ::= (TESTS-TERMINAL (:REP0 Test) :RESULT $1))
	   (Test  ::= (Prologue ASSERT-TERMINAL (:OPT String) AssertTarget :RESULT (append (list 'ASSERT :name (opt-value $2)) $3)))
	   (AssertTarget ::= (:OR ((:OPT FILTER-TERMINAL) Constraint INPUT-TERMINAL DataBlock OUTPUT-TERMINAL ExpectedValues
				   :RESULT (list (if (opt-yes-p $0) :filter :expression) $1 :input $3 :output $5))
				  (SelectQuery ValuesClause TRIPLES-TERMINAL |{-TERMINAL| TriplesBlockNoVars |}-TERMINAL| SOLUTIONS-TERMINAL DataBlock
					       :RESULT (list :select (build-query-expression (append $0 (opt-value $1))) :triples $4 :solutions $7))))
	   ;; (Test  ::= (Prologue ASSERT-TERMINAL (:OPT String) AssertTarget ExpectedValues ValuesClause :RESULT (cons 'ASSERT (funcall $3 (opt-value $2) (opt-value $5) $4))))
	   ;; (AssertTarget ::= (:OR (FILTER-TERMINAL Constraint :RESULT #'(lambda (name values expect) (append (list :name name :filter $1 :expect expect) values)))
	   ;; 			  (Constraint :RESULT #'(lambda (name values expect) (append (list :name name :expression $0 :expect expect) values)))))
	   (ExpectedValues ::= (:OR (ExpectedValue :RESULT (list $0)) (|{-TERMINAL| (:REP0 ExpectedValue) |}-TERMINAL| :RESULT $1)))
	   (ExpectedValue ::= (:OR ErrorValue DataBlockValue))
	   (ErrorValue ::= (ERROR-TERMINAL (:OPT (|(-TERMINAL| String (:REP0 DataBlockValue) |)-TERMINAL| :RESULT (make-instance 'sparql-error :format $1 :arguments $2)))
					   :RESULT (if (opt-yes-p $1) (opt-value $1) (make-instance 'sparql-error))))
	   (TriplesBlockNoVars ::= ((:RESULT (setf replace-blank-nodes-by-vars-p nil)) TriplesBlock :RESULT (progn (setf replace-blank-nodes-by-vars-p t) $1)))
	   ;; (QueryUnit ::= Query)
	   ;; (Query ::= (Prologue (:OR SelectQuery ConstructQuery DescribeQuery AskQuery) ValuesClause))
	   ;; (UpdateUnit ::= Update)
	   (Query1 ::= ((:OR SelectQuery ConstructQuery DescribeQuery AskQuery) ValuesClause :RESULT (build-query-expression (append $0 (opt-value $1)))))
	   (Prologue ::= (:REP0 (:OR BaseDecl PrefixDecl)))
	   (BaseDecl ::= (BASE-TERMINAL IRIREF-TERMINAL :RESULT (progn (set-base $1) (list 'BASE $1))))
	   (PrefixDecl ::= (PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL :RESULT (progn (set-prefix $1 $2) (list 'PREFIX (car $1) $2))))
	   (SelectQuery ::= (SelectClause ((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0))) WhereClause SolutionModifier
					  :RESULT (append '(:query-form SELECT) $0 $1 $2 $3)))
	   (SubSelect ::= (SelectClause WhereClause SolutionModifier ValuesClause
					:RESULT (build-query-expression (append '(:query-form SELECT) $0 $1 $2 (opt-value $3)))))
					; `(SELECT ,@$0 :where ,$1 ,@$2 ,@(if (opt-yes-p $3) (opt-value $3)))))
	   (SelectClause ::= (SELECT-TERMINAL ((:OPT (:OR (DISTINCT-TERMINAL :RESULT :distinct) (REDUCED-TERMINAL :RESULT :reduced)))
					       :RESULT (if (opt-yes-p $0) (list (opt-value $0) t)))
					      (:OR (:REP1 (:OR Var (|(-TERMINAL| Expression AS-TERMINAL Var |)-TERMINAL| :RESULT (list 'AS $3 $1))))
						   (|*-TERMINAL| :RESULT '*))
					      :RESULT (append $1 (list :project $2))))
	   (ConstructQuery ::= (CONSTRUCT-TERMINAL (:OR ((ConstructTemplate :RESULT (list :construct-template $0)) ((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0))) WhereClause SolutionModifier 
							 :RESULT (append '(:query-form CONSTRUCT) $0 $1 $2 $3))
							(((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0)))
							 WHERE-TERMINAL |{-TERMINAL| (:OPT (TriplesTemplate :RESULT (list :construct-template $0))) |}-TERMINAL| SolutionModifier
							 :RESULT (append '(:query-form CONSTRUCT) $0 (opt-value $3) $5)))
						   :RESULT $1))
	   (DescribeQuery ::= (DESCRIBE-TERMINAL (:OR (:REP1 VarOrIri) (|*-TERMINAL| :RESULT '*)) ((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0)))
						 (:OPT WhereClause) SolutionModifier
						 :RESULT (append '(:query-form DESCRIBE) (list :target $1) $2 (opt-value $3) $4)))
	   (AskQuery ::= (ASK-TERMINAL ((:REP0 DatasetClause) :RESULT (and $0 (list :dataset $0))) WhereClause SolutionModifier
				       :RESULT (append '(:query-form ASK) $1 $2 $3)))
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
				    (|(-TERMINAL| Expression (:OPT (AS-TERMINAL Var :RESULT $1)) |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'AS $2 $1) $1)) Var))
	   (HavingClause ::= (HAVING-TERMINAL (:REP1 HavingCondition) :RESULT (list :having $1)))
	   (HavingCondition ::= Constraint)
	   (OrderClause ::= (ORDER-TERMINAL BY-TERMINAL (:REP1 OrderCondition) :RESULT (list :order-by $2)))
	   (OrderCondition ::= (:OR ((ASC-TERMINAL :RESULT 'ASC) (DESC-TERMINAL :RESULT 'DESC) BrackettedExpression :RESULT (list $0 $1))
				    (Constraint :RESULT (list 'ASC $0))
				    (Var :RESULT (list 'ASC $0))))
	   (LimitOffsetClauses ::= (:OR (LimitClause (:OPT OffsetClause) :RESULT (append (list :limit $0) (and (opt-yes-p $1) (list :offset (opt-value $1)))))
					(OffsetClause (:OPT LimitClause) :RESULT (append (list :offset $0) (and (opt-yes-p $1) (list :limit (opt-value $1)))))))
	   (LimitClause ::= (LIMIT-TERMINAL INTEGER-TERMINAL :RESULT $1))
	   (OffsetClause ::= (OFFSET-TERMINAL INTEGER-TERMINAL :RESULT $1))
	   (ValuesClause ::= (:OPT (VALUES-TERMINAL DataBlock :RESULT (list :datablock $1))))
	   ;; (Update ::= (Prologue (:OPT (Update1 (:OPT (|;-TERMINAL| Update))))))
	   (Update1 ::= (:OR Load Clear Drop Add Move Copy Create InsertData DeleteData DeleteWhere Modify) :RESULT (build-query-expression $0))
	   (_OptSilent ::= (:OPT SILENT-TERMINAL) :RESULT (if (opt-yes-p $0) (list :silent t)))
	   (Load ::= (LOAD-TERMINAL _OptSilent iri (:OPT (INTO-TERMINAL GraphRef :RESULT $1)) :RESULT (append '(:query-form LOAD) $1 (list :from $2) (if (opt-yes-p $3) (list :to (opt-value $3))))))
	   (Clear ::= (CLEAR-TERMINAL _OptSilent GraphRefAll :RESULT (append '(:query-form CLEAR) $1 $2)))
	   (Drop ::= (DROP-TERMINAL _OptSilent GraphRefAll :RESULT (append '(:query-form DROP) $1 $2)))
	   (Create ::= (CREATE-TERMINAL _OptSilent GraphRef :RESULT (append '(:query-form CREATE) $1 $2)))
	   (Add ::= (ADD-TERMINAL _OptSilent GraphOrDefault TO-TERMINAL GraphOrDefault :RESULT (append '(:query-form ADD) $1 (list :graph $2 :to $4))))
	   (Move ::= (MOVE-TERMINAL _OptSilent GraphOrDefault TO-TERMINAL GraphOrDefault :RESULT (append '(:query-form MOVE) $1 (list :graph $2 :to $4))))
	   (Copy ::= (COPY-TERMINAL _OptSilent GraphOrDefault TO-TERMINAL GraphOrDefault :RESULT (append '(:query-form COPY) $1 (list :graph $2 :to $4))))
	   (InsertData ::= (INSERT-DATA-TERMINAL QuadData :RESULT (list 'INSERT-DATA $1)))
	   (DeleteData ::= (DELETE-DATA-TERMINAL QuadData :RESULT (list 'DELETE-DATA $1)))
	   (DeleteWhere ::= (DELETE-WHERE-TERMINAL QuadPattern :RESULT (list 'DELETE-WHERE $1)))
	   (Modify ::= (((:OPT (WITH-TERMINAL iri :RESULT $1)) :RESULT (if (opt-yes-p $0) (list :with (opt-value $0))))
			(:OR (DeleteClause (:OPT InsertClause) :RESULT (append $0 (opt-value $1)))
			     (InsertClause))
			((:REP0 UsingClause) :RESULT (if $0 (cons :using $0)))
			(WHERE-TERMINAL GroupGraphPattern :result (list :where $1))
			:RESULT (append '(:query-form DELETE-INSERT) $0 $1 $2 $3)))
	   (DeleteClause ::= (DELETE-TERMINAL QuadPattern :RESULT (list :delete-clause $1)))
	   (InsertClause ::= (INSERT-TERMINAL QuadPattern :RESULT (list :insert-clause $1)))
	   (UsingClause ::= (USING-TERMINAL (:OR iri (NAMED-TERMINAL iri :RESULT $1))))
	   (GraphOrDefault ::= (:OR (DEFAULT-TERMINAL :RESULT :default) ((:OPT GRAPH-TERMINAL) iri :RESULT $1)))
	   (GraphRef ::= (GRAPH-TERMINAL iri :RESULT (list :graph $1)))
	   (GraphRefAll ::= (:OR GraphRef (DEFAULT-TERMINAL :RESULT :default) (NAMED-TERMINAL :RESULT :named) (ALL-TERMINAL :RESULT :all)) :RESULT (list :graph $0))
	   (QuadPattern ::= (|{-TERMINAL| Quads |}-TERMINAL| :RESULT $1))
	   (QuadData ::= (|{-TERMINAL| Quads |}-TERMINAL| :RESULT $1))
	   (Quads ::= (((:OPT TriplesTemplate) :RESULT (if (opt-yes-p $0) (cons 'BGP (get-triples))))
		       (:REP0 (QuadsNotTriples (:OPT |.-TERMINAL|) ((:OPT TriplesTemplate) :RESULT (if (opt-yes-p $0) (cons 'BGP (get-triples)))) :RESULT (append $0 $2)))
		       :RESULT (apply #'append $0 $1)))
	   (QuadsNotTriples ::= (GRAPH-TERMINAL VarOrIri |{-TERMINAL| (:OPT TriplesTemplate) |}-TERMINAL| :RESULT (list 'GRAPH $1 (cons 'BGP (get-triples)))))
	   (TriplesTemplate ::= (TriplesSameSubject (:OPT (|.-TERMINAL| (:OPT TriplesTemplate)))))
	   (GroupGraphPattern ::= (|{-TERMINAL| (:OR SubSelect GroupGraphPatternSub) |}-TERMINAL| :RESULT $1))
	   (GroupGraphPatternSub ::= ((:OPT TriplesBlock) ((:REP0 (GraphPatternNotTriples (:OPT |.-TERMINAL|) (:OPT TriplesBlock)
											  :RESULT (cons $0 (and (opt-yes-p $2) (list (opt-value $2))))))
							   :RESULT (apply #'append $0))
				      :RESULT (translate-group-graph-pattern (if (opt-yes-p $0) (cons (opt-value $0) $1) $1))))
	   (TriplesBlock ::= (TriplesBlockInner :RESULT (cons 'BGP (get-triples))))
	   (TriplesBlockInner ::= (TriplesSameSubjectPath (:OPT (|.-TERMINAL| (:OPT TriplesBlockInner)))))
	   (GraphPatternNotTriples ::= (:OR GroupOrUnionGraphPattern OptionalGraphPattern MinusGraphPattern GraphGraphPattern ServiceGraphPattern Filter Bind InlineData))
	   (OptionalGraphPattern ::= (OPTIONAL-TERMINAL GroupGraphPattern) :RESULT (list 'OPTIONAL $1))
	   (GraphGraphPattern ::= (GRAPH-TERMINAL VarOrIri GroupGraphPattern :RESULT (list 'GRAPH $1 $2)))
	   (ServiceGraphPattern ::= (SERVICE-TERMINAL _OptSilent VarOrIri GroupGraphPattern :RESULT (append '(:query-form SERVICE) $1 (list :endpoint $2) (list :pattern $3))))
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
	   (ConstructTemplate ::= (|{-TERMINAL| (:OPT ConstructTriples) |}-TERMINAL| :RESULT (cons 'BGP (get-triples))))
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
	   (BlankNodePropertyList ::= (|[-TERMINAL| PropertyListNotEmpty |]-TERMINAL| :RESULT (emit-subj-pred-obj-list (generate-blank-node-or-var) $1)))
	   (TriplesNodePath ::= (:OR CollectionPath BlankNodePropertyListPath))
	   (BlankNodePropertyListPath ::= (|[-TERMINAL| PropertyListPathNotEmpty |]-TERMINAL| :RESULT (emit-subj-pred-obj-list (generate-blank-node-or-var) $1)))
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
								   (IN-TERMINAL ExpressionList :RESULT #'(lambda (a) (list 'create-call "in" a $1)))
								   (NOT-TERMINAL IN-TERMINAL ExpressionList :RESULT #'(lambda (a) (create-call "not-in" a $1))))))
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
				 (IRI-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (make-instance 'rdf-iri :string $2))
				 (URI-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (make-instance 'rdf-iri :string $2))
				 (BNODE-TERMINAL (:OR (|(-TERMINAL| Expression |)-TERMINAL| :RESULT $1) (NIL-TERMINAL :RESULT (progn nil))) :RESULT (create-call $0 $1))
				 (RAND-TERMINAL NIL-TERMINAL :RESULT (create-call $0))
				 (ABS-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
				 (CEIL-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
				 (FLOOR-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
				 (ROUND-TERMINAL |(-TERMINAL| Expression |)-TERMINAL| :RESULT (create-call $0 $2))
				 (CONCAT-TERMINAL ExpressionList :RESULT (create-call $0 $1))
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
				 (COALESCE-TERMINAL ExpressionList :RESULT (create-call $0 $1))
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
					       :RESULT (if (opt-yes-p $2) (list 'COUNT :distinct t $3) (list 'COUNT $3)))
			       (SUM-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'SUM :distinct t $3) (list 'SUM $3)))
			       (MIN-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'MIN :distinct t $3) (list 'MIN $3)))
			       (MAX-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'MAX :distinct t $3) (list 'MAX $3)))
			       (AVG-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'AVG :distinct t $3) (list 'AVG $3)))
			       (SAMPLE-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression |)-TERMINAL| :RESULT (if (opt-yes-p $2) (list 'SAMPLE :distinct t $3) (list 'SAMPLE $3)))
			       (GROUP_CONCAT-TERMINAL |(-TERMINAL| (:OPT DISTINCT-TERMINAL) Expression
						      (:OPT (|;-TERMINAL| SEPARATOR-TERMINAL |=-TERMINAL| String :RESULT $3)) |)-TERMINAL|
						      :RESULT (if (opt-yes-p $2) (list 'GROUP_CONCAT :distinct t $3 (opt-value $4)) (list 'GROUP_CONCAT $3 (opt-value $4))))))
	   (iriOrFunction ::= (iri (:OPT ArgList) :RESULT (if (opt-no-p $1) $0 (create-call-through-iri $0 (opt-value $1)))))
	   (RDFLiteral		  ::= (String (:OPT (:OR (LANGTAG-TERMINAL :RESULT #'(lambda (s) (create-rdf-literal-with-lang s (subseq $0 1))))
	   						 (^^-TERMINAL iri :RESULT #'(lambda (s) (create-rdf-literal-with-type s $1)))))
	   				      :RESULT (if (opt-yes-p $1) (funcall (opt-value $1) $0) $0)))
	   (NumericLiteral ::= (:OR NumericLiteralUnsigned NumericLiteralPositive NumericLiteralNegative))
	   (NumericLiteralUnsigned ::= (:OR INTEGER-TERMINAL DECIMAL-TERMINAL DOUBLE-TERMINAL))
	   (NumericLiteralPositive ::= (:OR INTEGER_POSITIVE-TERMINAL DECIMAL_POSITIVE-TERMINAL DOUBLE_POSITIVE-TERMINAL))
	   (NumericLiteralNegative ::= (:OR INTEGER_NEGATIVE-TERMINAL DECIMAL_NEGATIVE-TERMINAL DOUBLE_NEGATIVE-TERMINAL))
	   (BooleanLiteral ::= (:OR (TRUE-TERMINAL :RESULT t) (FALSE-TERMINAL :RESULT (progn nil))))
	   (String ::= (:OR STRING_LITERAL1-TERMINAL STRING_LITERAL2-TERMINAL STRING_LITERAL_LONG1-TERMINAL STRING_LITERAL_LONG2-TERMINAL))
	   (iri ::= (:OR IRIREF-TERMINAL PrefixedName))
	   (PrefixedName ::= (:OR (PNAME_LN-TERMINAL :RESULT (pname-to-iri lexer (first $0) (second $0)))
				  (PNAME_NS-TERMINAL :RESULT (or (second $0) (parsing-failure "Unbound prefix ~A" (first $0))))))
	   (BlankNode ::= (:OR (BLANK_NODE_LABEL-TERMINAL :RESULT (make-var (concatenate 'string "!BLANK_" $0))) (ANON-TERMINAL :RESULT (generate-blank-node-or-var))))
	   )
	#'(lambda (sparql-lexer &rest keys &key &allow-other-keys)
	    (setf lexer sparql-lexer)
	    (init)
	    (setf mode (if testingp :testing :query))
	    (apply #'sparql-parser lexer keys))))))

(defun sparql-parse-file (instans query-file &rest keys &key show-parse-p (newline-positions (list nil)) test-mode-p)
  (declare (ignorable show-parse-p newline-positions test-mode-p))
  (with-open-file (input-stream query-file)
    (apply #'sparql-parse-stream instans input-stream keys)))

(defun sparql-parse-stream (instans input-stream &key base show-parse-p (newline-positions (list nil)) test-mode-p)
  (declare (ignorable show-parse-p))
  (let* ((lexer (make-instance 'sparql-lexer :instans instans :input-stream input-stream :newline-positions newline-positions :base base))
	 (parser (make-sparql-parser test-mode-p)))
    (funcall parser lexer :show-parse-p show-parse-p)))

(defun sparql-parse-files (instans directory-path &key show-parse-p print-input-p print-result-p test-mode-p)
  (loop for file in (directory directory-path)
	for newline-positions = (list nil)
        do (progn
	     (inform "File ~A:" file)
	     (when print-input-p
	       (with-open-file (input file)
		 (loop for line = (read-line input nil nil)
		       while line
		       do (inform "~A" line))))
	     (let ((result (sparql-parse-file instans file :show-parse-p show-parse-p :newline-positions newline-positions :test-mode-p test-mode-p)))
	       (cond ((not (parsing-succeeded-p result))
		      (inform "~A:~A" file (parsing-error-message result)))
		     (print-result-p
		      (loop for item in (car (parsing-result-stack result))
			    do (inform "~S" item))))))))

