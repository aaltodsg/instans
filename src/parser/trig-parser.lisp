;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defvar *triple-count* 0)
;; (defvar *triple-sizes* 0)

(defun term-size (x)
  (cond ((consp x)
	 (+ (term-size (car x)) (term-size (cdr x))))
	((stringp x)
	 (length x))
	(t 1)))

(defun triple-size (tr)
  (loop for x in tr
	sum (term-size x)))

(defmacro define-trig-or-turtle-parser (creator-function input-type)
  (unless (member input-type '(:trig :turtle)) (error* "Unknown triple input type ~S" input-type))
  `(defun ,creator-function (instans input-stream &key base graph subscribe triple-callback block-callback document-callback prefix-callback)
     ,@(if (eq input-type :trig) `((when graph (warn "Ignoring graph with Trig input"))) `((declare (ignorable graph))))
     (when (null base) (setf base (parse-iri "http://")))
     (let* ((lexer (make-instance ',(case input-type (:trig 'trig-lexer) (:turtle 'turtle-lexer))
				  :input-stream input-stream :instans instans :base base :show-parses-p subscribe))
	    (current-subject nil)
	    (current-predicate nil)
	    ;; ,@(if (eq input-type :trig)
	    ;; 	  '((current-graph nil)))
	    (current-graph ,(if (eq input-type :trig) nil 'graph))
	    (block-triples (list nil))
	    (block-triples-last block-triples)
	    (document-triples (list nil))
	    (document-triples-last document-triples)
	    (parser nil))
       (labels ((set-prefix (prefix-binding expansion)
		  (bind-prefix lexer prefix-binding expansion)
		  (when prefix-callback (funcall prefix-callback prefix-binding expansion)))
		(set-base (b)
		  (set-lexer-base lexer b)
		  (when prefix-callback (funcall prefix-callback "BASE" b)))
		(pname2iri (prefix suffix)
		  (or (pname-to-iri lexer prefix suffix) (ll-parser-failure "Unbound prefix ~S" prefix)))
		(make-blank (name) (make-named-blank-node instans name))
		(generate-blank () (generate-anonymous-blank-node instans))
		(clear-block-triples ()
		  (when block-callback
		    (setf block-triples (list nil))
		    (setf block-triples-last block-triples)))
		(get-block-triples ()
		  (when block-callback
		    (prog1 (cdr block-triples)
		      (clear-block-triples))))
		(clear-document-triples ()
		  (when document-callback
		    (setf document-triples (list nil))
		    (setf document-triples-last document-triples)))
		(get-document-triples ()
		  (when document-callback
		    (prog1 (cdr document-triples)
		      (clear-document-triples))))
		(set-subject (s) (setf current-subject s))
		(set-predicate (p) (setf current-predicate p))
		,@(if (eq input-type :trig) '((set-graph (g) (setf current-graph g))))
		(emit-current (current-object)
		  (incf *triple-count*)
		  ;; (incf *triple-sizes* (+ (term-size current-subject) (term-size current-predicate) (term-size current-object)))
		  (when (debugp subscribe :triples)
		    (inform "emit ~S ~S ~S ~S~%" current-subject current-predicate current-object current-graph))
		  ;; (let ((new ,(if (eq input-type :trig)
		  ;; 		  '(list current-subject current-predicate current-object current-graph)
		  ;; 		  '(list current-subject current-predicate current-object))))
		  (let ((new (list current-subject current-predicate current-object current-graph)))
		    (when block-callback
		      (setf (cdr block-triples-last) (list new))
		      (setf block-triples-last (cdr block-triples-last)))
		    (when document-callback
		      (setf (cdr document-triples-last) (list new))
		      (setf document-triples-last (cdr document-triples-last)))
		    (when triple-callback 
		      (apply triple-callback new)))))
	 (setf parser
	       (generate-ll1-parser triple ()
		 ,@(if (eq input-type :trig)
		       '((trigDoc	     ::= ((:REP0 (:OR directive block)) :RESULT (when document-callback (funcall document-callback (get-document-triples)))))
			 (block		     ::= ((:OR triplesOrGraph
						       ((:RESULT (set-graph nil)) wrappedGraph)
						       ((:RESULT (set-graph nil)) triples2)
						       (GRAPH-TERMINAL labelOrSubject wrappedGraph))
						  :RESULT (progn (when block-callback (funcall block-callback (get-block-triples))) (values))))
			 (triplesOrGraph     ::= (labelOrSubject (:OR wrappedGraph ((:RESULT (set-graph nil)) predicateObjectList .-TERMINAL))))
			 (triples2           ::= (:OR ((blankNodePropertyList :RESULT (set-subject $0)) (:OPT predicateObjectList) .-TERMINAL)
						      (collection predicateObjectList .-TERMINAL)))
			 (wrappedGraph       ::= (|{-TERMINAL| (:OPT triplesBlock) |}-TERMINAL|))
			 (triplesBlock       ::= (triples (:OPT .-TERMINAL (:OPT triplesBlock))))
			 (labelOrSubject     ::= (:OR iri BlankNode) :RESULT (progn (set-graph $0) (set-subject $0))))
		       '((turtleDoc	     ::= ((:REP0 statement) :RESULT (when document-callback (funcall document-callback (get-document-triples)))))
			 (statement	     ::= (:OR directive (triples .-TERMINAL)))))
		 (directive	             ::= ((:OR prefixID base sparqlPrefix sparqlBase) :RESULT (values)))
		 (prefixID		     ::= (@PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL .-TERMINAL :RESULT (set-prefix $1 $2)))
		 (base		             ::= (@BASE-TERMINAL (IRIREF-TERMINAL :RESULT (progn (set-base $0))) .-TERMINAL))
		 (sparqlBase	             ::= ((BASE-TERMINAL :RESULT (lexer-delay-iri-expansion lexer t)) IRIREF-TERMINAL :RESULT (progn (set-base $1) (lexer-delay-iri-expansion lexer nil))))
		 (sparqlPrefix	             ::= (PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL :RESULT (set-prefix $1 $2)))
		 (triples		     ::= (:OR (subject predicateObjectList)
						      ((blankNodePropertyList :RESULT (set-subject $0)) (:OPT predicateObjectList)))
					     ,@(if (eq input-type :turtle) '(:RESULT (progn (when block-callback (funcall block-callback (get-block-triples))) (values)))))
		 (predicateObjectList        ::= ((verb objectList) (:REP0 (|;-TERMINAL| (:OPT (verb objectList))))))
		 (objectList	             ::= (object (:REP0 (|,-TERMINAL| object))))
		 (verb		             ::= (:OR predicate (A-TERMINAL :RESULT *rdf-type*)) :RESULT (set-predicate $0))
		 (subject		     ::= (:OR iri BlankNode collection) :RESULT (set-subject $0))
		 (predicate	             ::= (iri))
		 (object		     ::= (:OR iri BlankNode collection blankNodePropertyList literal) :RESULT (emit-current $0))
		 (literal		     ::= (:OR RDFLiteral NumericLiteral BooleanLiteral))
		 (blankNodePropertyList      ::= ([-TERMINAL
						  (:RESULT (let ((b (generate-blank)))
							     (list current-subject current-predicate (set-subject b))))
						  predicateObjectList
						  ]-TERMINAL
						  :RESULT (progn (set-subject (first $1)) (set-predicate (second $1)) (third $1))))
		 (collection	             ::= (|(-TERMINAL|
						  (:OR (:RESULT *rdf-nil*)
						       ((:RESULT (let ((b (generate-blank)))
								   (prog1 (list current-subject current-predicate b)
								     (set-subject b)
								     (set-predicate *rdf-first*))))
							object
							(:REP0 ((:RESULT (let ((b (generate-blank)))
									   (set-predicate *rdf-rest*)
									   (emit-current b)
									   (set-subject b)
									   (set-predicate *rdf-first*)))
								object))
							:RESULT (progn (set-predicate *rdf-rest*) (emit-current *rdf-nil*)
								       (set-subject (first $0)) (set-predicate (second $0))
								       (third $0))))
						  |)-TERMINAL| :RESULT $1))
		 (NumericLiteral	     ::= (:OR INTEGER-TERMINAL DECIMAL-TERMINAL DOUBLE-TERMINAL))
		 (RDFLiteral	             ::= (String (:OPT (:OR (LANGTAG-TERMINAL :RESULT #'(lambda (s) (create-rdf-literal-with-lang s (subseq $0 1))))
								    (^^-TERMINAL iri :RESULT #'(lambda (s) (nth-value 0 (create-rdf-literal-with-type s $1))))))
						 :RESULT (if (opt-yes-p $1) (funcall (opt-value $1) $0) $0)))
		 (BooleanLiteral	     ::= (:OR (TRUE-TERMINAL :RESULT t) (FALSE-TERMINAL :RESULT (progn nil))))
		 (String		     ::= (:OR STRING_LITERAL_QUOTE-TERMINAL STRING_LITERAL_SINGLE_QUOTE-TERMINAL
						      STRING_LITERAL_LONG_SINGLE_QUOTE-TERMINAL STRING_LITERAL_LONG_QUOTE-TERMINAL))
		 (iri		             ::= (:OR IRIREF-TERMINAL PrefixedName))
		 (PrefixedName	             ::= (:OR (PNAME_LN-TERMINAL :RESULT (pname2iri (first $0) (second $0)))
						      (PNAME_NS-TERMINAL :RESULT (or (pname2iri $0 "") (ll-parser-failure "Unbound prefix ~A" $0)))))
		 (BlankNode	             ::= (:OR (BLANK_NODE_LABEL-TERMINAL :RESULT (make-blank $0)) (ANON-TERMINAL :RESULT (generate-blank))))))
	 (setf (ll-parser-lexer parser) lexer)
	 (setf (ll-parser-subscribe parser) subscribe)
	 parser))))

(defmacro define-trig-or-turtle-parser2 (creator-function input-type)
  (unless (member input-type '(:trig :turtle)) (error* "Unknown triple input type ~S" input-type))
  `(defun ,creator-function (instans input-stream &key base graph subscribe triple-callback block-callback document-callback prefix-callback)
     ,@(if (eq input-type :trig) `((when graph (warn "Ignoring graph with Trig input"))) `((declare (ignorable graph))))
     (when (null base) (setf base (parse-iri "http://")))
     (let* ((lexer (make-instance ',(case input-type (:trig 'trig-lexer) (:turtle 'turtle-lexer))
				  :input-stream input-stream :instans instans :base base :show-parses-p subscribe))
	    (current-subject nil)
	    (current-predicate nil)
	    ;; ,@(if (eq input-type :trig)
	    ;; 	  '((current-graph nil)))
	    (current-graph ,(if (eq input-type :trig) nil 'graph))
	    (block-triples (list nil))
	    (block-triples-last block-triples)
	    (document-triples (list nil))
	    (document-triples-last document-triples)
	    (parser nil))
       (labels ((set-prefix (prefix-binding expansion)
		  (bind-prefix lexer prefix-binding expansion)
		  (when prefix-callback (funcall prefix-callback prefix-binding expansion)))
		(set-base (b)
		  (set-lexer-base lexer b)
		  (when prefix-callback (funcall prefix-callback "BASE" b)))
		(pname2iri (prefix suffix)
		  (or (pname-to-iri lexer prefix suffix) (ll-parser-failure "Unbound prefix ~S" prefix)))
		(make-blank (name) (make-named-blank-node instans name))
		(generate-blank () (generate-anonymous-blank-node instans))
		(clear-block-triples ()
		  (when block-callback
		    (setf block-triples (list nil))
		    (setf block-triples-last block-triples)))
		(get-block-triples ()
		  (when block-callback
		    (prog1 (cdr block-triples)
		      (clear-block-triples))))
		(clear-document-triples ()
		  (when document-callback
		    (setf document-triples (list nil))
		    (setf document-triples-last document-triples)))
		(get-document-triples ()
		  (when document-callback
		    (prog1 (cdr document-triples)
		      (clear-document-triples))))
		(set-subject (s) (setf current-subject s))
		(set-predicate (p) (setf current-predicate p))
		,@(if (eq input-type :trig) '((set-graph (g) (setf current-graph g))))
		(emit-current (current-object)
		  (incf *triple-count*)
		  ;; (incf *triple-sizes* (+ (term-size current-subject) (term-size current-predicate) (term-size current-object)))
		  (when (debugp subscribe :triples)
		    (inform "emit ~S ~S ~S ~S~%" current-subject current-predicate current-object current-graph))
		  ;; (let ((new ,(if (eq input-type :trig)
		  ;; 		  '(list current-subject current-predicate current-object current-graph)
		  ;; 		  '(list current-subject current-predicate current-object))))
		  (let ((new (list current-subject current-predicate current-object current-graph)))
		    (when block-callback
		      (setf (cdr block-triples-last) (list new))
		      (setf block-triples-last (cdr block-triples-last)))
		    (when document-callback
		      (setf (cdr document-triples-last) (list new))
		      (setf document-triples-last (cdr document-triples-last)))
		    (when triple-callback 
		      (apply triple-callback new))
		    (values))))
	 (setf parser
	       (generate-ll1-parser triple ()
		 ,@(if (eq input-type :trig)
		       '((trigDoc	     ::= ((:REP0 (:OR directive block)) :ACTION (when document-callback (funcall document-callback (get-document-triples))))) ; Action
			 (block		     ::= ((:OR triplesOrGraph
						       ((:ACTION (set-graph nil)) wrappedGraph)
						       ((:ACTION (set-graph nil)) triples2)
						       (GRAPH-TERMINAL labelOrSubject wrappedGraph))
						  :ACTION (when block-callback (funcall block-callback (get-block-triples))))) ; Action
			 (triplesOrGraph     ::= ((labelOrSubject (:OR wrappedGraph ((:ACTION (set-graph nil)) predicateObjectList .-TERMINAL))))) ; NOOP
			 (triples2           ::= (:OR ((blankNodePropertyList :ACTION (set-subject $0)) (:OPT predicateObjectList) .-TERMINAL)
						  (collection predicateObjectList .-TERMINAL))) ; NOOP
			 (wrappedGraph       ::= (|{-TERMINAL| (:OPT triplesBlock) |}-TERMINAL|)) ; NOOP
			 (triplesBlock       ::= (triples (:OPT .-TERMINAL (:OPT triplesBlock)))) ; NOOP
			 (labelOrSubject     ::= (:OR iri BlankNode) :ACTION (progn (set-graph $0) (set-subject $0))))
		       '((turtleDoc	     ::= ((:REP0 statement) :ACTION (when document-callback (funcall document-callback (get-document-triples)))))
			 (statement	     ::= (:OR directive (triples .-TERMINAL))))) ; NOOP
		 (directive	             ::= (:OR prefixID base sparqlPrefix sparqlBase)) ; NOOP
		 (prefixID		     ::= (@PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL .-TERMINAL :ACTION (set-prefix $1 $2)))
		 (base		             ::= (@BASE-TERMINAL (IRIREF-TERMINAL :ACTION (progn (set-base $0))) .-TERMINAL))
		 (sparqlBase	             ::= ((BASE-TERMINAL :RESULT (lexer-delay-iri-expansion lexer t)) IRIREF-TERMINAL :ACTION (progn (set-base $1) (lexer-delay-iri-expansion lexer nil))))
		 (sparqlPrefix	             ::= (PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL :ACTION (set-prefix $1 $2)))
		 (triples		     ::= (:OR (subject predicateObjectList)
						      ((blankNodePropertyList :ACTION (set-subject $0)) (:OPT predicateObjectList))) ; NOOP
					         ,@(if (eq input-type :turtle) '(:ACTION (progn (when block-callback (funcall block-callback (get-block-triples)))))))
		 (predicateObjectList        ::= ((verb objectList) (:REP0 (|;-TERMINAL| (:OPT (verb objectList)))))); NOOP
		 (objectList	             ::= (object (:REP0 (|,-TERMINAL| object)))) ; NOOP
		 (verb		             ::= (:OR predicate (A-TERMINAL :RESULT *rdf-type*)) :ACTION (set-predicate $0))
		 (subject		     ::= (:OR iri BlankNode collection) :ACTION (set-subject $0))
		 (predicate	             ::= (iri) :RESULT)
		 (object		     ::= (:OR iri BlankNode collection blankNodePropertyList literal) :ACTION (emit-current $0))
		 (literal		     ::= (:OR RDFLiteral NumericLiteral BooleanLiteral) :RESULT)
		 (blankNodePropertyList      ::= ([-TERMINAL
						  (:RESULT (let ((b (generate-blank)))
							     (list current-subject current-predicate (set-subject b))))
						  predicateObjectList
						  ]-TERMINAL
						  :RESULT (progn (set-subject (first $1)) (set-predicate (second $1)) (third $1)))) ; Result needed
		 (collection	             ::= (|(-TERMINAL|
						  (:OR (:RESULT *rdf-nil*)
						       ((:RESULT (let ((b (generate-blank)))
								   (prog1 (list current-subject current-predicate b)
								     (set-subject b)
								     (set-predicate *rdf-first*))))
							object
							(:REP0 ((:RESULT (let ((b (generate-blank)))
									   (set-predicate *rdf-rest*)
									   (emit-current b)
									   (set-subject b)
									   (set-predicate *rdf-first*)))
								object))
							:RESULT (progn (set-predicate *rdf-rest*) (emit-current *rdf-nil*)
								       (set-subject (first $0)) (set-predicate (second $0))
								       (third $0))))
						  |)-TERMINAL| :RESULT $1)) ; Result needed
		 (NumericLiteral	     ::= (:OR INTEGER-TERMINAL DECIMAL-TERMINAL DOUBLE-TERMINAL) :RESULT) ; Result needed
		 (RDFLiteral	             ::= (String (:OPT (:OR (LANGTAG-TERMINAL :RESULT #'(lambda (s) (create-rdf-literal-with-lang s (subseq $0 1))))
								    (^^-TERMINAL iri :RESULT #'(lambda (s) (nth-value 0 (create-rdf-literal-with-type s $1))))))
							 :RESULT (if (opt-yes-p $1) (funcall (opt-value $1) $0) $0)))
		 (BooleanLiteral	     ::= (:OR (TRUE-TERMINAL :RESULT t) (FALSE-TERMINAL :RESULT nil)))
		 (String		     ::= (:OR STRING_LITERAL_QUOTE-TERMINAL STRING_LITERAL_SINGLE_QUOTE-TERMINAL
						      STRING_LITERAL_LONG_SINGLE_QUOTE-TERMINAL STRING_LITERAL_LONG_QUOTE-TERMINAL) :RESULT)
		 (iri		             ::= (:OR IRIREF-TERMINAL PrefixedName) :RESULT)
		 (PrefixedName	             ::= (:OR (PNAME_LN-TERMINAL :RESULT (pname2iri (first $0) (second $0)))
						      (PNAME_NS-TERMINAL :RESULT (or (pname2iri $0 "") (ll-parser-failure "Unbound prefix ~A" $0)))))
		 (BlankNode	             ::= (:OR (BLANK_NODE_LABEL-TERMINAL :RESULT (make-blank $0)) (ANON-TERMINAL :RESULT (generate-blank))))))
	 (setf (ll-parser-lexer parser) lexer)
	 (setf (ll-parser-subscribe parser) subscribe)
	 parser))))

;; (define-trig-or-turtle-parser2 make-turtle-parser :turtle)

;; (define-trig-or-turtle-parser2 make-trig-parser :trig)

(define-trig-or-turtle-parser make-turtle-parser :turtle)

(define-trig-or-turtle-parser make-trig-parser :trig)

