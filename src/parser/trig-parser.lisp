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
  `(defun ,creator-function (instans input-stream &key base graph subscribe triple-callback block-callback document-callback)
     ,@(if (eq input-type :trig) `((when graph (warn "Ignoring graph with Trig input"))) `((declare (ignorable graph))))
     (when (null base) (setf base (parse-iri "http://")))
     (let* ((lexer (make-instance ',(case input-type (:trig 'trig-lexer) (:turtle 'turtle-lexer))
				  :input-stream input-stream :instans instans :base base :show-parses-p subscribe))
	    (current-subject nil)
	    (current-predicate nil)
	    ,@(if (eq input-type :trig)
		  '((current-graph nil)))
	    (block-triples (list nil))
	    (block-triples-last block-triples)
	    (document-triples (list nil))
	    (document-triples-last document-triples)
	    (parser nil))
       (labels ((set-prefix (prefix-binding expansion) (rebind-prefix lexer prefix-binding expansion))
		(set-base (b) (set-lexer-base lexer b))
		(pname2iri (prefix-binding suffix-string)
		  (or (pname-to-iri lexer prefix-binding suffix-string) (ll-parser-failure "Cannot transform ~A and ~A to an IRI" prefix-binding suffix-string)))
		(make-blank (name) (make-rdf-blank-node instans name))
		(generate-blank () (generate-rdf-blank-node instans))
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
;		  (inform "emit ~S ~S ~S" current-subject current-predicate current-object)
		  (let ((new ,(if (eq input-type :trig)
				  '(list current-subject current-predicate current-object current-graph)
				  '(list current-subject current-predicate current-object))))
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
						  :RESULT (when block-callback (funcall block-callback (get-block-triples)))))
			 (triplesOrGraph     ::= (labelOrSubject (:OR wrappedGraph ((:RESULT (set-graph nil)) predicateObjectList .-TERMINAL))))
			 (triples2           ::= (:OR (blankNodePropertyList (:OPT predicateObjectList) .-TERMINAL) (collection predicateObjectList .-TERMINAL)))
			 (wrappedGraph       ::= (|{-TERMINAL| (:OPT triplesBlock) |}-TERMINAL|))
			 (triplesBlock       ::= (triples (:OPT .-TERMINAL (:OPT triplesBlock))))
			 (labelOrSubject     ::= (:OR iri BlankNode) :RESULT (progn (set-graph $0) (set-subject $0))))
		       '((turtleDoc	     ::= ((:REP0 statement) :RESULT (when document-callback (funcall document-callback (get-document-triples)))))
			 (statement	     ::= (:OR directive (triples .-TERMINAL)))))
		 (directive	             ::= (:OR prefixID base sparqlPrefix sparqlBase))
		 (prefixID		     ::= (@PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL .-TERMINAL :RESULT (set-prefix $1 $2)))
		 (base		             ::= (@BASE-TERMINAL (IRIREF-TERMINAL :RESULT (progn (set-base $0))) .-TERMINAL))
		 (sparqlBase	             ::= (BASE-TERMINAL IRIREF-TERMINAL :RESULT (set-base $1)))
		 (sparqlPrefix	             ::= (PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL :RESULT (set-prefix $1 $2)))
		 (triples		     ::= (:OR (subject predicateObjectList)
						      (blankNodePropertyList (:OPT predicateObjectList)))
					     ,@(if (eq input-type :turtle) '(:RESULT (when block-callback (funcall block-callback (get-block-triples))))))
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
		 (BooleanLiteral	     ::= (:OR (TRUE-TERMINAL :RESULT t) (FALSE-TERMINAL :RESULT nil)))
		 (String		     ::= (:OR STRING_LITERAL_QUOTE-TERMINAL STRING_LITERAL_SINGLE_QUOTE-TERMINAL
						      STRING_LITERAL_LONG_SINGLE_QUOTE-TERMINAL STRING_LITERAL_LONG_QUOTE-TERMINAL))
		 (iri		             ::= (:OR IRIREF-TERMINAL PrefixedName))
		 (PrefixedName	             ::= (:OR (PNAME_LN-TERMINAL :RESULT (pname2iri (first $0) (second $0)))
						      (PNAME_NS-TERMINAL :RESULT (or (second $0) (ll-parser-failure "Unbound prefix ~A" (first $0))))))
		 (BlankNode	             ::= (:OR (BLANK_NODE_LABEL-TERMINAL :RESULT (make-blank $0)) (ANON-TERMINAL :RESULT (generate-blank))))))
	 (setf (ll-parser-lexer parser) lexer)
	 (setf (ll-parser-subscribe parser) subscribe)
	 parser))))

(define-trig-or-turtle-parser make-turtle-parser :turtle)

(define-trig-or-turtle-parser make-trig-parser :trig)

