;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defvar *triple-count* 0)
(defvar *triple-sizes* 0)

(defun term-size (x)
  (cond ((consp x)
	 (+ (term-size (car x)) (term-size (cdr x))))
	((stringp x)
	 (length x))
	(t 1)))

(defun triple-size (tr)
  (loop for x in tr
	sum (term-size x)))


(defun make-turtle-parser (instans input-stream &key base subscribe triple-callback triples-block-callback)
  (when (null base) (setf base (parse-iri "http://")))
  (let* ((lexer (make-instance 'turtle-lexer :input-stream input-stream :instans instans :base base :show-parses-p subscribe))
	 (triples (list nil))
	 (triples-last triples)
	 (parser nil))
    (labels ((set-prefix (prefix-binding expansion) (rebind-prefix lexer prefix-binding expansion))
	     (set-base (b) (set-lexer-base lexer b) (values))
	     (pname2iri (prefix-binding suffix-string)
	       (or (pname-to-iri lexer prefix-binding suffix-string) (ll-parser-failure "Cannot transform ~A and ~A to an IRI" prefix-binding suffix-string)))
	     (make-blank (name) (make-rdf-blank-node instans name))
	     (generate-blank () (generate-rdf-blank-node instans))
	     (clear-triples ()
	       (setf triples (list nil))
	       (setf triples-last triples))
	     (get-triples ()
	       (prog1 (cdr triples)
		 (clear-triples)))
	     (emit (s p o)
	       (incf *triple-count*)
	       (incf *triple-sizes* (+ (term-size s) (term-size p) (term-size o)))
;	       (inform "emit ~S ~S ~S" s p o)
	       (setf (cdr triples-last) (list (list s p o)))
	       (setf triples-last (cdr triples-last))
	       (when triple-callback (funcall triple-callback s p o)))
	     (emit-subj-pred-obj-list (s pol)
;	       (inform "emit-subj-pred-obj-list ~S ~S" s pol)
	       (loop for (p . ol) in pol do (loop for o in ol do (emit s p o)))
	       s)
	     (expand-collection (col)
	       (cond ((null col) *rdf-nil*)
		     (t
		      (let ((subj (generate-blank))
			    (rest (expand-collection (cdr col))))
			(emit subj *rdf-rest* rest)
			(emit subj *rdf-first* (car col))
			subj)))))
      (setf parser
            (generate-ll1-parser turtle2
              (turtleDoc	     ::= ((:REP0 statement) :RESULT (values)))
              (statement	     ::= (:OR directive (triples .-TERMINAL :RESULT (values))))
              (directive	     ::= (:OR prefixID base sparqlPrefix sparqlBase) :RESULT (values))
              (prefixID		     ::= (@PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL .-TERMINAL :RESULT (set-prefix $1 $2)))
              (base		     ::= (@BASE-TERMINAL (IRIREF-TERMINAL :RESULT (progn (set-base $0))) .-TERMINAL))
              (sparqlBase	     ::= (BASE-TERMINAL IRIREF-TERMINAL :RESULT (set-base $1)))
              (sparqlPrefix	     ::= (PREFIX-TERMINAL PNAME_NS-TERMINAL IRIREF-TERMINAL :RESULT (set-prefix $1 $2)))
              (triples		     ::= (:OR (subject predicateObjectList :RESULT (emit-subj-pred-obj-list $0 $1))
					      (blankNodePropertyList (:OPT predicateObjectList) :RESULT (emit-subj-pred-obj-list $0 (opt-value $1))))
				     :RESULT (progn (when triples-block-callback (funcall triples-block-callback (get-triples))) (values)))
              (predicateObjectList   ::= ((verb objectList :RESULT (cons $0 $1))
					  (:REP0 (|;-TERMINAL| (:OPT (verb objectList :RESULT (list (cons $0 $1)))) :RESULT (opt-value $1))))
				     :RESULT (cons $0 (apply #'append $1)))
              (objectList	     ::= (object (:REP0 (|,-TERMINAL| object :RESULT $1)) :RESULT (cons $0 $1)))
              (verb		     ::= (:OR predicate (A-TERMINAL :RESULT *rdf-type*)))
              (subject		     ::= (:OR iri BlankNode collection))
              (predicate	     ::= (iri))
              (object		     ::= (:OR iri BlankNode collection blankNodePropertyList literal))
              (literal		     ::= (:OR RDFLiteral NumericLiteral BooleanLiteral))
              (blankNodePropertyList ::= ([-TERMINAL predicateObjectList ]-TERMINAL :RESULT (emit-subj-pred-obj-list (generate-blank) $1)))
              (collection	     ::= (|(-TERMINAL| (:REP0 object) |)-TERMINAL|) :RESULT (expand-collection $1))
              (NumericLiteral	     ::= (:OR INTEGER-TERMINAL DECIMAL-TERMINAL DOUBLE-TERMINAL))
              (RDFLiteral	     ::= (String (:OPT (:OR (LANGTAG-TERMINAL :RESULT #'(lambda (s) (create-rdf-literal-with-lang s (subseq $0 1))))
							    (^^-TERMINAL iri :RESULT #'(lambda (s) (nth-value 0 (create-rdf-literal-with-type s $1))))))
						 :RESULT (if (opt-yes-p $1) (funcall (opt-value $1) $0) $0)))
              (BooleanLiteral	     ::= (:OR (TRUE-TERMINAL :RESULT t) (FALSE-TERMINAL :RESULT nil)))
              (String		     ::= (:OR STRING_LITERAL_QUOTE-TERMINAL STRING_LITERAL_SINGLE_QUOTE-TERMINAL
                                         STRING_LITERAL_LONG_SINGLE_QUOTE-TERMINAL STRING_LITERAL_LONG_QUOTE-TERMINAL))
              (iri		     ::= (:OR IRIREF-TERMINAL PrefixedName))
              (PrefixedName	     ::= (:OR (PNAME_LN-TERMINAL :RESULT (pname2iri (first $0) (second $0)))
					      (PNAME_NS-TERMINAL :RESULT (or (second $0) (ll-parser-failure "Unbound prefix ~A" (first $0))))))
              (BlankNode	     ::= (:OR (BLANK_NODE_LABEL-TERMINAL :RESULT (make-blank $0)) (ANON-TERMINAL :RESULT (generate-blank))))))
      (setf (ll-parser-lexer parser) lexer)
      (setf (ll-parser-subscribe parser) subscribe)
      parser)))

(defun parse-turtle-file (file &key subscribe base triple-callback triples-block-callback)
  (let ((*triple-count* 0)
	(*triple-sizes* 0))
    (time
       (with-open-file (stream file)
	 (let* ((instans (make-instance 'instans :name file))
		(parser (make-turtle-parser instans stream :base base :subscribe subscribe :triples-block-callback triples-block-callback :triple-callback triple-callback))
		(lexer (ll-parser-lexer parser))
		(result (parse parser)))
	   (inform "result = ~S" result)
	   (inform "~%triple-count = ~D, triple-sizes = ~D~%strings: ~D elems, prefixes: ~D elems, keywords: ~D elems"
		   *triple-count* *triple-sizes* (hash-table-count (lexer-string-table lexer)) (hash-table-count (lexer-prefix-table lexer)) (hash-table-count (lexer-keyword-table lexer))))))))

(defun parse-turtle-file-single-triple (file &key subscribe base)
  (time
   (with-open-file (stream file)
     (let* ((instans (make-instance 'instans :name file))
	    (parser (make-turtle-parser instans stream :base base :subscribe subscribe
					:triple-callback #'(lambda (s p o)
							     (inform "Triple callback got triple ~A ~A ~A" s p o)
							     (ll-parser-yields (list s p o))))))
       (loop while (not (ll-parser-finished-p parser))
	     do (progn
		  (multiple-value-bind (p triple) (parse parser)
		    (assert (eq p parser))
		    (case (ll-parser-state parser)
		      (:succeeded (inform "Parser returned ~A" (ll-parser-result parser)))
		      (:failed (inform "Parser failed ~A" (ll-parser-error-messages parser)))
		      (:yield (inform "Parser yielded ~A" triple))
		      (t (inform "Parser state now ~A" (ll-parser-state parser)))))))))))

(defun parse-turtle-file-triples-block (file &key subscribe base)
  (time
   (with-open-file (stream file)
     (let* ((instans (make-instance 'instans :name file))
	    (parser (make-turtle-parser instans stream :base base :subscribe subscribe
					:triples-block-callback #'(lambda (triples)
								    (inform "Triples block callback got triples:")
								    (loop for triple in triples do (inform "~{~A~^ ~}" triple))
								    (ll-parser-yields triples)))))
       (loop while (not (ll-parser-finished-p parser))
	     do (progn
		  (multiple-value-bind (p triples) (parse parser)
		    (assert (eq p parser))
		    (case (ll-parser-state parser)
		      (:succeeded (inform "Parser returned ~A" (ll-parser-result parser)))
		      (:failed (inform "Parser failed ~A" (ll-parser-error-messages parser)))
		      (:yield (inform "Parser yielded ~A" triples))
		      (t (inform "Parser state now ~A" (ll-parser-state parser)))))))))))

(defun parse-turtle-file-both (file &key subscribe base)
  (time
   (with-open-file (stream file)
     (let* ((instans (make-instance 'instans :name file))
	    (parser (make-turtle-parser instans stream :base base :subscribe subscribe
					:triple-callback #'(lambda (s p o)
							     (inform "Triple callback got triple ~A ~A ~A" s p o)
							     (ll-parser-yields (list s p o)))
					:triples-block-callback #'(lambda (triples)
								    (inform "Triples block callback got triples:")
								    (loop for triple in triples do (inform "~{~A~^ ~}" triple))
								    (ll-parser-yields triples)))))
       (loop while (not (ll-parser-finished-p parser))
	     do (progn
		  (multiple-value-bind (p value) (parse parser)
		    (assert (eq p parser))
		    (case (ll-parser-state parser)
		      (:succeeded (inform "Parser returned ~A" (ll-parser-result parser)))
		      (:failed (inform "Parser failed ~A" (ll-parser-error-messages parser)))
		      (:yield (inform "Parser yielded ~A" value))
		      (t (inform "Parser state now ~A" (ll-parser-state parser)))))))))))

