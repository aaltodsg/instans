;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; TODO
;;; * How should we name sparql-functions? Should we use sparql-zap or just zap? The latter is nice and short, but the function calls may be difficult to understand,
;;;   since we have to add the sparql- in front of the name?
;;; * How to handle string literal argument compatibility?
;;; * How to handle errors in parameter and result types?
;;; * The explanation of 17.4.1.7 RDFterm-equal is strange.

(in-package #:instans)

;; 17.2.2 Effective Boolean Value (EBV)
;; ------------------------------------
;; Effective boolean value is used to calculate the arguments to the logical functions logical-and,
;; logical-or, and fn:not, as well as evaluate the result of a FILTER expression.
;;
;; The XQuery Effective Boolean Value rules rely on the definition of XPath's fn:boolean. The following
;; rules reflect the rules for fn:boolean applied to the argument types present in SPARQL queries:
;;
;;     The EBV of any literal whose type is xsd:boolean or numeric is false if the lexical form is not
;;     valid for that datatype (e.g. "abc"^^xsd:integer).
;;     If the argument is a typed literal with a datatype of xsd:boolean, and it has a valid lexical
;;     form, the EBV is the value of that argument.
;;     If the argument is a plain literal or a typed literal with a datatype of xsd:string, the EBV is
;;     false if the operand value has zero length, otherwise the EBV is true.
;;     If the argument is a numeric type or a typed literal with a datatype derived from a numeric
;;     type, and it has a valid lexical form, the EBV is false if the operand value is NaN or is
;;     numerically equal to zero, otherwise the EBV is true.
;;     All other arguments, including unbound arguments, produce a type error.
;;
;; An EBV of true is represented as a typed literal with a datatype of xsd:boolean and a lexical value
;; of "true", an EBV of false is represented as a typed literal with a datatype of xsd:boolean and a
;; lexical value of "false".
(define-sparql-function "ebv" (:arguments ((v ebv)) :returns xsd-boolean-value :hiddenp t)
  (:method ((v xsd-boolean-value)) v)
  (:method ((v xsd-string-value)) (not (zerop (length v))))
  (:method ((v xsd-number-value)) (not (or (zerop v) (ieee-nan-p v)))) ;;; ieee-nan-p not properly implemented, returns nil
  (:method ((v rdf-literal))
    (cond ((rdf-plain-literal-p v)
	   (not (zerop (length (rdf-literal-string v)))))
	  ((null (rdf-literal-value v))
	   (let* ((type-iri (rdf-literal-type v))
		  (type-descriptor (find-type-descriptor (rdf-iri-string type-iri))))
	     (if (or (subtypep (type-descriptor-lisp-type type-descriptor) 'xsd-number-value)
		     (eq (type-descriptor-lisp-type type-descriptor) 'xsd-boolean-value))
		 nil
		 (sparql-error "EBV: ~A cannot be used as an ebv" v))))
	  (t (sparql-call "ebv" (rdf-literal-value v)))))) ;;; This should never happen.

;; XQuery Unary Operators
;; ======================
;; ! A	xsd:boolean (EBV) -> xsd:boolean  =>  fn:not(A)
(define-sparql-function "not" (:arguments ((x ebv)) :returns xsd-boolean-value :hiddenp t)
  (:method ((x ebv))
    (let ((v (sparql-call "ebv" x)))
      (cond ((eq v t) nil)
	    ((eq v nil) t)
	    (t v))))) ; must be sparql-error

;; + A 	numeric           -> numeric      =>  op:numeric-unary-plus(A)
(define-sparql-function "numeric-unary-plus" (:arguments ((x xsd-number-value)) :returns xsd-number-value :hiddenp t)
  (:method ((x xsd-number-value)) x))

;; - A 	numeric           -> numeric      =>  op:numeric-unary-minus(A)
(define-sparql-function "numeric-unary-minus" (:arguments ((x xsd-number-value)) :returns xsd-number-value :hiddenp t)
  (:method ((x xsd-number-value)) (- x)))

;; Logical Connectives
;; ===================
;; A || B	xsd:boolean (EBV), xsd:boolean (EBV) -> xsd:boolean  =>  logical-or(A, B)
(define-sparql-form "logical-or" (:arguments ((a ebv) (b ebv)) :returns xsd-boolean-value :hiddenp t)
  (let ((avar (gensym "A"))
	(bvar (gensym "B")))
    `(let ((,avar (sparql-call "ebv" ,a)))
       (cond ((eq ,avar t) t)
	     ((null ,avar) (sparql-call "ebv" ,b))
	     (t
	      (let ((,bvar (sparql-call "ebv" ,b)))
		(cond ((null ,bvar) ,avar)
		      (t ,bvar))))))))

;; A && B	xsd:boolean (EBV), xsd:boolean (EBV) -> xsd:boolean  =>  logical-and(A, B)
(define-sparql-form "logical-and" (:arguments ((a ebv) (b ebv)) :returns xsd-boolean-value :hiddenp t)
  (let ((avar (gensym "A"))
	(bvar (gensym "B")))
    `(let ((,avar (sparql-call "ebv" ,a)))
       (cond ((null ,avar) nil)
	     ((eq ,avar t) (sparql-call "ebv" ,b))
	     (t
	      (let ((,bvar (sparql-call "ebv" ,b)))
		(cond ((null ,bvar) nil)
		      (t ,avar))))))))

;; XPath Tests
;; ===========
;; A = B	numeric, numeric		-> xsd:boolean  =>  op:numeric-equal(A, B) 
;; A = B	simple literal,simple literal	-> xsd:boolean  =>  op:numeric-equal(fn:compare(A, B), 0) 
;; A = B	xsd:string, xsd:string		-> xsd:boolean  =>  op:numeric-equal(fn:compare(STR(A), STR(B)), 0) 
;; A = B	xsd:boolean, xsd:boolean	-> xsd:boolean  =>  op:boolean-equal(A, B) 
;; A = B	xsd:dateTime, xsd:dateTime	-> xsd:boolean  =>  op:dateTime-equal(A, B) 
;; SPARQL Tests
;; ============
;; A = B	RDF term, RDF term -> xsd:boolean  =>  RDFterm-equal(A, B) 
(define-sparql-function "=" (:arguments ((a term-or-value) (b term-or-value)) :returns xsd-boolean-value)
  (:method ((a xsd-number-value) (b xsd-number-value)) (= a b))
  (:method ((a xsd-string-value) (b xsd-string-value)) (string= a b))
  (:method ((a xsd-boolean-value) (b xsd-boolean-value)) (eq a b))
  (:method ((a xsd-datetime-value) (b xsd-datetime-value)) (datetime= a b))
  (:method ((a term-or-value) (b term-or-value)) (sparql-call "RDFterm-equal" a b)))

;; A != B	numeric, numeric		-> xsd:boolean  =>  fn:not(op:numeric-equal(A, B)) 
;; A != B	simple literal, simple literal	-> xsd:boolean  =>  fn:not(op:numeric-equal(fn:compare(A, B), 0)) 
;; A != B	xsd:string, xsd:string		-> xsd:boolean  =>  fn:not(op:numeric-equal(fn:compare(STR(A), STR(B)), 0)) 
;; A != B	xsd:boolean, xsd:boolean	-> xsd:boolean  =>  fn:not(op:boolean-equal(A, B)) 
;; A != B	xsd:dateTime, xsd:dateTime	-> xsd:boolean  =>  fn:not(op:dateTime-equal(A, B)) 
;; SPARQL Tests
;; ============
;; A != B	RDF term, RDF term -> xsd:boolean  =>  fn:not(RDFterm-equal(A, B))
(define-sparql-function "!=" (:arguments ((a term-or-value) (b term-or-value)) :returns xsd-boolean-value)
  (:method ((a term-or-value) (b term-or-value)) (sparql-call "not" (sparql-call "=" a b))))

;; A < B	numeric, numeric		-> xsd:boolean  =>  op:numeric-less-than(A, B) 
;; A < B	simple literal, simple literal	-> xsd:boolean  =>  op:numeric-equal(fn:compare(A, B), -1) 
;; A < B	xsd:string, xsd:string		-> xsd:boolean  =>  op:numeric-equal(fn:compare(STR(A), STR(B)), -1) 
;; A < B	xsd:boolean, xsd:boolean	-> xsd:boolean  =>  op:boolean-less-than(A, B) 
;; A < B	xsd:dateTime, xsd:dateTime	-> xsd:boolean  =>  op:dateTime-less-than(A, B) 
(define-sparql-function "<" (:arguments ((a xsd-value) (b xsd-value)) :returns xsd-boolean-value)
  (:method ((a xsd-number-value) (b xsd-number-value)) (< a b))
  (:method ((a xsd-string-value) (b xsd-string-value)) (string< a b))
  (:method ((a xsd-boolean-value) (b xsd-boolean-value)) (and (not a) b))
  (:method ((a xsd-datetime-value) (b xsd-datetime-value)) (datetime< a b)))

;; A > B	numeric, numeric		-> xsd:boolean  =>  op:numeric-greater-than(A, B) 
;; A > B	simple literal, simple literal	-> xsd:boolean  =>  op:numeric-equal(fn:compare(A, B), 1) 
;; A > B	xsd:string, xsd:string		-> xsd:boolean  =>  op:numeric-equal(fn:compare(STR(A), STR(B)), 1) 
;; A > B	xsd:boolean, xsd:boolean	-> xsd:boolean  =>  op:boolean-greater-than(A, B) 
;; A > B	xsd:dateTime, xsd:dateTime	-> xsd:boolean  =>  op:dateTime-greater-than(A, B) 
(define-sparql-function ">" (:arguments ((a xsd-value) (b xsd-value)) :returns xsd-boolean-value)
  (:method ((a xsd-number-value) (b xsd-number-value)) (> a b))
  (:method ((a xsd-string-value) (b xsd-string-value)) (string> a b))
  (:method ((a xsd-boolean-value) (b xsd-boolean-value)) (and a (not b)))
  (:method ((a xsd-datetime-value) (b xsd-datetime-value)) (datetime> a b)))

;; A <= B	numeric, numeric		-> xsd:boolean  =>  logical-or(op:numeric-less-than(A, B), op:numeric-equal(A, B)) 
;; A <= B	simple literal,	simple literal	-> xsd:boolean  =>  fn:not(op:numeric-equal(fn:compare(A, B), 1)) 
;; A <= B	xsd:string, xsd:string		-> xsd:boolean  =>  fn:not(op:numeric-equal(fn:compare(STR(A), STR(B)), 1)) 
;; A <= B	xsd:boolean, xsd:boolean	-> xsd:boolean  =>  fn:not(op:boolean-greater-than(A, B)) 
;; A <= B	xsd:dateTime, xsd:dateTime	-> xsd:boolean  =>  fn:not(op:dateTime-greater-than(A, B)) 
(define-sparql-function "<=" (:arguments ((a xsd-value) (b xsd-value)) :returns xsd-boolean-value)
  (:method ((a xsd-number-value) (b xsd-number-value)) (<= a b))
  (:method ((a xsd-string-value) (b xsd-string-value)) (string<= a b))
  (:method ((a xsd-boolean-value) (b xsd-boolean-value)) (or (not a) b))
  (:method ((a xsd-datetime-value) (b xsd-datetime-value)) (datetime<= a b)))

;; A >= B	numeric, numeric		-> xsd:boolean  =>  logical-or(op:numeric-greater-than(A, B), op:numeric-equal(A, B)) 
;; A >= B	simple literal, simple literal	-> xsd:boolean  =>  fn:not(op:numeric-equal(fn:compare(A, B), -1)) 
;; A >= B	xsd:string, xsd:string		-> xsd:boolean  =>  fn:not(op:numeric-equal(fn:compare(STR(A), STR(B)), -1)) 
;; A >= B	xsd:boolean, xsd:boolean	-> xsd:boolean  =>  fn:not(op:boolean-less-than(A, B)) 
;; A >= B	xsd:dateTime, xsd:dateTime	-> xsd:boolean  =>  fn:not(op:dateTime-less-than(A, B)) 
(define-sparql-function ">=" (:arguments ((a xsd-value) (b xsd-value)) :returns xsd-boolean-value)
  (:method ((a xsd-number-value) (b xsd-number-value)) (>= a b))
  (:method ((a xsd-string-value) (b xsd-string-value)) (string>= a b))
  (:method ((a xsd-boolean-value) (b xsd-boolean-value)) (or a (not b)))
  (:method ((a xsd-datetime-value) (b xsd-datetime-value)) (datetime>= a b)))

;; XPath Arithmetic
;; ================
;; A * B	numeric, numeric 	 -> numeric  	 => op:numeric-multiply(A, B)
(define-sparql-function "*" (:arguments ((a xsd-number-value) (b xsd-number-value)) :returns xsd-number-value)
  (:method ((a xsd-number-value) (b xsd-number-value)) (* a b)))

;; A / B	xsd:integer, xsd:integer -> xsd:decimal  => op:numeric-divide(A, B)
;; A / B	numeric, numeric 	 -> numeric  	 => op:numeric-divide(A, B)
(define-sparql-function "/" (:arguments ((a xsd-number-value) (b xsd-number-value)) :returns xsd-number-value)
  (:method ((a xsd-integer-value) (b xsd-integer-value)) (if (zerop b) (sparql-error "Divide-by-zero") (/ (float a) b)))
  (:method ((a xsd-integer-value) (b xsd-decimal-value)) (if (zerop b) (sparql-error "Divide-by-zero") (/ a b)))
  (:method ((a xsd-decimal-value) (b xsd-integer-value)) (if (zerop b) (sparql-error "Divide-by-zero") (/ a b)))
  (:method ((a xsd-decimal-value) (b xsd-decimal-value)) (if (zerop b) (sparql-error "Divide-by-zero") (/ a b)))
  (:method ((a xsd-number-value) (b xsd-number-value)) (/ a b)))

;; A + B	numeric, numeric 	 -> numeric  	 => op:numeric-add(A, B)
(define-sparql-function "+" (:arguments ((a xsd-number-value) (b xsd-number-value)) :returns xsd-number-value)
  (:method ((a xsd-number-value) (b xsd-number-value)) (+ a b)))

;; A - B	numeric, numeric	 -> numeric  	 => op:numeric-subtract(A, B)
(define-sparql-function "-" (:arguments ((a xsd-number-value) (b xsd-number-value)) :returns xsd-number-value)
  (:method ((a xsd-number-value) (b xsd-number-value)) (- a b)))

;; SPARQL Tests (See = and != above)
;; ============
;; A = B	RDF term, RDF term -> xsd:boolean  =>  RDFterm-equal(A, B) 
;; A != B	RDF term, RDF term -> xsd:boolean  =>  fn:not(RDFterm-equal(A, B))

;; 17.4.1 Functional Forms
;; =======================

;; 17.4.1.1 bound
;; --------------
;; xsd:boolean  BOUND (variable var)
;;
;; Note! The variables are evaluated before calling the function. Unbound variables are replaced by sparql-unbound.
(define-sparql-function "bound" (:arguments (x) :returns xsd-boolean-value)
  (:method (x) (not (sparql-unbound-p x))))

;; 17.4.1.2 IF
;; -----------
;; rdfTerm  IF (expression1, expression2, expression3)
(define-sparql-form "if" (:arguments ((test ebv) (then term-or-value) (else term-or-value)) :returns term-or-value)
  `(if (sparql-call "ebv" ,test) ,then ,else))

;; 17.4.1.3 COALESCE !!! Missing !!!
;; -----------------
;; rdfTerm  COALESCE(expression, ....)

;; 17.4.1.4 NOT EXISTS and EXISTS (Implemented at RETE level)
;; ------------------------------
;; xsd:boolean  EXISTS { pattern }
;; xsd:boolean  NOT EXISTS { pattern } is equivalent to fn:not(EXISTS { pattern })

;; 17.4.1.5 logical-or (See above A || B)
;; -------------------
;; logical-or	xsd:boolean, xsd:boolean -> xsd:boolean  =>  left || right

;; 17.4.1.6 logical-and (See above A && B)
;; --------------------
;; logical-and	xsd:boolean, xsd:boolean -> xsd:boolean  =>  left && right

;; 17.4.1.7 RDFterm-equal
;; ----------------------
;; RDFterm-equal	RDF term, RDF term -> xsd:boolean	 =>  term1 = term2

;;    Returns TRUE if term1 and term2 are the same RDF term as defined in Resource Description Framework (RDF): Concepts and Abstract Syntax [CONCEPTS],
;;    produces a type error if the arguments are both literal but are not the same RDF term *, returns FALSE otherwise.
;;    term1 and term2 are the same if any of the following is true:

;;      term1 and term2 are equivalent IRIs as defined in 6.4 RDF URI References of [CONCEPTS].
;;      term1 and term2 are equivalent literals as defined in 6.5.1 Literal Equality of [CONCEPTS].
;;      term1 and term2 are the same blank node as described in 6.6 Blank Nodes of [CONCEPTS].
(define-sparql-function "RDFterm-equal" (:arguments ((t1 term-or-value) (t2 term-or-value)) :returns xsd-boolean :hiddenp t)
  (:method ((v1 xsd-value) (v2 xsd-value)) (sparql-call "=" v1 v2))
  (:method ((i1 rdf-iri) (i2 rdf-iri)) (rdf-iri= i1 i2))
  (:method ((b1 rdf-blank-node) (b2 rdf-blank-node)) (string= (uniquely-named-object-name b1) (uniquely-named-object-name b2)))
  (:method ((l1 rdf-literal) (l2 rdf-literal)) (or (and (string= (rdf-literal-string l1) (rdf-literal-string l2))
							(cond ((rdf-literal-lang l1)
							       (and (rdf-literal-lang l2) (string= (rdf-literal-lang l1) (rdf-literal-lang l2))))
							      ((rdf-literal-type l1)
							       (and (rdf-literal-type l2) (rdf-iri= (rdf-literal-type l1) (rdf-literal-type l2))))
							      (t (error* "A literal with neither a type nor a lang ~A" l1))))
						   (sparql-error "RDFterm-equal: Literals ~A and ~A are not the same term" l1 l2))))

;; 17.4.1.8 sameTerm
;; -----------------
;; sameTerm	RDF term, RDF term -> xsd:boolean	 => term1 = term2
;;    Returns TRUE iff term1 and term2 are the same RDF term as defined in Resource Description Framework (RDF): Concepts and Abstract Syntax [CONCEPTS]
(define-sparql-function "sameTerm" (:arguments ((t1 term-or-value) (t2 term-or-value)) :returns xsd-boolean :hiddenp t)
  (:method ((v1 xsd-value) (v2 xsd-value)) (sparql-call "=" v1 v2))
  (:method ((i1 rdf-iri) (i2 rdf-iri)) (rdf-iri= i1 i2))
  (:method ((b1 rdf-blank-node) (b2 rdf-blank-node)) (string= (uniquely-named-object-name b1) (uniquely-named-object-name b2)))
  (:method ((l1 rdf-literal) (l2 rdf-literal)) (and (string= (rdf-literal-string l1) (rdf-literal-string l2))
						    (cond ((rdf-literal-lang l1)
							   (and (rdf-literal-lang l2) (string= (rdf-literal-lang l1) (rdf-literal-lang l2))))
							  ((rdf-literal-type l1)
							   (and (rdf-literal-type l2) (rdf-iri= (rdf-literal-type l1) (rdf-literal-type l2))))
							  (t (error* "A literal with neither a type nor a lang ~A" l1)))))
  (:default nil))

;; 17.4.1.9 IN
;; -----------
;; xsd:boolean rdfTerm IN(expression, ...)
;;    The IN operator is equivalent to the SPARQL expression: (lhs = expression1) || (lhs = expression2) || ...
(define-sparql-form "in" (:arguments (x &rest expressions) :returns xsd-boolean-value)
  (let ((xvar (gensym "X")))
    `(let ((,xvar ,x))
       (or ,@(loop for expr in expressions collect `(sparql-call "=" ,xvar ,expr))))))

;; 17.4.1.10 NOT IN
;; ----------------
;; xsd:boolean rdfTerm NOT IN(expression, ...)
;;    Equivalent to (lhs != expression1) && (lhs != expression2) && ... and !(IN (expression1, expression2, ...))
;;
(define-sparql-form "not in" (:arguments (x &rest expressions) :returns xsd-boolean-value)
  (let ((xvar (gensym "X")))
    `(let ((,xvar ,x))
       (and ,@(loop for expr in expressions collect `(not (sparql-call "=" ,xvar ,expr)))))))

;; 17.4.2 Functions on RDF Terms
;; =============================

;; 17.4.2.1 isIRI
;; --------------
;; xsd:boolean  isIRI (RDF term term)
;; xsd:boolean  isURI (RDF term term)
(define-sparql-function "isIRI" (:arguments ((term term-or-value)) :returns xsd-boolean-value)
  (:method ((term rdf-iri)) (declare (ignorable term)) t)
  (:method ((term term-or-value)) (declare (ignorable term)) nil))

(define-sparql-function "isURI" (:arguments ((term term-or-value)) :returns xsd-boolean-value)
  (:method ((term rdf-iri)) (declare (ignorable term)) t)
  (:method ((term term-or-value)) (declare (ignorable term)) nil))

;; 17.4.2.2 isBlank
;; ----------------
;; xsd:boolean  isBlank (RDF term term)
(define-sparql-function "isBlank" (:arguments ((term term-or-value)) :returns xsd-boolean-value)
  (:method ((term rdf-blank-node)) (declare (ignorable term)) t)
  (:method ((term term-or-value)) (declare (ignorable term)) nil))

;; 17.4.2.3 isLiteral
;; ------------------
;; xsd:boolean  isLiteral (RDF term term)
(define-sparql-function "isLiteral" (:arguments ((term term-or-value)) :returns xsd-boolean-value)
  (:method ((term literal)) (declare (ignorable term)) t)
  (:method ((term term-or-value)) (declare (ignorable term)) nil))

;; 17.4.2.4 isNumeric
;; ------------------
;; xsd:boolean  isNumeric (RDF term term)
(define-sparql-function "isNumeric" (:arguments ((term term-or-value)) :returns xsd-boolean-value)
  (:method ((term xsd-number-value)) (declare (ignorable term)) t)
  (:method ((term term-or-value)) (declare (ignorable term)) nil))

;; 17.4.2.5 str
;; ------------
;; simple literal  STR (literal ltrl)
;; simple literal  STR (IRI rsrc)
(define-sparql-function "str" (:arguments ((x iri-or-literal)) :returns xsd-string)
  (:method ((x xsd-string-value)) x)
  (:method ((x rdf-literal)) (rdf-literal-string x))
  (:method ((x xsd-integer-value)) (format nil "~D" x))
  (:method ((x xsd-decimal-value)) (format nil "~F" x))
  (:method ((x xsd-number-value)) (format nil "~A" x))
  (:method ((x xsd-boolean-value)) (if x "true" "false"))
  (:method ((x xsd-datetime-value)) (datetime-canonic-string x)))

;; 17.4.2.6 lang
;; -------------
;; simple literal  LANG (literal ltrl)
(define-sparql-function "lang" (:arguments ((x literal)) :returns xsd-string-value)
  (:method ((x rdf-literal)) (rdf-literal-lang x))
  (:method ((x literal)) (declare (ignorable x)) ""))

;; 17.4.2.7 datatype
;; -----------------
;; iri  DATATYPE (literal literal)
(define-sparql-function "datatype" (:arguments ((x literal)) :returns rdf-iri)
  (:method ((x rdf-literal)) (rdf-literal-type x))
  (:method ((x literal)) (declare (ignorable x)) *xsd-string-iri*))

;; 17.4.2.8 IRI
;; ------------
;; iri  IRI(simple literal)
;; iri  IRI(xsd:string)
;; iri  IRI(iri)
;; iri  URI(simple literal)
;; iri  URI(xsd:string)
;; iri  URI(iri)
(define-sparql-function "iri" (:arguments ((x iri-or-string)) :returns rdf-iri)
  (:method ((x rdf-iri)) x)
  (:method ((x xsd-string-value)) (make-instance 'rdf-iri :string x)))

(define-sparql-function "uri" (:arguments ((x iri-or-string)) :returns rdf-iri)
  (:method ((x rdf-iri)) x)
  (:method ((x xsd-string-value)) (make-instance 'rdf-iri :string x)))

;; 17.4.2.9 BNODE
;; --------------
;; blank node  BNODE()
;; blank node  BNODE(simple literal)
;; blank node  BNODE(xsd:string)
(define-sparql-function "bnode" (:arguments (&optional (x xsd-string-value)) :returns rdf-blank-node)
  (:method (&optional (x xsd-string-value)) (make-rdf-blank-node x)))

;; 17.4.2.10 STRDT
;; ---------------
;; <**> literal  STRDT(simple literal lexicalForm, IRI datatypeIRI)
(define-sparql-function "strdt" (:arguments ((string xsd-string-value) (type rdf-iri)) :returns literal)
  (:method ((string xsd-string-value) (type rdf-iri))
    (create-rdf-literal-with-type string type)))

;; 17.4.2.11 STRLANG
;; -----------------
;; <**> literal  STRLANG(simple literal lexicalForm, simple literal langTag)
(define-sparql-function "strlang" (:arguments ((string xsd-string-value) (lang xsd-string-value)) :returns literal)
  (:method ((string xsd-string-value) (lang xsd-string-value)) (create-rdf-literal-with-lang string lang)))

;; 17.4.2.12 UUID !!! Missing !!!
;; --------------
;; iri  UUID()

;; 17.4.2.13 STRUUID !!! Missing !!!
;; -----------------
;; simple literal  STRUUID()

;; 17.4.3 Functions on Strings
;; ===========================
;; string literal = simple literal, plain literal with tag, or literal with type string

;; 17.4.3.1 Strings in SPARQL Functions
;; ------------------------------------

;; 17.4.3.1.1 String arguments

;; Certain functions (e.g. REGEX, STRLEN, CONTAINS) take a string literal as an argument and accept a simple literal, a plain literal with language tag, or a literal with datatype xsd:string. They then act on the lexcial form of the literal.

;; The term string literal is used in the function descriptions for this. Use of any other RDF term will cause a call to the function to raise an error.

;; 17.4.3.1.2 Argument Compatibility Rules

;; The functions STRSTARTS, STRENDS, CONTAINS, STRBEFORE and STRAFTER take two arguments. These arguments must be compatible otherwise invocation of one of these functions raises an error.

;; Compatibility of two arguments is defined as:

;;     The arguments are simple literals or literals typed as xsd:string
;;     The arguments are plain literals with identical language tags
;;     The first argument is a plain literal with language tag and the second argument is a simple literal or literal typed as xsd:string

;; Argument1	  Argument2	  Compatible?
;; ---------------------------------------------
;; "abc"		  "b"		  yes
;; "abc"		  "b"^^xsd:string yes
;; "abc"^^xsd:string "b"		  yes
;; "abc"^^xsd:string "b"^^xsd:string yes
;; "abc"@en	  "b"		  yes
;; "abc"@en	  "b"^^xsd:string yes
;; "abc"@en	  "b"@en	  yes
;; "abc"@fr	  "b"@ja	  no
;; "abc"	          "b"@ja	  no
;; "abc"	          "b"@en	  no
;; "abc"^^xsd:string "b"@en	  no

;; 17.4.3.1.3 String Literal Return Type

;; Functions that return a string literal do so with the string literal of the same kind as the first argument (simple literal, plain literal with same language tag, xsd:string). This includes SUBSTR, STRBEFORE and STRAFTER.

;; The function CONCAT returns a string literal based on the details of all its arguments.

;; 7.4.3.2 STRLEN
;; --------------
;; xsd:integer  STRLEN(string literal str)
(define-sparql-function "strlen" (:arguments ((lit literal)) :returns xsd-integer-value)
  (:method ((str xsd-string-value)) (length str))
  (:method ((lit rdf-literal)) (length (rdf-literal-string lit))))

;; 17.4.3.3 SUBSTR
;; ---------------
;; string literal  SUBSTR(string literal source, xsd:integer startingLoc)
;; string literal  SUBSTR(string literal source, xsd:integer startingLoc, xsd:integer length)
(define-sparql-function "substr" (:arguments ((lit literal) (start xsd-integer-value) &optional (length xsd-integer-value)) :returns literal)
  (:method ((str xsd-string-value) (start xsd-integer-value) &optional (length xsd-integer-value))
	   (if (null length) (subseq str start) (subseq str start (+ start length))))
  (:method ((lit rdf-literal) (start xsd-integer-value) &optional (length xsd-integer-value))
	   (let ((str (rdf-literal-string lit)))
	     (create-rdf-literal-with-lang (if (null length) (subseq str start) (subseq str start (+ start length))) (rdf-literal-lang lit)))))

;; 17.4.3.4 UCASE
;; --------------
;; string literal  UCASE(string literal str)
(define-sparql-function "ucase" (:arguments ((lit literal)) :returns literal)
  (:method ((str xsd-string-value)) (string-upcase str))
  (:method ((lit rdf-literal))
    (create-rdf-literal-with-lang (string-upcase (rdf-literal-string lit)) (rdf-literal-lang lit))))

;; 17.4.3.5 LCASE
;; --------------
;; string literal  LCASE(string literal str)
(define-sparql-function "lcase" (:arguments ((lit literal)) :returns literal)
  (:method ((str xsd-string-value)) (string-downcase str))
  (:method ((lit rdf-literal))
    (create-rdf-literal-with-lang (string-downcase (rdf-literal-string lit)) (rdf-literal-lang lit))))

;; 17.4.3.6 STRSTARTS
;; ------------------
;; xsd:boolean  STRSTARTS(string literal arg1, string literal arg2)
(define-sparql-two-string-function strstarts xsd-boolean-value (lambda (arg1 arg2) (eql 0 (search arg2 arg1))))

;; 17.4.3.7 STRENDS
;; ----------------
;; xsd:boolean  STRENDS(string literal arg1, string literal arg2)
(define-sparql-two-string-function strends xsd-boolean-value (lambda (arg1 arg2) (eql (- (length arg1) (length arg2)) (search arg2 arg1 :from-end t))))

;; 17.4.3.8 CONTAINS
;; -----------------
;; xsd:boolean  CONTAINS(string literal arg1, string literal arg2)
(define-sparql-two-string-function contains xsd-boolean-value (lambda (arg1 arg2) (eql 0 (search arg2 arg1))))

;; 17.4.3.9 STRBEFORE
;; ------------------
;; literal  STRBEFORE(string literal arg1, string literal arg2)
(define-sparql-two-string-function strbefore literal-or-string (lambda (arg1 arg2)
								   (let ((index (search arg2 arg1)))
								     (cond ((not index) "")
									   (t (subseq arg1 0 index) arg1)))))

;; 17.4.3.10 STRAFTER
;; ------------------
;; literal  STRAFTER(string literal arg1, string literal arg2)
(define-sparql-two-string-function strafter literal-or-string (lambda (arg1 arg2)
								  (cond ((zerop (length arg1)) "")
									((zerop (length arg2)) arg1)
									(t
									 (let ((index (search arg2 arg1)))
									   (cond ((not index) "")
										 (t (subseq arg1 (+ index (length arg2))))))))))

;; 17.4.3.11 ENCODE_FOR_URI !!! Missing !!!
;; ------------------------
;; simple literal  ENCODE_FOR_URI(string literal ltrl)

;; 17.4.3.12 CONCAT
;; ----------------
;; string literal  CONCAT(string literal ltrl1 ... string literal ltrln)
(define-sparql-function "concat" (:arguments (&rest literals) :returns literal)
  (:method (&rest literals)
    (loop with lang = :unset
	  for lit in literals
	  unless (typep lit 'literal-or-string)
	  do (sparql-error "CONCAT: Illegal parameter ~A" lit)
	  when (typep lit 'rdf-literal) do (cond ((eq lang :unset) (setf lang (rdf-literal-lang lit)))
						 ((not (and lang (string= lang (rdf-literal-lang lit)))) (setf lang nil)))
	  else do (setf lang nil)
	  collect (if (typep lit 'rdf-literal) (rdf-literal-string lit) lit) into strings
	  finally (cond ((stringp lang)
			 (create-rdf-literal-with-lang (concatenate 'string strings) lang))
			(t
			 (concatenate 'string strings))))))

;; 17.4.3.13 langMatches !!! Missing !!!
;; ---------------------
;; xsd:boolean  langMatches (simple literal language-tag, simple literal language-range)

;; 17.4.3.14 REGEX !!! Missing !!!
;; ---------------
;; xsd:boolean  REGEX (string literal text, simple literal pattern)
;; xsd:boolean  REGEX (string literal text, simple literal pattern, simple literal flags)

;; 17.4.3.15 REPLACE !!! Missing !!!
;; -----------------
;; string literal  REPLACE (string literal arg, simple literal pattern, simple literal replacement )
;; string literal  REPLACE (string literal arg, simple literal pattern, simple literal replacement,  simple literal flags)

;; 17.4.4 Functions on Numerics
;; ============================

;; 17.4.4.1 abs
;; ------------
;; numeric  ABS (numeric term)
(define-sparql-function "abs" (:arguments ((arg xsd-number-value)) :returns xsd-number-value)
  (:method ((arg xsd-number-value)) (abs arg)))

;; 17.4.4.2 round
;; --------------
;; numeric  ROUND (numeric term)
(define-sparql-function "round" (:arguments ((arg xsd-number-value)) :returns xsd-number-value)
  (:method ((x xsd-integer-value)) x)
  (:method ((x xsd-number-value)) (float (floor (+ x 0.5)))))

;; 17.4.4.3 ceil
;; -------------
;; numeric  CEIL (numeric term)
(define-sparql-function "ceil" (:arguments ((arg xsd-number-value)) :returns xsd-number-value)
  (:method ((x xsd-integer-value)) x)
  (:method ((x xsd-number-value)) (float (ceiling x))))

;; 17.4.4.4 floor
;; --------------
;; numeric  FLOOR (numeric term)
(define-sparql-function "floor" (:arguments ((arg xsd-number-value)) :returns xsd-number-value)
  (:method ((x xsd-integer-value)) x)
  (:method ((x xsd-number-value)) (float (floor x))))

;; 17.4.4.5 RAND
;; -------------
;; xsd:double  RAND ( )
(define-sparql-function "rand" (:arguments () :returns xsd-double-value)
  (:method () (random 1.0)))

;; 17.4.5 Functions on Dates and Times
;; ===================================

;; 17.4.5.1 now
;; ------------
;; xsd:dateTime  NOW ()
(define-sparql-function "now" (:arguments () :returns xsd-datetime-value)
  (:method () (datetime-now)))

;; 17.4.5.2 year
;; -------------
;; xsd:integer  YEAR (xsd:dateTime arg)
(define-sparql-function "year" (:arguments ((dt xsd-datetime-value)) :returns xsd-integer-value)
  (:method ((dt xsd-datetime-value)) (datetime-year dt)))

;; 17.4.5.3 month
;; --------------
;; xsd:integer  MONTH (xsd:dateTime arg)
(define-sparql-function "month" (:arguments ((dt xsd-datetime-value)) :returns xsd-integer-value)
  (:method ((dt xsd-datetime-value)) (datetime-month dt)))

;; 17.4.5.4 day
;; ------------
;; xsd:integer  DAY (xsd:dateTime arg)
(define-sparql-function "day" (:arguments ((dt xsd-datetime-value)) :returns xsd-integer-value)
  (:method ((dt xsd-datetime-value)) (datetime-day dt)))

;; 17.4.5.5 hours
;; --------------
;; xsd:integer  HOURS (xsd:dateTime arg)
(define-sparql-function "hours" (:arguments ((dt xsd-datetime-value)) :returns xsd-integer-value)
  (:method ((dt xsd-datetime-value)) (datetime-hours dt)))

;; 17.4.5.6 minutes
;; ----------------
;; xsd:integer  MINUTES (xsd:dateTime arg)
(define-sparql-function "minutes" (:arguments ((dt xsd-datetime-value)) :returns xsd-integer-value)
  (:method ((dt xsd-datetime-value)) (datetime-minutes dt)))

;; 17.4.5.7 seconds
;; ----------------
;; xsd:decimal SECONDS (xsd:dateTime arg)
(define-sparql-function "seconds" (:arguments ((dt xsd-datetime-value)) :returns xsd-decimal-value)
  (:method ((dt xsd-datetime-value)) (datetime-seconds dt)))

;; 17.4.5.8 timezone
;; -----------------
;; xsd:dayTimeDuration  TIMEZONE (xsd:dateTime arg)
(define-sparql-function "timezone" (:arguments ((dt xsd-datetime-value)) :returns xsd-integer-value)
  (:method ((dt xsd-datetime-value))
    (cond ((null (datetime-tz-string dt))
	   (sparql-error "sparql-timezone: Missing timezone in datetime"))
	  (t
	   (datetime-tz dt)))))

;; 17.4.5.9 tz
;; -----------
;; simple literal  TZ (xsd:dateTime arg)
(define-sparql-function "tz" (:arguments ((dt xsd-datetime-value)) :returns xsd-string-value)
  (:method ((dt xsd-datetime-value)) (datetime-tz-string dt)))

;; 17.4.6 Hash Functions
;; =====================
;;
;; There is a problem with these: could not find an implementation in Common Lisp for these! Maybe we could use foreign c function calls for these.

;; 17.4.6.1 MD5 !!! Missing !!!
;; -------------
;; simple literal  MD5 (simple literal arg)
;; simple literal  MD5 (xsd:string arg)

;; 17.4.6.2 SHA1 !!! Missing !!!
;; -------------
;; simple literal  SHA1 (simple literal arg)
;; simple literal  SHA1 (xsd:string arg)

;; 17.4.6.3 SHA256 !!! Missing !!!
;; ---------------
;; simple literal  SHA256 (simple literal arg)
;; simple literal  SHA256 (xsd:string arg)

;; 17.4.6.4 SHA384 !!! Missing !!!
;; ---------------
;; simple literal  SHA384 (simple literal arg)
;; simple literal  SHA384 (xsd:string arg)

;; 17.4.6.5 SHA512 !!! Missing !!!
;; ---------------
;; simple literal  SHA512 (simple literal arg)
;; simple literal  SHA512 (xsd:string arg)

;; 17.5 XPath Constructor Functions !!! Missing !!!
;; ================================

;; From \ To	str	flt	dbl	dec	int	dT	bool
;; str		Y	M	M	M	M	M	M
;; flt		Y	Y	Y	M	M	N	Y
;; dbl		Y	Y	Y	M	M	N	Y
;; dec		Y	Y	Y	Y	Y	N	Y
;; int		Y	Y	Y	Y	Y	N	Y
;; dT		Y	N	N	N	N	Y	N
;; bool		Y	Y	Y	Y	Y	N	Y
;; IRI		Y	N	N	N	N	N	N
;; ltrl		Y	M	M	M	M	M	M

