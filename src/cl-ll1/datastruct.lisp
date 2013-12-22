;;; -*- Mode: Lisp -*-

;;;
;;; Operations for Chomsky grammars: Grammar data structures
;;;
;;; Copyright 1996, 2013 Esko Nuutila
;;; All Rights Reserved
;;;

(in-package #:instans)

(defstruct (production :named (:type list))
  (number)
  (lhs)
  (rhs)
  (grammar-symbol-value)
  (properties)
  (result-arglist)
  (result-arg-count)
  (result-func-name)
  (result-func-lambda)
  (result-func))

(define-class grammar ()
  ((name :accessor grammar-name :initarg :name :initform nil)
   (start-symbol :accessor grammar-start-symbol :initarg :start-symbol :initform nil)
   (terminals :accessor grammar-terminals :initarg :terminals :initform nil)
   (nonterminals :accessor grammar-nonterminals :initarg :nonterminals :initform nil)
   (productions :accessor grammar-productions :initarg :productions :initform nil)
   (nonterminal-firsts :accessor grammar-nonterminal-firsts :initarg :nonterminal-firsts :initform nil)
   (followers :accessor grammar-followers :initarg :followers :initform nil)
   (nullable :accessor grammar-nullable :initarg :nullable :initform nil)
   (ll1-table :accessor grammar-ll1-table :initarg :ll1-table :initform nil)
   (symbol-value-tag :accessor grammar-symbol-value-tag :initarg :symbol-value-tag :initform nil)
;   (grammar-function-def :accessor grammar-function-def :initarg :grammar-function-def :initform nil)
   (default-result :accessor grammar-default-result :initarg :default-result :initform nil)
   (state :accessor grammar-state :initform nil)))

(define-class ll-parser (grammar)
  ((lexer :accessor ll-parser-lexer :initarg :lexer)
   (show-parse-p :accessor ll-parser-show-parse-p :initarg :show-parse-p :initform nil)
   (stack :accessor ll-parser-stack :initform nil)
   (result-stack :accessor ll-parser-result-stack :initform nil)
   (phases :accessor ll-parser-phases :initform nil)
   (parsed-input :accessor ll-parser-parsed-input :initform nil)
   (position :accessor ll-parser-position :initform 0)
   (end-of-input-p :accessor ll-parser-end-of-input-p :initform nil)
   (state :accessor ll-parser-state :initform nil)
   (error-messages :accessor ll-parser-error-messages :initform nil)))

(defun ll-parser-result (ll-parser)
  (car (ll-parser-result-stack ll-parser)))

(defmethod initialize-instance :after ((this ll-parser) &key &allow-other-keys) nil)

(defmethod print-object ((this ll-parser) stream)
  (format stream "#<~A ~A state=~A~@[: \"~A\"~]>" (type-of this) (grammar-name this) (ll-parser-state this) (ll-parser-error-messages this)))

(defgeneric equal-grammar (g1 g2)
  (:method ((g1 grammar) (g2 grammar))
    (and (equal (grammar-start-symbol g1) (grammar-start-symbol g2))
	 (equal (grammar-terminals g1) (grammar-terminals g2))
	 (equal (grammar-nonterminals g1) (grammar-nonterminals g2))
	 (equal (grammar-productions g1) (grammar-productions g2))
	 (equal (grammar-nonterminal-firsts g1) (grammar-nonterminal-firsts g2))
	 (equal (grammar-followers g1) (grammar-followers g2))
	 (equal (grammar-nullable g1) (grammar-nullable g2)))))

(defstruct (input-token :named (:type list))
  (type)
  (value)
  (position))

(defun ll-parser-succeeded-p (ll-parser)
  (eq (ll-parser-state ll-parser) :succeeded))

(defun ll-parser-failed-p (ll-parser)
  (eq (ll-parser-state ll-parser) :failed))

(defun make-error-input-token (msg position)
  (make-input-token :type :error :value msg :position position))

(defun make-eof-input-token (position)
  (make-input-token :type :eof :value "End of input" :position position))

(defun error-input-token-p (input-token)
  (and (input-token-p input-token) (eq :error (input-token-type input-token))))

(defun eof-input-token-p (input-token)
  (and (input-token-p input-token) (eq :eof (input-token-type input-token))))

(defun serialize-parser (parser &key (serialize-production nil))
  (let* ((nonterminals (grammar-nonterminals parser))
	 (nonterminal-dimension (length nonterminals))
	 (terminals (grammar-terminals parser))
	 (terminal-dimension (1+ (length terminals))) ; the table contains a column for '$
	 (productions (grammar-productions parser))
	 (parse-table (grammar-ll1-table parser)))
    `(make-instance
      ',(type-of parser)
      :name ',(grammar-name parser)
      :start-symbol ',(grammar-start-symbol parser)
      :terminals ',terminals
      :nonterminals ',nonterminals
      :productions ,(if serialize-production `(make-array ,(length productions) :initial-contents (list ,@(mapcar serialize-production productions))) `',(grammar-productions parser))
      :nonterminal-firsts ',(grammar-nonterminal-firsts parser)
      :followers ',(grammar-followers parser)
      :nullable ',(grammar-nullable parser)
      :ll1-table ,(and parse-table `(make-array ',(list nonterminal-dimension terminal-dimension)
						:initial-contents ',(loop for i from 0 below nonterminal-dimension collect
									 (loop for j from 0 below terminal-dimension collect (aref parse-table i j)))))
;      :grammar-function-def ',(grammar-function-def parser)
      :default-result ',(grammar-default-result parser))))

(defun serialize-production (p &key ignoring-fields field-functions)
  `(make-production
    ,@(loop for field in '(:number :lhs :rhs :properties :result-arglist :result-arg-count :result-func-name :result-func-lambda :result-func)
	    for accessor in '(production-number production-lhs production-rhs production-properties production-result-arglist production-result-arg-count production-result-func-name production-result-func-lambda production-result-func)
	    unless (member field ignoring-fields)
	    nconc (list field (let ((value (funcall accessor p))
				    (field-processor (second (assoc field field-functions))))
				(cond ((not (null field-processor))
				       (funcall field-processor p field value))
				      ((or (numberp value) (characterp value) (stringp value) (member value '(t nil))) value)
				      (t `',value)))))))
