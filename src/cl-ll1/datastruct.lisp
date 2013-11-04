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
  (parser-symbol-value)
  (properties)
  (result-arglist)
  (result-arg-count)
  (result-func-name)
  (result-func-lambda)
  (result-func))

(define-class parser ()
  ((name :accessor parser-name :initarg :name :initform nil)
   (start-symbol :accessor parser-start-symbol :initarg :start-symbol :initform nil)
   (terminals :accessor parser-terminals :initarg :terminals :initform nil)
   (nonterminals :accessor parser-nonterminals :initarg :nonterminals :initform nil)
   (productions :accessor parser-productions :initarg :productions :initform nil)
   (nonterminal-firsts :accessor parser-nonterminal-firsts :initarg :nonterminal-firsts :initform nil)
   (followers :accessor parser-followers :initarg :followers :initform nil)
   (nullable :accessor parser-nullable :initarg :nullable :initform nil)
   (ll1-table :accessor parser-ll1-table :initarg :ll1-table :initform nil)
   (symbol-value-tag :accessor parser-symbol-value-tag :initarg :symbol-value-tag :initform nil)
   (parser-function-def :accessor parser-function-def :initarg :parser-function-def :initform nil)
   (default-result :accessor parser-default-result :initarg :default-result :initform nil)
   (state :accessor parser-state :initform nil)))

(define-class parsing ()
  ((parser :accessor parsing-parser :initarg :parser)
   (lexer :accessor parsing-lexer :initarg :lexer)
   (show-parse-p :accessor parsing-show-parse-p :initarg :show-parse-p :initform nil)
   (stack :accessor parsing-stack :initform nil)
   (result-stack :accessor parsing-result-stack :initform nil)
   (phases :accessor parsing-phases :initform nil)
   (parsed-input :accessor parsing-parsed-input :initform nil)
   (position :accessor parsing-position :initform 0)
   (end-of-input-p :accessor parsing-end-of-input-p :initform nil)
   (state :accessor parsing-state :initform nil)
   (error-message :accessor parsing-error-message :initform nil)))

(defun parsing-result (parsing)
  (car (parsing-result-stack parsing)))

(defmethod initialize-instance :after ((this parsing) &key &allow-other-keys) nil)

(defmethod print-object ((this parsing) stream)
  (format stream "#<~A ~A state=~A~@[: \"~A\"~]>" (type-of this) (parser-name (parsing-parser this)) (parsing-state this) (parsing-error-message this)))

(defgeneric parser-equal-grammar (p1 p2)
  (:method ((p1 parser) (p2 parser))
    (and (equal (parser-start-symbol p1) (parser-start-symbol p2))
	 (equal (parser-terminals p1) (parser-terminals p2))
	 (equal (parser-nonterminals p1) (parser-nonterminals p2))
	 (equal (parser-productions p1) (parser-productions p2))
	 (equal (parser-nonterminal-firsts p1) (parser-nonterminal-firsts p2))
	 (equal (parser-followers p1) (parser-followers p2))
	 (equal (parser-nullable p1) (parser-nullable p2)))))

(defstruct (input-token :named (:type list))
  (type)
  (value)
  (position))

(defun parsing-succeeded-p (parsing)
  (eq (parsing-state parsing) :succeeded))

(defun parsing-failed-p (parsing)
  (eq (parsing-state parsing) :failed))

(defun make-error-input-token (msg position)
  (make-input-token :type :error :value msg :position position))

(defun make-eof-input-token (position)
  (make-input-token :type :eof :value "End of input" :position position))

(defun error-input-token-p (input-token)
  (and (input-token-p input-token) (eq :error (input-token-type input-token))))

(defun eof-input-token-p (input-token)
  (and (input-token-p input-token) (eq :eof (input-token-type input-token))))

(defun serialize-parser (p &key (serialize-production nil))
  (let* ((nonterminals (parser-nonterminals p))
	 (nonterminal-dimension (length nonterminals))
	 (terminals (parser-terminals p))
	 (terminal-dimension (1+ (length terminals))) ; the table contains a column for '$
	 (productions (parser-productions p))
	 (parse-table (parser-ll1-table p)))
    `(make-instance
      'parser
      :name ',(parser-name p)
      :start-symbol ',(parser-start-symbol p)
      :terminals ',terminals
      :nonterminals ',nonterminals
      :productions ,(if serialize-production `(make-array ,(length productions) :initial-contents (list ,@(mapcar serialize-production productions))) `',(parser-productions p))
      :nonterminal-firsts ',(parser-nonterminal-firsts p)
      :followers ',(parser-followers p)
      :nullable ',(parser-nullable p)
      :ll1-table ,(and parse-table `(make-array ',(list nonterminal-dimension terminal-dimension)
						:initial-contents ',(loop for i from 0 below nonterminal-dimension collect
									 (loop for j from 0 below terminal-dimension collect (aref parse-table i j)))))
      :parser-function-def ',(parser-function-def p)
      :default-result ',(parser-default-result p))))

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
