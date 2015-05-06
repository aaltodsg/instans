;;; -*- Mode: Lisp -*-

(in-package #:instans)

(defun form-with-result-p (form)
  (eq (car (last (butlast form))) :RESULT))

(defun split-rhs-by-result (form)
  (loop for rest on form
       for item = (car rest)
       while (not (eq item :RESULT))
       collect item into sans
       finally (return (values sans (second rest)))))

(defun rulestring (lhs rhs &optional result)
  (format nil "~A ::= ~S~@[ :result ~S~]" lhs rhs result))

(defun make-ebnf-rule (lhs rhs &optional result) (if result (list lhs ::= rhs :RESULT result) (list lhs ::= rhs)))
(defun ebnf-lhs (rule) (first rule))
(defun ebnf-rhs (rule) (getf (cdr rule) ::=))
(defun ebnf-result (rule) (getf (cdr rule) :RESULT))

(defun expand-lhs-rhs-result (lhs rhs result)
  (cond ((keywordp rhs) 
	 (error* "Cannot use a keyword (~S) as a grammar symbol" rhs))
	((and (atom rhs) (not (null rhs)))
	 (list (make-ebnf-rule lhs (list rhs) result)))
	((not (null result))
	 (cond ((not (or (member (car rhs) '(:OR :REP0 :REP1 :OPT)) (form-with-result-p rhs)))
		(dbg "~%expand-lhs-rhs-result ~A: has result, can be embedded in rhs" (rulestring lhs rhs result))
		(expand-lhs-rhs-result lhs (append rhs (list :result result)) nil))
	       (t
		(dbg "~%expand-lhs-rhs-result ~A: has result, cannot be embedded in rhs" (rulestring lhs rhs result))
		(let ((new-lhs (make-nonterminal lhs)))
		  (cons (make-ebnf-rule lhs (list new-lhs) result)
			(expand-lhs-rhs-result new-lhs rhs nil))))))
	((eq (car rhs) :OR)
	 (dbg "~%expand-lhs-rhs-result ~A: (eq (car rhs) :OR)" (rulestring lhs rhs result))
	 (loop for sub-rhs in (cdr rhs) nconc (expand-lhs-rhs-result lhs sub-rhs nil)))
	((member (car rhs) '(:REP0 :REP1 :OPT))
	 (dbg "~%expand-lhs-rhs-result ~A: (member (car rhs) '(:REP0 :REP1 :OPT))" (rulestring lhs rhs result))
	 (expand-rep-or-opt lhs rhs))
	(t; Flat rhs, may contain an embedded result
	 (when (form-with-result-p rhs)
	   (dbg "~%expand-lhs-rhs-result ~A: (form-with-result-p rhs)" (rulestring lhs rhs result))
	   (multiple-value-setq (rhs result) (split-rhs-by-result rhs)))
	 (multiple-value-bind (new-rhs sub-rules)
	     (expand-rhs lhs rhs)
	   (cons (make-ebnf-rule lhs new-rhs result) sub-rules)))))

(defun expand-rhs (lhs rhs)
  (dbg "~%expand-rhs: ~S ~S" lhs rhs)
  (loop with sub-rules = nil
	for item in rhs
	do (dbg "~%  rhs = ~S, item = ~S, new-rhs = ~S, new-rules = ~S" rhs item new-rhs sub-rules)
	when (keywordp item)
	do (error* "Cannot use a keyword (~S) as a grammar symbol" item)
	else when (and (atom item) (not (null item)))
	collect item into new-rhs
	else
	collect (let ((new-lhs (make-nonterminal lhs :sub)))
		  (setf sub-rules (nconc sub-rules (expand-lhs-rhs-result new-lhs item nil)))
		  new-lhs) into new-rhs
	finally (return (progn (dbg "~%rhs = ~S, new-rhs = ~S" rhs new-rhs)
			       (values new-rhs sub-rules)))))

(defun expand-rep-or-opt (lhs rhs)
  (let ((kind (car rhs))
	(sub-rhs (cdr rhs)))
    (cond ((form-with-result-p sub-rhs)
	   (dbg "~%expand-rep-or-opt ~A: (form-with-result-p sub-rhs)" (rulestring lhs rhs))
	   (let ((new-lhs (make-nonterminal lhs :result)))
	     (append (expand-rep-or-opt lhs (list kind new-lhs))
		     (multiple-value-bind (rhs-sans-result result) (split-rhs-by-result sub-rhs)
		       (expand-lhs-rhs-result new-lhs rhs-sans-result result)))))
	  (t ; Flat rhs, no result
	   (dbg "~%expand-rep-or-opt ~A: t" (rulestring lhs rhs))
	   (multiple-value-bind (new-rhs sub-rules)
	       (expand-rhs lhs sub-rhs)
	     (append (labels ((unit-results (m) (if (= m 1) '$0 `(list ,@(loop for i from 0 below m collect (result-var i)))))
			      (recursive-results (n) `(cons ,(unit-results (1- n)) ,(result-var (1- n)))))
		       (let* ((rhs-length (length sub-rhs)))
			 (case kind
			   (:rep0
			    (list (make-ebnf-rule lhs (append new-rhs (list lhs)) (recursive-results (1+ rhs-length)))
				  (make-ebnf-rule lhs nil '(progn nil))))
			   (:rep1 
			    ;; (list (make-ebnf-rule lhs (append new-rhs (list lhs)) (recursive-results (1+ rhs-length)))
			    ;; 	  (make-ebnf-rule lhs new-rhs (unit-results rhs-length))))
			    (let ((new-nonterminal (gensym (format nil "~A%" lhs))))
			      (list (make-ebnf-rule lhs (append new-rhs (list new-nonterminal)) (recursive-results (1+ rhs-length)))
				    (make-ebnf-rule new-nonterminal (list lhs))
				    (make-ebnf-rule new-nonterminal nil '(progn nil)))))
			   (:opt
			    (list (make-ebnf-rule lhs new-rhs `(opt-yes ,(unit-results rhs-length)))
				  (make-ebnf-rule lhs nil `(opt-no)))))))
		     sub-rules))))))


(defun expand-ebnf-rule (rule)
  (expand-lhs-rhs-result (ebnf-lhs rule) (ebnf-rhs rule) (ebnf-result rule)))

(defun expand-ebnf-rules (rules)
  (loop for rule in rules
	nconc (expand-ebnf-rule rule)))
