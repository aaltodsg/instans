;;; -*- Mode: Lisp -*-

;;; Todo
;;; + Add action table
;;; + Produce example parse without actions
;;; + Redesing tables: one table[nonterminals, terminals+$] with entries for rhs, production number, another table[production] for actions and pop (or argument) counts
;;; + Add action code
;;; + Write turtle without actions and test
;;; + Write turtle with actions and test
;;; - Check pop counts for left factoring and left recursion removal
;;; - Test results with left factoring and left recursion removal

;;; -------------------- Auxiliary routines --------------------

(in-package #:instans)

(defun eassoc (tag alist)
  (let ((item (assoc tag alist)))
    (cond ((null item)
	   (error* "Tag ~A not found in alist ~A" tag alist))
	  (t
	   item))))

(defun tokenify-input (input)
  (mapcar #'(lambda (it) (if (consp it) (make-input-token :type (first it) :value (second it)) (make-input-token :type it :value it))) input))

;;; -------------------- Creating grammar  --------------------

(defun convert-from-rhs-list-form (rules)
  (dbg "Converting rules:")
  (loop for rule in rules do (dbg "  ~S" rule))
  (let ((new-rules
	 (cond ((find ::= (first rules))
		(expand-ebnf-rules rules))
	       (t
		(loop for rule in rules nconc (loop for rhs in (rest rule) collect (list (car rule) ::= rhs)))))))
    (dbg "Converted rules to:")
    (loop for rule in new-rules do (dbg "  ~S" rule))
    (dbg "~&")
    new-rules))

(defun create-grammar (name specs &rest keys &key &allow-other-keys)
  (let* ((rules (convert-from-rhs-list-form (copy-tree specs)))
	 (productions (loop for i from 1 for p in rules collect (make-production :lhs (car p) :number i :rhs (getf (cdr p) ::=) :properties (progn (remf (cdr p) ::=) (cdr p)))))
	 (grammar-symbols nil)
	 (nonterminals (delete-duplicates (mapcar #'production-lhs productions)))
	 (start-symbol (production-lhs (first productions))))
    (unless (getf keys :default-result)
      (setf (getf keys :default-result) '(if (= 1 (length $*)) $0 $*)))
    (loop for p in productions do (loop for sym in (production-rhs p) do (pushnew sym grammar-symbols :test #'equal)))
    (dbg "~%nonterminals: ~A~%grammar-symbols: ~A~%terminals: ~A~&"
	 nonterminals grammar-symbols 
	 (list-difference grammar-symbols nonterminals))
    (apply #'make-instance 'parser
	   :name name
	   :start-symbol start-symbol
	   :productions productions
	   :nonterminals nonterminals
	   :terminals (list-difference grammar-symbols nonterminals)
	   keys)))

;;; -------------------- Accessors etc.--------------------

(defun nonterminal-productions (parser nonterminal)
  (loop for p in (parser-productions parser)
	when (eq (production-lhs p) nonterminal)
	collect p))

(defun nonterminal-rhs-list (parser nonterminal)
  (mapcar #'production-rhs (nonterminal-productions parser nonterminal)))

(defun make-new-nonterminal (parser prefix-nonterminal)
  (let ((nonterminals (parser-nonterminals parser))
	(terminals (parser-terminals parser)))
    (loop for new-nonterminal = (intern (format nil "~A'" prefix-nonterminal))
          while (or (member new-nonterminal nonterminals)
		    (member new-nonterminal terminals))
	  do (setf prefix-nonterminal new-nonterminal)
	  finally (return new-nonterminal))))

;;; -------------------- Left recursion elimination --------------------

(defun remove-immediate-left-recursion-of-nonterminal (parser nonterminal)
  (loop for rhs in (nonterminal-rhs-list parser nonterminal)
	when (eq (first rhs) nonterminal)
	collect rhs into recursive
	else collect rhs into non-recursive
	finally (cond ((null recursive) nil)
		      (t
		       (let ((new-nonterminal (make-new-nonterminal parser nonterminal)))
			 (return (values (append (loop for rhs in non-recursive collect (make-production :lhs nonterminal :rhs (append rhs (list new-nonterminal))))
						 (loop for rhs in recursive collect (make-production :lhs new-nonterminal :rhs (append (rest rhs) (list new-nonterminal))))
						 (list (make-production :lhs new-nonterminal :rhs nil)))
					 new-nonterminal)))))))

(defun remove-immediate-left-recursion (parser)
  (let ((result-productions nil)
	(modifiedp nil)
	(result-nonterminals nil))
    (dolist (nonterminal (parser-nonterminals parser))
      (push nonterminal result-nonterminals)
      (multiple-value-bind (new-productions new-nonterminal)
	  (remove-immediate-left-recursion-of-nonterminal parser nonterminal)
	(cond ((null new-productions)
	       (setf result-productions (append result-productions (nonterminal-productions parser nonterminal))))
	      (t
	       (pushnew new-nonterminal result-nonterminals)
	       (setf result-productions (append result-productions new-productions))
	       (setf modifiedp t)))))
    (cond ((not modifiedp)
	   parser)
	  (t
	   (loop for i from 0 for p in result-productions do (setf (production-number p) i))
	   (make-instance 'parser
			  :name (parser-name parser)
			  :start-symbol (parser-start-symbol parser)
			  :terminals (parser-terminals parser)
			  :nonterminals (nreverse result-nonterminals)
			  :productions result-productions)))))

;;; -------------------- Left factoring --------------------

(defun left-factor-one (parser nonterminal productions)
  (dbg "~%left-factor-one ~A, ~A" nonterminal productions)
  (loop with classes = nil
	with modifiedp = nil
	for p in productions
	for rhs = (production-rhs p)
	for class = (find (first rhs) classes :key #'(lambda (x) (first (first x))) :test #'equal)
	do (dbg "~S => ~S" nonterminal rhs)
	when (null class)
	do (push (list rhs) classes)
	else do (progn (setq modifiedp t)
		       (rplacd (last class) (list rhs)))
	finally (progn (dbg "~%left-factor-one: modifiedp = ~A, classes =  ~A" modifiedp classes)
		       (and modifiedp
			    (loop with new-nt-prods-list = nil
				  with modified-productions = nil
				  for class in classes
				  when (null (rest class)) do (push (make-production :lhs nonterminal :rhs (first class)) modified-productions)
				  else do (loop with prefix = nil
						with new-nonterminal = (make-new-nonterminal parser nonterminal)
						while (every #'(lambda (x) (eq (first x) (first (first class)))) (rest class))
						do (progn (push (first (first class)) prefix)
							  (setf class (mapcar #'rest class)))
						finally (progn (setf prefix (nreverse (cons new-nonterminal prefix)))
							       (dbg "producing new rules for ~S, prefix = ~S" new-nonterminal prefix)
							       (push (make-production :lhs nonterminal :rhs prefix) modified-productions)
							       (setf new-nt-prods-list
								     (append new-nt-prods-list (cons new-nonterminal (loop for rhs in class collect (make-production :lhs new-nonterminal :rhs rhs)))))))
				  finally (return-from left-factor-one (values (cons nonterminal (nreverse modified-productions)) new-nt-prods-list)))))))

(defun left-factoring (parser)
  (let ((result-productions nil)
	(modifiedp nil)
	(nonterminal-and-productions-list (loop for nonterminal in (parser-nonterminals parser) collect (cons nonterminal (nonterminal-productions parser nonterminal)))))
    (loop while nonterminal-and-productions-list
	  for (nonterminal . productions) = (pop nonterminal-and-productions-list)
	  do (multiple-value-bind (modified-nt-prods new-nt-prods-list)
		 (left-factor-one parser nonterminal productions)
	       (dbg "left-factor-one ~S => modified-nt-prods = ~S  new-nt-prods-list = ~S" productions modified-nt-prods new-nt-prods-list)
	       (cond ((null modified-nt-prods)
		      (setf result-productions (append result-productions productions)))
		     (t
		      (setf modifiedp t)
		      (setf result-productions (append result-productions (rest modified-nt-prods)))
		      (setf nonterminal-and-productions-list (cons new-nt-prods-list nonterminal-and-productions-list))))
	       (dbg "nonterminal-and-productions-list = ~S " nonterminal-and-productions-list)))
    (cond ((not modifiedp)
	   parser)
	  (t
	   (loop for i from 0 for p in result-productions do (setf (production-number p) i))
	   (make-instance 'parser
			  :name (parser-name parser)
			  :start-symbol (parser-start-symbol parser)
			  :terminals (parser-terminals parser)
			  :nonterminals (delete-duplicates (mapcar #'production-lhs result-productions))
			  :productions result-productions)))))

;;; -------------------- FIRST --------------------

(defun symbol-list-firsts (list nonterminal-first-alist)
  (dbg "enter symbol-list-firsts: ~A, ~A" list nonterminal-first-alist)
  (dbginc 2)
  (let ((epsilonp t)
	(firsts nil))
    (loop while (and epsilonp list)
	  do (progn (dbg "while epsilonp: firsts = ~A" firsts)
		    (dbginc 2)
		    (let* ((symbol (pop list))
			   (nt-item (assoc symbol nonterminal-first-alist)))
		      (cond ((null nt-item)
			     (pushnew symbol firsts)
			     (setf epsilonp nil))
			    (t
			     (setf firsts (copy-list (list-union firsts (remove nil (rest nt-item)))))
			     (setf epsilonp (member nil (rest nt-item)))))
		      (dbg "nt-item = ~S, firsts = ~S, epsilonp = ~S" nt-item firsts epsilonp))
		    (dbginc -2)))
    (when (and epsilonp (null list))
      (push nil firsts))
    (dbg "end while epsilonp")
    (dbginc -2)
    (dbg "exit symbol-list-firsts: firsts = ~A" firsts)
    firsts))

(defun nonterminal-firsts (parser)
  (dbg "enter nonterminal-firsts:")
  (dbginc 2)
  (dbg (print-grammar parser nil))
  (dbginc -2)
  (let* ((productions (parser-productions parser))
	 (nonterminals (parser-nonterminals parser))
	 (terminals (parser-terminals parser))
	 (nonterminal-first-alist (mapcar #'(lambda (x) (list x)) nonterminals))
	 (modifiedp t))
    (dbg "nonterminal-first-alist = ~S" nonterminal-first-alist)
    (loop while modifiedp
	  do (progn (dbg "while modifiedp")
		    (dbginc 2)
		    (setq modifiedp nil)
		    (loop for p in productions
			  for new-firsts = (progn (dbg "production ~A" (print-production p terminals nil))
						  (dbginc 2)
						  (prog1 (symbol-list-firsts (production-rhs p) nonterminal-first-alist)
						    (dbginc -2)))
			  for first-item = (eassoc (production-lhs p) nonterminal-first-alist)
			  unless (subsetp new-firsts first-item)
			  do (progn (setf (rest first-item) (copy-list (list-union (rest first-item) new-firsts)))
				    (setf modifiedp t)))
		    (dbginc -2)))
    (dbg "exit nonterminal-firsts: result = ~A" nonterminal-first-alist)
    (setf (parser-nonterminal-firsts parser) 
	  (mapcar #'(lambda (f)
		      (cons (first f)
			    (sort (rest f) #'(lambda (x y)
					       (or (null y)
						   (and (not (null x))
							(< (terminal-position x terminals)
							   (terminal-position y terminals))))))))
		  nonterminal-first-alist))))

;;; -------------------- FOLLOW --------------------

(defun followers (parser)
  (dbg "enter followers")
  (dbginc 2)
  (when (null (parser-nonterminal-firsts parser))
    (nonterminal-firsts parser))
  (let* ((productions (parser-productions parser))
	 (nonterminal-first-alist (parser-nonterminal-firsts parser))
	 (nonterminals (parser-nonterminals parser))
	 (terminals (append (parser-terminals parser) (list '$)))
	 (followers-alist (mapcar #'(lambda (x) (list x)) nonterminals))
	 (modifiedp t))
    (push '$ (rest (eassoc (parser-start-symbol parser) followers-alist)))
    (dbg "nonterminal-first-alist = ~S" nonterminal-first-alist)
    (loop while modifiedp
	  do (progn (dbg "while modifiedp")
		    (dbginc 2)
		    (setf modifiedp nil)
		    (loop for p in productions
			  for nt = (production-lhs p)
			  for rhs = (production-rhs p)
			  do (progn
			       (dbg "~A" (print-production p terminals nil))
			       (dbginc 2)
			       (loop while rhs
				     for symbol = (pop rhs)
				     for followers-item = (assoc symbol followers-alist)
				     do (progn (dbg "while rhs")
					       (dbg "followers-alist = ~S" followers-alist)
					       (dbginc 2)
					       (dbg "Symbol = ~S" symbol)
					       (dbg "Followers-item = ~S" followers-item)
					       (when followers-item
						 (let* ((rest-firsts
							 (symbol-list-firsts rhs nonterminal-first-alist))
							(epsilonp (member nil rest-firsts))
							(new-followers (remove nil rest-firsts)))
						   (dbg "New-followers = ~S" new-followers)
						   (when epsilonp
						     (dbg "Epsilonp = t, using lhs followers")
						     (setf new-followers 
							   (copy-list (list-union new-followers (rest (eassoc nt followers-alist)))))
						     (dbg "New-followers = ~S" new-followers))
						   (cond ((not (subsetp new-followers (rest followers-item)))
							  (setf (rest followers-item)
								(copy-list (list-union new-followers (rest followers-item))))
							  (dbg "Modifying followers of ~S to ~S" symbol (rest followers-item))
							  (setf modifiedp t))
							 (t
							  (dbg "Not modifying followers of ~s, still ~s" symbol (rest followers-item)))))))
				    (dbginc -2))
			       (dbginc -2)))
		    (dbginc -2)))
    (dbginc -2)
    (dbg "Followers-alist is ~S" followers-alist)
    (dbg "Terminals = ~S" terminals)
    (setf (parser-followers parser) 
	  (mapcar #'(lambda (f)
		      (cons (first f)
			    (sort (copy-list (rest f))
				  #'(lambda (x y)
				      (dbg "Sort: comparing ~S (~S) and ~S (~S) -> ~S" 
					   x (terminal-position x terminals) y (terminal-position y terminals)
					   (< (terminal-position x terminals)
					      (terminal-position y terminals)))
				      (< (terminal-position x terminals)
					 (terminal-position y terminals))))))
		  followers-alist))
    (dbg "Result is ~S" (parser-followers parser))
    (parser-followers parser)))

;;; -------------------- Creating LL(1) parsing table --------------------

(defun generate-ll1-table (parser)
  (let* ((terminals (append (parser-terminals parser) (list '$)))
	 (nonterminals (parser-nonterminals parser))
	 (followers-alist (or (parser-followers parser) (followers parser)))
	 (nonterminal-first-alist (parser-nonterminal-firsts parser))
	 (parse-table (make-array (list (length nonterminals) (length terminals)) :initial-element 'error))
	 (productions (parser-productions parser))
	 (default-result (parser-default-result parser))
	 (errors nil))
					;    (print-grammar parser)
    (flet ((make-arglist (n) (loop for i from 0 below n collect (intern (format nil "$~D" i)))))
      (loop for i from 0
	    for p in productions
	    for lhs = (production-lhs p)
	    for rhs = (production-rhs p)
	    for result = (getf (production-properties p) :result)
	    for rhs-firsts = (symbol-list-firsts rhs nonterminal-first-alist)
	    do (progn
		 (setf (production-number p) i)
		 (dbg "Production = [~D]: result = ~S, default-result = ~S" (production-number p) result default-result)
		 (dolist (s (remove nil rhs-firsts))
		   (dbg "~%(setf (aref parse-table ~d ~d) ~S)" (nonterminal-position lhs nonterminals) (terminal-position s terminals) p)
					;(setf (aref parse-table (nonterminal-position lhs nonterminals) (terminal-position s terminals)) p)
		   (setf (aref parse-table (nonterminal-position lhs nonterminals) (terminal-position s terminals)) (production-number p)))
		 (when (member nil rhs-firsts)
		   (dolist (s (rest (eassoc lhs followers-alist)))
		     (cond ((not (eq (aref parse-table (nonterminal-position lhs nonterminals) (terminal-position s terminals)) 'error))
			    (dbg "~%(setf (aref parse-table ~d ~d)) CONFLICT!" (nonterminal-position lhs nonterminals) (terminal-position s terminals))
			    (push (list lhs s p) errors))
			   (t
			    (dbg "~%(setf (aref parse-table ~d ~d) ~S)" (nonterminal-position lhs nonterminals) (terminal-position s terminals) p)
					;(setf (aref parse-table (nonterminal-position lhs nonterminals) (terminal-position s terminals)) p)
			    (setf (aref parse-table (nonterminal-position lhs nonterminals) (terminal-position s terminals)) (production-number p))))))
		 (let* ((with-args-p (and (consp result) (eq (car result) 'with-args)))
			(result-arglist (if with-args-p
					    (if (numberp (second result)) (make-arglist (second result)) (second result))
					    (make-arglist (length rhs))))
			(result-func-name (and (or result default-result) (intern (format nil "~A-~D" (production-lhs p) (production-number p)))))
			(result-func-body (if with-args-p `(progn ,@(cddr result)) (or result default-result)))
			(result-func-lambda `(lambda ($p ,@result-arglist &aux ($* (list ,@result-arglist))) (declare (ignorable $p ,@result-arglist $*)) ,result-func-body))
					;		     (result-func (if (or result default-result) (compile nil result-func-lambda)))
			)
		   (cond ((null result-func-name)
			  (setf (production-result-arglist p) nil)
			  (setf (production-result-arg-count p) nil)
			  (setf (production-result-func-name p) nil)
			  (setf (production-result-func-lambda p) nil)
					;		       (setf (production-result-func p) nil)
			  )
			 (t
			  (setf (production-result-arglist p) result-arglist)
			  (setf (production-result-arg-count p) (length result-arglist))
			  (setf (production-result-func-name p) result-func-name)
			  (setf (production-result-func-lambda p) result-func-lambda)
					;		       (setf (production-result-func p) result-func)
			  (if with-args-p (barf "production = ~S" p))
			  )))))
      (setf (parser-ll1-table parser) parse-table)
      (cond ((not (null errors))
	     (barf "~%Not an LL(1) parser:~%")
	     (print-grammar parser)
	     ;;(print-ll1-table parser *error-output*)
	     (dolist (error (nreverse errors))
	       (let ((lhs (first error))
		     (s (second error))
		     (p (third error)))
		 ;; (barf "~%~S ~S ~S" lhs s p)
		 (barf "~%    parse-table[~S, ~S]: conflict ~A vs. ~A" 
		 	 lhs s 
		 	 (print-production (nth (aref parse-table (nonterminal-position lhs nonterminals) (terminal-position s terminals)) productions)
		 			   terminals nil)
		 	 (print-production p terminals nil))
		 ))
	     (error* "Errors in parse-table creation"))
	    (t
	     parse-table)))))

;;; -------------------- LL(1) parsing --------------------

(defun execute-rule (func p args)
  (apply func p args))

(defun nonterminal-position (symbol nonterminals)
					; (position symbol nonterminals)
  (let ((p (position symbol nonterminals)))
    (when (null p)
      (error* "nonterminal-position ~S -> nil" symbol))
    p))

(defun terminal-position (symbol terminals)
					;  (position symbol terminals :test #'EQUAL))
  (let ((p (position symbol terminals)))
    (when (null p)
      (error* "terminal-position ~S -> nil" symbol))
    p))

(defmacro parser-symbol-value (parser x)
  `(get ,x (parser-symbol-value-tag ,parser)))

(defun assign-numbers-to-symbols-and-productions (parser &optional show-parse-p)
  (declare (ignorable show-parse-p))
  (setf (parser-symbol-value-tag parser) (gensym "SYMBOL-VALUE"))
  (loop for i from 0 below (length (parser-productions parser))
	for p = (aref (parser-productions parser) i)
	do (progn (setf (production-parser-symbol-value p) (1- (- i)))
		  (when show-parse-p (barf "~%(production-parser-symbol-value ~S) = ~S"
					   (cons (production-lhs p) (cons '-> (production-rhs p))) (production-parser-symbol-value p)))
		  ))
  (loop for nonterminal in (parser-nonterminals parser)
	do (progn (setf (parser-symbol-value parser nonterminal) 
			(nonterminal-position nonterminal (parser-nonterminals parser)))
		  (when show-parse-p (barf "~%(parser-symbol-value ~S) = ~S" nonterminal (parser-symbol-value parser nonterminal)))
		  ))
  (loop for terminal in (parser-terminals parser)
	do (progn (setf (parser-symbol-value parser terminal)
			(+ (length (parser-nonterminals parser))
			   (terminal-position terminal (parser-terminals parser)))))
       (when show-parse-p (barf "~%(parser-symbol-value ~S) = ~S" terminal (parser-symbol-value parser terminal)))
       )
  (setf (parser-symbol-value parser '$) (+ (length (parser-nonterminals parser)) (length (parser-terminals parser))))
  (when show-parse-p (barf "~%(parser-symbol-value ~S) = ~S" '$ (parser-symbol-value parser '$)))
  )

(defun number-to-symbol-or-production (n parser)
  (let* ((production-count (length (parser-productions parser)))
	 (nonterminal-count (length (parser-nonterminals parser)))
	 (terminal-count (length (parser-terminals parser)))
	 (symbol-count (+ nonterminal-count terminal-count)))
    (cond ((not (numberp n)) nil)
	  ((or (< n (1- (- production-count))) (> n symbol-count))
	   (error* "Number ~D does not occur in parser symbol or production numbers" n))
	  ((< n 0)
	   (list 'production (elt (parser-productions parser) (1- (- n)))))
	  ((< n nonterminal-count)
	   (list 'nonterminal (elt (parser-nonterminals parser) n)))
	  ((< n symbol-count)
	   (list 'terminal (elt (parser-terminals parser) (- n nonterminal-count))))
	  (t (list 'end-symbol '$)))))

(defgeneric make-parsing-error-message (lexer position msg args)
  (:method ((lexer abstract-lexer) position msg args)
    (cond ((null position)
	   (format nil "~A" (apply #'format nil msg args)))
	  (t
	   (multiple-value-bind (row column)
	       (lexer-position-to-row-and-column lexer position)
	     (format nil "~A:~A: ~A" row column (apply #'format nil msg args)))))))

(defvar *parsing* nil)

(defun return-from-parser (succeededp &optional fmt &rest args)
  (let ((parsing *parsing*))
    (setf (parsing-state parsing) (if succeededp :succeeded :failed))
    (setf (parsing-phases parsing) (nreverse (parsing-phases parsing)))
    (unless succeededp
      (setf (parsing-error-message parsing) (make-parsing-error-message (parsing-lexer parsing) (parsing-position parsing) fmt args)))
    (throw 'parsed parsing)))

(defun parsing-failure (fmt &rest args)
  (apply #'return-from-parser nil fmt args))

(defun parsing-success ()
  (return-from-parser t))

(defun ll-parse (parsing)
  (catch 'parsed
    (let* ((parser (parsing-parser parsing))
	   (lexer (parsing-lexer parsing))
	   (show-parse-p (parsing-show-parse-p parsing))
	   (parse-table (parser-ll1-table parser))
	   (nonterminals (parser-nonterminals parser))
					;(terminals (append (parser-terminals parser) (list '$)))
	   (nonterminal-count (length nonterminals))
	   (productions (parser-productions parser)))
      (setf *parsing* parsing)
      (assign-numbers-to-symbols-and-productions parser show-parse-p)
      (setf (parsing-stack parsing) (list (parser-symbol-value parser (parser-start-symbol parser)) (parser-symbol-value parser '$)))
      (labels ((nonterminalp (parser-symbol-value) (< -1 parser-symbol-value nonterminal-count))
	       (terminalp (parser-symbol-value) (<= nonterminal-count parser-symbol-value))
	       (parse-table-entry (nonterminal-pos terminal-pos)
		 ;(barf "parse-table-entry ~A ~A" nonterminal-pos terminal-pos)
		 (aref parse-table nonterminal-pos (- terminal-pos nonterminal-count)))
	       (next-input-token ()
		 (and (not (parsing-end-of-input-p parsing))
		      (let ((input-token (get-input-token lexer)))
			(when show-parse-p (barf "~%lexer yields ~S" input-token))
			(cond ((error-input-token-p input-token)
			       (parsing-failure "Lexer error ~A" (input-token-value input-token)))
			      ((eof-input-token-p input-token)
			       (setf (parsing-end-of-input-p parsing) t)
			       (make-input-token :type (parser-symbol-value parser '$)))
			      (t
			       (let ((token-symbol-value (parser-symbol-value parser (second input-token))))
				 (unless token-symbol-value (error* "Could not find the symbol value of token ~A" (second input-token)))
				 (setf (second input-token) token-symbol-value)
				 input-token)))))))
	(loop with input-token = (next-input-token)
	      for round from 0
	      while (and (parsing-stack parsing) input-token)
	      do (when show-parse-p (barf "~%Round ~D:~&input-token =  ~S~&stack =  ~A~&result ="
					  round input-token (reverse (parsing-stack parsing)))
		       (loop for v in (parsing-result-stack parsing) do (let ((*print-right-margin* 150)) (barf "  ~S" v))))
	      do (progn
		   (setf (parsing-position parsing) (input-token-position input-token))
		   (when show-parse-p (push (list (copy-list (parsing-stack parsing)) nil (copy-list (parsing-parsed-input parsing))) (parsing-phases parsing)))
		   (let ((top (first (parsing-stack parsing)))
			 (symbol (input-token-type input-token)))
		     (when show-parse-p (barf "~%Top = ~S, symbol = ~S" (number-to-symbol-or-production top parser) (number-to-symbol-or-production symbol parser)))
		     (cond ((terminalp top)
			    (when show-parse-p (barf "Terminal ~S" (number-to-symbol-or-production top parser)))
			    (cond ((equal top symbol)
				   (pop (parsing-stack parsing))
				   (push (input-token-value input-token) (parsing-result-stack parsing))
				   (when show-parse-p
				     (setf (rest (last (first (parsing-phases parsing)))) (list (list 'advance top)))
				     (push symbol (parsing-parsed-input parsing)))
				   (setf input-token (next-input-token)))
				  (t
				   (parsing-failure "Expecting ~A instead of ~A (~A)" 
					    (second (number-to-symbol-or-production top parser)) (second (number-to-symbol-or-production symbol parser)) input-token))))
			   ((nonterminalp top)
			    (when show-parse-p (barf "Nonterminal ~S (~S)" (number-to-symbol-or-production top parser) top))
			    (let ((table-value (parse-table-entry top symbol)))
;			      (when show-parse-p (barf "haa 0"))
			      (cond ((eq table-value 'error)
				     (when show-parse-p (barf "No legal action for nonterminal ~A and input symbol ~A" top symbol))
				     (parsing-failure "No legal action for nonterminal ~A and input symbol (~A ~S))"
					      (second (number-to-symbol-or-production top parser)) (second (number-to-symbol-or-production (input-token-type input-token) parser))
					      (input-token-value input-token)))
				    (t
;				     (when show-parse-p (barf "haa 1"))
				     (let* ((p (elt productions table-value))
					    (rhs (production-rhs p)))
;				       (when show-parse-p (barf "haa 2"))
				       (when show-parse-p (setf (rest (last (first (parsing-phases parsing)))) (list (cons 'push rhs))))
				       (pop (parsing-stack parsing))
					;(push (production-number p) (parsing-stack parsing))
				       (push (production-parser-symbol-value p) (parsing-stack parsing))
					;(dolist (x (reverse rhs)) (push x (parsing-stack parsing)))
				       (dolist (x (reverse rhs)) (push (parser-symbol-value parser x) (parsing-stack parsing)))
;				       (when show-parse-p (barf "haa 2"))
				       )))))
			   (t ; (productionp top)
					;			    (let ((p (elt productions top)))
			    (let* ((pnum (- (1+ top)))
				   (p (elt productions pnum)))
			      (when show-parse-p (barf "Production number ~D: ~S" pnum p))
			      (pop (parsing-stack parsing))
			      (when show-parse-p (setf (rest (last (first (parsing-phases parsing)))) (list (list 'execute (format nil "[~d]" (production-number p))))))
			      (let ((result-func (production-result-func p)))
				(when result-func
				  (let ((args nil))
				    (loop repeat (production-result-arg-count p) do (push (pop (parsing-result-stack parsing)) args))
				    (when show-parse-p (barf "Rule [~D], arg-count = ~D" (production-number p) (production-result-arg-count p)))
				    (let ((values (multiple-value-list (execute-rule result-func p args))))
				      (when show-parse-p (barf "  Execute rule [~D] with args = ~A ->~&  ~A" (production-number p) args values))
				      (loop for value in values do (push value (parsing-result-stack parsing))))))))))))
	      finally (cond ((and (null (parsing-stack parsing)) (null input-token))
			     (when show-parse-p (push (list nil nil (copy-list (parsing-parsed-input parsing)) (list 'accept nil)) (parsing-phases parsing)))
			     (pop (parsing-result-stack parsing)) ; Remove the value corresponding '$
			     (parsing-success))
			    ((null (parsing-stack parsing))
			     (parsing-failure "Extraneous input ~A" input-token))
			    (t
			     (parsing-failure "End of input while expecting ~A" (second (number-to-symbol-or-production (first (parsing-stack parsing)) parser))))))))))

(defgeneric parse (parser lexer)
  (:method ((parser parser) (lexer abstract-lexer))
    (let ((parsing (make-instance 'parsing :parser parser :lexer lexer)))
      (ll-parse parsing))))

;;; -------------------- Generate an LL(1) parser --------------------

(defun opt-yes (x)
  (list 'opt-yes x))

(defun opt-no ()
  (list 'opt-no nil))

(defun opt-yes-p (x)
  (and (consp x) (eq (car x) 'opt-yes)))

(defun opt-no-p (x)
  (and (consp x) (eq (car x) 'opt-no)))

(defun opt-value (x)
  (second x))

(defun generate-ll1-parser (name rules-or-file &rest keys &key (error-on-non-ll1-parser-p t) warn-about-transformations-p (warn-stream *error-output*) &allow-other-keys)
  "Generate an LL(1) parser from a set of rules (or a file)"
  (remf keys :warn-about-transformations-p)
  (remf keys :warn-stream)
  (let* ((rules (cond ((consp rules-or-file) rules-or-file)
		     (t
		      (with-open-file (input-stream rules-or-file)
			(read input-stream)))))
	 (g1 (apply #'create-grammar name rules keys))
	 (g2 (remove-immediate-left-recursion g1))
	 (g3 (left-factoring g2)))
    (when (not (parser-equal-grammar g1 g2))
      (cond (error-on-non-ll1-parser-p
	     (error* "This is not an LL1-parser; you should remove left recursion"))
	    (warn-about-transformations-p
	     (print-grammar g1)
	     (format warn-stream "~&Left recursion removed~&"))))
    (when (not (parser-equal-grammar g2 g3))
      (cond (error-on-non-ll1-parser-p
	     (error* "This is not an LL1-parser; it needs left factoring"))
	    (warn-about-transformations-p 
	     (print-grammar g2)
	     (format warn-stream "~&Left factoring~&"))))
    (nonterminal-firsts g3)
    (followers g3)
    (generate-ll1-table g3)
    (print-grammar g3)
    g3))
