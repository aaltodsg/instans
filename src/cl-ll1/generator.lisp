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

(defun create-grammar (name class specs &rest keys &key &allow-other-keys)
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
    (apply #'make-instance class
	   :name name
	   :start-symbol start-symbol
	   :productions productions
	   :nonterminals nonterminals
	   :terminals (list-difference grammar-symbols nonterminals)
	   keys)))

(defun create-parser-grammar (parser specs)
  (let* ((rules (convert-from-rhs-list-form (copy-tree specs)))
	 (productions (loop for i from 1 for p in rules collect (make-production :lhs (car p) :number i :rhs (getf (cdr p) ::=) :properties (progn (remf (cdr p) ::=) (cdr p)))))
	 (grammar-symbols nil)
	 (nonterminals (delete-duplicates (mapcar #'production-lhs productions)))
	 (start-symbol (production-lhs (first productions))))
    (loop for p in productions do (loop for sym in (production-rhs p) do (pushnew sym grammar-symbols :test #'equal)))
    (dbg "~%nonterminals: ~A~%grammar-symbols: ~A~%terminals: ~A~&"
	 nonterminals grammar-symbols 
	 (list-difference grammar-symbols nonterminals))
    (setf (grammar-start-symbol parser) start-symbol)
    (setf (grammar-productions parser) productions)
    (setf (grammar-nonterminals parser) nonterminals)
    (setf (grammar-terminals parser) (list-difference grammar-symbols nonterminals)))
  parser)

;;; -------------------- Accessors etc.--------------------

(defun nonterminal-productions (grammar nonterminal)
  (loop for p in (grammar-productions grammar)
	when (eq (production-lhs p) nonterminal)
	collect p))

(defun nonterminal-rhs-list (grammar nonterminal)
  (mapcar #'production-rhs (nonterminal-productions grammar nonterminal)))

(defun make-new-nonterminal (grammar prefix-nonterminal)
  (let ((nonterminals (grammar-nonterminals grammar))
	(terminals (grammar-terminals grammar)))
    (loop for new-nonterminal = (intern (format nil "~A'" prefix-nonterminal))
          while (or (member new-nonterminal nonterminals)
		    (member new-nonterminal terminals))
	  do (setf prefix-nonterminal new-nonterminal)
	  finally (return new-nonterminal))))

;;; -------------------- Left recursion elimination --------------------

(defun remove-immediate-left-recursion-of-nonterminal (grammar nonterminal)
  (loop for rhs in (nonterminal-rhs-list grammar nonterminal)
	when (eq (first rhs) nonterminal)
	collect rhs into recursive
	else collect rhs into non-recursive
	finally (cond ((null recursive) nil)
		      (t
		       (let ((new-nonterminal (make-new-nonterminal grammar nonterminal)))
			 (return (values (append (loop for rhs in non-recursive collect (make-production :lhs nonterminal :rhs (append rhs (list new-nonterminal))))
						 (loop for rhs in recursive collect (make-production :lhs new-nonterminal :rhs (append (rest rhs) (list new-nonterminal))))
						 (list (make-production :lhs new-nonterminal :rhs nil)))
					 new-nonterminal)))))))

(defun remove-immediate-left-recursion (grammar &key updatep)
  (let ((result-productions nil)
	(modifiedp nil)
	(result-nonterminals nil))
    (dolist (nonterminal (grammar-nonterminals grammar))
      (push nonterminal result-nonterminals)
      (multiple-value-bind (new-productions new-nonterminal)
	  (remove-immediate-left-recursion-of-nonterminal grammar nonterminal)
	(cond ((null new-productions)
	       (setf result-productions (append result-productions (nonterminal-productions grammar nonterminal))))
	      (t
	       (pushnew new-nonterminal result-nonterminals)
	       (setf result-productions (append result-productions new-productions))
	       (setf modifiedp t)))))
    (values
     (cond ((not modifiedp)
	    grammar)
	   (t
	    (loop for i from 0 for p in result-productions do (setf (production-number p) i))
	    (cond ((not updatep)
		   (make-instance (type-of grammar)
				  :name (grammar-name grammar)
				  :start-symbol (grammar-start-symbol grammar)
				  :terminals (grammar-terminals grammar)
				  :nonterminals (nreverse result-nonterminals)
				  :productions result-productions))
		  (t
		   (setf (grammar-nonterminals grammar) (nreverse result-nonterminals))
		   (setf (grammar-productions grammar) result-productions)
		   grammar))))
     modifiedp)))

;;; -------------------- Left factoring --------------------

(defun left-factor-one (grammar nonterminal productions)
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
						with new-nonterminal = (make-new-nonterminal grammar nonterminal)
						while (every #'(lambda (x) (eq (first x) (first (first class)))) (rest class))
						do (progn (push (first (first class)) prefix)
							  (setf class (mapcar #'rest class)))
						finally (progn (setf prefix (nreverse (cons new-nonterminal prefix)))
							       (dbg "producing new rules for ~S, prefix = ~S" new-nonterminal prefix)
							       (push (make-production :lhs nonterminal :rhs prefix) modified-productions)
							       (setf new-nt-prods-list
								     (append new-nt-prods-list (cons new-nonterminal (loop for rhs in class collect (make-production :lhs new-nonterminal :rhs rhs)))))))
				  finally (return-from left-factor-one (values (cons nonterminal (nreverse modified-productions)) new-nt-prods-list)))))))

(defun left-factoring (grammar &key updatep)
  (let ((result-productions nil)
	(modifiedp nil)
	(nonterminal-and-productions-list (loop for nonterminal in (grammar-nonterminals grammar) collect (cons nonterminal (nonterminal-productions grammar nonterminal)))))
    (loop while nonterminal-and-productions-list
	  for (nonterminal . productions) = (pop nonterminal-and-productions-list)
	  do (multiple-value-bind (modified-nt-prods new-nt-prods-list)
		 (left-factor-one grammar nonterminal productions)
	       (dbg "left-factor-one ~S => modified-nt-prods = ~S  new-nt-prods-list = ~S" productions modified-nt-prods new-nt-prods-list)
	       (cond ((null modified-nt-prods)
		      (setf result-productions (append result-productions productions)))
		     (t
		      (setf modifiedp t)
		      (setf result-productions (append result-productions (rest modified-nt-prods)))
		      (setf nonterminal-and-productions-list (cons new-nt-prods-list nonterminal-and-productions-list))))
	       (dbg "nonterminal-and-productions-list = ~S " nonterminal-and-productions-list)))
    (values 
     (cond ((not modifiedp)
	    grammar)
	   (t
	    (loop for i from 0 for p in result-productions do (setf (production-number p) i))
	    (cond ((not updatep)
		   (make-instance (type-of grammar)
				  :name (grammar-name grammar)
				  :start-symbol (grammar-start-symbol grammar)
				  :terminals (grammar-terminals grammar)
				  :nonterminals (delete-duplicates (mapcar #'production-lhs result-productions))
				  :productions result-productions))
		  (t
		   (setf (grammar-nonterminals grammar) (delete-duplicates (mapcar #'production-lhs result-productions)))
		   (setf (grammar-productions grammar) result-productions)
		   grammar))))
     modifiedp)))

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

(defun nonterminal-firsts (grammar)
  (dbg "enter nonterminal-firsts:")
  (dbginc 2)
  (dbg (print-grammar grammar nil))
  (dbginc -2)
  (let* ((productions (grammar-productions grammar))
	 (nonterminals (grammar-nonterminals grammar))
	 (terminals (grammar-terminals grammar))
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
    (setf (grammar-nonterminal-firsts grammar) 
	  (mapcar #'(lambda (f)
		      (cons (first f)
			    (sort (rest f) #'(lambda (x y)
					       (or (null y)
						   (and (not (null x))
							(< (terminal-position x terminals)
							   (terminal-position y terminals))))))))
		  nonterminal-first-alist))))

;;; -------------------- FOLLOW --------------------

(defun followers (grammar)
  (dbg "enter followers")
  (dbginc 2)
  (when (null (grammar-nonterminal-firsts grammar))
    (nonterminal-firsts grammar))
  (let* ((productions (grammar-productions grammar))
	 (nonterminal-first-alist (grammar-nonterminal-firsts grammar))
	 (nonterminals (grammar-nonterminals grammar))
	 (terminals (append (grammar-terminals grammar) (list '$)))
	 (followers-alist (mapcar #'(lambda (x) (list x)) nonterminals))
	 (modifiedp t))
    (push '$ (rest (eassoc (grammar-start-symbol grammar) followers-alist)))
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
    (setf (grammar-followers grammar) 
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
    (dbg "Result is ~S" (grammar-followers grammar))
    (grammar-followers grammar)))

;;; -------------------- Creating LL(1) Parsing Table --------------------

(defun generate-ll1-table (grammar)
  (let* ((terminals (append (grammar-terminals grammar) (list '$)))
	 (nonterminals (grammar-nonterminals grammar))
	 (followers-alist (or (grammar-followers grammar) (followers grammar)))
	 (nonterminal-first-alist (grammar-nonterminal-firsts grammar))
	 (parse-table (make-array (list (length nonterminals) (length terminals)) :initial-element 'error))
	 (productions (grammar-productions grammar))
	 (default-result (grammar-default-result grammar))
	 (errors nil))
					;    (print-grammar grammar)
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
			  (setf (production-result-func-lambda p) nil))
			 (t
			  (setf (production-result-arglist p) result-arglist)
			  (setf (production-result-arg-count p) (length result-arglist))
			  (setf (production-result-func-name p) result-func-name)
			  (setf (production-result-func-lambda p) result-func-lambda))))))
      (setf (grammar-ll1-table grammar) parse-table)
      (cond ((not (null errors))
	     (warn "~%Not an LL(1) grammar:~%")
	     (print-grammar grammar)
	     ;;(print-ll1-table grammar *error-output*)
	     (dolist (error (nreverse errors))
	       (let ((lhs (first error))
		     (s (second error))
		     (p (third error)))
		 (warn "~%    parse-table[~S, ~S]: conflict ~A vs. ~A" 
		       lhs s 
		       (print-production (nth (aref parse-table (nonterminal-position lhs nonterminals) (terminal-position s terminals)) productions) terminals nil)
		       (print-production p terminals nil))))
	     (error* "Errors in parse-table creation"))
	    (t
	     parse-table)))))

;;; -------------------- LL(1) Parsing --------------------

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

(defmacro grammar-symbol-value (grammar x)
  `(get ,x (grammar-symbol-value-tag ,grammar)))

(defun assign-numbers-to-symbols-and-productions (grammar &optional subscribe)
  (declare (ignorable subscribe))
  (setf (grammar-symbol-value-tag grammar) (gensym "SYMBOL-VALUE"))
  (loop for i from 0 below (length (grammar-productions grammar))
	for p = (aref (grammar-productions grammar) i)
	do (progn (setf (production-grammar-symbol-value p) (1- (- i)))
		  (when (debugp subscribe :parser)
		    (inform "~%(production-grammar-symbol-value ~S) = ~S"
			    (cons (production-lhs p) (cons '-> (production-rhs p))) (production-grammar-symbol-value p)))
		  ))
  (loop for nonterminal in (grammar-nonterminals grammar)
	do (progn (setf (grammar-symbol-value grammar nonterminal) 
			(nonterminal-position nonterminal (grammar-nonterminals grammar)))
		  (when (debugp subscribe :parser) (inform "~%(grammar-symbol-value ~S) = ~S" nonterminal (grammar-symbol-value grammar nonterminal)))))
  (loop for terminal in (grammar-terminals grammar)
	do (progn (setf (grammar-symbol-value grammar terminal)
			(+ (length (grammar-nonterminals grammar))
			   (terminal-position terminal (grammar-terminals grammar)))))
       (when (debugp subscribe :parser) (inform "~%(grammar-symbol-value ~S) = ~S" terminal (grammar-symbol-value grammar terminal)))
       )
  (setf (grammar-symbol-value grammar '$) (+ (length (grammar-nonterminals grammar)) (length (grammar-terminals grammar))))
  (when (debugp subscribe :parser) (inform "~%(grammar-symbol-value ~S) = ~S" '$ (grammar-symbol-value grammar '$))))

(defun number-to-symbol-or-production (n grammar)
  (let* ((production-count (length (grammar-productions grammar)))
	 (nonterminal-count (length (grammar-nonterminals grammar)))
	 (terminal-count (length (grammar-terminals grammar)))
	 (symbol-count (+ nonterminal-count terminal-count)))
    (cond ((not (numberp n)) nil)
	  ((or (< n (1- (- production-count))) (> n symbol-count))
	   (error* "Number ~D does not occur in grammar symbol or production numbers" n))
	  ((< n 0)
	   (list 'production (elt (grammar-productions grammar) (1- (- n)))))
	  ((< n nonterminal-count)
	   (list 'nonterminal (elt (grammar-nonterminals grammar) n)))
	  ((< n symbol-count)
	   (list 'terminal (elt (grammar-terminals grammar) (- n nonterminal-count))))
	  (t (list 'end-symbol '$)))))

(defgeneric make-ll-parser-error-message (lexer position msg args)
  (:method ((lexer abstract-lexer) position msg args)
    (cond ((null position)
	   (format nil "~A" (apply #'format nil msg args)))
	  (t
	   (multiple-value-bind (row column)
	       (lexer-position-to-row-and-column lexer position)
	     (format nil "~A:~A:~A" row column (apply #'format nil msg args)))))))

(defvar *parser* nil)

(defun initialize-ll-parser (parser)
  (assign-numbers-to-symbols-and-productions parser (ll-parser-subscribe parser))
  (setf (ll-parser-stack parser) (list (grammar-symbol-value parser (grammar-start-symbol parser)) (grammar-symbol-value parser '$)))
  (setf (ll-parser-state parser) :initialized))

(defun return-from-parser (state &optional fmt &rest args)
  (let ((parser *parser*))
    (setf (ll-parser-state parser) state)
    (setf (ll-parser-phases parser) (nreverse (ll-parser-phases parser)))
    (when (eq state :failed)
      (push-to-end (make-ll-parser-error-message (ll-parser-lexer parser) (ll-parser-position parser) fmt args) (ll-parser-error-messages parser)))
    (throw 'parsed parser)))

(defun ll-parser-failure (fmt &rest args)
  (apply #'return-from-parser :failed fmt args))

(defun ll-parser-success ()
  (return-from-parser :succeeded))

(defun ll-parser-yields (value)
  (let ((parser *parser*))
    (setf (ll-parser-state parser) :yield)
    (throw 'parsed (values parser value))))

(defun ll-parse (parser)
  (catch 'parsed
    (let* ((*parser* parser)
	   (lexer (ll-parser-lexer parser))
	   (subscribe (ll-parser-subscribe parser))
	   (parse-table (grammar-ll1-table parser))
	   (nonterminals (grammar-nonterminals parser))
	   (nonterminal-count (length nonterminals))
	   (productions (grammar-productions parser)))
      (when (eq (ll-parser-state parser) :uninitialized)
	(initialize-ll-parser parser))
;      (inform "Entering parser")
      (labels ((nonterminalp (grammar-symbol-value) (< -1 grammar-symbol-value nonterminal-count))
	       (terminalp (grammar-symbol-value) (<= nonterminal-count grammar-symbol-value))
	       (parse-table-entry (nonterminal-pos terminal-pos)
		 (aref parse-table nonterminal-pos (- terminal-pos nonterminal-count)))
	       (next-input-token ()
		 (cond ((ll-parser-saved-input-token parser)
;			(inform "using saved token ~S" (ll-parser-saved-input-token parser))
			(prog1 (ll-parser-saved-input-token parser)
			  (setf (ll-parser-saved-input-token parser) nil))) ; this is needed, when the parser yields and then we resume
		       ((not (ll-parser-end-of-input-p parser))
			(let ((input-token (get-input-token lexer)))
			  (when (debugp subscribe :token) (inform "~%lexer yields ~S" input-token))
			  (cond ((error-input-token-p input-token)
				 (ll-parser-failure "Lexer error: ~A" (input-token-value input-token)))
				((eof-input-token-p input-token)
				 (setf (ll-parser-end-of-input-p parser) t)
				 (make-input-token :type (grammar-symbol-value parser '$)))
				(t
				 (let ((token-symbol-value (grammar-symbol-value parser (second input-token))))
				   (unless token-symbol-value (error* "Could not find the symbol value of token ~A" (second input-token)))
				   (setf (second input-token) token-symbol-value)
				   input-token))))))))
	(loop with input-token = (next-input-token)
	      for round from 0
	      while (and (ll-parser-stack parser) input-token)
	      do (when (debugp subscribe :parse-operations)
		   (inform "~%Round ~D:~&input-token = ~A~&stack = "
			   round (if (and (consp input-token) (eq (car input-token) 'input-token))
				     (format nil "(~A ~S ~{~S~^ ~})" (car input-token) (number-to-symbol-or-production (second input-token) parser) (cddr input-token))
				     input-token))
		   (loop for x in (reverse (ll-parser-stack parser))
			 do (inform "~S " (number-to-symbol-or-production x parser)))
		   (inform "~&result = ")
		   (loop for v in (ll-parser-result-stack parser) do (let ((*print-right-margin* 150)) (inform "  ~S" v))))
	      do (progn
		   (setf (ll-parser-position parser) (input-token-position input-token))
		   (when (debugp subscribe :phases) (push (list (copy-list (ll-parser-stack parser)) nil (copy-list (ll-parser-parsed-input parser))) (ll-parser-phases parser)))
		   (let ((top (first (ll-parser-stack parser)))
			 (symbol (input-token-type input-token)))
		     (when (debugp subscribe :parse-operations) (inform "~%Top = ~S, symbol = ~S" (number-to-symbol-or-production top parser) (number-to-symbol-or-production symbol parser)))
		     (cond ((terminalp top)
			    (when (debugp subscribe :parse-operations) (inform "Terminal ~S" (number-to-symbol-or-production top parser)))
			    (cond ((equal top symbol)
				   (pop (ll-parser-stack parser))
				   (push (input-token-value input-token) (ll-parser-result-stack parser))
				   (when (debugp subscribe :phases)
				     (setf (rest (last (first (ll-parser-phases parser)))) (list (list 'advance top)))
				     (push symbol (ll-parser-parsed-input parser)))
				   (setf input-token (next-input-token)))
				  (t
				   (ll-parser-failure "Expecting ~A instead of ~A (~A)" 
					    (second (number-to-symbol-or-production top parser)) (second (number-to-symbol-or-production symbol parser)) input-token))))
			   ((nonterminalp top)
			    (when (debugp subscribe :parse-operations) (inform "Nonterminal ~S (~S)" (number-to-symbol-or-production top parser) top))
			    (let ((table-value (parse-table-entry top symbol)))
			      (cond ((eq table-value 'error)
				     (when (debugp subscribe :parse-operations) (inform "No legal action for nonterminal ~A and input symbol ~A" top symbol))
				     (ll-parser-failure "No legal action for nonterminal ~A and input symbol (~A ~S))"
					      (second (number-to-symbol-or-production top parser)) (second (number-to-symbol-or-production (input-token-type input-token) parser))
					      (input-token-value input-token)))
				    (t
				     (let* ((p (elt productions table-value))
					    (rhs (production-rhs p)))
				       (when (debugp subscribe :phases) (setf (rest (last (first (ll-parser-phases parser)))) (list (cons 'push rhs))))
				       (pop (ll-parser-stack parser))
				       (push (production-grammar-symbol-value p) (ll-parser-stack parser))
				       (dolist (x (reverse rhs)) (push (grammar-symbol-value parser x) (ll-parser-stack parser))))))))
			   (t ; (productionp top)
			    (let* ((pnum (- (1+ top)))
				   (p (elt productions pnum)))
			      (when (debugp subscribe :parse-operations) (inform "Production number ~D: ~S" pnum p))
			      (pop (ll-parser-stack parser))
			      (when (debugp subscribe :phases) (setf (rest (last (first (ll-parser-phases parser)))) (list (list 'execute (format nil "[~d]" (production-number p))))))
			      (let ((result-func (production-result-func p)))
				(when result-func
				  (let ((args nil))
				    (loop repeat (production-result-arg-count p) do (push (pop (ll-parser-result-stack parser)) args))
				    (when (debugp subscribe :parse-operations) (inform "Rule [~D], arg-count = ~D" (production-number p) (production-result-arg-count p)))
				    (setf (ll-parser-saved-input-token parser) input-token) ; Save input-token, in case result-func does a nonlocal exit
;				    (inform "Saving input token: (ll-parser-saved-input-token parser) = ~S" (ll-parser-saved-input-token parser))
				    (let ((values (multiple-value-list (apply result-func p args))))
				      (setf (ll-parser-saved-input-token parser) nil) ; Unset saved-input-token, if we did not do a nonlocal exit
				      (when (debugp subscribe :parse-operations) (inform "  Execute rule [~D] with args = ~A ->~&  ~A" (production-number p) args values))
				      (loop for value in values do (push value (ll-parser-result-stack parser))))))))))))
	      finally (cond ((and (null (ll-parser-stack parser)) (null input-token))
			     (when (debugp subscribe :phases) (push (list nil nil (copy-list (ll-parser-parsed-input parser)) (list 'accept nil)) (ll-parser-phases parser)))
			     (pop (ll-parser-result-stack parser)) ; Remove the value corresponding '$
			     (ll-parser-success))
			    ((null (ll-parser-stack parser))
			     (ll-parser-failure "Extraneous input ~A" input-token))
			    (t
			     (ll-parser-failure "End of input while expecting ~A" (second (number-to-symbol-or-production (first (ll-parser-stack parser)) parser))))))))))

(defgeneric parse (ll-parser)
  (:method ((this ll-parser))
    (ll-parse this)))

;;; -------------------- Generate an LL(1) grammar --------------------

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

(defun generate-ll1-grammar (name class rules-or-file &rest keys &key default-result (error-on-non-ll1-grammar-p t) warn-about-transformations-p (warn-stream *error-output*) &allow-other-keys)
  "Generate an LL(1) grammar from a set of rules (or a file)"
  (declare (ignorable default-result))
  (remf keys :warn-about-transformations-p)
  (remf keys :warn-stream)
  (let* ((rules (cond ((consp rules-or-file) rules-or-file)
		      (t
		       (with-open-file (input-stream rules-or-file)
			 (read input-stream)))))
	 (g1 (apply #'create-grammar name class rules keys))
	 (g2 (remove-immediate-left-recursion g1))
	 (g3 (left-factoring g2)))
    (when (not (equal-grammar g1 g2))
      (cond (error-on-non-ll1-grammar-p
	     (error* "This is not an LL1-grammar; you should remove left recursion"))
	    (warn-about-transformations-p
	     (print-grammar g1)
	     (format warn-stream "~&Left recursion removed~&"))))
    (when (not (equal-grammar g2 g3))
      (cond (error-on-non-ll1-grammar-p
	     (error* "This is not an LL1-grammar; it needs left factoring"))
	    (warn-about-transformations-p 
	     (print-grammar g2)
	     (format warn-stream "~&Left factoring~&"))))
    (nonterminal-firsts g3)
    (followers g3)
    (generate-ll1-table g3)
					;    (describe g3)
					;    (print-grammar g3)
    g3))

(defun generate-parser-ll1-grammar (parser rules-or-file)
  "Generate an LL(1) grammar from a set of rules (or a file) for a parser object"
  (let ((rules (cond ((consp rules-or-file) rules-or-file)
		     (t
		      (with-open-file (input-stream rules-or-file)
			(read input-stream))))))
    (create-parser-grammar parser rules)
    (multiple-value-bind (parser1 modifiedp1) (remove-immediate-left-recursion parser :updatep t)
      (declare (ignorable parser1))
      (when modifiedp1
	(error* "Not accepting a grammar with left recursion"))
      (multiple-value-bind (parser2 modifiedp2) (left-factoring parser :updatep t)
	(declare (ignorable parser2))
	(when modifiedp2
	  (error* "Not accepting a grammar that need left factoring"))))
    (nonterminal-firsts parser)
    (followers parser)
    (generate-ll1-table parser)
    (print-grammar parser)
    parser))
