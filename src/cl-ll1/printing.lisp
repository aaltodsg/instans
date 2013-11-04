;;; -*- Mode: Lisp -*-

;;; -------------------- Debug messages --------------------

(in-package #:instans)

(proclaim '(optimize safety))

(defvar *caseifyp* nil)

(defun symbol-list-string (lst terminals)
  (cond ((null lst)
	 " epsilon ")
	((eq lst 'error)
	 "  ")
	(t
	 (format nil " ~{~A ~}" 
		 (mapcar #'(lambda (x)
			     (cond ((production-p x)
				    (format nil "[~D]" (production-number x)))
				   ((member x terminals)	
				    (if *caseifyp* (format nil "~(~A~)" x) (format nil "~A" x)))
				   (t
				    (if *caseifyp* (format nil "~:@(~A~)" x) (format nil "~A" x)))))
			 lst)))))

(defun print-production (production terminals &optional (stream *error-output*) (arrow-column nil) (resultp t) (result-column nil))
  (let* ((lhs-string (string (production-lhs production)))
	 (lhs-rhs-string (format nil "~:[~;~3D: ~]~A~V@T-> ~{~A~^ ~}" (production-number production) (production-number production) lhs-string (if arrow-column (- arrow-column (length lhs-string)) 1)
				 (loop for symbol in (production-rhs production) collect (format nil "~:[~:@(~A~)~;~(~A~)~]" (member symbol terminals) symbol))))
	 (result-string (if resultp (let ((result (getf (production-properties production) :result)))
				      (if result (format nil "~V@T~(:result ~S~)" (if result-column (- result-column (length lhs-rhs-string)) 1) result) ""))
			 ""))
	 (rule-string (format nil "~A~A" lhs-rhs-string result-string)))
    (if (null stream)
	rule-string
	(format stream "~A" rule-string))))

(defun print-grammar (parser &optional (stream *error-output*))
  (let* ((terminals (parser-terminals parser))
	 (nonterminals (parser-nonterminals parser))
	 (max-nonterminal-length (apply #'max (mapcar #'(lambda (sym) (length (string sym))) nonterminals)))
	 (productions (parser-productions parser))
	 (max-lhs-rhs-length (loop for production in productions maximize (length (print-production production terminals nil max-nonterminal-length nil)))))
    (format stream "~&")
    (loop for production in productions
       do (format stream "~A~&" (print-production production terminals nil (1+ max-nonterminal-length) t (+ 5 max-lhs-rhs-length))))))

(defun print-firsts (parser &optional (stream *error-output*))
  (let ((terminals (parser-terminals parser)))
    (when (parser-nonterminal-firsts parser)
      (format stream "~%")
      (dolist (f (parser-nonterminal-firsts parser))
	(format stream "~%FIRST(~A) ~12T=~{ ~A~}" (first f) 
		(mapcar #'(lambda (x)
			    (cond ((null x) "epsilon")
				  ((member x terminals)
				   (format nil "~(~A~)" x))
				  (t
				   (format nil "~:@(~A~)" x))))
			    (rest f)))))))

(defun print-followers (parser &optional (stream *error-output*))
  (let ((terminals (parser-terminals parser)))
    (when (parser-followers parser)
      (format stream "~%")
      (dolist (f (parser-followers parser))
	(format stream "~%FOLLOW(~A) ~12T=~{ ~A~}" (first f) 
		(mapcar #'(lambda (x)
			    (cond ((member x terminals)
				   (format nil "~(~A~)" x))
				  (t
				   (format nil "~:@(~A~)" x))))
			(rest f)))))))

;;; -------------------- Printintg --------------------

(defun print-ll-parse-table (parser &optional (stream *error-output*))
  (let* ((terminals (append (parser-terminals parser) (list '$)))
	 (nonterminals (parser-nonterminals parser))
	 (parse-table (parser-ll1-table parser))
	 (productions (parser-productions parser))
	 (*print-pretty* nil)
	 (max-width 0))
    (flet ((table-cell-as-string (j i)
	     (let ((entry (aref parse-table j i)))
	       (symbol-list-string (if (numberp entry) (production-rhs (nth entry productions)) entry) terminals))))
      (dotimes (i (length terminals))
	(setf max-width (max max-width (length (format nil " ~A " (nth i terminals)))))
	(dotimes (j (length nonterminals))
	  (setf max-width (max max-width (length (table-cell-as-string j i))))))
      (format stream "~%~V@T|~{~A~}" max-width (mapcar #'(lambda (x) (format nil " ~(~VA~) |" (- max-width 2) x)) terminals))
      (dotimes (i (length nonterminals))
	(format stream "~%")
	(dotimes (k (* (+ max-width 1) (1+ (length terminals))))
	  (format stream "-"))
	(format stream "~%~VA|" max-width (nth i nonterminals))
	(dotimes (j (length terminals))
	  (format stream "~VA|" max-width (table-cell-as-string i j))))
      (format stream "~%")
      (dotimes (k (* (+ max-width 1) (1+ (length terminals))))
	(format stream "-")))))

(defun ll-parse-table2csv (parser &optional (stream *error-output*))
  (let* ((terminals (append (parser-terminals parser) (list '$)))
	 (nonterminals (parser-nonterminals parser))
	 (parse-table (parser-ll1-table parser))
	 (*print-pretty* nil))
    (format stream "\"\",~{~S~^,~}~&" terminals)
    (dotimes (i (length nonterminals))
      (format stream "~A,~{~S~^,~}~&" (nth i nonterminals) (loop for j from 0 below (length terminals) collect (symbol-list-string (let ((p (aref parse-table i j))) (if (production-p p) (production-rhs p) p)) terminals))))))

(defun print-ll-parse (parse parser input &optional (stream *error-output*) show-productions-p)
  (let* ((terminals (parser-terminals parser))
	 (max-input-len (length (symbol-list-string (append input (list '$)) nil)))
	 (max-stack-len
	  (apply #'max
		 (mapcar #'(lambda (x)
			     (length (symbol-list-string (first x) nil)))
			 parse))))
    (format stream "~%Parsing of ~A" (symbol-list-string input terminals))
    (dolist (phase parse)
      (cond ((not show-productions-p)
	     (format stream "~%~VA    ~V@A    ~(~A~)~A" 
		     max-stack-len 
		     (symbol-list-string (reverse (first phase)) terminals)
		     max-input-len
		     (symbol-list-string (second phase) terminals)
		     (first (fourth phase))
		     (symbol-list-string (rest (fourth phase)) terminals)))
	    (t
	     (format stream "~%~VA    ~V@A    ~:[~;~A~]"
		     max-stack-len 
		     (symbol-list-string (reverse (first phase)) terminals)
		     max-input-len
		     (symbol-list-string (second phase) terminals)
		     (fourth phase) 
		     (if (eq (first (fourth phase)) 'accept) '|accept|
		       (print-production (fourth phase) terminals nil))))))))

(defun print-ll-derivation (parse parser input &optional (stream *error-output*) show-productions-p)
  (declare (ignorable show-productions-p))
  (let* ((terminals (parser-terminals parser))
	 (start (format nil "~A" (parser-start-symbol parser)))
	 (phases (mapcar #'(lambda (x) (remove '$ (append (reverse (third x)) (first x)))) parse))
	 (prev nil))
    (format stream "~%Leftmost derivation of ~A" (symbol-list-string input terminals))
    (format stream "~%~A =>" start)
    (dolist (phase (rest phases))
      (unless (equal prev phase)
	(format stream "~%~V@T => ~A" 
		(length start) 
		(symbol-list-string phase terminals)))
      (setq prev phase))
    (format stream "~%")))
