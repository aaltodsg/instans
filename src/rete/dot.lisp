;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun dot-node-number (node)
  (let* ((name-string (string (node-name node)))
	 (pos (position-if-not #'digit-char-p name-string :from-end t))
	 (num-string (subseq name-string (1+ pos))))
    (parse-integer num-string)))

(defun dot-node-name (node)
  (string-downcase (coerce (substitute #\_ #\- (coerce (node-name node) 'list)) 'string)))

(defun dot-subscript-number-format ()
  "<sub><font point-size=\"10\">~D</font></sub>")

(defun dot-default-pretty-name-format (class-part)
  (format nil "~A~A" class-part (dot-subscript-number-format)))

(defgeneric dot-node-pretty-name-format-string (node)
  (:documentation "Pretty version of the name of this node to be used with HTML-like labels.")
  (:method ((this triple-pattern-node)) (dot-default-pretty-name-format "&tau;"))
  (:method ((this alpha-node)) (dot-default-pretty-name-format "&alpha;"))
  (:method ((this alpha-memory)) (dot-default-pretty-name-format "&alpha;<sub>m</sub>"))
  (:method ((this beta-memory)) (dot-default-pretty-name-format "&beta;<sub>m</sub>"))
  (:method ((this join-node)) (dot-default-pretty-name-format "J"))
  (:method ((this exists-start-node)) (format nil "~AE~A{" (case (exists-kind this) (:simple-exists "") (:simple-not-exists "!") (t "@")) (dot-subscript-number-format)))
  (:method ((this exists-end-node)) (dot-default-pretty-name-format "}E"))
  (:method ((this optional-start-node)) (concatenate 'string "?" (dot-subscript-number-format) "{"))
  (:method ((this optional-end-node)) (dot-default-pretty-name-format "}?"))
  (:method ((this union-start-node)) (concatenate 'string "U" (dot-subscript-number-format) "{"))
  (:method ((this union-end-node)) (dot-default-pretty-name-format "}U"))
  (:method ((this filter-node)) (dot-default-pretty-name-format "F"))
  (:method ((this filter-memory)) (dot-default-pretty-name-format "FM"))
  (:method ((this bind-node)) (dot-default-pretty-name-format (format nil "B[~A]" (uniquely-named-object-name (bind-variable this)))))
  (:method ((this aggregate-join-node)) (dot-default-pretty-name-format "AJ"))
  (:method ((this select-node)) (dot-default-pretty-name-format "S"))
  (:method ((this construct-node)) (dot-default-pretty-name-format "C"))
  (:method ((this modify-node)) (dot-default-pretty-name-format "DI"))
  (:method ((this node)) (format nil "~A~~D" (type-of this))))

(defun dot-node-pretty-name (node)
  (format nil (dot-node-pretty-name-format-string node) (dot-node-number node)))

(defun dot-pretty-triple-pattern (triple-pattern &optional (shortp t))
  (flet ((shorten-string (s)
	   (if (not shortp) s (let ((i (position-if-not #'alphanumericp s :from-end t))) (if (and (numberp i) (< 0 i (length s))) (subseq s (+ i 1)) s)))))
    (format nil "~{~a~^ ~}" (mapcar #'(lambda (x) (cond ((sparql-var-p x) (uniquely-named-object-name x))
							((rdf-iri-p x) (shorten-string (rdf-iri-string x)))
							((rdf-literal-p x)
							 (shorten-string (rdf-literal-string x)))
							(t x)))
				    triple-pattern))))

(defun var-name (node var)
  (let* ((to (second var))
	 (from (reverse-resolve-binding (node-bindings node) to)))
    (format nil "~A &rarr; ~A" from to)))

(defun var-names (node l)
  (mapcar #'(lambda (var) (var-name node var)) l))

(defgeneric dot-node-label (node &key html-labels-p show-vars-p)
  (:method ((this triple-pattern-node) &key &allow-other-keys)
    (format nil "~A: ~(~A~)"
	    (dot-node-pretty-name this)
	    (dot-pretty-triple-pattern (triple-pattern-node-triple-pattern this))))
  (:method ((this alpha-node) &key &allow-other-keys)
    (format nil "~A" (dot-node-pretty-name this)))
  (:method ((this alpha-memory) &key &allow-other-keys)
    (format nil "~A" (dot-node-pretty-name this)))
  (:method ((this beta-memory) &key &allow-other-keys)
    (format nil "~A" (dot-node-pretty-name this)))
  (:method ((this node) &key html-labels-p show-vars-p)
    (cond ((null html-labels-p)
	   (format nil "~A" (dot-node-pretty-name this)))
	  (t
	   (apply #'concatenate 'string
		  (append 
		   (list (format nil "<TABLE BORDER=\"0\" CELLBORDER=\"~D\" CELLSPACING=\"0\" CELLPADDING=\"4\">" (if show-vars-p 1 0)))
		   (cond ((not show-vars-p)
			  (list (format nil "<TR><TD>~A</TD></TR>" (dot-node-pretty-name this))))
			 (t
			  (append
			   (list (format nil "<TR><TD ROWSPAN=\"~D\" ALIGN=\"LEFT\">~A</TD>" (if (or (typep this 'memory) (typep this 'join-node)) 5 4)
					 (dot-node-pretty-name this))
				 (format nil "<TD ALIGN=\"LEFT\">def ~(~{~a~^, ~}~)</TD><TD ALIGN=\"LEFT\">def&lsaquo; ~(~{~a~^, ~}~)</TD></TR>"
					 (var-names this (node-def this))
					 (var-names this (node-def-prec this)))
				 (format nil "<TR><TD ALIGN=\"LEFT\">use ~(~{~a~^, ~}~)</TD><TD ALIGN=\"LEFT\">use&rsaquo; ~(~{~a~^, ~}~)</TD></TR>"
					 (var-names this (node-use this))
					 (var-names this (node-use-succ this)))
				 (format nil "<TR><TD ALIGN=\"LEFT\">kill ~(~{~a~^, ~}~)</TD><TD ALIGN=\"LEFT\"></TD></TR>"
					 (var-names this (node-kill this)))
				 (format nil "<TR><TD ALIGN=\"LEFT\">in ~(~{~a~^, ~}~)</TD><TD ALIGN=\"LEFT\">out ~(~{~a~^, ~}~)</TD></TR>"
					 (var-names this (node-all-vars-in this))
					 (var-names this (node-all-vars-out this)))
				 (format nil "<TR><TD ALIGN=\"LEFT\">in ~(~{~a~^, ~}~)</TD><TD ALIGN=\"LEFT\">out ~(~{~a~^, ~}~)</TD></TR>"
					 (var-names this (node-visible-vars-in this))
					 (var-names this (node-visible-vars-out this)))
				 )
			   (and (join-node-p this)
				(list (format nil "<TR><TD ALIGN=\"LEFT\">vm ~(~{~a~^, ~}~)</TD></TR>"
					      (node-def-preceq (join-alpha this)))))
			   )))
		   (list (format nil "</TABLE>"))))))))

(defgeneric dot-node-color (node)
  (:method ((this node))
    (declare (special *node-color-alist*))
    (or (cdr (assoc this *node-color-alist*)) "Black")))

(defun var-orig-names (node canonic-vars)
  (let ((alist (node-bindings node)))
    (loop for var in canonic-vars
;	  do (inform "var = ~S" var)
	  unless (null var)
	  collect (uniquely-named-object-name (car (rassoc var alist))))))

(defgeneric dot-node-tooltip (node)
  (:method ((this node))
    (format nil "~A: ~@[~%def ~A~]~@[~%use ~A~]~@[~%def< ~A~]~@[~%use> ~A~]~@[~%all-vars-in ~A~]~@[~%all-vars-out ~A~]~@[~%visible-vars-in ~A~]~@[~%visible-vars-out ~A~]" (node-name this)
	    (var-orig-names this (node-def this)) (var-orig-names this (node-use this))
	    (var-orig-names this (node-def-prec this)) (var-orig-names this (node-use-succ this))
	    (var-orig-names this (node-all-vars-in this)) (var-orig-names this (node-all-vars-out this))
	    (var-orig-names this (node-visible-vars-in this)) (var-orig-names this (node-visible-vars-out this))
	    )))

(defmethod dot-node-tooltip :around ((this triple-pattern-node))
  (format nil "~A~%[~A]" (call-next-method) (dot-pretty-triple-pattern (triple-pattern-node-triple-pattern this) nil)))

(defmethod dot-node-tooltip :around ((this join-node))
  (format nil "~A~%a-b ~A~%b-a ~A" (call-next-method) (var-orig-names this (join-alpha-minus-beta-vars this)) (var-orig-names this (join-beta-minus-alpha-vars this))))

(defmethod dot-node-tooltip :around ((this bind-node))
  (format nil "~A~%~S" (call-next-method) (bind-form-lambda this)))

(defmethod dot-node-tooltip :around ((this filter-node))
  (format nil "~A~%~S" (call-next-method) (filter-test-lambda this)))

(defmethod dot-node-tooltip :around ((this modify-node))
  (format nil "~A~%~S~%~S" (call-next-method) (modify-delete-lambda this) (modify-insert-lambda this)))

(defmethod dot-node-tooltip :around ((this query-node))
  (format nil "~A~%project-vars: ~A" (call-next-method) (var-orig-names this (solution-modifiers-project-vars this))))

(defgeneric dot-node-description (node &key html-labels-p shape show-vars-p)
  (:documentation "Returns a dot node description.")
  (:method ((this node) &key (html-labels-p t) shape show-vars-p)
    (when (and (null shape) html-labels-p show-vars-p)
      (setf shape "box"))
    (let ((color (dot-node-color this)))
      (format nil "~A[class=\"~A\" id=\"~A\",color=~A, shape=~A, label=<~A>, margin=0, tooltip=~S];" (dot-node-name this) (type-of this) (dot-node-name this)
	      color shape (dot-node-label this :html-labels-p html-labels-p :show-vars-p show-vars-p) (dot-node-tooltip this)))))

(defun print-dot-nodes (stream nodes &key rank (node-shape "oval") (html-labels-p t) show-vars-p)
  (cond ((null rank)
	 (format stream "~%  {"))
	(t
	 (format stream "~%  { rank=~a; " rank )))
  (dolist (node nodes)
    (format stream "~%    ~A" (dot-node-description node :html-labels-p html-labels-p :shape node-shape :show-vars-p show-vars-p)))
  (format stream "~%  }"))

(defun print-dot (net &key (stream *standard-output*) show-vars-p (html-labels-p t) (node-shape "oval"))
  (let* ((nodes (instans-nodes net))
	 (alphas (filter #'(lambda (node) (typep node 'alpha-node)) nodes))
	 (alphamems (filter #'(lambda (node) (typep node 'alpha-memory)) nodes))
	 (other-nodes (list-difference nodes (list-union alphas alphamems))))
    (format stream "~%digraph ~S{" (instans-name net))
    (print-dot-nodes stream alphas :rank "same" :show-vars-p show-vars-p :node-shape node-shape :html-labels-p html-labels-p)
    (print-dot-nodes stream alphamems :rank "same" :show-vars-p show-vars-p :node-shape node-shape :html-labels-p html-labels-p)
    (print-dot-nodes stream other-nodes :show-vars-p show-vars-p :node-shape node-shape :html-labels-p html-labels-p)
    (dolist (node nodes)
      (flet ((edge-attrs (vars parent)
	       (format nil "[label=<~(~{~a~^<br/>~}~)>~:[, style=\"dashed\"~;~]]" vars (member node (node-succ parent)))))
	(loop with parent-slots = (node-parent-slots node)
	      for parent-slot in parent-slots
	      for parent = (slot-value node parent-slot)
	      for anchor-point in (if (= 2 (length parent-slots)) '(":nw" ":ne") '(""))
	      for vars = (and (eq parent-slot 'alpha) (mapcar #'(lambda (vm) (format nil "~D: ~A" (car vm) (var-name node (cdr vm)))) (node-def (join-alpha node))))
	      when parent
	      do (format stream "~%  ~A -> ~A~A~A;" (dot-node-name parent) (dot-node-name node) anchor-point (edge-attrs vars parent))))
      (when (and (typep node 'subgraph-end-node) (subgraph-start-node node))
	(format stream "~%  ~A -> ~A[dir=\"none\", style=\"dotted\", color=\"gray\"];" (dot-node-name node) (dot-node-name (subgraph-start-node node)))))
    (format stream "~%}~%")))

(defun print-dot-file (net file &key (html-labels-p t))
  (with-open-file (stream file :direction :output :if-exists :supersede) (print-dot net :stream stream :show-vars-p t :html-labels-p html-labels-p)))

