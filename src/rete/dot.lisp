;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun dot-node-number (node)
  (let* ((name-string (string (node-name node)))
	 (pos (position-if-not #'digit-char-p name-string :from-end t))
	 (num-string (subseq name-string (1+ pos))))
;    (inform "dot-node-number ~A, name-string = ~S, pos = ~S, num-string = ~S" node name-string pos num-string)
    (parse-integer num-string)))

(defun dot-node-name (node)
  (string-downcase (coerce (substitute #\_ #\- (coerce (node-name node) 'list)) 'string)))

(defun dot-subscript-number-format ()
  "<sub><font point-size=\"6\">~D</font></sub>")

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
  (:method ((this minus-node)) (dot-default-pretty-name-format "-"))
  (:method ((this filter-node)) (dot-default-pretty-name-format "F"))
  (:method ((this filter-with-previous-value)) (dot-default-pretty-name-format "FM"))
  (:method ((this bind-node)) (dot-default-pretty-name-format (format nil "B[~A]" (uniquely-named-object-pretty-name (bind-variable this)))))
  (:method ((this aggregate-join-node)) (dot-default-pretty-name-format "AJ"))
  (:method ((this select-node)) (dot-default-pretty-name-format "S"))
  (:method ((this construct-node)) (dot-default-pretty-name-format "C"))
  (:method ((this modify-node)) (dot-default-pretty-name-format "DI"))
  (:method ((this node)) (format nil "~A~~D" (type-of this))))

(defun dot-node-pretty-name (node)
  (format nil (dot-node-pretty-name-format-string node) (dot-node-number node)))

(defun dot-pretty-triple-pattern (node)
  (format nil "~{~a~^ ~}" (mapcar #'(lambda (x) (cond ((sparql-var-p x)
						       (string-downcase (string (uniquely-named-object-pretty-name (reverse-resolve-binding (node-instans node) x)))))
						      ((or (rdf-iri-p x) (rdf-literal-p x))
						       (html-entities:encode-entities (sparql-value-to-string x :instans (node-instans node))))
						      (t x)))
				  (triple-pattern-node-triple-pattern node))))

(defun var-name (node var)
  (let* ((to (second var))
	 (from (reverse-resolve-binding (node-bindings node) to)))
    (format nil "~A &rarr; ~A" from to)))

(defun var-names (node l)
  (mapcar #'(lambda (var) (var-name node var)) l))

(defgeneric dot-node-label (node &key html-labels-p show-vars-p)
  (:method ((this triple-pattern-node) &key &allow-other-keys)
    (format nil "~A: ~A"
	    (dot-node-pretty-name this)
	    (dot-pretty-triple-pattern this)))
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
			   (list (format nil "<TR><TD ROWSPAN=\"~D\" ALIGN=\"LEFT\">~A</TD>" (if (or (typep this 'token-store) (typep this 'join-node)) 5 4)
					 (dot-node-pretty-name this))
				 (format nil "<TD ALIGN=\"LEFT\">def ~(~{~a~^, ~}~)</TD><TD ALIGN=\"LEFT\">def&lsaquo; ~(~{~a~^, ~}~)</TD></TR>"
					 (var-names this (node-def this))
					 (var-names this (node-def-prec this)))
				 ;; (format nil "<TR><TD ALIGN=\"LEFT\">use ~(~{~a~^, ~}~)</TD><TD ALIGN=\"LEFT\">use&rsaquo; ~(~{~a~^, ~}~)</TD></TR>"
				 ;; 	 (var-names this (node-use this))
				 ;; 	 (var-names this (node-use-succ this)))
				 (format nil "<TR><TD ALIGN=\"LEFT\">use ~(~{~a~^, ~}~)</TD><TD ALIGN=\"LEFT\">kill ~(~{~a~^, ~}~)</TD></TR>"
					 (var-names this (node-use this))
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
    (or (cdr (assoc this (instans-node-color-alist (node-instans this)))) "Black")))

(defun var-orig-names (node canonic-vars)
  (let ((alist (node-bindings node)))
    (loop for var in canonic-vars
;	  do (inform "var = ~S" var)
	  unless (null var)
	  collect (uniquely-named-object-pretty-name (car (rassoc var alist))))))

(defgeneric dot-node-tooltip (node)
  (:method ((this node))
    (format nil "~A: ~@[~%def ~A~]~@[~%use ~A~]~@[~%def< ~A~]~@[~%all-vars-in ~A~]~@[~%all-vars-out ~A~]~@[~%visible-vars-in ~A~]~@[~%visible-vars-out ~A~]" (node-name this)
	    (var-orig-names this (node-def this)) (var-orig-names this (node-use this))
	    (var-orig-names this (node-def-prec this)) 
	    (var-orig-names this (node-all-vars-in this)) (var-orig-names this (node-all-vars-out this))
	    (var-orig-names this (node-visible-vars-in this)) (var-orig-names this (node-visible-vars-out this))
	    )))

(defmethod dot-node-tooltip :around ((this triple-pattern-node))
  (format nil "~A~%[~A]" (call-next-method) (dot-pretty-triple-pattern this)))

(defmethod dot-node-tooltip :around ((this join-node))
  (format nil "~A~%a-b ~A~%b-a ~A" (call-next-method) (var-orig-names this (join-alpha-minus-beta-vars this)) (var-orig-names this (join-beta-minus-alpha-vars this))))

(defmethod dot-node-tooltip :around ((this bind-node))
  (format nil "~A~%~S" (call-next-method) (bind-form-lambda this)))

(defmethod dot-node-tooltip :around ((this filter-node))
  (format nil "~A~%~S" (call-next-method) (filter-test-lambda this)))

(defmethod dot-node-tooltip :around ((this modify-node))
  (format nil "~A~%~S~%~S" (call-next-method) (modify-delete-lambda this) (modify-insert-lambda this)))

(defmethod dot-node-tooltip :around ((this construct-node))
  (format nil "~A~%~S" (call-next-method) (construct-lambda this)))

(defmethod dot-node-tooltip :around ((this query-node))
  (format nil "~A~%project-vars: ~A" (call-next-method) (var-orig-names this (solution-modifiers-project-vars this))))

(defgeneric dot-node-description (node &key html-labels-p shape show-vars-p)
  (:documentation "Returns a dot node description.")
  (:method ((this node) &key (html-labels-p t) shape show-vars-p)
    (when (and (null shape) html-labels-p show-vars-p)
      (setf shape "box"))
    (let ((color (dot-node-color this)))
      (format nil "~A[class=\"~A\" id=\"~A\",fontname=\"Courier\",fontsize=\"10\", color=~A, shape=~A, label=<~A>, margin=0, tooltip=~S];" (dot-node-name this) (type-of this) (dot-node-name this)
	      color shape (dot-node-label this :html-labels-p html-labels-p :show-vars-p show-vars-p) (dot-node-tooltip this)))))

(defun print-dot-nodes (stream nodes &key rank (node-shape "ellipse") (html-labels-p t) show-vars-p)
  (cond ((null rank)
	 (format stream "~%  {"))
	(t
	 (format stream "~%  { rank=~a; " rank )))
  (dolist (node nodes)
    (format stream "~%    ~A" (dot-node-description node :html-labels-p html-labels-p :shape node-shape :show-vars-p show-vars-p)))
  (format stream "~%  }"))

(defun print-dot (net &key (stream *standard-output*) show-vars-p (html-labels-p t) (node-shape "ellipse"))
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
	       (format nil "[id=~A, label=<~(~{~a~^<br/>~}~)>~:[, style=\"dashed\"~;~]]"
		       (format nil "~A_to_~A" (dot-node-name parent) (dot-node-name node))
		       vars (member node (node-succ parent)))))
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

(defun output-rete-html-page (instans rules-file html-file)
  (let (name dir)
;    (inform "(directoryp ~A) = ~A" html-file (directoryp html-file))
    (cond ((directoryp html-file)
	   (setf dir (expand-dirname html-file))
	   (setf name (pathname-name (if (rdf-iri-p rules-file) (rdf-iri-path rules-file) rules-file)))
	   (setf html-file (format nil "~A~A.html" dir name)))
	  (t
	   (setf dir (expand-dirname (if (= (length (directory-namestring html-file)) 0) "." (directory-namestring html-file))))
	   (setf name (pathname-name html-file))))
    (let* ((dot-output-file (create-temp-file-name :directory dir :name-prefix name :type "dot"))
	   (dot-svg-output-file (create-temp-file-name :directory dir :name-prefix name :type "dot.svg")))
      (print-dot-file instans dot-output-file :html-labels-p nil)
      (multiple-value-bind (dot-ok output error)
	  (shell-cmd "dot" "-Tsvg" "-O" dot-output-file)
	(declare (ignorable output))
	(unless dot-ok
	  (inform "Running 'dot -Tsvg -O ~A failed:~%A" dot-output-file error)
	  (return-from output-rete-html-page nil))
	(let* ((rules (read-from-url-or-file rules-file))
	       (bindings (with-output-to-string (str)
			   (format str "Bindings:")
			   (loop for (from . to) in (instans-bindings instans)
				 do (format str "~%  ~A -> ~A" (uniquely-named-object-pretty-name from) (uniquely-named-object-pretty-name to)))))
	       (sparql-algebra (with-output-to-string (str)
				 (loop for expr in (instans-canonic-algebra-expr-list instans)
;				       do (inform "algebra-expr:~%~A" expr)
				       do (let* ((*print-circle* nil)
						 (*print-pretty* t)
						 (*print-right-margin* 110))
					    (print expr str)))))
	       (svg (read-from-url-or-file dot-svg-output-file)))
	  (with-open-file (out html-file :direction :output :if-exists :supersede)
	    (format out "
<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<html>
<head>
  <meta charset=\"utf-8\"> 
  <title>~A</title>
</head>
<body>
  <table border=\"1\" cellspacing=\"0\" cellpadding=\"4\">
  <tr>
    <td>
      <pre>
~A
      </pre>
    </td>
    <td rowspan=\"2\" valign=\"bottom\">
      <pre>
~A
      </pre>
      </td>
    </tr>
    <tr>
      <td>
      <pre>
~A
      </pre>
      </td>
    </tr>
  </table>
  <hr/>
~A
  <hr/>
</body>
</html>"
		    name
		    (html-entities:encode-entities sparql-algebra)
		    (html-entities:encode-entities bindings)
		    (html-entities:encode-entities rules)
		    svg))))))
  t)
