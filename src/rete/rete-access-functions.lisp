;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Printing

(defmethod print-object ((this instans) stream)
  (format stream "#<~A ~A>" (type-of this) (instans-name this)))

(defmethod print-object ((this node) stream)
  (format stream "#<~A ~A>" (type-of this) (node-number this)))

(defmethod print-object ((this hash-token-index) stream)
  (format stream "#<~A ~A>" (type-of this) (hash-token-index-id this)))

;;; Node initialize-instance :after methods


(defmethod initialize-instance :after ((this triple-pattern-node) &key triple-pattern &allow-other-keys)
  (assert triple-pattern)
  (let* ((dataset (triple-pattern-node-dataset this))
	 (vars (append (loop for x in triple-pattern when (sparql-var-p x) collect x) (if (sparql-var-p dataset) (list dataset)))))
    (setf (alpha-node-variables this) vars)
    (add-triple-pattern-node (instans-triple-pattern-matcher (node-instans this)) this)))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (let ((instans (node-instans node)))
    (when instans
      (push-to-end-new node (instans-nodes instans))
      (when (null (node-name node))
	(setf (node-number node) (incf (instans-node-id-counter instans)))
	(setf (node-name node) (format nil "~A~D" (type-of node) (node-number node)))))))

(defmethod initialize-instance :after ((this instans) &key &allow-other-keys)
  (when (and (instans-use-quad-store-p this) (null (instans-quad-store this)))
    (setf (instans-quad-store this) (make-instance 'list-quad-store)))
  (setf (instans-rule-instance-queue this) (make-instance 'rule-instance-queue :instans this))
  (setf (instans-triple-pattern-matcher this) (make-instance 'triple-pattern-matcher :instans this)))

;;; Var and blank creationg

(defgeneric make-rdf-blank-node (instans name)
  (:method ((this instans) name)
    (make-uniquely-named-object (instans-blank-node-factory this) name)))

(defgeneric generate-rdf-blank-node (instans &optional name-prefix)
  (:method ((this instans) &optional (name-prefix "_:!"))
    (generate-object-with-unique-name (instans-blank-node-factory this) :name-prefix name-prefix)))

(defgeneric make-sparql-var (instans name)
  (:method ((this instans) name)
    (make-uniquely-named-object (instans-var-factory this) name)))

(defgeneric generate-sparql-var (instans &optional name-prefix)
  (:method ((this instans) &optional name-prefix)
    (generate-object-with-unique-name (instans-var-factory this) :name-prefix name-prefix)))

;;; Node access

(defgeneric node-bindings (node)
  (:method ((this node))
    (let ((instans (node-instans this)))
      (and instans (instans-bindings instans)))))

(defgeneric node-parent-slots (node)
  (:method ((this join-node)) '(beta alpha))
  (:method ((this union-end-node)) '(prev1 prev2))
  (:method ((this node)) '(prev)))

(defun node-parents (node)
  (mapcar #'(lambda (s) (slot-value node s)) (node-parent-slots node)))

(defun source-nodes (net)
  (filter #'(lambda (x) (every #'null (node-parents x)))
	  (instans-nodes net)))

(defun sink-nodes (net)
  (filter-not #'node-succ (instans-nodes net)))

(defgeneric node-all-precs (node)
  (:method ((this union-end-node))
    (let ((prev1 (union-end-prev1 this))
	  (prev2 (union-end-prev2 this)))
      (cond ((null prev1) (and prev2 (list prev2)))
	    ((null prev2) (list prev1))
	    (t (list prev1 prev2)))))
  (:method ((this join-node))
    (list (join-beta this) (join-alpha this)))
  (:method ((this node))
    (let ((prev (node-prev this)))
      (and prev (list prev)))))

(defgeneric node-effective-precs (node)
  (:method ((this union-end-node))
    (let ((prev1 (union-end-prev1 this))
	  (prev2 (union-end-prev2 this)))
      (cond ((null prev1) (and prev2 (list prev2)))
	    ((null prev2) (list prev1))
	    (t (list prev1 prev2)))))
  (:method ((this join-node))
    (list (join-beta this)))
  (:method ((this node))
    (let ((prev (node-prev this)))
      (and prev (list prev)))))

(defun node-def-preceq (node)
  (list-union (node-def-prec node) (node-def node) :test #'equal))

(defun node-use-succeq (node)
  (list-union (node-use node) (node-use-succ node) :test #'equal))

(defgeneric node-defines-vars (node)
  (:method ((this alpha-node))
    (alpha-node-variables this))
  (:method ((this filter-memory))
    (list (filter-memory-prev-value-var this)))
  (:method ((this bind-node))
    (list (bind-variable this)))
  (:method ((this existence-start-node))
    (list (existence-active-p-var this) (existence-counter-var this)))
  (:method ((this aggregate-join-node))
    (loop for (var . rest) in (aggregate-join-var-aggr-list this)
	  collect var))
  (:method ((this query-node))
    (solution-modifiers-project-vars this))
  (:method ((this node)) nil))

(defgeneric node-uses-vars (node)
  (:method ((this join-node))
    (list-intersection (node-def-preceq (join-alpha this)) (node-def-preceq (join-beta this)) :test #'equal))
  (:method ((this bind-node))
    (bind-form-parameters this))
  (:method ((this filter-memory))
    (filter-test-parameters this))
  (:method ((this filter-node))
    (filter-test-parameters this))
  (:method ((this existence-start-node))
    (list (existence-counter-var this) (existence-active-p-var this)))
  (:method ((this existence-end-node))
    (let ((start-node (subgraph-start-node this)))
      (list (existence-counter-var start-node) (existence-active-p-var start-node))))
  (:method ((this aggregate-join-node))
    (collect-expression-variables (aggregate-join-group this)))
  (:method ((this query-node))
    (list-union (unless (eq (solution-modifiers-project this) '*)
		  (loop for item in (solution-modifiers-project-vars this)
			when (and (consp item) (eq (first item) 'AS))
			nconc (collect-expression-variables (third item))))
		(loop for ord in (solution-modifiers-order-by this)
		      nconc (cond ((member (car ord) '(ASC DESC))
				   (collect-expression-variables (second ord)))
				  (t
				   (collect-expression-variables ord))))))
  (:method ((this modify-node))
    (list-union (modify-delete-parameters this) (modify-insert-parameters this)
		:test #'equal))
  (:method ((this node)) nil))

;;; The main operation

(defun compute-node-defs (nodes)
  (let ((visited nil))
    (labels ((visit (node)
	       (unless (member node visited)
		 (push node visited)
		 (let ((precs (node-effective-precs node)))
		   (loop for prec in precs do (visit prec))
		   (let ((def-prec (reduce #'list-union (mapcar #'node-def-preceq precs) :initial-value nil)))
		     (setf (node-def-prec node) def-prec)
		     (setf (node-def node) (node-defines-vars node)))))))
      (loop for node in nodes do (visit node))))
  nodes)

(defun compute-node-uses (nodes)
  (let ((visited nil))
    (labels ((visit (node)
	       (unless (member node visited)
		 (push node visited)
		 (let ((succs (node-succ node)))
		   (loop for succ in succs do (visit succ))
		   (let ((use-succ (reduce #'list-union (mapcar #'node-use-succeq succs) :initial-value nil)))
		     (setf (node-use-succ node) use-succ)
		     (setf (node-use node) (node-uses-vars node)))))))
      (loop for node in nodes do (visit node))))
  nodes)

;;; NEW NOTE! We should not delete any vars, since this may prevent right number of SPARQL instances being generated.
;;; Note! vars_add and vars_del can be used to optimize the contents of the tokens.  If we want to be able to add rules in runtime,
;;; it may be beneficial not to delete much of anything in tokens, since we may later find out that nodes generated for a new rule may use these variables.
;;; Then the tokens created before would be invalid and they have to be generated again. Not nice! However, if we are not going to add rules at runtime,
;;; we may save space by deleting vars. This depends also on the data structure used for variables.
(defun compute-node-vars (nodes)
  (inform "enter compute-node-vars")
  (let ((visited nil))
    (labels ((visit (node)
	       (unless (member node visited)
		 (push node visited)
		 (let ((precs (node-effective-precs node)))
		   (loop for prec in precs do (visit prec))
		   (let ((all-vars-in (reduce #'list-union (mapcar #'node-all-vars-out precs) :initial-value nil))
			 (visible-vars-in (reduce #'list-union (mapcar #'node-visible-vars-out precs) :initial-value nil)))
		     (setf (node-all-vars-in node) all-vars-in)
		     (setf (node-visible-vars-in node) visible-vars-in)
		     (setf (node-all-vars-out node) (list-union all-vars-in (node-def node) :test #'sparql-var-equal))
		     (setf (node-visible-vars-out node)
			   (list-difference (list-union visible-vars-in (node-def node) :test #'sparql-var-equal)
					    (node-kill node) :test #'sparql-var-equal))
		     (flet ((mn (vars) (mapcar #'(lambda (v) (reverse-resolve-binding (node-instans node) v)) vars)))
		       (inform "~%visit ~S~%def ~S~%use ~S~%kill ~S~%all-vars-in ~S~%all-vars-out ~S~%visible-vars-in ~S~%visible-vars-out ~S"
			       node (mn (node-def node)) (mn (node-use node)) (mn (node-kill node))
			       (mn (node-all-vars-in node)) (mn (node-all-vars-out node))
			       (mn (node-visible-vars-in node)) (mn (node-visible-vars-out node))))
		     )))))
      (loop for node in nodes do (visit node))))
  (inform "exit compute-node-vars")
  nodes)

(defun compute-node-uses-defs-and-vars (nodes)
  (catch 'compute-node-uses-defs-and-vars
    (compute-node-defs nodes)
    (compute-node-uses nodes)
    (compute-node-vars nodes)))

;;; -------
;;; Instans
;;; -------

(defun instans-matching-nodes (instans name)
  (loop for node in (instans-nodes instans)
	when (search name (node-name node))
	collect node))

(defgeneric instans-storage-sizes (instans)
  (:method ((this instans))
    (loop for node in (instans-nodes this)
	  when (typep node 'memory)
	  sum (if (memory-store node) (hash-table-count (memory-store node)) 0) into store-sizes
	  else when (typep node 'join-node)
	  sum (+ (if (join-alpha-index node) (hash-table-count (hash-token-index-table (join-alpha-index node))) 0)
		 (if (join-beta-index node) (hash-table-count (hash-token-index-table (join-beta-index node))) 0)) into index-sizes
	  finally (return (values store-sizes index-sizes)))))

(defun show-stores (instans)
  (loop for node in (instans-nodes instans)
	when (and (typep node 'memory) (memory-store node))
	do (inform "~A:~{~%  ~A~}" node (maph #'(lambda (k v) (declare (ignorable k)) (token-to-pretty-string node v)) (memory-store node)))))

(defun show-indices (instans)
  (let ((index-item-format "~%~A: ~A ~{~%  ~{~A~^ ->~}~}")
	(index-values-format "~{        ~A~^~%~}"))
    (loop for node in (instans-nodes instans)
	  when (typep node 'join-node)
	  do (progn
	       (when (join-beta-index node)
		 (inform index-item-format node "beta"
			 (maph #'(lambda (k v) (list k (format nil index-values-format (mapcar #'(lambda (tok) (token-to-pretty-string node tok)) v))))
			       (hash-token-index-table (join-beta-index node)))))
	       (when (join-alpha-index node)
		 (inform index-item-format node "alpha"
			 (maph #'(lambda (k v) (list k (format nil index-values-format (mapcar #'(lambda (tok) (token-to-pretty-string node tok)) v))))
			       (hash-token-index-table (join-alpha-index node)))))))))

(defun term-to-pretty-string (term)
  (cond ((rdf-iri-p term)
	 (format nil "<~A>" (rdf-iri-string term)))
	((rdf-literal-p term)
	 (format nil "~A" term))
	(t (format nil "~A" term))))

(defun token-to-pretty-string (node token)
  (cond ((null token) "Empty token")
	(t
	 (format nil "~{~A~^, ~}"
		 (loop with bindings = (node-bindings node)
		       for field in token
		       collect (cond ((and (consp field) (null (car field)) (numberp (second field)))
				      (let ((hnum (format nil "~D" (second field))))
					(format nil "[Hash:~A..~A]" (subseq hnum 0 3) (subseq hnum (- (length hnum) 3)))))
				     ((and (consp field) (consp (car field)) (sparql-var-p (car field)))
				      (let ((pretty-name (car (rassoc (second (car field)) bindings))))
					(format nil "~A = ~A" pretty-name (term-to-pretty-string (second field)))))
				     (t
				      (term-to-pretty-string field))))))))

;;;

(defgeneric get-constant-iri (instans iri)
  (:method ((this instans) iri)
    (let* ((string (rdf-iri-string iri))
	   (item (assoc string (instans-constant-iri-var-alist this) :test #'string=)))
      (when (null item)
	(setf item (list string (intern string)))
	(push-to-end item (instans-constant-iri-var-alist this)))
      (second item))))

(defgeneric get-constant-literal (instans literal)
  (:method ((this instans) literal)
    (let* ((string (rdf-literal-to-string literal))
	   (item (assoc string (instans-constant-literal-var-alist this) :test #'string=)))
      (when (null item)
	(setf item (append (list string (intern string) (rdf-literal-string literal))
			   (if (rdf-literal-lang literal) (list :lang (rdf-literal-lang literal))
			       (if (rdf-literal-type literal) (list :type (get-constant-iri (rdf-literal-type literal)))))))
	(push-to-end item (instans-constant-literal-var-alist this)))
      (second item))))
