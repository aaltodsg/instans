;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; ----------------------
;;; Triple/quad add/remove
;;; ----------------------

(defgeneric rete-add (network subj pred obj graph)
  (:method ((this network) subj pred obj graph)
    (let ((quad-store (network-quad-store this)))
      (when quad-store (add-quad quad-store (list subj pred obj graph))))
;    (incf (network-add-quad-count this))
    (loop for (alpha . args) in (match-quad (network-triple-pattern-matcher this) subj pred obj graph)
	  do (add-token alpha args))))

(defgeneric rete-remove (network subj pred obj graph)
  (:method ((this network) subj pred obj graph)
    (let ((quad-store (network-quad-store this)))
      (when quad-store (remove-quad quad-store (list subj pred obj graph))))
;    (incf (network-remove-quad-count this))
    (loop for (alpha . args) in (match-quad (network-triple-pattern-matcher this) subj pred obj graph)
	  do (remove-token alpha args))))

(defgeneric add-quads (network quads)
  (:method ((this network) quads)
    (loop for (s p o g) in quads do (rete-add this s p o g))))

(defgeneric remove-quads (network quads)
  (:method ((this network) quads)
    (loop for (s p o g) in quads do (rete-remove this s p o g))))

(defgeneric add-quads-from-file (network quads-file)
  (:method ((this network) quads-file)
    (assert "Not implemented yet")))

;;; ---------------
;;; Rule add/remove
;;; ---------------

(defgeneric add-rules-from-file (network rules-file &key output-directory)
  (:method ((this network) rules-file &key output-directory)
    (compile-sparql-file rules-file :network this :output-directory output-directory)))

(defgeneric rete-add-rule-instance (network node token)
  (:method ((this network) (node node) token)
    (rule-instance-queue-add (network-rule-instance-queue this) node token)))

(defgeneric rete-remove-rule-instance (network node token)
  (:method ((this network) (node node) token)
    (when (network-remove-rule-instances-p this)
      (rule-instance-queue-remove (network-rule-instance-queue this) node token))))

;;; ----------------------
;;; Initializing execution
;;; ----------------------

(defgeneric initialize-node (node new-nodes)
  ;;; Memory just creates store
  (:method ((this existence-start-node) new-nodes)
    ;;; An EQL hashtable, since we are using integers as keys!
    (setf (memory-store this) (make-hash-table))
    (when (null (node-prev this))
      (let* ((active-p-var (existence-active-p-var this))
	     (counter-var (existence-counter-var this))
	     (initial-token (make-token this *singleton-token* (list active-p-var counter-var) (list nil 0)))) ;;; Node is inactive; zero hits
	(store-put-token this initial-token))))
  (:method ((this memory) new-nodes)
    ;;; An EQL hashtable, since we are using integers as keys!
    (setf (memory-store this) (make-hash-table))
    (when (null (node-prev this))
      (store-put-token this *singleton-token*)))
  ;;; Join creates indices for alpha/beta memories only if the alpha and beta parents share common variables, i.e., (not (null node-use this))
  (:method ((this join-node) new-nodes)
    (let ((beta-key (node-use this))
	  (alpha-key (node-def-preceq (join-alpha this)))
	  (beta-memory (join-beta this))
	  (alpha-memory (join-alpha this)))
      (setf (join-has-dummy-beta-p this) nil)
      (cond ((null (node-prev beta-memory))
	     (setf (join-has-dummy-beta-p this) t))
	    ((node-use this)
	     (setf (join-beta-index this) (make-instance 'hash-token-index :key beta-key :id (format nil "beta-index ~A" (node-number this))))
	     (when (not (member beta-memory new-nodes))
	       (loop for beta-token in (store-tokens beta-memory)
		     for key = (join-beta-key this beta-token)
		     do (index-put-token (join-beta-index this) key beta-token)))
	     (setf (join-alpha-index this) (make-instance 'hash-token-index :key alpha-key :id (format nil "beta-alpha ~A" (node-number this))))
	     (when (not (member alpha-memory new-nodes))
	       (loop for alpha-token in (store-tokens alpha-memory)
		     for key = (join-alpha-key this alpha-token)
		     do (index-put-token (join-alpha-index this) key alpha-token)))))))
  (:method ((this aggregate-join-node) new-nodes)
    (setf (aggregate-join-group-partition this) (make-instance 'group-partition)))
  (:method ((this solution-modifiers-node) new-nodes)
    (when (solution-modifiers-distinct-p this)
      (setf (solution-modifiers-project-index this) (make-instance 'hash-token-index :key (solution-modifiers-project-vars this) :id (format nil "solution-modifiers-project-index ~A" (node-number this))))))
  (:method ((this node) new-nodes) this))

(defun dominator-nodes (nodes)
  (loop for node in nodes
	unless (some #'(lambda (p) (member p nodes)) (node-all-precs node))
	collect node))

(defun initialize-new-nodes (network new-nodes)
  (loop for node in new-nodes do (initialize-node node new-nodes))
  (when (network-active-p network)
    (let ((dominators (dominator-nodes new-nodes)))
      (loop for dominator in dominators
	    do (initialize-new-node-tokens network dominator))))
  network)

;;; Union?
(defgeneric initialize-new-node-tokens (network node &optional stack)
  (:method ((net network) (node triple-pattern-node) &optional stack)
    (assert (null stack))
    (let ((quad-store (network-quad-store net)))
      (when quad-store
	(error* "Quad store not implemented yet"))))
  (:method ((net network) (join join-node) &optional stack)
    (let* ((alpha-memory (join-alpha join))
	   (beta-memory (join-beta join)))
      (cond ((< (store-count beta-memory) (store-count alpha-memory))
	     (loop for beta-token in (store-tokens beta-memory)
		   do (add-beta-token join beta-token stack)))
	    (t
	     (loop for alpha-token in (store-tokens alpha-memory)
		   do (add-alpha-token join alpha-token stack))))))
  ;;; does this handle correctly new nodes with a beta-memory predecessor not in new nodes?
  (:method ((net network) (betamem beta-memory) &optional stack)
    (cond ((null stack)
	   (when (node-prev betamem)
	     (initialize-new-node-tokens net (node-prev betamem) (cons betamem stack))))
	  (t
	   (loop for beta-token in (store-tokens betamem)
		 when (join-node-p (car stack))
		 do (add-beta-token (car stack) beta-token (cdr stack))
		 else
		 do (add-token (car stack) beta-token (cdr stack))))))
  (:method ((net network) (node node) &optional stack)
    (when (node-prev node)
      (initialize-new-node-tokens net (node-prev node) (cons node stack)))))

(defun clear-network-contents (network)
  (loop for mem in (filter #'memoryp (network-nodes network))
	do (store-clear mem))
  (loop for join in (filter #'join-node-p (network-nodes network))
	do (when (node-use join)
	     (index-clear (join-alpha-index join))
	     (index-clear (join-beta-index join)))))

(defgeneric initialize-execution (network)
  (:method ((this network))
    (setf (network-input-count this) 0)
    (setf (network-add-quad-count this) 0)
    (setf (network-remove-quad-count this) 0)
    (let ((queue (network-rule-instance-queue this)))
      (setf (rule-instance-queue-add-count queue) 0)
      (setf (rule-instance-queue-remove-count queue) 0)
      (setf (rule-instance-queue-select-count queue) 0)
      (setf (rule-instance-queue-construct-count queue) 0)
      (setf (rule-instance-queue-modify-count queue) 0))
    (initialize-datablock-nodes this)))

(defgeneric initialize-datablock-nodes (network)
  (:method ((this network))
    (loop for node in (filter #'datablock-node-p (network-nodes this))
	  do (loop for token in (datablock-tokens node)
		   do (add-token node token)))))

(defgeneric datablock-tokens (node)
  (:method ((this datablock-node))
    (apply #'mapcar #'(lambda (&rest values) (make-token this nil (alpha-node-variables this) values)) (datablock-values this))))

;;; ---------
;;; Execution
;;; ---------

(defgeneric execute-rules (network)
  (:method ((this network))
    (let* ((policy (network-execution-policy this))
	   (queue (network-rule-instance-queue this)))
      (case policy
	(:first
	 (rule-instance-queue-execute-first queue))
	(:snapshot
	 (rule-instance-queue-execute-snapshot queue))
	(:repeat-first
	 (loop while (rule-instance-queue-execute-first queue)))
	(:repeat-snapshot
	 (loop while (rule-instance-queue-execute-snapshot queue)))
	(t (error* "Unknown execution policy ~A" policy))))))

(defgeneric execute-system (network)
  (:method ((this network))
    (initialize-execution this)
    (let ((input-function (network-input-function this))
	  (input-function-arguments (network-input-function-arguments this)))
      (loop for input = (apply input-function input-function-arguments)
	    while input
	    do (incf (network-input-count this))
	    do (let ((op (if (member (car input) '(:add :remove :add-exec-remove)) (pop input) :add)))
		 (case op
		   (:add
		    (loop for (s p o g) in input do (rete-add this s p o g))
		    (execute-rules this))
		   (:remove
		    (loop for (s p o g) in input do (rete-remove this s p o g))
		    (execute-rules this))
		   (:execute
		    (loop for (s p o g) in input do (rete-add this s p o g))
		    (execute-rules this)
		    (loop for (s p o g) in input do (rete-remove this s p o g)))
		   (t
		    (error* "Illegal op ~S" op))))))))

(defun call-succ-nodes (func node token stack)
  (cond ((null stack)
	 ;; (loop for rest on (node-succ node)
	 ;;       do (assert* (not (member (car rest) (cdr rest))) "~%~S appears twice in (node-succ ~S) = ~S" (car rest) node (node-succ node)))
	 (loop for succ in (node-succ node) do (funcall func succ token nil)))
	(t
	 (funcall func (car stack) token (cdr stack)))))

(defgeneric add-token (node token &optional stack)
  (:method ((this triple-pattern-node) values &optional stack)
    (assert (null stack))
    (let ((dataset (triple-pattern-node-dataset this))
	  (graph (first values)))
      (cond ((sparql-var-p dataset)
	     (add-token (car (node-succ this)) (make-token this nil (cons dataset (alpha-node-variables this)) values)))
	    ((rdf-iri-p dataset)
	     (when (and (rdf-iri-p graph) (rdf-iri= dataset graph))
	       (add-token (car (node-succ this)) (make-token this nil (alpha-node-variables this) (cdr values))))) ; Drop graph
	    ((null graph)
	     (add-token (car (node-succ this)) (make-token this nil (alpha-node-variables this) (cdr values))))))) ; Drop graph
  (:method ((this alpha-node) values &optional stack)
    (assert (null stack))
    (add-token (car (node-succ this)) (make-token this nil (alpha-node-variables this) values)))
  (:method ((this alpha-memory) token &optional stack)
    (assert (null stack))
    (unless (store-get-token this token)
      (store-put-token this token)
      (call-succ-nodes #'add-alpha-token this token nil)))
  (:method ((this beta-memory) token &optional stack)
    (unless (store-get-token this token)
      (store-put-token this token)
      (call-succ-nodes #'add-beta-token this token stack)))
  (:method ((this filter-node) token &optional stack)
    (when (eval-sparql-filter (filter-test-func this) (loop for var in (node-use this) collect (token-value this token var)))
      (call-succ-nodes #'add-token this token stack)))
  (:method ((this filter-memory) token &optional stack)
    (let ((new-value (eval-sparql-filter (filter-test-func this) (loop for var in (node-use this) collect (token-value this token var))))
	  (prev-value-var (filter-memory-prev-value-var this))
	  (stored-token (store-get-token this token)))
      (cond ((null stored-token)
	     (setf token (make-token this token (list prev-value-var) (list new-value)))
	     (store-put-token this token)
	     (when new-value (call-succ-nodes #'add-token this token stack)))
	    (t
	     (let ((prev-value (token-value this stored-token prev-value-var)))
	       (when (not (eq prev-value new-value))
		 (setf (token-value this stored-token prev-value-var) new-value)
		 (call-succ-nodes (if new-value #'add-token #'remove-token) this stored-token stack)))))))
  (:method ((this bind-node) token &optional stack)
    (let ((value (catch :sparql-error (apply (bind-form-func this) (loop for var in (node-use this) collect (token-value this token var))))))
      (unless (sparql-error-p value)
	(setf token (make-token this token (list (bind-variable this)) (list value))))
      (call-succ-nodes #'add-token this token stack)))
  ;;; Currently not handling order and slice
  (:method ((this datablock-node) token &optional stack)
    (call-succ-nodes #'add-token this token stack))
  (:method ((this solution-modifiers-node) token &optional stack)
    (cond ((solution-modifiers-distinct-p this)
	   (let ((key (loop for var in (solution-modifiers-project-vars this) collect (token-value this token var)))
		 (index (solution-modifiers-project-index this)))
	     (when (index-put-token index key token) ;;; First token with this key added
	       (call-succ-nodes #'add-token this key stack))))
    	  (t
	   (call-succ-nodes #'add-token this token stack))))
  (:method ((this select-node) token &optional stack)
    (assert (null stack))
    (rete-add-rule-instance (node-network this) this token))
  (:method ((this exists-start-node) token &optional stack)
    (unless (store-get-token this token)
      (let* ((active-p-var (existence-active-p-var this))
	     (counter-var (existence-counter-var this))
	     (new-token (make-token this token (list active-p-var counter-var) (list t 0)))) ;;; Node is active; zero hits
	(store-put-token this new-token)
	(add-beta-token (car (node-succ this)) new-token stack)
	(setf (token-value this new-token active-p-var) nil) ;;; Deactivate this node
	(case (exists-kind this)
	  (:simple-exists
	   (when (plusp (token-value this new-token counter-var)) (call-succ-nodes #'add-token (subgraph-end-node this) new-token stack))) 
	  (:simple-not-exists
	   (when (zerop (token-value this new-token counter-var)) (call-succ-nodes #'add-token (subgraph-end-node this) new-token stack)))
	  (t
	   (call-succ-nodes #'add-token (subgraph-end-node this) new-token stack))))))
  (:method ((this exists-end-node) token &optional stack)
    (let* ((start-node (subgraph-start-node this))
	   (active-p (token-value this token (existence-active-p-var start-node)))
	   (counter (incf (token-value this token (existence-counter-var start-node)))))
      (when (not active-p)
	(case (exists-kind this)
	  (:simple-exists
	   (when (= 1 counter) (call-succ-nodes #'add-token this (start-node-token this token) stack)))
	  (:simple-not-exists
	   (when (= 1 counter) (call-succ-nodes #'remove-token this (start-node-token this token) stack)))
	  (t
	   (when (= 1 counter) (call-succ-nodes #'add-token this (start-node-token this token) stack)))))))
  (:method ((this modify-node) token &optional stack)
    (assert (null stack))
    (rete-add-rule-instance (node-network this) this token))
  (:method ((this aggregate-join-node) token &optional stack)
    (let* ((group-form-args (loop for var in (node-use this) collect (token-value this token var)))
	   (key (apply (aggregate-join-group-form-func this) group-form-args))
	   (partition (aggregate-join-group-partition this))
	   (part (group-partition-get-part partition key)))
      (cond ((null part)
					;(setf part (group-partition-insert-part partition key))
	     (error* "Not ready yet"))
	    (t
	     (call-succ-nodes #'remove-token this (group-partition-part-token part) stack)))
      (let ((part-token (group-partition-part-add-token part this token)))
	(call-succ-nodes #'add-token this part-token stack))))
  (:method ((this optional-start-node) token &optional stack)
    (unless (store-get-token this token)
      (let* ((active-p-var (existence-active-p-var this))
	     (counter-var (existence-counter-var this))
	     (new-token (make-token this token (list active-p-var counter-var) (list t 0)))) ;;; Node is active; zero hits
	(store-put-token this new-token)
	(add-beta-token (car (node-succ this)) new-token stack)
	(setf (token-value this new-token active-p-var) nil) ;;; Deactivate this node
	(when (zerop (token-value this new-token counter-var))
	  (call-succ-nodes #'add-token (subgraph-end-node this) token stack))))) ; We pass the original token
  (:method ((this optional-end-node) token &optional stack)
    (let* ((start-node (subgraph-start-node this))
	   (active-p (token-value this token (existence-active-p-var start-node)))
	   (counter (incf (token-value this token (existence-counter-var start-node)))))
      (when (and (not active-p) (= 1 counter))
	(call-succ-nodes #'remove-token this (start-node-token this token) stack))
      (call-succ-nodes #'add-token this token stack)))
  (:method ((this construct-node) token &optional stack)
    (declare (ignorable this token stack))
    (error* "Not implemented yet"))
  (:method ((this union-start-node) token &optional stack)
    (call-succ-nodes #'add-token this token stack))
  (:method ((this union-end-node) token &optional stack)
    (call-succ-nodes #'add-token this token stack))
  (:method ((this node) token &optional stack)
    (declare (ignorable token stack))
    (error* "Don't know how to use node ~A" this)))

(defgeneric add-alpha-token (join alpha-token &optional stack)
  (:method ((this join-node) alpha-token &optional stack)
    (let ((key (join-alpha-key this alpha-token)))
      (pop alpha-token) ; Drop hashkey
      (when (node-use this)
	(index-put-token (join-alpha-index this) key alpha-token))
      (loop for beta-token in (cond ((null key) (store-tokens (join-beta this)))
				    (t (index-get-tokens (join-beta-index this) key)))
	    for new-token = (make-token this beta-token (node-def this) (loop for var in (node-def this) collect (second (assoc var alpha-token))))
	    do (call-succ-nodes #'add-token this new-token stack)))))

(defgeneric add-beta-token (join beta-token &optional stack)
  (:method ((this join-node) beta-token &optional stack)
    (let ((key (join-beta-key this beta-token)))
      (when (node-use this)
	(index-put-token (join-beta-index this) key beta-token))
      (loop for alpha-token in (cond ((null key)
				      (store-tokens (join-alpha this)))
				     (t (index-get-tokens (join-alpha-index this) key)))
	    for new-token = (make-token this beta-token (node-def this) alpha-token)
	    do (call-succ-nodes #'add-token this new-token stack)))))

(defgeneric remove-token (node token &optional stack)
  (:method ((this triple-pattern-node) values &optional stack)
    (assert (null stack))
    (let ((dataset (triple-pattern-node-dataset this))
	  (graph (first values)))
      (cond ((sparql-var-p dataset)
	     (remove-token (car (node-succ this)) (make-token this nil (cons dataset (alpha-node-variables this)) values)))
	    ((rdf-iri-p dataset)
	     (when (and (rdf-iri-p graph) (rdf-iri= dataset graph))
	       (remove-token (car (node-succ this)) (make-token this nil (alpha-node-variables this) (cdr values))))) ; Drop graph
	    ((null graph)
	     (remove-token (car (node-succ this)) (make-token this nil (alpha-node-variables this) (cdr values))))))) ; Drop graph
  (:method ((this alpha-node) values &optional stack)
    (assert (null stack))
    (remove-token (car (node-succ this)) (make-token this nil (alpha-node-variables this) values)))
  (:method ((this alpha-memory) token &optional stack)
    (assert (null stack))
    (when (store-get-token this token)
      (store-remove-token this token)
      (call-succ-nodes #'remove-alpha-token this token stack)))
  (:method ((this beta-memory) token &optional stack)
    (when (store-get-token this token)
      (store-remove-token this token)
      (call-succ-nodes #'remove-beta-token this token stack)))
  (:method ((this filter-node) token &optional stack)
    (when (eval-sparql-filter (filter-test-func this) (loop for var in (node-use this) collect (token-value this token var)))
      (call-succ-nodes #'remove-token this token stack)))
  (:method ((this filter-memory) token &optional stack)
    (let ((stored-token (store-get-token this token)))
      (unless (null stored-token)
	(store-remove-token this stored-token)
	(call-succ-nodes #'remove-token this stored-token stack))))
  (:method ((this bind-node) token &optional stack)
    (let ((value (catch :sparql-error (apply (bind-form-func this) (loop for var in (node-use this) collect (token-value this token var))))))
      (unless (sparql-error-p value)
	(setf token (make-token this token (list (bind-variable this)) (list value))))
      (call-succ-nodes #'remove-token this token stack)))
  ;;; Currently not handling order and slice
  (:method ((this datablock-node) token &optional stack)
    (call-succ-nodes #'remove-token this token stack))
  (:method ((this solution-modifiers-node) token &optional stack)
    (cond ((solution-modifiers-distinct-p this)
	   (let ((key (loop for var in (solution-modifiers-project-vars this) collect (token-value this token var)))
		 (index (solution-modifiers-project-index this)))
	     (when (index-remove-token index key token) ;;; Last token with this key removed!
	       (call-succ-nodes #'remove-token key token stack))))
	  (t
	   (call-succ-nodes #'remove-token this token stack))))
  (:method ((this select-node) token &optional stack)
    (assert (null stack))
    (rete-remove-rule-instance (node-network this) this token))
  (:method ((this exists-start-node) token &optional stack)
    (let ((stored-token (store-get-token this token)))
      (store-remove-token this stored-token)
      (setf (token-value this stored-token (existence-active-p-var this)) t) ; Activate this node
      (remove-beta-token (car (node-succ this)) stored-token stack)
      (setf (token-value this stored-token (existence-active-p-var this)) nil) ;;; Deactivate this node
      (call-succ-nodes #'remove-token (subgraph-end-node this) stored-token stack)))
  (:method ((this exists-end-node) token &optional stack)
    (let* ((start-node (subgraph-start-node this))
	   (active-p (token-value this token (existence-active-p-var start-node)))
	   (counter (decf (token-value this token (existence-counter-var start-node)))))
      (when (not active-p)
	(case (exists-kind this)
	  (:simple-exists
	   (when (zerop counter) (call-succ-nodes #'remove-token this (start-node-token this token) stack)))
	  (:simple-not-exists
	   (when (zerop counter) (call-succ-nodes #'add-token this (start-node-token this token) stack)))
	  (t
	   (call-succ-nodes #'remove-token this (start-node-token this token) stack))))))
  (:method ((this modify-node) token &optional stack)
    (assert (null stack))
    (rete-remove-rule-instance (node-network this) this token)
    )
  (:method ((this aggregate-join-node) token &optional stack)
    (let* ((group-form-args (loop for var in (node-use this) collect (token-value this token var)))
	   (key (apply (aggregate-join-group-form-func this) group-form-args))
	   (partition (aggregate-join-group-partition this))
	   (part (group-partition-get-part partition key)))
      (call-succ-nodes #'remove-token this (group-partition-part-token part) stack)
      (let ((part-token (group-partition-part-remove-token part this token)))
	(when part-token
	  (call-succ-nodes #'add-token this part-token stack)))))
  (:method ((this optional-start-node) token &optional stack)
    (let ((stored-token (store-get-token this token)))
      (store-remove-token this stored-token)
      (let ((counter (token-value this stored-token (existence-counter-var this))))
	(cond ((zerop counter) ; no matches of optional
	       (call-succ-nodes #'remove-token (subgraph-end-node this) token stack)) ; we take care of removing all matches after optional-end
	      (t
	       (setf (token-value this stored-token (existence-active-p-var this)) t) ; Activate this node
	       (remove-beta-token (car (node-succ this)) stored-token stack) ; optional-end node takes care of removing all matches further down
	       (setf (token-value this stored-token (existence-active-p-var this)) nil)))))) ;;; Deactivate this node
  (:method ((this optional-end-node) token &optional stack)
    (let* ((start-node (subgraph-start-node this))
	   (active-p (token-value this token (existence-active-p-var start-node)))
	   (counter (decf (token-value this token (existence-counter-var start-node)))))
      (when (and (not active-p) (zerop counter))
	(call-succ-nodes #'add-token this (start-node-token this token) stack)) ; Have to add the skip optional token.
      (call-succ-nodes #'remove-token this token stack)))
  (:method ((this construct-node) token &optional stack)
    (declare (ignorable this token stack))
    (error* "Not implemented yet"))
  (:method ((this union-start-node) token &optional stack)
    (call-succ-nodes #'remove-token this token stack))
  (:method ((this union-end-node) token &optional stack)
    (call-succ-nodes #'remove-token this token stack))
  (:method ((this node) token &optional stack)
    (declare (ignorable token stack))
    (error* "Don't know how to use node ~A" this)))

(defgeneric remove-alpha-token (join alpha-token &optional stack)
  (:method ((this join-node) alpha-token &optional stack)
					;(pop alpha-token) ;;; Get rid of the hash key
    (let ((key (join-alpha-key this alpha-token)))
      (pop alpha-token) ; Drop hashkey
      (when (node-use this)
	(index-remove-token (join-alpha-index this) key alpha-token))
      (loop for beta-token in (cond ((null key) (store-tokens (join-beta this)))
				    (t (index-get-tokens (join-beta-index this) key)))
	    for new-token = (make-token this beta-token (node-def this) alpha-token)
	    do (call-succ-nodes #'remove-token this new-token stack)))))

(defgeneric remove-beta-token (join beta-token &optional stack)
  (:method ((this join-node) beta-token &optional stack)
    (let ((key (join-beta-key this beta-token)))
      (when (node-use this)
	(index-remove-token (join-beta-index this) key beta-token))
      (loop for alpha-token in (cond ((null key)
				      (store-tokens (join-alpha this)))
				     (t (index-get-tokens (join-alpha-index this) key)))
	    for new-token = (make-token this beta-token (node-def this) alpha-token)
	    do (call-succ-nodes #'remove-token this new-token stack)))))

(defun rule-instance-queue-empty-p (queue)
  (null (rule-instance-queue-head queue)))

(defun rule-instance-queue-add (queue node token)
  (let* ((instance (make-instance 'rule-instance :node node :token token))
	 (new-cell (list instance)))
    (cond ((null (rule-instance-queue-head queue))
	   (setf (rule-instance-queue-head queue) new-cell)
	   (setf (rule-instance-queue-tail queue) new-cell))
	  (t
	   (setf (cdr (rule-instance-queue-tail queue)) new-cell)
	   (setf (rule-instance-queue-tail queue) new-cell)))
    (incf (rule-instance-queue-add-count queue))))

(defun rule-instance-queue-remove (queue node token)
  (loop for prev = nil then list
	for list on (rule-instance-queue-head queue)
	for instance = (car list)
	when (and (eq node (rule-instance-node instance)) (equal token (rule-instance-token instance)))
	do (progn
	     (cond ((null prev)
		    (setf (rule-instance-queue-head queue) (cdr list)))
		   (t
		    (setf (cdr prev) (cdr list))))
	     (when (null (cdr list))
	       (setf (rule-instance-queue-tail queue) prev))
	     (incf (rule-instance-queue-remove-count queue))
	     (return :found))
	finally (return :not-found)))

(defun rule-instance-queue-execute-instance (queue instance)
  (let ((node (rule-instance-node instance))
	(token (rule-instance-token instance)))
    (report-rule-execution node token)
    (cond ((typep node 'select-node)
	   (incf (rule-instance-queue-select-count queue)))
	  ((typep node 'modify-node)
	   (incf (rule-instance-queue-modify-count queue)))
	  ((typep node 'construct-node)
	   (incf (rule-instance-queue-construct-count queue)))
	  (t
	   (error* "Illegal kind of node ~S" node)))
    (execute-rule-node node token)))

(defun rule-instance-queue-execute-first (queue)
  (cond ((null (rule-instance-queue-head queue))
	 nil)
	(t
	 (let ((instance (car (rule-instance-queue-head queue))))
	   (cond ((eq (rule-instance-queue-head queue) (rule-instance-queue-tail queue))
		  (setf (rule-instance-queue-head queue) nil)
		  (setf (rule-instance-queue-tail queue) nil))
		 (t
		  (setf (rule-instance-queue-head queue) (cdr (rule-instance-queue-head queue)))))
	   (rule-instance-queue-execute-instance queue instance)
	   t))))

(defun rule-instance-queue-execute-snapshot (queue)
  (cond ((null (rule-instance-queue-head queue))
	 nil)
	(t
	 (let ((instances (rule-instance-queue-head queue)))
	   (setf (rule-instance-queue-head queue) nil)
	   (setf (rule-instance-queue-tail queue) nil)
	   (loop for instance in instances
		 do (rule-instance-queue-execute-instance queue instance)))
	 t)))

(defun rule-instance-queue-execute-count (queue)
  (+ (rule-instance-queue-select-count queue) (rule-instance-queue-construct-count queue) (rule-instance-queue-modify-count queue)))

(defun report-rule-execution (node token)
  (let* ((used-vars (if (select-node-p node) (node-use (node-prev node)) (node-use node)))
	 (displayed-bindings (loop for var in used-vars
				   for var-orig-name = (car (rassoc var (bindings-alist (node-bindings node))))
				   collect (list var-orig-name (token-value node token var)))))
    (inform "Rule ~A~%~{~{       ~A = ~S~}~^,~%~}~%" node displayed-bindings)))

(defgeneric execute-rule-node (node token)
  (:method ((this select-node) token)
    (let* ((network (node-network this))
	   (select-function (network-select-function network)))
      (unless (null select-function)
	(apply select-function this token (network-select-function-arguments network)))))
  (:method ((this modify-node) token)
    (let* ((network (node-network this))
	   (modify-function (network-modify-function network)))
      (unless (null modify-function)
	(apply modify-function this token (network-modify-function-arguments network))))
    (when (modify-delete-func this)
      (apply (modify-delete-func this) (node-network this) (loop for var in (modify-delete-parameters this) collect (token-value this token var))))
    (when (modify-insert-func this)
      (apply (modify-insert-func this) (node-network this) (loop for var in (modify-insert-parameters this) collect (token-value this token var)))))
  (:method ((this construct-node) token)
    (declare (ignore this token))
    (error* "Not implemented yet")))

;;; Group partition

(defmethod initialize-instance :after ((this group-partition) &key &allow-other-keys)
  (setf (group-partition-parts this) (make-hash-table :test #'equal)))

(defgeneric group-partition-get-part (partition key)
  (:method ((this group-partition) key)
    (gethash key (group-partition-parts this))))

;; (defgeneric group-partition-insert-part (partition key aggr-join)
;;   (:method ((this group-partition) key)
;;     (setf (gethash key (group-partition-parts this))
;; 	  (make-instance 'group-partition-part :key key
;; 			 :aggr-values (loop repeat (length aggr-join) collect nil)
;; 			 :aggr-values-histories (and (find-if #'(lambda (x) (member (third x) '(min max group-concat))) (aggregate-join-var-aggr-list aggr-join))
;; 						     (loop repeat (length aggr-join) collect 

(defgeneric group-partition-part-add-token (part aggr-join token)
  (:method ((this group-partition-part) (aggr-join aggregate-join-node) token)
    (loop with prev-values = (group-partition-part-aggr-values this)
	  for prev-value = (pop prev-values)
	  for (output-var input-var op scalars) in (aggregate-join-var-aggr-list aggr-join)
	  collect output-var into output-vars
	  collect (case op
		    (sum (+ (or prev-value 0) (token-value aggr-join token input-var)))
		    (min (if (null prev-value) (token-value aggr-join token input-var)
			     (min prev-value (token-value aggr-join token input-var))))
		    (max (if (null prev-value) (token-value aggr-join token input-var)
			     (max prev-value (token-value aggr-join token input-var))))
		    (sample (token-value aggr-join token input-var))
		    (t (error* "Not implemented yet!")))
	  into new-values
	  finally (return (progn
			    (incf (group-partition-part-input-token-count this))
			    (setf (group-partition-part-aggr-values this) new-values)
			    (setf (group-partition-part-token this) (make-token aggr-join nil output-vars new-values))))))) ; returns updated part-token

(defgeneric group-partition-part-remove-token (part aggr-join token)
  (:method ((this group-partition-part) (aggr-join aggregate-join-node) token)
    (cond ((zerop (decf (group-partition-part-input-token-count this)))
	   (setf (group-partition-part-aggr-values this) nil)
	   (setf (group-partition-part-token this) nil)) ; returns nil
	  (t
	   (loop with prev-values = (group-partition-part-aggr-values this)
		 for prev-value = (pop prev-values)
		 for (output-var input-var op scalars) in (aggregate-join-var-aggr-list aggr-join)
		 collect output-var into output-vars
		 collect (case op
			   (sum (- prev-value (token-value aggr-join token input-var)))
			   (min (if (null prev-value) (token-value aggr-join token input-var)
				    (min prev-value (token-value aggr-join token input-var))))
			   (max (if (null prev-value) (token-value aggr-join token input-var)
				    (max prev-value (token-value aggr-join token input-var))))
			   (sample (token-value aggr-join token input-var))
			   (t (error* "Not implemented yet!")))
		 into new-values
		 finally (return (progn 
				   (setf (group-partition-part-aggr-values this) new-values)
				   (setf (group-partition-part-token this) (make-token aggr-join nil output-vars new-values))))))))) ; returns updated part-token

