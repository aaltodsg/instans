;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; ----------------------
;;; Triple/quad add/remove
;;; ----------------------

(defgeneric rete-add (instans subj pred obj graph)
  (:method ((this instans) subj pred obj graph)
    (let ((quad-store (instans-quad-store this)))
      (when quad-store (add-quad quad-store (list subj pred obj graph))))
    (incf (instans-add-quad-count this))
    ;;; Note: graph will be first in args!
    (loop for (alpha . args) in (match-quad (instans-triple-pattern-matcher this) subj pred obj graph)
	  do (add-token alpha args))))

(defgeneric rete-remove (instans subj pred obj graph)
  (:method ((this instans) subj pred obj graph)
    (let ((quad-store (instans-quad-store this)))
      (when quad-store (remove-quad quad-store (list subj pred obj graph))))
    (incf (instans-remove-quad-count this))
    ;;; Note: graph will be first in args!
    (loop for (alpha . args) in (match-quad (instans-triple-pattern-matcher this) subj pred obj graph)
	  do (remove-token alpha args))))

;;; ---------------
;;; Rule add/remove
;;; ---------------

(defgeneric rete-add-rule-instance (instans node token)
  (:method ((this instans) (node node) token)
    (rule-instance-queue-add (instans-rule-instance-queue this) node token)))

(defgeneric rete-remove-rule-instance (instans node token)
  (:method ((this instans) (node node) token)
    (when (eq (instans-rule-instance-removal-policy this) :remove)
      (rule-instance-queue-remove (instans-rule-instance-queue this) node token))))

;;; ----------------------
;;; Initializing execution
;;; ----------------------



(defun initialize-stores-and-indices (instans)
  (loop for node in (instans-nodes instans) do (create-stores-and-indices node)))

(defgeneric create-stores-and-indices (node)
  (:method ((this existence-start-node))
    ;;; An EQL hashtable, since we are using integers as keys!
    (setf (memory-store this) (make-hash-table)))
  (:method ((this memory))
    ;;; An EQL hashtable, since we are using integers as keys!
    (setf (memory-store this) (make-hash-table)))
  ;;; Join creates indices for alpha/beta memories only if the alpha and beta parents share common variables, i.e., (not (null node-use this))
  (:method ((this join-node))
    (let ((beta-key (node-use this))
	  (alpha-key (node-def-preceq (join-alpha this))))
      (setf (join-has-dummy-beta-p this) nil)
      (cond ((null (node-prev (join-beta this)))
	     (setf (join-has-dummy-beta-p this) t))
	    ((node-use this)
	     (setf (join-beta-index this) (make-instance 'hash-token-index :key beta-key :id (format nil "beta-index ~A" (node-number this))))
	     (setf (join-alpha-index this) (make-instance 'hash-token-index :key alpha-key :id (format nil "alpha-index ~A" (node-number this))))))))
  (:method ((this aggregate-join-node))
    (setf (aggregate-join-groups this) (make-hash-table :test #'equal)))
  (:method ((this query-node))
    (when (solution-modifiers-distinct-p this)
      (setf (solution-modifiers-project-index this) (make-instance 'hash-token-index :key (solution-modifiers-project-vars this) :id (format nil "solution-modifiers-project-index ~A" (node-number this))))))
  (:method ((this node)) this))

(defun initialize-data (instans)
  (loop for node in (instans-nodes instans)
;	do (inform "add-initial-data ~S" node)
	do (add-initial-data node)))

(defgeneric add-initial-data (node)
  ;;; Memory just creates store
  (:method ((this existence-start-node))
    ;;; An EQL hashtable, since we are using integers as keys!
    (when (null (node-prev this))
      (let* ((active-p-var (existence-active-p-var this))
	     (counter-var (existence-counter-var this))
	     (initial-token (make-token this (make-singleton-token) (list active-p-var counter-var) (list nil 0)))) ;;; Node is inactive; zero hits
	(add-token this initial-token))))
  (:method ((this memory))
    ;;; An EQL hashtable, since we are using integers as keys!
    (when (null (node-prev this))
      (add-token this (make-singleton-token))))
  ;;; Join creates indices for alpha/beta memories only if the alpha and beta parents share common variables, i.e., (not (null node-use this))
  (:method ((this join-node))
    (let ((beta-memory (join-beta this))
	  (alpha-memory (join-alpha this)))
      (cond ((node-use this)
	     (loop for beta-token in (store-tokens beta-memory)
		   for key = (join-beta-key this beta-token)
		   do (index-put-token (join-beta-index this) key beta-token))
	     (loop for alpha-token in (store-tokens alpha-memory)
		   for key = (join-alpha-key this alpha-token)
		   do (index-put-token (join-alpha-index this) key alpha-token))))))
  (:method ((this aggregate-join-node))
    (setf (aggregate-join-groups this) (make-hash-table :test #'equal)))
  (:method ((this query-node))
    (when (solution-modifiers-distinct-p this)
      (setf (solution-modifiers-project-index this) (make-instance 'hash-token-index :key (solution-modifiers-project-vars this) :id (format nil "solution-modifiers-project-index ~A" (node-number this))))))
  (:method ((this node)) this))

;; (defgeneric initialize-node (node new-nodes)
;;   ;;; Memory just creates store
;;   (:method ((this existence-start-node) new-nodes)
;;     ;;; An EQL hashtable, since we are using integers as keys!
;;     (setf (memory-store this) (make-hash-table))
;;     (when (null (node-prev this))
;;       (let* ((active-p-var (existence-active-p-var this))
;; 	     (counter-var (existence-counter-var this))
;; 	     (initial-token (make-token this (make-singleton-token) (list active-p-var counter-var) (list nil 0)))) ;;; Node is inactive; zero hits
;; 	(add-token this initial-token))))
;;   (:method ((this memory) new-nodes)
;;     ;;; An EQL hashtable, since we are using integers as keys!
;;     (setf (memory-store this) (make-hash-table))
;;     (when (null (node-prev this))
;;       (add-token this (make-singleton-token))))
;;   ;;; Join creates indices for alpha/beta memories only if the alpha and beta parents share common variables, i.e., (not (null node-use this))
;;   (:method ((this join-node) new-nodes)
;;     (let ((beta-key (node-use this))
;; 	  (alpha-key (node-def-preceq (join-alpha this)))
;; 	  (beta-memory (join-beta this))
;; 	  (alpha-memory (join-alpha this)))
;;       (setf (join-has-dummy-beta-p this) nil)
;;       (cond ((null (node-prev beta-memory))
;; 	     (setf (join-has-dummy-beta-p this) t))
;; 	    ((node-use this)
;; 	     (setf (join-beta-index this) (make-instance 'hash-token-index :key beta-key :id (format nil "beta-index ~A" (node-number this))))
;; 	     (when (not (member beta-memory new-nodes))
;; 	       (loop for beta-token in (store-tokens beta-memory)
;; 		     for key = (join-beta-key this beta-token)
;; 		     do (index-put-token (join-beta-index this) key beta-token)))
;; 	     (setf (join-alpha-index this) (make-instance 'hash-token-index :key alpha-key :id (format nil "beta-alpha ~A" (node-number this))))
;; 	     (when (not (member alpha-memory new-nodes))
;; 	       (loop for alpha-token in (store-tokens alpha-memory)
;; 		     for key = (join-alpha-key this alpha-token)
;; 		     do (index-put-token (join-alpha-index this) key alpha-token)))))))
;;   (:method ((this aggregate-join-node) new-nodes)
;;     (setf (aggregate-join-groups this) (make-instance 'group)))
;;   (:method ((this solution-modifiers-node) new-nodes)
;;     (when (solution-modifiers-distinct-p this)
;;       (setf (solution-modifiers-project-index this) (make-instance 'hash-token-index :key (solution-modifiers-project-vars this) :id (format nil "solution-modifiers-project-index ~A" (node-number this))))))
;;   (:method ((this node) new-nodes) this))

(defun dominator-nodes (nodes)
  (loop for node in nodes
	unless (some #'(lambda (p) (member p nodes)) (node-all-precs node))
	collect node))

;; (defun initialize-new-nodes (instans new-nodes)
;;   (loop for node in new-nodes do (initialize-node node new-nodes))
;;   (when (instans-active-p instans)
;;     (let ((dominators (dominator-nodes new-nodes)))
;;       (loop for dominator in dominators
;; 	    do (initialize-new-node-tokens instans dominator))))
;;   instans)

;; ;;; Union?
;; (defgeneric initialize-new-node-tokens (instans node &optional stack)
;;   (:method ((instans instans) (node triple-pattern-node) &optional stack)
;;     (assert (null stack))
;;     (let ((quad-store (instans-quad-store instans)))
;;       (when quad-store
;; 	(error* "Quad store not implemented yet"))))
;;   (:method ((instans instans) (join join-node) &optional stack)
;;     (let* ((alpha-memory (join-alpha join))
;; 	   (beta-memory (join-beta join)))
;;       (cond ((< (store-count beta-memory) (store-count alpha-memory))
;; 	     (loop for beta-token in (store-tokens beta-memory)
;; 		   do (add-beta-token join beta-token stack)))
;; 	    (t
;; 	     (loop for alpha-token in (store-tokens alpha-memory)
;; 		   do (add-alpha-token join alpha-token stack))))))
;;   ;;; does this handle correctly new nodes with a beta-memory predecessor not in new nodes?
;;   (:method ((instans instans) (betamem beta-memory) &optional stack)
;;     (cond ((null stack)
;; 	   (when (node-prev betamem)
;; 	     (initialize-new-node-tokens instans (node-prev betamem) (cons betamem stack))))
;; 	  (t
;; 	   (loop for beta-token in (store-tokens betamem)
;; 		 when (join-node-p (car stack))
;; 		 do (add-beta-token (car stack) beta-token (cdr stack))
;; 		 else
;; 		 do (add-token (car stack) beta-token (cdr stack))))))
;;   (:method ((instans instans) (node node) &optional stack)
;;     (when (node-prev node)
;;       (initialize-new-node-tokens instans (node-prev node) (cons node stack)))))

(defun clear-instans-contents (instans)
  (loop for mem in (filter #'memoryp (instans-nodes instans))
	do (store-clear mem))
  (loop for join in (filter #'join-node-p (instans-nodes instans))
	do (when (node-use join)
	     (index-clear (join-alpha-index join))
	     (index-clear (join-beta-index join)))))

(defgeneric initialize-execution (instans)
  (:method ((this instans))
    (setf (instans-add-quad-count this) 0)
    (setf (instans-remove-quad-count this) 0)
    (let ((queue (instans-rule-instance-queue this)))
      (setf (rule-instance-queue-add-count queue) 0)
      (setf (rule-instance-queue-remove-count queue) 0)
      (setf (rule-instance-queue-select-count queue) 0)
      (setf (rule-instance-queue-ask-count queue) 0)
      (setf (rule-instance-queue-describe-count queue) 0)
      (setf (rule-instance-queue-construct-count queue) 0)
      (setf (rule-instance-queue-modify-count queue) 0)
      (initialize-constant-iris this)
      (initialize-stores-and-indices this)
      (initialize-data this)
      (loop while (rule-instance-queue-head queue)
	   do (execute-rules this))
;      (describe this)
      (instans-add-status this 'instans-initialization-succeeded))))

;; (defgeneric initial-data-ops (instans)
;;   (:method ((this instans))
;;     (flet ((doit (op data)
;; 	     (inform "doit ~A ~A" op data)
;; 	     (loop with func = (case op (INSERT-DATA #'rete-add) (DELETE-DATA #'rete-remove) (t (error* "Unknown init op ~A" op)))
;; 		for item in data
;; 		do (case (car item)
;; 		     (GRAPH (loop for triple in (rest (third item)) do (funcall func this (first triple) (second triple) (third triple) (second item))))
;; 		     (BGP (loop for triple in (rest item) do (funcall func this (first triple) (second triple) (third triple) nil)))
;; 		     (t (error* "Malformed item ~A in data op" item))))))
;;       (loop for op in (instans-initial-data-ops this)
;; 	 do (inform "initial-op ~A" op)
;; 	 do (case (car op)
;; 	      ((INSERT-DATA DELETE-DATA)
;; 	       (doit (first op) (second op))))))))

(defgeneric initialize-constant-iris (instans)
  (:method ((this instans))
    (loop for (iri-string var) in (instans-constant-iri-var-alist this)
	  do (set var (parse-iri iri-string)))))

(defgeneric initialize-constant-literals (instans)
  (:method ((this instans))
    (loop for (key var string &rest args) in (instans-constant-literal-var-alist this)
	  for lang = (getf args :lang)
	  for type = (getf args :type)
	  when lang do (set var (create-rdf-literal-with-lang string lang))
	  else when type do (set var (create-rdf-literal-with-type string type))
	  else do (error* "No type or lang in literal creation args ~S" args))))

(defgeneric initialize-datablock-nodes (instans)
  (:method ((this instans))
    (loop for node in (filter #'datablock-node-p (instans-nodes this))
	  do (loop for token in (datablock-tokens node)
		   do (add-token node token)))))

(defgeneric datablock-tokens (node)
  (:method ((this datablock-node))
    (apply #'mapcar #'(lambda (&rest values) (make-token this nil (alpha-node-variables this) values)) (datablock-values this))))

;;; ---------
;;; Execution
;;; ---------

(defgeneric run-query-input-processors (instans)
  (:method ((this instans))
    (loop with continuep = t
	  while continuep
	  do (setf continuep nil)
	  do (loop for processor in (instans-query-input-processors this)
		   for ll-parser = (run-query-input-processor processor)
		   unless (ll-parser-finished-p ll-parser)
		   do (setf continuep t)))))

(defgeneric run-query-input-processor (query-input-processor)
  (:method ((this query-input-processor))
    (parse (query-input-processor-parser this))))

(defgeneric instans-close-open-streams (instans)
  (:method ((this instans))
    (loop for ip in (instans-query-input-processors this)
	  do (close (lexer-input-stream (ll-parser-lexer (query-input-processor-parser ip)))))
    (when (instans-select-output-processor this)
      (close-query-output-processor (instans-select-output-processor this)))
    (when (instans-construct-output-processor this)
      (close-query-output-processor (instans-construct-output-processor this)))))

;; (defgeneric process-triples (query-input-processor triples)
;;   (:method ((this query-input-processor) triples)
;;     (process-query-input (query-input-processor-instans this) triples :graph (query-input-processor-graph this) :ops (query-input-processor-operations this))))

(defgeneric process-query-input (instans inputs &key graph ops)
  (:method ((this instans) inputs &key graph ops)
    (setf ops (or ops (instans-query-processing-operations this)))
    (when (symbolp ops)
      (setf ops (list ops)))
    (let ((*instans* this))
      (declare (special *instans*))
      (loop for op in ops 
	 do (case op
	      (:add
	       (loop for (subj pred obj . rest) in inputs
		     do (rete-add this subj pred obj (if rest (first rest) graph))))
	      (:remove
	       (loop for (subj pred obj . rest) in inputs
		     do (rete-remove this subj pred obj (if rest (first rest) graph))))
	      (:execute
	       (execute-rules this))
	      (t
	       (error* "Illegal op ~S" op)))))))

(defgeneric execute-rules (instans)
  (:method ((this instans))
    (let* ((policy (instans-queue-execution-policy this))
	   (queue (instans-rule-instance-queue this)))
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

(defgeneric output-quad-or-triple (instans &rest args)
  (:method ((this instans) &rest args)
    (unless (null (instans-construct-output-processor this))
      (write-select-output (instans-construct-output-processor this) this args))))

(defgeneric report-execution-status (instans &key stream)
  (:method ((this instans) &key (stream (instans-default-output this)))
    (let ((queue (instans-rule-instance-queue this)))
      (when (rule-instance-queue-report-p queue)
	(format stream "~&add-quad-count = ~S~%" (instans-add-quad-count this))
	(format stream "remove-quad-count = ~S~%" (instans-remove-quad-count this))
	(format stream "queue-add-count = ~S~%" (rule-instance-queue-add-count queue))
	(format stream "queue-remove-count = ~S~%" (rule-instance-queue-remove-count queue))
	(format stream "queue-select-count = ~S~%" (rule-instance-queue-select-count queue))
	(format stream "queue-ask-count = ~S~%" (rule-instance-queue-ask-count queue))
	(format stream "queue-describe-count = ~S~%" (rule-instance-queue-describe-count queue))
	(format stream "queue-construct-count = ~S~%" (rule-instance-queue-construct-count queue))
	(format stream "queue-modify-count = ~S~%" (rule-instance-queue-modify-count queue))))))

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
;      (inform "add-token ~S" values)
      (cond ((rdf-iri-p dataset)
	     (when (and (rdf-iri-p graph) (rdf-iri= dataset graph))
	       (add-token (car (node-succ this)) (make-token this nil (alpha-node-variables this) (cdr values)))))
	    ((sparql-var-p dataset)
	     (when (rdf-iri-p graph)
	       (add-token (car (node-succ this)) (make-token this nil (cons dataset (alpha-node-variables this)) values)))) ; Drop graph
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
      (loop for next in (node-succ this)
	   do (cond ((typep next 'join-node)
		     (add-beta-token next token stack))
		    (t
		     (add-token next token stack))))))
  (:method ((this filter-node) token &optional stack)
    (let ((arguments (loop for var in (node-use this) collect (token-value this token var))))
;      (inform "~%in add-token ~S (calling ~S ~{~A~^ ~})~%" this (filter-test-func this) arguments)
      (when (eval-sparql-filter (filter-test-func this) arguments)
	(call-succ-nodes #'add-token this token stack))))
  ;;; (add-token filter-memory)
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
  ;;; (add-token exists-start-node)
  (:method ((this exists-start-node) token &optional stack)
    (unless (store-get-token this token)
      (let* ((active-p-var (existence-active-p-var this))
	     (counter-var (existence-counter-var this))
	     (new-token (make-token this token (list active-p-var counter-var) (list t 0)))) ;;; Node is active; zero hits
	(store-put-token this new-token)
	;; (inform "add-token ~A calls store-put-token, token=~%~A~%new-token=~%~A" this token new-token)
	(let ((next (car (node-succ this))))
	  (cond ((typep next 'join-node)
		 (add-beta-token next new-token stack))
		(t
		 (add-token next new-token stack))))
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
      ;; (inform "~%add-token ~S: this=~%" this)
      ;; (describe this)
      ;; (inform "~%start-node=~S~%" start-node)
      ;; (describe start-node)
;      (inform "~%add-token ~S, counter = ~S, active-p = ~S~%" this counter active-p)
      (when (not active-p)
	(case (exists-kind this)
	  (:simple-exists
;	   (inform "Hit!~%")
	   (when (= 1 counter) (call-succ-nodes #'add-token this (start-node-token this token) stack)))
	  (:simple-not-exists
	   (when (= 1 counter) (call-succ-nodes #'remove-token this (start-node-token this token) stack)))
	  (t
	   (when (= 1 counter) (call-succ-nodes #'add-token this (start-node-token this token) stack)))))))
  (:method ((this aggregate-join-node) token &optional stack)
      (multiple-value-bind (group newp) (aggregate-join-get-group this token)
	(unless newp
	  (call-succ-nodes #'remove-token this (group-token group) stack))
;	(describe (first (group-aggregates group)))
	(let* ((aggr-args (loop for var in (aggregate-join-aggr-vars this) collect (token-value this token var))))
;	  (inform "calling aggregate-join-aggr-add-func in ~A.~%Group = ~A,~%aggr-vars = ~A,~%aggr-args = ~A~%" this group  (aggregate-join-aggr-vars this) aggr-args)
	  (apply (aggregate-join-aggr-add-func this) (node-instans this) group aggr-args))
	(call-succ-nodes #'add-token this (group-token group) stack)))
  (:method ((this optional-start-node) token &optional stack)
    (unless (store-get-token this token)
      (let* ((active-p-var (existence-active-p-var this))
	     (counter-var (existence-counter-var this))
	     (new-token (make-token this token (list active-p-var counter-var) (list t 0)))) ;;; Node is active; zero hits
	(store-put-token this new-token)
	(let ((next (car (node-succ this))))
	  (cond ((typep next 'join-node)
		 (add-beta-token next new-token stack))
		(t
		 (add-token next new-token stack))))
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
  (:method ((this union-start-node) token &optional stack)
    (call-succ-nodes #'add-token this token stack))
  (:method ((this union-end-node) token &optional stack)
    (call-succ-nodes #'add-token this token stack))
  (:method ((this query-node) token &optional stack)
    (when (or (not (solution-modifiers-distinct-p this))
	      (let ((key (loop for var in (solution-modifiers-project-vars this) collect (token-value this token var)))
		    (index (solution-modifiers-project-index this)))
		(index-put-token index key token))) ;;; First token with this key added
      (cond ((null (node-succ this))
	     (assert (null stack))
	     (rete-add-rule-instance (node-instans this) this token))
	    (t
	     (call-succ-nodes #'add-token this token stack)))))
  (:method ((this modify-node) token &optional stack)
    (assert (null stack))
    (rete-add-rule-instance (node-instans this) this token))
  (:method ((this node) token &optional stack)
    (declare (ignorable token stack))
    (error* "Don't know how to use node ~A" this)))

(defgeneric add-alpha-token (join alpha-token &optional stack)
  (:method ((this join-node) alpha-token &optional stack)
    (let ((key (join-alpha-key this alpha-token)))
;      (pop alpha-token) ; Drop hashkey
      (when (node-use this)
	(index-put-token (join-alpha-index this) key alpha-token))
      (loop with missing-vars = (join-alpha-minus-beta-vars this)
	    for beta-token in (cond ((node-use this) (index-get-tokens (join-beta-index this) key))
				    (t (store-tokens (join-beta this))))
	    for new-token = (make-token this beta-token missing-vars (loop for var in missing-vars collect (second (assoc var alpha-token))))
	    do (call-succ-nodes #'add-token this new-token stack)))))

(defgeneric add-beta-token (join beta-token &optional stack)
  (:method ((this join-node) beta-token &optional stack)
    (let ((key (join-beta-key this beta-token)))
      (when (node-use this)
	(index-put-token (join-beta-index this) key beta-token))
      (loop with missing-vars = (join-alpha-minus-beta-vars this)
	    for alpha-token in (cond ((node-use this) (index-get-tokens (join-alpha-index this) key))
				     (t (store-tokens (join-alpha this))))
	    for new-token = (make-token this beta-token missing-vars (loop for var in missing-vars collect (second (assoc var alpha-token))))
	    do (call-succ-nodes #'add-token this new-token stack)))))

(defgeneric remove-token (node token &optional stack)
  (:method ((this triple-pattern-node) values &optional stack)
    (assert (null stack))
    (let ((dataset (triple-pattern-node-dataset this))
	  (graph (first values)))
      (cond ((rdf-iri-p dataset)
	     (when (and (rdf-iri-p graph) (rdf-iri= dataset graph))
	       (remove-token (car (node-succ this)) (make-token this nil (alpha-node-variables this) (cdr values)))))
	    ((sparql-var-p dataset)
	     (when (rdf-iri-p graph)
	       (remove-token (car (node-succ this)) (make-token this nil (cons dataset (alpha-node-variables this)) values)))) ; Drop graph
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
      (loop for next in (node-succ this)
	   do (cond ((typep next 'join-node)
		     (remove-beta-token next token stack))
		    (t
		     (remove-token next token stack))))))
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
  ;;; (remove-token exists-start-node)
  (:method ((this exists-start-node) token &optional stack)
    ;; (inform "remove-token ~A ~A" this token)
    (let ((stored-token (store-get-token this token)))
      ;; (inform "remove-token ~A calls store-remove-token, token=~%~A~%stored-token=~%~A" this token stored-token)
      (store-remove-token this stored-token)
      (setf (token-value this stored-token (existence-active-p-var this)) t) ; Activate this node
      (let ((next (car (node-succ this))))
	(cond ((typep next 'join-node)
	       (remove-beta-token next stored-token stack))
	      (t
	       (remove-token next stored-token stack))))
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
  (:method ((this aggregate-join-node) token &optional stack)
    (multiple-value-bind (group newp) (aggregate-join-get-group this token)
      (when newp (error* "Trying to access missing group"))
      (call-succ-nodes #'remove-token this (group-token group) stack)
      (let* ((aggr-args (loop for var in (aggregate-join-aggr-vars this) collect (token-value this token var))))
	(apply (aggregate-join-aggr-remove-func this) (node-instans this) group aggr-args))
      (call-succ-nodes #'add-token this (group-token group) stack)))
  (:method ((this optional-start-node) token &optional stack)
    (let ((stored-token (store-get-token this token)))
      (store-remove-token this stored-token)
      (let ((counter (token-value this stored-token (existence-counter-var this))))
	(cond ((zerop counter) ; no matches of optional
	       (call-succ-nodes #'remove-token (subgraph-end-node this) token stack)) ; we take care of removing all matches after optional-end
	      (t
	       (setf (token-value this stored-token (existence-active-p-var this)) t) ; Activate this node
	       (let ((next (car (node-succ this))))
		 (cond ((typep next 'join-node)
			(remove-beta-token next stored-token stack))
		       (t
			(remove-token next stored-token stack)))) ; optional-end node takes care of removing all matches further down
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
    (signal-sparql-error "(add-token ~A) not implemented yet" this))
  (:method ((this union-start-node) token &optional stack)
    (call-succ-nodes #'remove-token this token stack))
  (:method ((this union-end-node) token &optional stack)
    (call-succ-nodes #'remove-token this token stack))
  (:method ((this query-node) token &optional stack)
    (when (or (not (solution-modifiers-distinct-p this))
	      (let ((key (loop for var in (solution-modifiers-project-vars this) collect (token-value this token var)))
		    (index (solution-modifiers-project-index this)))
		(index-remove-token index key token))) ;;; Last token with this key removed!
      (cond ((null (node-succ this))
	     (assert (null stack))
	     (rete-remove-rule-instance (node-instans this) this token))
	    (t
	     (call-succ-nodes #'remove-token this token stack)))))
  (:method ((this modify-node) token &optional stack)
    (assert (null stack))
    (rete-remove-rule-instance (node-instans this) this token)
    )
  (:method ((this node) token &optional stack)
    (declare (ignorable token stack))
    (error* "Don't know how to use node ~A" this)))

(defgeneric remove-alpha-token (join alpha-token &optional stack)
  (:method ((this join-node) alpha-token &optional stack)
;    (pop alpha-token) ;;; Get rid of the hash key
    (let ((key (join-alpha-key this alpha-token)))
;      (pop alpha-token) ; Drop hashkey
      (when (node-use this)
	(index-remove-token (join-alpha-index this) key alpha-token))
      (loop with missing-vars = (join-alpha-minus-beta-vars this)
	    for beta-token in (cond ((node-use this) (index-get-tokens (join-beta-index this) key))
				    (t (store-tokens (join-beta this))))
	    for new-token = (make-token this beta-token missing-vars (loop for var in missing-vars collect (second (assoc var alpha-token)))) ; alpha-token
	    do (call-succ-nodes #'remove-token this new-token stack)))))

(defgeneric remove-beta-token (join beta-token &optional stack)
  (:method ((this join-node) beta-token &optional stack)
    (let ((key (join-beta-key this beta-token)))
      (when (node-use this)
	(index-remove-token (join-beta-index this) key beta-token))
      (loop with missing-vars = (join-alpha-minus-beta-vars this)
	    for alpha-token in (cond ((node-use this) (index-get-tokens (join-alpha-index this) key))
				     (t (store-tokens (join-alpha this))))
	    for new-token = (make-token this beta-token missing-vars (loop for var in missing-vars collect (second (assoc var alpha-token))))
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
	(token (rule-instance-token instance))
	(rule-types (rule-instance-queue-report-p queue)))
    (cond ((typep node 'select-node)
	   (if (member :select rule-types) (report-rule-execution node token))
	   (incf (rule-instance-queue-select-count queue)))
	  ((typep node 'ask-node)
	   (if (member :ask rule-types) (report-rule-execution node token))
	   (incf (rule-instance-queue-ask-count queue)))
	  ((typep node 'describe-node)
	   (if (member :describe rule-types) (report-rule-execution node token))
	   (incf (rule-instance-queue-describe-count queue)))
	  ((typep node 'modify-node)
	   (if (member :modify rule-types) (report-rule-execution node token))
	   (incf (rule-instance-queue-modify-count queue)))
	  ((typep node 'construct-node)
	   (if (member :construct rule-types) (report-rule-execution node token))
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
  (+ (rule-instance-queue-select-count queue)
     (rule-instance-queue-ask-count queue)
     (rule-instance-queue-describe-count queue)
     (rule-instance-queue-construct-count queue)
     (rule-instance-queue-modify-count queue)))

(defun report-rule-execution (node token &key (variables :project) (stream (instans-default-output (node-instans node))))
  (unless (typep node 'query-node)
    (when (eq variables :project)
      (setf variables :visible)))
  (let* ((instans (node-instans node))
	 (displayed-bindings (loop for var in (funcall (case variables
							 (:all #'node-all-vars-out)
							 (:visible #'node-visible-vars-out)
							 (:project #'solution-modifiers-project-vars ))
						       node)
				   unless (null var)
				   collect (list (uniquely-named-object-name (reverse-resolve-binding instans var)) (token-value node token var)))))
    (format stream "Rule ~A in ~A~%~{~{       ~A = ~S~}~^,~%~}~%" node (instans-name (node-instans node)) displayed-bindings)))
 
(defgeneric execute-rule-node (node token)
  (:method ((this select-node) token)
    (let ((instans (node-instans this)))
;      (inform "execute-rule-node ~A, ~A, processor = ~A" this token (instans-select-output-processor instans))
      (unless (null (instans-select-output-processor instans))
	(write-select-output (instans-select-output-processor instans) this token))))
  (:method ((this ask-node) token)
    (let ((instans (node-instans this)))
      (unless (null (instans-select-output-processor instans))
	(write-select-output (instans-select-output-processor instans) this token))))
  (:method ((this describe-node) token)
    (let ((instans (node-instans this)))
      (unless (null (instans-select-output-processor instans))
	(write-select-output (instans-select-output-processor instans) this token))))
  (:method ((this modify-node) token)
    (let* ((instans (node-instans this))
	   (modify-function (instans-modify-function instans)))
      (unless (null modify-function)
	(apply modify-function this token (instans-modify-function-arguments instans))))
    (when (modify-delete-func this)
      (apply (modify-delete-func this) (node-instans this) (loop for var in (modify-delete-parameters this) collect (token-value this token var))))
    (when (modify-insert-func this)
      (apply (modify-insert-func this) (node-instans this) (loop for var in (modify-insert-parameters this) collect (token-value this token var)))))
  (:method ((this construct-node) token)
    (declare (ignorable this token))
    (signal-sparql-error "(remove-token ~A) not implemented yet" this)))

;;; Group partition

(defgeneric aggregate-join-get-group (aggregate-join token)
  (:method ((this aggregate-join-node) token)
    (let* ((key-args (loop for var in (aggregate-join-key-vars this) collect (token-value this token var)))
	   (key (apply (aggregate-join-key-func this) key-args))
	   (group (gethash key (aggregate-join-groups this))))
      (cond ((null group)
	     (setf group (make-instance 'group :key key :aggregate-join this))
	     (setf (group-token group) (make-token this token (list (aggregate-join-group-var this)) (list group)))
	     (setf (gethash key (aggregate-join-groups this)) group)
	     (values group t))
	    (t
	     (values group nil))))))

;;;

(defun trace-rete ()
  (trace initialize-execution
	 rete-add rete-remove add-token remove-token add-alpha-token add-beta-token remove-alpha-token remove-beta-token match-quad
	 join-beta-key join-alpha-key
	 token-value make-token call-succ-nodes rete-add-rule-instance execute-rules rule-instance-queue-execute-instance execute-rule-node
	 write-select-output store-put-token store-get-token store-remove-token store-tokens index-put-token index-get-tokens index-remove-token
	 aggregate-get-value aggregate-add-value aggregate-remove-value))
