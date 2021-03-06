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
    (if (operation-report-p this :rete-add)	
	(format (instans-default-output this) "~&~A: Rete add to ~A ~A ~A ~A~%~%"
		(instans-name this)
		(if graph (sparql-value-to-string graph :instans this) "DEFAULT")
		 (sparql-value-to-string subj :instans this)
		 (sparql-value-to-string pred :instans this)
		 (sparql-value-to-string obj :instans this)))
    (let ((quad-store (instans-quad-store this)))
      (when quad-store (add-quad quad-store (list subj pred obj graph))))
    (incf (instans-add-quad-count this))
    ;;; Note: graph will be first in args!
    (loop for (alpha . args) in (match-quad (instans-triple-pattern-matcher this) subj pred obj graph)
	  do (setf (instans-current-op this) (list :rete-add subj pred obj graph))
	  do (add-token alpha args)
	  do (setf (instans-current-op this) nil)
	 )))

(defgeneric rete-remove (instans subj pred obj graph)
  (:method ((this instans) subj pred obj graph)
    (if (operation-report-p this :rete-remove)
	(format (instans-default-output this) "~&~A: Rete remove from ~A ~A ~A ~A~%~%"
		(instans-name this)
		(if graph (sparql-value-to-string graph :instans this) "DEFAULT")
		(sparql-value-to-string subj :instans this)
		(sparql-value-to-string pred :instans this)
		(sparql-value-to-string obj :instans this)))
    (let ((quad-store (instans-quad-store this)))
      (when quad-store (remove-quad quad-store (list subj pred obj graph))))
    (incf (instans-remove-quad-count this))
    ;;; Note: graph will be first in args!
    (loop for (alpha . args) in (match-quad (instans-triple-pattern-matcher this) subj pred obj graph)
	  do (setf (instans-current-op this) (list :rete-remove subj pred obj graph))
	  do (remove-token alpha args)
	  do (setf (instans-current-op this) nil)
	 )))

;;; ---------------
;;; Rule add/remove
;;; ---------------

(defgeneric rete-add-rule-instance (instans node token)
  (:method ((this instans) (node node) token)
    (rule-instance-queue-add (instans-rule-instance-queue this) node token)))

(defgeneric rete-remove-rule-instance (instans node token)
  (:method ((this instans) (node node) token)
    (when (instans-allow-rule-instance-removal-p this)
      (rule-instance-queue-remove (instans-rule-instance-queue this) node token))))

;;; ----------------------
;;; Initializing execution
;;; ----------------------



(defun initialize-stores-and-indices (instans)
  (loop for node in (instans-nodes instans) do (create-stores-and-indices node)))

(defgeneric create-stores-and-indices (node)
  (:method ((this existence-start-node))
    ;;; An EQL hashtable, since we are using integers as keys!
    (push
     (cons this (setf (memory-store this) (make-hash-table)))
     (instans-stores (node-instans this))))
  (:method ((this service-node))
    (push
     (cons this (setf (memory-store this) (make-hash-table)))
     (instans-stores (node-instans this)))
    (push
     (setf (service-node-index this) (make-instance 'hash-token-index :key (service-node-index-key-vars this) :id (format nil "service-node-index ~A" (node-number this))))
     (instans-indices (node-instans this))))
  (:method ((this memory))
    ;;; An EQL hashtable, since we are using integers as keys!
    (push
     (cons this (setf (memory-store this) (make-hash-table)))
     (instans-stores (node-instans this))))
  ;;; Join creates indices for alpha/beta memories only if the alpha and beta parents share common variables, i.e., (not (null node-use this))
  (:method ((this join-node))
    (let ((beta-key (node-use this))
	  (alpha-key (node-def-preceq (join-alpha this))))
      (setf (join-has-dummy-beta-p this) nil)
      (cond ((null (node-prev (join-beta this)))
	     (setf (join-has-dummy-beta-p this) t))
	    ((node-use this)
	     (push
	      (setf (join-beta-index this) (make-instance 'hash-token-index :key beta-key :id (format nil "beta-index ~A" (node-number this))))
	      (instans-indices (node-instans this)))
	     (push
	      (setf (join-alpha-index this) (make-instance 'hash-token-index :key alpha-key :id (format nil "alpha-index ~A" (node-number this))))
	      (instans-indices (node-instans this)))))))
  (:method ((this aggregate-join-node))
    (setf (aggregate-join-groups this) (make-hash-table :test #'equal)))
  (:method ((this query-node))
    (when (solution-modifiers-distinct-p this)
      (setf (solution-modifiers-project-index this) (make-instance 'hash-token-index :key (solution-modifiers-project-vars this) :id (format nil "solution-modifiers-project-index ~A" (node-number this))))))
  (:method ((this node)) this))

(defun initialize-data (instans)
  (let ((*instans* instans))
    (declare (special *instans*))
    (loop for node in (instans-nodes instans)
;	  do (inform "add-initial-data ~S" node)
	  do (add-initial-data node))))

(defgeneric add-initial-data (node)
  ;;; Memory just creates store
  (:method ((this existence-start-node))
    ;;; An EQL hashtable, since we are using integers as keys!
    (when (null (node-prev this))
      (let* ((active-p-var (existence-active-p-var this))
	     (counter-var (existence-counter-var this))
	     ;;; Order in the new token is ((nil key) (counter-var 0) (active-p nil) ..)
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


(defun dominator-nodes (nodes)
  (loop for node in nodes
	unless (some #'(lambda (p) (member p nodes)) (node-all-precs node))
	collect node))

(defun clear-instans-contents (instans)
  (loop for mem in (filter #'memoryp (instans-nodes instans))
	do (store-clear mem))
  (loop for join in (filter #'join-node-p (instans-nodes instans))
	do (when (node-use join)
	     (index-clear (join-alpha-index join))
	     (index-clear (join-beta-index join)))))

(defgeneric initialize-execution (instans)
  (:method ((this instans))
    (handler-case
	(progn
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
	    (loop while (rule-instance-queue-head queue) do (execute-rules this))
	    ;(describe this)
	    (instans-add-status this 'instans-rule-initialization-succeeded)
	    this))
      (t (e) 
	(declare (ignore e))
	(instans-add-status this 'instans-rule-initialization-failed)
	nil))))

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

;; (defgeneric run-input-processors (instans)
;;   (:method ((this instans))
;;     (loop with continuep = t
;; 	  while continuep
;; 	  do (setf continuep nil)
;; 	  do (loop for processor in (instans-input-processors this)
;; 		   for ll-parser = (run-input-processor processor)
;; 		   do (cond ((ll-parser-failed-p ll-parser)
;; 			     (instans-add-status this 'instans-rdf-parsing-failed (ll-parser-error-messages ll-parser))
;; 			     (return nil))
;; 			    ((ll-parser-succeeded-p ll-parser)
;; 			     (instans-add-status this 'instans-rdf-parsing-succeeded))
;; 			    (t
;; 			     (setf continuep t))))
;; 	 finally (return t))))

;; (defgeneric run-input-processor (instans-input-processor)
;;   (:method ((this instans-agent-input-processor))
;;     (let* ((instans (instans-input-processor-instans this))
;; 	   (statements (agent-receive instans)))
;;       (process-query-input this statements)))
;;   (:method ((this instans-stream-input-processor))
;;     (parse (instans-stream-input-processor-parser this))))


(defun run-instanses (instanses)
  (loop for runnable = (filter #'instans-runnable-p instanses)
        while runnable
        do (loop for instans in runnable do (run-input-processors instans nil))))

(defgeneric instans-runnable-input-processors (instans)
  (:method ((this instans))
    (filter #'instans-input-processor-runnable-p (instans-input-processors this))))

(defgeneric run-input-processors (instans continuouslyp)
  (:method ((this instans) continuouslyp)
    (loop for runnable = (instans-runnable-input-processors this)
	  for continuep = t then (and continuouslyp runnable)
	  while continuep
	  do (loop for processor in runnable do (run-input-processor processor)))))

(defgeneric run-input-processor (instans-input-processor)
  (:method ((this instans-agent-input-processor))
    (let ((instans (instans-input-processor-instans this)))
      (multiple-value-bind (statements receivedp)
	  (agent-receive instans nil)
	(cond ((eq statements :eof)
	       (setf (instans-input-processor-status this) :succeeded))
	      ((not receivedp)
	       (setf (instans-input-processor-status this) :runnable))
	      (t
	       (process-query-input this statements)
	       (setf (instans-input-processor-status this) :runnable))))))
  (:method ((this instans-stream-input-processor))
    (let ((ll-parser (parse (instans-stream-input-processor-parser this)))
	  (instans (instans-input-processor-instans this)))
      (cond ((ll-parser-failed-p ll-parser)
	     (instans-add-status instans 'instans-rdf-parsing-failed (ll-parser-error-messages ll-parser))
	     (setf (instans-input-processor-status this) :failed)
	     (unless (instans-runnable-p instans)
	       (instans-close-open-streams instans)))
	    ((ll-parser-succeeded-p ll-parser)
	     (instans-add-status instans 'instans-rdf-parsing-succeeded)
	     (setf (instans-input-processor-status this) :succeeded))
	    (t
	     (setf (instans-input-processor-status this) :runnable))))))

(defgeneric instans-close-open-streams (instans)
  (:method ((this instans))
;    (handler-case 
	(progn
	  (loop for ip in (instans-input-processors this)
		do (when (instans-stream-input-processor-p ip)
		     (close-stream (lexer-input-stream (ll-parser-lexer (instans-stream-input-processor-parser ip))) "instans-close-open-streams: close ~A")))
	  (when (instans-ask-output-processor this)
	    (close-output-processor (instans-ask-output-processor this)))
	  (when (instans-select-output-processor this)
	    (close-output-processor (instans-select-output-processor this)))
	  (when (instans-construct-output-processor this)
	    (close-output-processor (instans-construct-output-processor this))))
      ;; (t (e)
      ;; 	 (instans-add-status this 'instans-rule-running-failed (list (format nil "~A" e)))))
  ))

(defgeneric process-query-input (instans-input-processor inputs &key graph ops)
  (:method ((this instans-input-processor) inputs &key graph ops)
    (let ((instans (instans-input-processor-instans this)))
      (labels ((map-named-blank-node (x)
		 (cond ((rdf-named-blank-node-p x)
			(let ((tr (gethash (uniquely-named-object-name x) (instans-input-processor-blank-node-mapping this))))
			  (when (null tr)
			    (setf tr (generate-anonymous-blank-node instans))
;			    (inform "map ~S to ~S~%" x tr)
			    (setf (gethash (uniquely-named-object-name x) (instans-input-processor-blank-node-mapping this)) tr))
;			  (inform "map-named-blank-node (~S) -> ~S~%" x tr)
			  tr))
		       (t x))))
	(setf ops (or ops (instans-rdf-operations instans)))
					;    (inform "Ops = ~S" ops)
	(let ((*instans* instans)
	      (reportp (operation-report-p instans :rdf-operations)))
	  (declare (special *instans*))
	  (loop for op in ops 
		when reportp
		do (format (instans-default-output instans) "~%~A: Running RDF-operation ~A~%" (instans-name instans) op)
		do (case op
		     (:add
		      (loop for (subj pred obj . rest) in inputs
			    do (rete-add instans (map-named-blank-node subj) (map-named-blank-node pred) (map-named-blank-node obj)
					 (if rest (first rest) (map-named-blank-node graph)))))
		     (:remove
		      (loop for (subj pred obj . rest) in inputs
			    do (rete-remove instans (map-named-blank-node subj) (map-named-blank-node pred) (map-named-blank-node obj)
					    (if rest (first rest) (map-named-blank-node graph)))))
		     (:execute
		      (execute-rules instans))
		     (:execute-snapshot
		      (execute-rules instans :snapshot))
		     (:execute-first
		      (execute-rules instans :first))
		     (:execute-repeat-snapshot
		      (execute-rules instans :repeat-snapshot))
		     (:execute-repeat-first
		      (execute-rules instans :repeat-first))
		     (:flush
		      (if (instans-construct-output-processor instans)
			  (flush-output-processor (instans-construct-output-processor instans)))
		      (if (instans-select-output-processor instans)
			  (flush-output-processor (instans-select-output-processor instans))))
		     (t
		      (error* "Illegal op ~S" op)))))))))

(defgeneric initialize-reporting (instans reporting)
  (:method ((this instans) reporting)
    (setf (instans-report-operation-kinds this) reporting)
    (when (or (getf reporting :memory-sizes)  (getf reporting :memory-summaries) )
      (setf (instans-memory-sizes-report-interval this) (getf reporting :memory-sizes))
      (setf (instans-memory-sizes-report-counter this) 0)
      (setf (instans-memory-summaries-report-interval this) (getf reporting :memory-summaries))
      (setf (instans-memory-summaries-report-counter this) 0)
      (let ((store-sizes-alist (loop for (node . store) in (instans-stores this) collect (list node store (hash-table-count store)))))
	(setf (instans-store-sizes-alist this) store-sizes-alist))
      (let ((index-sizes-alist (loop for index in (instans-indices this) collect (list index (hash-table-count (hash-token-index-table index))))))
	(setf (instans-index-sizes-alist this) index-sizes-alist))
      ;; (inform "~A" (instans-store-sizes-alist this))
      ;; (inform "~A" (instans-index-sizes-alist this))
      )))
      

(defgeneric report-memory-summaries (instans)
 (:method ((this instans))
    (let ((stream (instans-default-output this)))
      (loop for item in (instans-store-sizes-alist this)
	    for count = (third item)
	    for new-count = (hash-table-count (second item))
	    for delta = (- new-count count)
	    do (setf (third item) new-count)
	    collect (list (first item) delta new-count) into store-sizes-delta-alist
	    finally (progn
		      (setf store-sizes-delta-alist (sort store-sizes-delta-alist #'> :key #'second))
		      (cond ((eql 0 (second (first store-sizes-delta-alist)))
			     (format stream "~%Store sizes did not grow"))
			    (t
			     (format stream "~%Largest gains in store sizes:")
			     (loop for (node delta new-count) in store-sizes-delta-alist
				   for i from 0
				   while (and (< i 10) (> delta 0))
				   do (format stream "~%  ~A: +~D now ~D" node delta new-count))))))
      (loop for item in (instans-index-sizes-alist this)
	    for count = (second item)
	    for new-count = (hash-table-count (hash-token-index-table (first item)))
	    for delta = (- new-count count)
	    do (setf (second item) new-count)
	    collect (list (first item) delta new-count) into index-sizes-delta-alist
	    finally (when index-sizes-delta-alist
		      (setf index-sizes-delta-alist (sort index-sizes-delta-alist #'> :key #'second))
		      (cond ((eql 0 (second (first index-sizes-delta-alist)))
			     (format stream "~%Index sizes did not grow"))
			    (t
			     (format stream "~%Largest gains in index sizes:")
			     (loop for i from 0
				   for (index delta new-count) in index-sizes-delta-alist
				   while (and (< i 10) (> delta 0))
				   do (format stream "~%  ~A: +~D now ~D" index delta new-count))))))
      (loop for item in (instans-stores this)
	    sum (hash-table-count (cdr item)) into sizes
	    finally (format stream "~%Store total size = ~D" sizes))
      (loop for index in (instans-indices this)
	    sum (hash-table-count (hash-token-index-table index)) into sizes
	    finally (format stream "~%Index total size = ~D" sizes))
      (let ((queue (instans-rule-instance-queue this)))
	(format stream "~&add-quad-count = ~S~%" (instans-add-quad-count this))
	(format stream "remove-quad-count = ~S~%" (instans-remove-quad-count this))
	(format stream "queue-add-count = ~S~%" (rule-instance-queue-add-count queue))
	(format stream "queue-remove-count = ~S~%" (rule-instance-queue-remove-count queue))
	(format stream "queue-select-count = ~S~%" (rule-instance-queue-select-count queue))
	(format stream "queue-modify-count = ~S~%" (rule-instance-queue-modify-count queue))))))

(defgeneric report-memory-sizes-headers (instans)
  (:method ((this instans))
    (let ((store-names (loop for item in (instans-store-sizes-alist this) collect (node-name (first item))))
	  (index-names (loop for item in (instans-index-sizes-alist this) collect (hash-token-index-id (first item)))))
      (format (instans-memory-sizes-report-stream this) "~&~(~{~A~^,~}~)~%" (append store-names index-names)))))

(defgeneric report-memory-sizes (instans)
  (:method ((this instans))
    (let* ((deltap (instans-memory-sizes-report-delta-p this))
	   (store-sizes (loop for item in (instans-store-sizes-alist this)
			      for count = (third item)
			      for new-count = (hash-table-count (second item))
			      do (setf (third item) new-count)
			      collect (if deltap (- new-count count) new-count)))
	   (index-sizes (loop for item in (instans-index-sizes-alist this)
			      for count = (second item)
			      for new-count = (hash-table-count (hash-token-index-table (first item)))
			      do (setf (second item) new-count)
			      collect (if deltap (- new-count count) new-count))))
      (format (instans-memory-sizes-report-stream this) "~&~{~A~^,~}~%" (append store-sizes index-sizes)))))

(defgeneric execute-rules (instans &optional policy)
  (:method ((this instans) &optional policy)
    (unless policy (setf policy (instans-queue-execution-policy this)))
    (let ((queue (instans-rule-instance-queue this)))
;      (inform "(instans-size-report-interval this) = ~A" (instans-size-report-interval this))
      (when (instans-memory-summaries-report-interval this)
	(when (zerop (mod (instans-memory-summaries-report-counter this) (instans-memory-summaries-report-interval this)))
	  (report-memory-summaries this))
	(incf (instans-memory-summaries-report-counter this)))
      (when (instans-memory-sizes-report-interval this)
	(when (zerop (mod (instans-memory-sizes-report-counter this) (instans-memory-sizes-report-interval this)))
	  (report-memory-sizes this))
	(incf (instans-memory-sizes-report-counter this)))
      (if (operation-report-p this :execute) 
	  (format (instans-default-output this) "~&~A: Execute rules with policy ~A~%~%" (instans-name this) policy))
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

(defgeneric output-quad-or-triple (instans s p o &optional g)
  (:method ((this instans) s p o &optional g)
    (unless (null (instans-construct-output-processor this))
      (construct-output (instans-construct-output-processor this) s p o g))))

(defgeneric report-execution-status (instans &key stream)
  (:method ((this instans) &key (stream (instans-default-output this)))
    (let ((queue (instans-rule-instance-queue this)))
      (when (operation-report-p this :statistics)
	(format stream "~&~A:~%add-quad-count = ~S~%" (instans-name this) (instans-add-quad-count this))
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
	 (let* ((instans (node-instans node))
		(reportp (operation-report-p instans :call-succ-nodes))
		(output (instans-default-output instans))
		(op (cond ((eq func #'add-token) :add-token)
			  ((eq func #'remove-token) :remove-token)
			  ((eq func #'add-alpha-token) :add-alpha-token)
			  ((eq func #'remove-alpha-token) :remove-alpha-token)
			  ((eq func #'add-beta-token) :add-beta-token)
			  ((eq func #'remove-beta-token) :remove-beta-token)
			  (t (error* "Unknown function ~S here" func)))))
	   (when reportp (format output "~&~A: calling successors of ~A using function ~A~%" (instans-name instans) node func))
	   (loop for succ in (node-succ node)
		 when reportp do (format output "~%  ~(~A~) ~A~%" op succ)
		 do (funcall func succ token nil))
	   (when reportp (format output "~%~A: called successors of ~A using function ~A~%~%" (instans-name instans) node func))))
	(t
	 (funcall func (car stack) token (cdr stack)))))

(defgeneric add-token (node token &optional stack)
  (:method ((this triple-pattern-node) values &optional stack)
    (assert (null stack))
    (let ((dataset (triple-pattern-node-dataset this))
	  (graph (first values)))
      ;; (inform "add-token ~S (dataset = ~S)" values dataset)
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
	   do (cond ((or (typep next 'join-node) (typep next 'minus-node))
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
	     ;;; Order in the new token is ((nil key) (counter-var 0) (active-p nil) ..)
	     (new-token (make-token this token (list active-p-var counter-var) (list t 0)))) ;;; Node is active; zero hits
	(store-put-token this new-token)
;	(inform "~%add-token ~S: this=~%, kind=~A" this (exists-kind this))
;	(instans-show-rete-status (node-instans this) this new-token "~%Before children calls")
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
	   (call-succ-nodes #'add-token (subgraph-end-node this) new-token stack)))
;	(instans-show-rete-status (node-instans this) this new-token "~%After children calls")
	)))
  (:method ((this exists-end-node) token &optional stack)
    (let* ((start-node (subgraph-start-node this))
	   (active-p (token-value this token (existence-active-p-var start-node)))
	   (counter (incf (token-value this token (existence-counter-var start-node)))))
;      (instans-show-rete-status (node-instans this) this token "~%Entering, kind = ~A, activep = ~A" (exists-kind this) active-p)
      (when (not active-p)
	(case (exists-kind this)
	  (:simple-exists
;	   (inform "Hit!~%")
	   (when (= 1 counter) (call-succ-nodes #'add-token this (start-node-token this token) stack)))
	  (:simple-not-exists
;	   (instans-show-rete-status (node-instans this) this (start-node-token this token) "~%Before children calls")
	   (when (= 1 counter) (call-succ-nodes #'remove-token this (start-node-token this token) stack))
;	   (instans-show-rete-status (node-instans this) this (start-node-token this token ) "~%After children calls")
	   )
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
	     ;;; Order in the new token is ((nil key) (counter-var 0) (active-p nil) ..)
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
;	(instans-show-rete-status (node-instans this) this token "~%Entering, activep = ~A, counter = ~D" active-p counter)
	(call-succ-nodes #'remove-token this (start-node-token this token) stack))
      (call-succ-nodes #'add-token this token stack)))
  (:method ((this union-start-node) token &optional stack)
    (call-succ-nodes #'add-token this token stack))
  (:method ((this union-end-node) token &optional stack)
    (call-succ-nodes #'add-token this token stack))
  (:method ((this service-node) token &optional stack)
    (unless (store-get-token this token)
      (store-put-token this token)
      (let ((key (service-node-index-key this token))
	    (instans (node-instans this)))
  	(multiple-value-bind (service-tokens definedp)
  	    (index-get-tokens-and-defined-p (service-node-index this) key)
  	  (when (not definedp)
  	    (let* ((query-token-strings
		    (loop for token-string in (service-node-query-token-strings this)
			  for translated = (cond ((and (> (length token-string) 0) (member (char token-string 0) '(#\? #\$) :test #'char=))
						  (let ((canonic-var (cdr (find-if #'(lambda (binding) (equal (uniquely-named-object-name (car binding)) (string-upcase token-string))) (instans-bindings instans)))))
						    (assert* canonic-var "Expected non-null var with name ~S" token-string)
						    (cond ((find canonic-var (service-node-index-key-vars this) :test #'uniquely-named-object-equal)
							   (sparql-value-to-string (token-value this token canonic-var)))
							  (t token-string))))
						 (t token-string))
; 			  do (inform "token-string = ~S, translated = ~S" token-string translated)
			  collect translated))
		   (query-string (format nil "SELECT * { ~{~A~^ ~} }" query-token-strings))
		   (response (flexi-streams:octets-to-string (drakma:http-request (rdf-iri-string (service-node-endpoint this))
										  :parameters (list (cons "query" query-string))
										  :accept "application/sparql-results+json; charset=utf-8") :external-format :utf-8))
		   (decoded-response (cl-json:decode-json-from-string response)))
	      ;; (inform "query = ~S" query-string)
	      ;; (inform "response = ~S~%decoded = ~S" response decoded-response)
	      (let (
		    (bindings (rest (assoc :bindings (rest (assoc :results decoded-response))))))
  	      (loop for var-name in (rest (assoc :vars (rest (assoc :head decoded-response))))
		    collect (intern-keyword (string-upcase var-name)) into name-keywords
		    collect (resolve-binding instans (make-sparql-var instans (format nil "?~A" (string-upcase var-name)))) into canonic-vars
		    finally (progn ;(inform "name-keywords = ~S~&canonic-vars = ~S" name-keywords canonic-vars)
				   (loop for binding in bindings
				  for service-token = (loop for name-keyword in name-keywords
							    collect (cdr (assoc :value (rest (assoc name-keyword binding)))) into values
							    finally (return (make-token this nil canonic-vars values)))
;				  do (inform "service-token = ~S" service-token)
				  do (index-put-token (service-node-index this) key service-token)
				  do (push service-token service-tokens)))))))
  	  (loop with missing-vars = (service-node-query-minus-index-key-vars this)
  		for service-token in service-tokens
  	        for new-token = (make-token this token missing-vars (loop for var in missing-vars
									  for binding = (assoc var service-token)
									  collect (if binding (second binding) (sparql-unbound))))
;	        do (inform "new-token = ~S" new-token)
  	        do (call-succ-nodes #'add-token this new-token stack))))))
  (:method ((this query-node) token &optional stack)
    (cond ((not (solution-modifiers-distinct-p this))
	   (cond ((null (node-succ this))
		  (assert (null stack))
		  (rete-add-rule-instance (node-instans this) this token))
		 (t
		  (call-succ-nodes #'add-token this token stack))))
	  (t
	   (let ((key (loop for var in (solution-modifiers-project-vars this) collect (token-value this token var)))
		 (index (solution-modifiers-project-index this)))
	     ;; (inform "Calling (index-put-token ~S ~S ~S)~%Before (index-get-tokens ~S ~S) = ~S" index key token index key (index-get-tokens index key))
	     ;; (inform "Index ~S:~%" index)
	     ;; (loop for x in (index-tokens index) do (inform "  ~S" x))
	     (when (index-put-token index key token) ;;; First token with this key added
	       ;; (inform "Returned T, (index-get-tokens ~S ~S) = ~S" index key (index-get-tokens index key))
	       ;; (inform "Index ~S:~%" index)
	       ;; (loop for x in (index-tokens index) do (inform "  ~S" x))
	       (cond ((null (node-succ this))
		      (assert (null stack))
		      (rete-add-rule-instance (node-instans this) this token))
		     (t
		      (call-succ-nodes #'add-token this token stack))))))))
  (:method ((this modify-node) token &optional stack)
    (assert (null stack))
    (rete-add-rule-instance (node-instans this) this token))
  (:method ((this node) token &optional stack)
    (declare (ignorable token stack))
    (error* "Don't know how to use node ~A" this)))

(defgeneric add-alpha-token (join alpha-token &optional stack)
  (:method ((this minus-node) alpha-token &optional stack)
    (let ((key (join-alpha-key this alpha-token)))
;      (pop alpha-token) ; Drop hashkey
      (assert (node-use this))
      (index-put-token (join-alpha-index this) key alpha-token)
      (loop for beta-token in (index-get-tokens (join-beta-index this) key)
	    do (call-succ-nodes #'remove-token this beta-token stack))))
  (:method ((this join-node) alpha-token &optional stack)
    (let ((key (join-alpha-key this alpha-token)))
;      (pop alpha-token) ; Drop hashkey
      (when (node-use this)
	(index-put-token (join-alpha-index this) key alpha-token))
      (loop with missing-vars = (join-alpha-minus-beta-vars this)
	    for beta-token in (cond ((node-use this) (index-get-tokens (join-beta-index this) key))
				    (t (store-tokens (join-beta this))))
	    for new-token = (make-token this beta-token missing-vars (loop for var in missing-vars
									   for binding = (assoc var alpha-token)
									   collect (if binding (second binding) (sparql-unbound))))
	    do (call-succ-nodes #'add-token this new-token stack)))))

(defgeneric add-beta-token (join beta-token &optional stack)
  (:method ((this minus-node) beta-token &optional stack)
    (let ((key (join-beta-key this beta-token)))
      (assert (node-use this))
      (index-put-token (join-beta-index this) key beta-token)
      (when (null (index-get-tokens (join-alpha-index this) key))
	(call-succ-nodes #'add-token this beta-token stack))))
  (:method ((this join-node) beta-token &optional stack)
    (let ((key (join-beta-key this beta-token)))
      (when (node-use this)
	(index-put-token (join-beta-index this) key beta-token))
      (loop with missing-vars = (join-alpha-minus-beta-vars this)
	    for alpha-token in (cond ((node-use this) (index-get-tokens (join-alpha-index this) key))
				     (t (store-tokens (join-alpha this))))
	    for new-token = (make-token this beta-token missing-vars (loop for var in missing-vars 
									   for binding = (assoc var alpha-token)
									   collect (if binding (second binding) (sparql-unbound))))
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
;;   (:method ((this exists-start-node) token &optional stack)
;;     ;; (inform "remove-token ~A ~A" this token)
;;     (let ((stored-token (store-get-token this token)))
;;       ;; (inform "remove-token ~A calls store-remove-token, token=~%~A~%stored-token=~%~A" this token stored-token)
;;       (store-remove-token this stored-token)
;;       (setf (token-value this stored-token (existence-active-p-var this)) t) ; Activate this node
;;       (let ((next (car (node-succ this))))
;; 	(cond ((typep next 'join-node)
;; 	       (remove-beta-token next stored-token stack))
;; 	      (t
;; 	       (remove-token next stored-token stack))))
;;       (setf (token-value this stored-token (existence-active-p-var this)) nil) ;;; Deactivate this node
;;       (call-succ-nodes #'remove-token (subgraph-end-node this) stored-token stack)))
  ;;; (remove-token exists-start-node)
  ;;; !!! This may be wrong, especially when kind != simple-(not-)exists!!!
  (:method ((this exists-start-node) token &optional stack)
    ;; (inform "remove-token ~A ~A" this token)
    (let ((stored-token (store-get-token this token)))
      (unless (null stored-token) ;;; May be already removed?!
	(let* ((counter-var (existence-counter-var this))
	       (prev-counter-value (token-value this stored-token counter-var)))
	  ;; (inform "remove-token ~A calls store-remove-token, token=~%~A~%stored-token=~%~A" this token stored-token)
	  (store-remove-token this stored-token)
	  (setf (token-value this stored-token (existence-active-p-var this)) t) ; Activate this node
	  (let ((next (car (node-succ this))))
	    (cond ((typep next 'join-node)
		   (remove-beta-token next stored-token stack))
		  (t
		   (remove-token next stored-token stack))))
	  (setf (token-value this stored-token (existence-active-p-var this)) nil) ;;; Deactivate this node
	  (case (exists-kind this)
	    (:simple-exists
	     (when (plusp prev-counter-value) (call-succ-nodes #'remove-token (subgraph-end-node this) stored-token stack)))
	    (:simple-not-exists
	     (when (zerop prev-counter-value) (call-succ-nodes #'remove-token (subgraph-end-node this) stored-token stack)))
	    (t
	     (call-succ-nodes #'remove-token (subgraph-end-node this) stored-token stack)))))))
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
      (unless (null stored-token)
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
		 (setf (token-value this stored-token (existence-active-p-var this)) nil))))))) ;;; Deactivate this node
  (:method ((this optional-end-node) token &optional stack)
    (let* ((start-node (subgraph-start-node this))
	   (active-p (token-value this token (existence-active-p-var start-node)))
	   (counter (decf (token-value this token (existence-counter-var start-node)))))
      (when (and (not active-p) (zerop counter))
	(call-succ-nodes #'add-token this (start-node-token this token) stack)) ; Have to add the skip optional token.
      (call-succ-nodes #'remove-token this token stack)))
  (:method ((this construct-node) token &optional stack)
    (declare (ignorable this token stack))
    (assert (null (node-succ this)))
    (assert (null stack))
    (rete-remove-rule-instance (node-instans this) this token))
  (:method ((this union-start-node) token &optional stack)
    (declare (special *oink*))
    (when *oink* (inform "add-token union-start-node ~A ~A" this token))
    (call-succ-nodes #'remove-token this token stack))
  (:method ((this union-end-node) token &optional stack)
    (declare (special *oink*))
    (when *oink* (inform "add-token union-end-node ~A ~A" this token))
    (call-succ-nodes #'remove-token this token stack))
  (:method ((this service-node) token &optional stack)
    (let ((stored-token (store-get-token this token)))
      (unless (null stored-token) ;;; May be already removed?!
  	(store-remove-token this stored-token)
	(let ((key (service-node-index-key this token)))
	  (multiple-value-bind (service-tokens definedp)
	      (index-get-tokens-and-defined-p (service-node-index this) key)
	    (assert definedp)
	    (loop with missing-vars = (service-node-query-minus-index-key-vars this)
		  for service-token in service-tokens
		  for new-token = (make-token this token missing-vars (loop for var in missing-vars
									    for binding = (assoc var service-token)
									    collect (if binding (second binding) (sparql-unbound))))
		  do (call-succ-nodes #'remove-token this new-token stack)))))))
  (:method ((this query-node) token &optional stack)
    (cond ((not (solution-modifiers-distinct-p this))
	   (cond ((null (node-succ this))
		  (assert (null stack))
		  (rete-remove-rule-instance (node-instans this) this token))
		 (t
		  (call-succ-nodes #'remove-token this token stack))))
	  (t
	   (let ((key (loop for var in (solution-modifiers-project-vars this) collect (token-value this token var)))
		 (index (solution-modifiers-project-index this)))
	     ;; (inform "Calling (index-remove-token ~S ~S ~S)~%Before (index-get-tokens ~S ~S) = ~S" index key token index key (index-get-tokens index key))
	     ;; (inform "Index ~S:~%" index)
	     ;; (loop for x in (index-tokens index) do (inform "  ~S" x))
	     (when (index-remove-token index key token) ;;; Last token with this key removed!
	       (cond ((null (node-succ this))
		      (assert (null stack))
		      (rete-remove-rule-instance (node-instans this) this token))
		     (t
		      (call-succ-nodes #'remove-token this token stack))))))))
  (:method ((this modify-node) token &optional stack)
    (assert (null stack))
    (rete-remove-rule-instance (node-instans this) this token)
    )
  (:method ((this node) token &optional stack)
    (declare (ignorable token stack))
    (error* "Don't know how to use node ~A" this)))

(defgeneric remove-alpha-token (join alpha-token &optional stack)
  (:method ((this minus-node) alpha-token &optional stack)
;    (pop alpha-token) ;;; Get rid of the hash key
    (let ((key (join-alpha-key this alpha-token)))
;      (pop alpha-token) ; Drop hashkey
      (assert (node-use this))
      (index-remove-token (join-alpha-index this) key alpha-token)
      (loop for beta-token in (index-get-tokens (join-beta-index this) key)
	    do (call-succ-nodes #'add-token this beta-token stack))))
  (:method ((this join-node) alpha-token &optional stack)
;    (pop alpha-token) ;;; Get rid of the hash key
    (let ((key (join-alpha-key this alpha-token)))
;      (pop alpha-token) ; Drop hashkey
      (when (node-use this)
	(index-remove-token (join-alpha-index this) key alpha-token))
      (loop with missing-vars = (join-alpha-minus-beta-vars this)
	    for beta-token in (cond ((node-use this) (index-get-tokens (join-beta-index this) key))
				    (t (store-tokens (join-beta this))))
	    for new-token = (make-token this beta-token missing-vars (loop for var in missing-vars
									   for binding = (assoc var alpha-token)
									   collect (if binding (second binding) (sparql-unbound))))
	    do (call-succ-nodes #'remove-token this new-token stack)))))

(defgeneric remove-beta-token (join beta-token &optional stack)
  (:method ((this minus-node) beta-token &optional stack)
    (let ((key (join-beta-key this beta-token)))
      (assert (node-use this))
      (index-remove-token (join-beta-index this) key beta-token)
      (unless (index-get-tokens (join-alpha-index this) key)
	(call-succ-nodes #'remove-token this beta-token stack))))
  (:method ((this join-node) beta-token &optional stack)
    (let ((key (join-beta-key this beta-token)))
      (when (node-use this)
	(index-remove-token (join-beta-index this) key beta-token))
      (loop with missing-vars = (join-alpha-minus-beta-vars this)
	    for alpha-token in (cond ((node-use this) (index-get-tokens (join-alpha-index this) key))
				     (t (store-tokens (join-alpha this))))
	    for new-token = (make-token this beta-token missing-vars (loop for var in missing-vars
									   for binding = (assoc var alpha-token)
									   collect (if binding (second binding) (sparql-unbound))))
	    do (call-succ-nodes #'remove-token this new-token stack)))))

(defun rule-instance-queue-empty-p (queue)
  (null (rule-instance-queue-head queue)))

(defun rule-instance-queue-add (queue node token)
  (let* ((rule-instance (make-instance 'rule-instance :node node :token token))
	 (new-cell (list rule-instance)))
    (cond ((null (rule-instance-queue-head queue))
	   (setf (rule-instance-queue-head queue) new-cell)
	   (setf (rule-instance-queue-tail queue) new-cell))
	  (t
	   (setf (cdr (rule-instance-queue-tail queue)) new-cell)
	   (setf (rule-instance-queue-tail queue) new-cell)))
    (if (operation-report-p (rule-instance-queue-instans queue) :queue)
	(report-rule-op queue "Added" rule-instance))
    (incf (rule-instance-queue-add-count queue))))

(defun rule-instance-queue-remove (queue node token)
  (loop for prev = nil then list
	for list on (rule-instance-queue-head queue)
	for rule-instance = (car list)
	when (and (eq node (rule-instance-node rule-instance)) (equal token (rule-instance-token rule-instance)))
	do (progn
	     (cond ((null prev)
		    (setf (rule-instance-queue-head queue) (cdr list)))
		   (t
		    (setf (cdr prev) (cdr list))))
	     (when (null (cdr list))
	       (setf (rule-instance-queue-tail queue) prev))
	     (incf (rule-instance-queue-remove-count queue))
	     (if (operation-report-p (rule-instance-queue-instans queue) :queue)
		 (report-rule-op queue "Removed existing" rule-instance))
	     (return :found))
	finally (let ((instans (rule-instance-queue-instans queue)))
		  (when (operation-report-p instans :queue)
		    ;; (format (instans-default-output instans) "~A: Tried to remove non-existing rule-instance.~%Rule ~A~%~{~{     ~A = ~A~}~^~%~}~%"
		    ;; 	    (instans-name instans) (rule-node-name-pretty node) (node-token-bindings-for-reporting node token))
		    (report-queue queue))
		  (return :not-found))))

(defun rule-instance-queue-execute-instance (queue rule-instance)
  (let ((node (rule-instance-node rule-instance))
	(token (rule-instance-token rule-instance))
	(instans (rule-instance-queue-instans queue)))
    (cond ((typep node 'select-node)
	   (if (operation-report-p instans :select) (report-rule-op queue "Executing" rule-instance))
	   (incf (rule-instance-queue-select-count queue)))
	  ((typep node 'ask-node)
	   (if (operation-report-p instans :ask) (report-rule-op queue "Executing" rule-instance))
	   (incf (rule-instance-queue-ask-count queue)))
	  ((typep node 'describe-node)
	   (if (operation-report-p instans :describe) (report-rule-op queue "Executing" rule-instance))
	   (incf (rule-instance-queue-describe-count queue)))
	  ((typep node 'modify-node)
	   (if (operation-report-p instans :modify) (report-rule-op queue "Executing" rule-instance))
	   (incf (rule-instance-queue-modify-count queue)))
	  ((typep node 'construct-node)
	   (if (operation-report-p instans :construct) (report-rule-op queue "Executing" rule-instance))
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

(defun operation-report-p (instans kind)
  (getf (instans-report-operation-kinds instans) kind))

(defgeneric rule-node-name-pretty (rule-node)
  (:method ((this rule-node))
    (if (rule-node-rule-name this) (format nil "~A (~(~A~))" (rule-node-rule-name this) (node-name this)) (node-name this))))

(defun node-token-bindings-for-reporting (node token)
  (let ((instans (node-instans node))
	(variables (cond ((typep node 'construct-node) (construct-parameters node))
			 ((typep node 'select-node) (solution-modifiers-project-vars node))
			 (t (node-visible-vars-out node)))))
    (loop for var in variables
	  unless (null var)
	  collect (list (uniquely-named-object-name (reverse-resolve-binding instans var))
			(sparql-value-to-string (token-value node token var) :instans instans)))))

(defun report-rule-op (queue op rule-instance &key stream)
  (let ((instans (rule-instance-queue-instans queue)))
    (when (null stream) (setf stream  (instans-default-output instans)))
    (format stream "~&~A: operation ~A~%Rule ~A~%~{~{     ~A = ~A~}~^~%~}~%"
	    (instans-name instans) op (rule-node-name-pretty (rule-instance-node rule-instance))
	    (node-token-bindings-for-reporting (rule-instance-node rule-instance) (rule-instance-token rule-instance)))
    (report-queue queue)
    (format stream "~%")))
 
(defun report-queue (queue &key stream)
  (let ((instans (rule-instance-queue-instans queue))
	(rule-instances (rule-instance-queue-head queue)))
    (when (null stream) (setf stream  (instans-default-output instans)))
    (format stream "~%     Queue has now ~D rules:" (length rule-instances))
    (loop for ri in rule-instances do
	 (format stream "~%     Rule ~A~%~{~{          ~A = ~A~}~^~%~}"
		 (rule-node-name-pretty (rule-instance-node ri))
		 (node-token-bindings-for-reporting (rule-instance-node ri) (rule-instance-token ri))))
  (format stream "~%")))

(defgeneric execute-rule-node (node token)
  (:method ((this select-node) token)
    (let ((instans (node-instans this)))
;      (inform "execute-rule-node ~A, ~A, processor = ~A" this token (instans-select-output-processor instans))
      (unless (null (instans-select-output-processor instans))
	(select-output (instans-select-output-processor instans) this token))))
  (:method ((this ask-node) token)
    (let ((instans (node-instans this)))
      ;; (inform "(instans-ask-output-processor instans) = ~A" (instans-ask-output-processor instans))
      (unless (null (instans-ask-output-processor instans))
	(ask-output (instans-ask-output-processor instans) this token))))
  (:method ((this describe-node) token)
    (let ((instans (node-instans this)))
      (unless (null (instans-select-output-processor instans))
	(select-output (instans-select-output-processor instans) this token))))
  (:method ((this modify-node) token)
    ;; (let* ((instans (node-instans this))
    ;; 	   (modify-function (instans-modify-function instans)))
    ;;   (unless (null modify-function)
    ;; 	(apply modify-function this token (instans-modify-function-arguments instans))))
    (when (modify-delete-func this)
      (apply (modify-delete-func this) (node-instans this) (loop for var in (modify-delete-parameters this) collect (token-value this token var))))
    (when (modify-insert-func this)
      (apply (modify-insert-func this) (node-instans this) (loop for var in (modify-insert-parameters this) collect (token-value this token var)))))
  (:method ((this construct-node) token)
    (declare (ignorable this token))
    (when (construct-func this)
      (apply (construct-func this) (node-instans this) (loop for var in (construct-parameters this) collect (token-value this token var))))))

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
	 select-output store-put-token store-get-token store-remove-token store-tokens index-put-token index-get-tokens index-remove-token
	 aggregate-get-value aggregate-add-value aggregate-remove-value start-node-token))
