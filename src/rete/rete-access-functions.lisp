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

(defmethod print-object ((this token-index) stream)
  (format stream "#<~A ~A>" (type-of this) (token-index-id this)))

(defmethod print-object ((this token-map) stream)
  (format stream "#<~A ~A>" (type-of this) (node-name (token-map-owner this))))

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

(defmethod initialize-instance :after ((this rule-node) &key comment &allow-other-keys)
  (when comment
    (setf (rule-node-annotations this) (parse-annotations comment))
    (setf (rule-node-rule-name this) (getf (rule-node-annotations this) :name))))
;; (defmethod initialize-instance :after ((node select-node) &rest keys &key &allow-other-keys)
;;  (inform "initialize-instance :after (~S) (~S)" node keys)
;; )

(defun parse-annotations (comment)
  (loop with annotations = nil
	with i = 0
	while (< i (length comment))
        do (let ((ch (char comment i)))
	     (cond ((char= ch #\@)
		    (incf i)
		    (flet ((scan-string (pred)
			     (loop while (and (< i (length comment)) (funcall pred (char comment i)))
				   collect (char comment i) into result-chars
				   do (incf i)
				   finally (return (coerce result-chars 'string)))))
		      (let ((id (scan-string #'alpha-char-p)))
			(cond ((string-equal "name" id)
			       (scan-string #'whitespace-char-p)
			       (let ((value (scan-string #'(lambda (ch) (or (char= ch #\_) (char= ch #\-) (alphanumericp ch))))))
				 (cond ((zerop (length value))
					(inform "Empty @name definition in ~A" comment))
				       (t
					(setf (getf annotations :name) value)))))))))
		   (t
		    (incf i))))
	finally (return annotations)))

(defmethod initialize-instance :after ((this instans) &key &allow-other-keys)
  (when (and (instans-use-quad-store-p this) (null (instans-quad-store this)))
    (setf (instans-quad-store this) (make-instance 'list-quad-store)))
  (setf (instans-rule-instance-queue this) (make-instance 'rule-instance-queue :instans this))
  (setf (instans-triple-pattern-matcher this) (make-instance 'triple-pattern-matcher :instans this)))

(defmethod initialize-instance :after ((this existence-start-node) &key &allow-other-keys)
  (setf (existence-start-node-token-map this) (make-instance 'token-map :owner this)))

(defmethod initialize-instance :after ((this filter-with-previous-value) &key &allow-other-keys)
  (setf (filter-with-previous-value-token-map this) (make-instance 'token-map :owner this)))

(defmethod initialize-instance :after ((this aggregate-join-node) &key group aggr-exprs &allow-other-keys)
  (setf (aggregate-join-group-var this) (third group))
  (setf (aggregate-join-key-vars this) (collect-expression-variables (setf (aggregate-join-key-exprs this) (second group))))
  (setf (aggregate-join-aggr-vars this) (collect-expression-variables (mapcar #'fifth aggr-exprs)))
  (setf (aggregate-join-token-group-map this) (make-instance 'token-map :owner this))
;  (describe this)
  )

;(defmethod initialize-instance :after ((this group) &key  &allow-other-keys)
;  (describe this))

;;; Var and blank creationg

(defgeneric make-named-blank-node (instans name)
  (:method ((this instans) name)
    (make-uniquely-named-object (instans-named-blank-node-factory this) (string-upcase name) :pretty-name name)))

(defgeneric generate-anonymous-blank-node (instans)
  (:method ((this instans))
    (generate-object-with-unique-name (instans-anonymous-blank-node-factory this) :name-prefix "_:")))

(defgeneric make-sparql-var (instans name)
  (:method ((this instans) name)
    (make-uniquely-named-object (instans-var-factory this) (string-upcase name) :pretty-name name)))

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

(defun source-nodes (nodes)
  (filter #'(lambda (x) (every #'null (node-parents x))) nodes))

(defun sink-nodes (nodes)
  (filter-not #'node-succ nodes))

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
    (list (join-beta this) (join-alpha this)))
  (:method ((this node))
    (let ((prev (node-prev this)))
      (and prev (list prev)))))

(defun node-def-preceq (node)
  (sparql-var-list-union (node-def-prec node) (node-def node)))

(defun compute-node-vars (nodes)
  (propagate-vars-down nodes))

(defun propagate-vars-down (nodes)
  ;; (inform "enter propagate-vars-down ~S" nodes)
  (let ((seen nil))
    (labels ((visit (node)
	       ;; (flet ((revresol (vars) (reverse-resolve-bindings (node-instans node) vars)))
	       (unless (member node seen)
		 ;; (inform "propagate-vars-down:visit ~S" node)
		 (push node seen)
		 (let ((precs (node-effective-precs node)))
		   (loop for prec in precs do (visit prec))
		   (setf (node-all-vars-in node) (reduce #'sparql-var-list-union (mapcar #'node-all-vars-out precs) :initial-value nil))
							     ;; (inform "(node-all-vars-in ~S) = ~S" node (revresol (node-all-vars-in node)))
		   (setf (node-visible-vars-in node) (reduce #'sparql-var-list-union (mapcar #'node-visible-vars-out precs) :initial-value nil))
							     ;; (inform "(node-visible-vars-in ~S) = ~S" node (revresol (node-visible-vars-in node)))
		   (setf (node-use node) (cond ((typep node 'join-node)
						(sparql-var-list-intersection (node-def-preceq (join-alpha node)) (node-def-preceq (join-beta node))))
					       ((typep node 'bind-node)
						(bind-form-parameters node))
					       ((typep node 'filter-with-previous-value)
						(filter-test-parameters node))
					       ((typep node 'filter-node)
						(filter-test-parameters node))
					       ((typep node 'existence-start-node)
						(list (existence-counter-var node))); (existence-active-p-var node)))
					       ((typep node 'existence-end-node)
						(let ((start-node (subgraph-start-node node)))
						  (list (existence-counter-var start-node)))); (existence-active-p-var start-node))))
					       ((typep node 'aggregate-join-node) ; This is not nice. Done in different order than some other
						(list-union (aggregate-join-key-vars node) (aggregate-join-aggr-vars node)))
					       ((typep node 'service-node)
						(service-node-query-vars node))
					       ((typep node 'query-node)
						(loop for ord in (solution-modifiers-order-by node)
						   nconc (cond ((member (car ord) '(ASC DESC))
								(collect-expression-variables (second ord)))
							       (t
								(collect-expression-variables ord)))))
					       ;; ((typep node 'query-node)
						;; (sparql-var-list-union (unless (eq (solution-modifiers-project node) '*)
						;; 			 (loop for item in (solution-modifiers-project-vars node)
						;; 			       when (and (consp item) (eq (first item) 'AS))
						;; 			       nconc (collect-expression-variables (third item))))
						;; 		       (loop for ord in (solution-modifiers-order-by node)
						;; 			     nconc (cond ((member (car ord) '(ASC DESC))
						;; 					  (collect-expression-variables (second ord)))
						;; 					 (t
						;; 					  (collect-expression-variables ord))))))
					       ((typep node 'modify-node)
						(sparql-var-list-union (modify-delete-parameters node) (modify-insert-parameters node)))
					       (t nil)))
							     ;; (inform "(node-use ~S) = ~S" node (revresol (node-use node)))
		   (setf (node-def-prec node) (reduce #'sparql-var-list-union (mapcar #'node-def-preceq precs) :initial-value nil))
							     ;; (inform "(node-def-prec ~S) = ~S" node (revresol (node-def-prec node)))
		   (setf (node-def node) (cond ((typep node 'alpha-node)
						(alpha-node-variables node))
					       ((typep node 'filter-with-previous-value) nil)
					       ((typep node 'bind-node)
						(list (bind-variable node)))
					       ((typep node 'existence-start-node)
						(list (existence-counter-var node))); (existence-active-p-var node)))
					       ;; ((typep node 'aggregate-join-node)
					       ;; 	(loop for aggr-var in (aggregate-join-aggr-var-list node)
					       ;; 	   collect (cdr aggr-var)))
					       ((typep node 'service-node)
						(sparql-var-list-difference (node-use node) (node-all-vars-in node)))
					       ((typep node 'query-node)
						(solution-modifiers-project-vars node))
					       (t nil)))
					;		     (inform "(node-def ~S) = ~S" node (revresol (node-def node)))
		   (when (exists-end-node-p node)
		     (setf (node-kill node) (sparql-var-list-difference (node-all-vars-in node) (node-all-vars-in (subgraph-start-node node)))))
		   (setf (node-all-vars-out node) (list-union (node-all-vars-in node) (node-def node) :test #'sparql-var-equal))
					;		     (inform "(node-all-vars-out ~S) = ~S" node (revresol (node-all-vars-out node)))
					;		     (inform "(node-kill ~S) = ~S" node (revresol (node-kill node)))
		   (setf (node-visible-vars-out node) (sparql-var-list-difference (sparql-var-list-union (node-visible-vars-in node) (node-def node)) (node-kill node)))
					;		     (inform "(node-visible-vars-out ~S) = ~S" node (revresol (node-visible-vars-out node)))
		   (when (service-node-p node)
		     (setf (service-node-index-key-vars node) (sparql-var-list-intersection (service-node-query-vars node) (node-all-vars-in node)))
		     (setf (service-node-query-minus-index-key-vars node) (sparql-var-list-difference (service-node-query-vars node) (node-all-vars-in node)))
;		     (describe node)
		     )
		   (when (join-node-p node)
		     (setf (join-beta-minus-alpha-vars node) (sparql-var-list-difference (node-all-vars-out (join-beta node)) (node-all-vars-out (join-alpha node))))
					;		       (inform "(join-beta-minus-alpha-vars ~S) = ~S" node (revresol (join-beta-minus-alpha-vars node)))
		     (setf (join-alpha-minus-beta-vars node) (sparql-var-list-difference (node-all-vars-out (join-alpha node)) (node-all-vars-out (join-beta node))))
					;		       (inform "(join-alpha-minus-beta-vars ~S) = ~S" node (revresol (join-alpha-minus-beta-vars node)))
		     )
					;		   (describe node)
		   ))))
	    (loop for node in (sink-nodes nodes) do (visit node))))
;  (inform "exit propagate-vars-down ~S" nodes)
  )

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
	  when (typep node 'token-store)
	  sum (if (token-store-hash-table node) (hash-table-count (token-store-hash-table node)) 0) into store-sizes
	  else when (typep node 'join-node)
	  sum (+ (if (join-alpha-index node) (hash-table-count (hash-token-index-table (join-alpha-index node))) 0)
		 (if (join-beta-index node) (hash-table-count (hash-token-index-table (join-beta-index node))) 0)) into index-sizes
	  finally (return (values store-sizes index-sizes)))))

(defun show-stores (instans)
  (loop for node in (instans-nodes instans)
	when (and (typep node 'token-store) (token-store-hash-table node))
	do (inform "~A:~{~%  ~A~}" node (maph #'(lambda (k v) (declare (ignorable k)) (token-to-pretty-string node v)) (token-store-hash-table node)))))

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
	(setf item (list string (intern-instans string)))
	(push-to-end item (instans-constant-iri-var-alist this)))
      (second item))))

(defgeneric get-constant-literal (instans literal)
  (:method ((this instans) literal)
    (let* ((string (rdf-literal-to-string literal))
	   (item (assoc string (instans-constant-literal-var-alist this) :test #'string=)))
      (when (null item)
	(setf item (append (list string (intern-instans string) (rdf-literal-string literal))
			   (if (rdf-literal-lang literal) (list :lang (rdf-literal-lang literal))
			       (if (rdf-literal-type literal) (list :type (get-constant-iri this (rdf-literal-type literal)))))))
	(push-to-end item (instans-constant-literal-var-alist this)))
      (second item))))

(defgeneric instans-policies (instans)
  (:method ((this instans))
    (list :rdf-input-unit (instans-rdf-input-unit this)
	  :rdf-operations (instans-rdf-operations this)
	  :allow-rule-instance-removal-p (instans-allow-rule-instance-removal-p this)
	  :queue-execution-policy (instans-queue-execution-policy this))))

(defgeneric aggregate-get-value (aggregate)
  (:method ((this aggregate-count)) (aggregate-count this))
  (:method ((this aggregate-sum)) (aggregate-sum this))
  (:method ((this aggregate-avg))
    (if (zerop (aggregate-count this))
	(sparql-unbound)
	(sparql-call "/" (aggregate-sum this) (aggregate-count this))))
  (:method ((this aggregate-min))
    (cond ((null (aggregate-history this))
	   (sparql-unbound))
	  (t
	   (first (aggregate-history this)))))
  (:method ((this aggregate-max))
    (cond ((null (aggregate-history this))
	   (sparql-unbound))
	  (t
	   (first (aggregate-history this)))))
  (:method ((this aggregate-sample))
    (cond ((null (aggregate-history this))
	   (sparql-unbound))
	  (t
	   (first (aggregate-history this)))))
  (:method ((this aggregate-group-concat))
;    (inform "here ~S" (aggregate-group-concat-separator this))
    (loop with separator = (coerce (aggregate-group-concat-separator this) 'list)
	  for firstp = t then nil
	  for elem in (aggregate-history this)
	  do (inform "~A" elem)
	  when firstp nconc (coerce (sparql-call "xsd:string" elem) 'list) into chars
	  else nconc (append separator (coerce (sparql-call "xsd:string" elem) 'list)) into chars
	  finally (return (coerce chars 'string)))))

(defgeneric aggregate-add-value (aggregate new-value)
  (:method ((this aggregate-count) new-value) (incf (aggregate-count this)))
  (:method ((this aggregate-sum) new-value)
    (when (typep new-value 'xsd-number-value) (incf (aggregate-sum this) new-value)))
  (:method ((this aggregate-avg) new-value)
    (incf (aggregate-count this))
    (when (typep new-value 'xsd-number-value) (incf (aggregate-sum this) new-value)))
  (:method ((this aggregate-min) new-value)
    (when (typep new-value 'xsd-number-value)
      (let ((history (aggregate-history this)))
	(setf (aggregate-history this)
	      (cond ((null history) (list new-value))
		    ((< new-value (first history)) (cons new-value history))
		    (t (cons (first history) (cons new-value (rest history)))))))))
  (:method ((this aggregate-max) new-value)
    (when (typep new-value 'xsd-number-value)
      (let ((history (aggregate-history this)))
	(setf (aggregate-history this)
	      (cond ((null history) (list new-value))
		    ((> new-value (first history)) (cons new-value history))
		    (t (cons (first history) (cons new-value (rest history)))))))))
  (:method ((this aggregate-with-history) new-value)
    (push new-value (aggregate-history this))))

(defgeneric aggregate-remove-value (aggregate value)
  (:method ((this aggregate-count) value) (decf (aggregate-count this)))
  (:method ((this aggregate-sum) value)
    (when (typep value 'xsd-number-value) (decf (aggregate-sum this) value)))
  (:method ((this aggregate-avg) value)
    (decf (aggregate-count this))
    (when (typep value 'xsd-number-value) (decf (aggregate-sum this) value)))
  (:method ((this aggregate-min) value)
    (when (typep value 'xsd-number-value)
      (let ((history (aggregate-history this)))
	(setf (aggregate-history this)
	      (cond ((null history)
		     (error* "Trying to remove ~A from an empty group" value))
		    ((sparql-call "=" value (first history))
		     (cond ((null (rest history)) nil)
			   (t
			    (loop with smallest = (first (rest history))
			       for rest on (rest history)
			       while (rest rest)
			       when (sparql-call "<" (first (rest rest)) smallest)
			       do (psetf smallest (first (rest rest)) (first (rest rest)) smallest)
			       finally (cons smallest (rest rest))))))
		    (t
		     (remove value history :test #'(lambda (a b) (sparql-call "=" a b)))))))))
  (:method ((this aggregate-max) value)
    (when (typep value 'xsd-number-value)
      (let ((history (aggregate-history this)))
	(setf (aggregate-history this)
	      (cond ((null history)
		     (error* "Trying to remove ~A from an empty group" value))
		    ((sparql-call "=" value (first history))
		     (cond ((null (rest history)) nil)
			   (t
			    (loop with smallest = (first (rest history))
			       for rest on (rest history)
			       while (rest rest)
			       when (sparql-call ">" (first (rest rest)) smallest)
			       do (psetf smallest (first (rest rest)) (first (rest rest)) smallest)
			       finally (cons smallest (rest rest))))))
		    (t
		     (remove value history :test #'(lambda (a b) (sparql-call "=" a b)))))))))
  (:method ((this aggregate-with-history) value)
    (setf (aggregate-history this)
	  (remove value (aggregate-history this) :test #'(lambda (a b) (sparql-call "=" a b))))))

(defgeneric instans-add-status (instans status-type &optional messages)
  (:method ((this instans) status-type &optional messages)
    (push (make-instance status-type :messages messages) (instans-status this))))

(defgeneric instans-find-status (instans status-type)
  (:method ((this instans) status-type)
    (find-if #'(lambda (x) (typep x status-type)) (instans-status this))))

;;; This is strange: using typep produces a strange internal error in SBCL
(defgeneric instans-has-status (instans status-type)
  (:method ((this instans) status-type)
    (some #'(lambda (x) (typep x status-type)) (instans-status this))))

(defgeneric instans-next-color (instans)
  (:method ((this instans))
    (when (null (instans-colors this))
      (setf (instans-colors this) (list "Black" "Red" "Blue" "Green" "Orange")))
    (pop (instans-colors this))))

(defgeneric instans-debug-subscribe (instans &rest topics)
  (:method ((this instans) &rest topics)
    (setf (instans-debug-topics this) (union (instans-debug-topics this) topics))))

(defgeneric instans-debug-unsubscribe (instans &rest topics)
  (:method ((this instans) &rest topics)
    (setf (instans-debug-topics this) (if (eq topics t) nil (set-difference (instans-debug-topics this) topics)))))

(defgeneric instans-debug-p (instans &rest topics)
  (:method ((this instans) &rest topics)
    (intersection (instans-debug-topics this) topics)))

(defgeneric instans-debug-message (instans topic-or-topics fmt &rest args)
  (:method ((this instans) topic-or-topics fmt &rest args)
    (when (apply #'instans-debug-p this (if (listp topic-or-topics) topic-or-topics (list topic-or-topics)))
      (apply #'inform fmt args))))

(defgeneric instans-show-rete-status (instans node token fmt &rest args)
  (:method ((this instans) (node node) token fmt &rest args)
    (let ((node-name (string (node-name node))))
      (inform "~A at ~A~%instans-op = ~{~A~^ ~}" (apply #'format nil fmt args) node-name
	      (mapcar #'(lambda (x) (sparql-value-to-string x :instans this)) (instans-current-op this)))
      (inform "   ~A" (token-pretty-string node token 3)))))
    
(defgeneric instans-store-prefix-binding (instans prefix expansion)
  (:method ((this instans) prefix expansion)
    (let ((item (assoc prefix (instans-prefixes this) :test #'string=))
	  (expansion-string (iri-to-string expansion)))
      (setf expansion-string (subseq expansion-string 1 (- (length expansion-string) 1)))
      (cond ((null item)
	     (push-to-end (cons prefix expansion-string) (instans-prefixes this)))
	    (t (setf (cdr item) expansion-string)))
      (setf (instans-prefixes-sorted this) (copy-list (instans-prefixes this)))
      (setf (instans-prefixes-sorted this) (sort (instans-prefixes-sorted this) #'(lambda (kv1 kv2) (> (length (cdr kv1)) (length (cdr kv2)))))))))

;;;

(defvar *default-main-dir* nil)

(defun instans-encode-prefixes (instans encode-prefixes-p)
  (setf (instans-encode-prefixes-p instans) encode-prefixes-p)
  (when encode-prefixes-p
    (unless (instans-prefixes instans)
      (setf (instans-prefixes instans) (create-initial-prefix-alist)))))

(defgeneric set-instans-rdf-operations (instans ops)
  (:method ((this instans) ops)
    (when (symbolp ops)
      (setf ops (list ops)))
    (setf ops (loop for op in ops
		    unless (member op (instans-allowed-rdf-operations this))
		    do (error* "Illegal rdf operations ~A" op)
		    else
		    nconc (if (eq op :event) (list :add :execute :remove :execute) (list op))))
    (setf (instans-rdf-operations this) ops)))

(defun pathname-type-as-keyword (pathname)
  (let ((type (pathname-type (parse-namestring pathname))))
    (and type (intern-keyword (string-upcase type)))))

(defgeneric instans-configure (instans configuration)
  (:method ((this instans) configuration)
    (let ((base nil)
	  (directory (parse-iri (format nil "file://~A" (expand-dirname (or *default-main-dir* "."))))))
      (loop for (key value) in configuration
	    do (case key
		 (:directory (setf directory (parse-iri (if (http-or-file-iri-string-p value) value (format nil "file://~A" (expand-dirname value))))))
		 (:base (setf base (parse-iri value)))
		 (:rules
		  (instans-add-rules this value :base base)
		  (unless (instans-find-status this 'instans-rule-translation-succeeded)
		    (let ((status (first (instans-status this))))
		      (error* "Adding rules to ~A failed: ~A~{~%~A~}~%" this (type-of status) (instans-status-messages status)))))
		 (:rdf-input-unit
		  (let ((unit (intern-keyword (string-upcase value))))
		    (unless (member unit (instans-allowed-rdf-input-units this))
		      (error* "Illegal input unit ~A" value))
		    (setf (instans-rdf-input-unit this) unit)))
		 (:input (instans-add-stream-input-processor this (expand-iri directory value)
							     :base base
							     :input-type (pathname-type-as-keyword value)))
		 (:agent-input (instans-add-agent-input-processor this :name value))
		 (:input-trig
		  (instans-add-stream-input-processor this (expand-iri directory value) :base base :input-type :trig))
		 (:input-turtle
		  (instans-add-stream-input-processor this (expand-iri directory value) :base base :input-type :ttl))
		 (:input-nq
		  (instans-add-stream-input-processor this (expand-iri directory value) :base base :input-type :nq))
		 (:input-nt
		  (instans-add-stream-input-processor this (expand-iri directory value) :base base :input-type :nt))
		 (:input-triples
		  (setf (instans-rdf-input-unit this) :single)
		  (instans-add-stream-input-processor this (expand-iri directory value)
						      :base base :input-type (pathname-type-as-keyword value)))
		 (:input-blocks
		  (setf (instans-rdf-input-unit this) :block)
		  (instans-add-stream-input-processor this (expand-iri directory value)
						      :base base :input-type (pathname-type-as-keyword value)))
		 (:input-document
		  (setf (instans-rdf-input-unit this) :document)
		  (instans-add-stream-input-processor this (expand-iri directory value)
						      :base base :input-type (pathname-type-as-keyword value)))
		 (:input-events
		  (set-instans-rdf-operations this :event)
		  (setf (instans-rdf-input-unit this) :block)
		  (instans-add-stream-input-processor this (expand-iri directory value)
						      ::base base
						      :input-type (pathname-type-as-keyword value)))

		 (:rdf-operations
		  (set-instans-rdf-operations this value))
		 (:allow-rule-instance-removal
		  (setf (instans-allow-rule-instance-removal-p this) value))
		 (:prefix-encoding
		  (instans-encode-prefixes this value))
		 (:select-output
		  (setf (instans-select-output-processor this) (create-select-output-processor this value (pathname-type-as-keyword value))))
		 (:select-output-csv
		  (setf (instans-select-output-processor this) (create-select-output-processor this value :csv)))
		 (:construct-agent-output
		  (setf (instans-construct-output-processor this)
			(create-construct-agent-output-processor this (getf value :name) (getf value :type) (getf value :destinations))))
		 (:construct-output
		  (setf (instans-construct-output-processor this) (create-construct-output-processor this value (pathname-type-as-keyword value))))
		 (:construct-output-trig
		  (setf (instans-construct-output-processor this) (create-construct-output-processor this value :trig)))
		 (:construct-output-ttl
		  (setf (instans-construct-output-processor this) (create-construct-output-processor this value :ttl)))
		 (:construct-output-nq
		  (setf (instans-construct-output-processor this) (create-construct-output-processor this value :nq)))
		 (:construct-output-nt
		  (setf (instans-construct-output-processor this) (create-construct-output-processor this value :nt)))
		 (:reporting
		  (let ((reporting (loop for kind in value
					 when (eq kind :all)
					 append '(:select :construct :modify :all :rete-add :rete-remove :queue :rdf-operations :execute)
					 else when (eql 0 (search "SUMMARY" (string kind)))
					 append (prog1 (list :storage) (setf (instans-summary-report-interval this) (parse-integer (string kind) :start 7)))
					 else when (eql 0 (search "SIZES" (string kind)))
					 append (prog1 (list :sizes) (setf (instans-summary-report-interval this) (parse-integer (string kind) :start 5)))
					 else append (list kind))))
		    (loop for kind in reporting
			  unless (member kind '(:select :construct :modify :rete-add :rete-remove :queue :call-succ-nodes :all :sizes :summary :rdf-operations :execute))
			  do (error* "Illegal recording tag ~A" kind))
		    (setf (instans-report-operation-kinds this) reporting))))))
    this))

(defgeneric instans-rule-types (instans)
  (:method ((this instans))
    (loop with rule-types = nil
	  for node in (instans-nodes this)
	  when (and (typep node 'rule-node) (null (node-succ node)))
	  do (pushnew (type-of node) rule-types)
	  finally (return rule-types))))
