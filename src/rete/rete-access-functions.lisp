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

(defmethod initialize-instance :after ((this csv-output-processor) &key output-name &allow-other-keys)
  (let ((stream (cond ((null output-name) *standard-output*)
		      (t
		       (open output-name :direction :output :if-exists :supersede)))))
    (setf (query-output-processor-output-stream this) stream)
    (setf (csv-output-processor-csv-output this) (make-instance 'csv-output :stream stream))))

(defmethod initialize-instance :after ((this construct-output-processor) &key output-name &allow-other-keys)
  (let ((stream (cond ((null output-name) *standard-output*)
		      (t
		       (open output-name :direction :output :if-exists :supersede)))))
    (setf (query-output-processor-output-stream this) stream)))

(defmethod initialize-instance :after ((this instans) &key &allow-other-keys)
  (when (and (instans-use-quad-store-p this) (null (instans-quad-store this)))
    (setf (instans-quad-store this) (make-instance 'list-quad-store)))
  (setf (instans-rule-instance-queue this) (make-instance 'rule-instance-queue :instans this))
  (setf (instans-triple-pattern-matcher this) (make-instance 'triple-pattern-matcher :instans this)))

(defmethod initialize-instance :after ((this aggregate-join-node) &key group aggr-exprs &allow-other-keys)
  (setf (aggregate-join-group-var this) (third group))
  (setf (aggregate-join-key-vars this) (collect-expression-variables (setf (aggregate-join-key-exprs this) (second group))))
  (setf (aggregate-join-aggr-vars this) (collect-expression-variables (mapcar #'fifth aggr-exprs)))
;  (describe this)
  )

;(defmethod initialize-instance :after ((this group) &key  &allow-other-keys)
;  (describe this))

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
;  (inform "enter propagate-vars-down ~S" nodes)
  (let ((seen nil))
    (labels ((visit (node)
;	       (flet ((revresol (vars) (reverse-resolve-bindings (node-instans node) vars)))
	       (unless (member node seen)
					;		 (inform "propagate-vars-down:visit ~S" node)
		 (push node seen)
		 (let ((precs (node-effective-precs node)))
		   (loop for prec in precs do (visit prec))
		   (setf (node-all-vars-in node) (reduce #'sparql-var-list-union (mapcar #'node-all-vars-out precs) :initial-value nil))
					;		     (inform "(node-all-vars-in ~S) = ~S" node (revresol (node-all-vars-in node)))
		   (setf (node-visible-vars-in node) (reduce #'sparql-var-list-union (mapcar #'node-visible-vars-out precs) :initial-value nil))
					;		     (inform "(node-visible-vars-in ~S) = ~S" node (revresol (node-visible-vars-in node)))
		   (setf (node-use node) (cond ((typep node 'join-node)
						(sparql-var-list-intersection (node-def-preceq (join-alpha node)) (node-def-preceq (join-beta node))))
					       ((typep node 'bind-node)
						(bind-form-parameters node))
					       ((typep node 'filter-memory)
						(filter-test-parameters node))
					       ((typep node 'filter-node)
						(filter-test-parameters node))
					       ((typep node 'existence-start-node)
						(list (existence-counter-var node) (existence-active-p-var node)))
					       ((typep node 'existence-end-node)
						(let ((start-node (subgraph-start-node node)))
						  (list (existence-counter-var start-node) (existence-active-p-var start-node))))
					       ((typep node 'aggregate-join-node) ; This is not nice. Done in different order than some other
						(list-union (aggregate-join-key-vars node) (aggregate-join-aggr-vars node)))
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
					;		     (inform "(node-use ~S) = ~S" node (revresol (node-use node)))
		   (setf (node-def-prec node) (reduce #'sparql-var-list-union (mapcar #'node-def-preceq precs) :initial-value nil))
					;		     (inform "(node-def-prec ~S) = ~S" node (revresol (node-def-prec node)))
		   (setf (node-def node) (cond ((typep node 'alpha-node)
						(alpha-node-variables node))
					       ((typep node 'filter-memory)
						(list (filter-memory-prev-value-var node)))
					       ((typep node 'bind-node)
						(list (bind-variable node)))
					       ((typep node 'existence-start-node)
						(list (existence-active-p-var node) (existence-counter-var node)))
					       ;; ((typep node 'aggregate-join-node)
					       ;; 	(loop for aggr-var in (aggregate-join-aggr-var-list node)
					       ;; 	   collect (cdr aggr-var)))
					       ((typep node 'query-node)
						(solution-modifiers-project-vars node))
					       (t nil)))
					;		     (inform "(node-def ~S) = ~S" node (revresol (node-def node)))
		   (when (existence-end-node-p node)
		     (setf (node-kill node) (sparql-var-list-difference (node-all-vars-in node) (node-all-vars-in (subgraph-start-node node)))))
		   (setf (node-all-vars-out node) (list-union (node-all-vars-in node) (node-def node) :test #'sparql-var-equal))
					;		     (inform "(node-all-vars-out ~S) = ~S" node (revresol (node-all-vars-out node)))
					;		     (inform "(node-kill ~S) = ~S" node (revresol (node-kill node)))
		   (setf (node-visible-vars-out node) (sparql-var-list-difference (sparql-var-list-union (node-visible-vars-in node) (node-def node)) (node-kill node)))
					;		     (inform "(node-visible-vars-out ~S) = ~S" node (revresol (node-visible-vars-out node)))
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
	(setf item (list string (intern string :instans)))
	(push-to-end item (instans-constant-iri-var-alist this)))
      (second item))))

(defgeneric get-constant-literal (instans literal)
  (:method ((this instans) literal)
    (let* ((string (rdf-literal-to-string literal))
	   (item (assoc string (instans-constant-literal-var-alist this) :test #'string=)))
      (when (null item)
	(setf item (append (list string (intern string :instans) (rdf-literal-string literal))
			   (if (rdf-literal-lang literal) (list :lang (rdf-literal-lang literal))
			       (if (rdf-literal-type literal) (list :type (get-constant-iri this (rdf-literal-type literal)))))))
	(push-to-end item (instans-constant-literal-var-alist this)))
      (second item))))

(defgeneric instans-policies (instans)
  (:method ((this instans))
    (list :query-input-policy (instans-query-input-policy this)
	  :query-processing-operations (instans-query-processing-operations this)
	  :rule-instance-removal-policy (instans-rule-instance-removal-policy this)
	  :queue-execution-policy (instans-queue-execution-policy this))))

(defgeneric write-csv-headers (csv-output headers)
  (:method ((this csv-output) headers)
    (when (slot-boundp this 'headers)
      (error* "Trying to write headers twice in ~S" this))
    (setf (csv-output-headers this) headers)
    (write-csv-record this headers)))

(defgeneric write-csv-record (csv-output record)
  (:method ((this csv-output) record)
    (when (and (csv-output-require-headers-p this) (not (slot-boundp this 'headers)))
      (error* "Trying to write record without writing headers first"))
    (let* ((cr (code-char #x0D))
	   (lf (code-char #x0A))
	   (dquote (code-char #x22))
	   (comma (code-char #x2C))
	   (escape-chars (list cr lf dquote comma))
	   (stream (csv-output-stream this)))
      (loop for field in record
	    for separator = nil then comma
	    do (progn
		 (when separator (princ separator stream))
		 (cond ((and (stringp field) (some #'(lambda (x) (char-in-set-p* x escape-chars)) field))
			(princ dquote stream)
			(loop for i from 0 below (length field)
			      for char = (char field i)
			      when (char= char dquote) do (princ dquote stream)
			      do (princ char stream))
			(princ dquote stream))
		       (t
			(princ field stream)))))
      (princ (csv-output-separator this) stream))))

(defgeneric add-query-input-processor (instans processor)
  (:method ((this instans) processor)
    (push-to-end processor (instans-query-input-processors this))))

(defun create-select-output-processor (output-name output-type)
  (case output-type
    (:csv (make-instance 'csv-output-processor :output-name output-name))
    (:solution-set (make-instance 'solution-set-output-processor :output-name output-name))
    (t (error* "Unknown select output processor type ~S" output-type))))

(defun create-construct-output-processor (output-name output-type)
  (case output-type
    ((:ttl :turtle) (make-instance 'turtle-output-processor :output-name output-name))
    (:trig (make-instance 'trig-output-processor :output-name output-name))
    (:nt (make-instance 'nt-output-processor :output-name output-name))
    (:nq (make-instance 'nq-output-processor :output-name output-name))
    (:solution-set (make-instance 'solution-set-output-processor :output-name output-name))
    (t (error* "Unknown select output processor type ~S" output-type))))

(defun solution-bindings (node token)
  (let* ((vars (solution-modifiers-project-vars node))
	 (values (mapcar #'(lambda (var)
			     (let ((value (token-value node token var)))
			       (cond ((rdf-term-p value) (rdf-term-as-string value))
				     ((typep value 'xsd-boolean-value) (if value "true" "false"))
				     ((typep value 'xsd-datetime-value) (datetime-canonic-string value))
				     (t value))))
			 vars)))
    (values vars values)))

(defgeneric write-select-output (query-output-processor node token)
  (:method ((this csv-output-processor) node token)
    (multiple-value-bind (vars values) (solution-bindings node token)
      (let ((query-output-stream (csv-output-processor-csv-output this)))
	(unless (slot-boundp query-output-stream 'headers)
	  (write-csv-headers query-output-stream (mapcar #'(lambda (var) (format nil "~(~A~)" (subseq (uniquely-named-object-name (reverse-resolve-binding (node-instans node) var)) 1))) vars)))
	(write-csv-record query-output-stream values))))
  (:method ((this solution-set-output-processor) node token)
    (multiple-value-bind (vars values) (solution-bindings node token)
      (unless (slot-boundp this 'variables)
	(setf (solution-set-output-processor-variables this)
	      (mapcar #'(lambda (var) (format nil "~(~A~)" (subseq (uniquely-named-object-name (reverse-resolve-binding (node-instans node) var)) 1))) vars))
	(setf (solution-set-output-processor-end this) (setf (solution-set-output-processor-bindings this) (list nil))))
      (setf (solution-set-output-processor-end this)
	    (setf (cdr (solution-set-output-processor-end this)) (list values))))))

(defgeneric write-construct-output (query-output-processor instans s p o &optional g)
  (:method ((this nq-output-processor) instans s p o &optional g)
    (let ((stream (query-output-processor-output-stream  this)))
      (if g
	  (format stream "~S ~S ~S ~S .~%" s p o g)
	  (format stream "~S ~S ~S .~%" s p o)))))

(defgeneric close-query-output-processor (query-output-processor)
  (:method ((this stream-query-output-processor-mixin))
;    (inform "close-query-output-processor: ~S" this)
    (unless (member (query-output-processor-output-stream this) (list *standard-output* *error-output*))
      (close (query-output-processor-output-stream this))))
  (:method ((this solution-set-output-processor))
    (if (slot-boundp this 'bindings)
	(pop (solution-set-output-processor-bindings this))
	nil)))

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

