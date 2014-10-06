;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Input processors are objects that read RDF input from streams and agent mailboxes, and maybe lates from other sources too.

(define-class instans-input-processor ()
  ((instans :accessor instans-input-processor-instans :initarg :instans)
   (name :accessor instans-input-processor-name :initarg :name)
   (status :accessor instans-input-processor-status :initform :runnable)
   (operations :accessor instans-input-processor-operations :initarg :operations)
   (base :accessor instans-input-processor-base :initarg :base)
   (graph :accessor instans-input-processor-graph :initarg :graph)
   (blank-node-mapping :accessor instans-input-processor-blank-node-mapping :initform (make-hash-table :test #'equal))
   (subscribe :accessor instans-input-processor-subscribe :initarg :subscribe :initform nil)))

(define-class instans-stream-input-processor (instans-input-processor)
  ((input-unit :accessor instans-stream-input-processor-input-unit :initarg :input-unit)
   (parser :accessor instans-stream-input-processor-parser :initarg :parser)))

;;; These are for different RDF encodings
(define-class instans-nt-input-processor (instans-stream-input-processor) ())
(define-class instans-nq-input-processor (instans-stream-input-processor) ())
(define-class instans-turtle-input-processor (instans-stream-input-processor) ())
(define-class instans-trig-input-processor (instans-stream-input-processor) ())

(define-class instans-agent-input-processor (instans-input-processor)
  ((source :accessor instans-agent-input-processor-source :initarg :source)))

;;; Writers do the actual writing of output to streams and agent mailboxes.

(define-class instans-writer ()
  ((appendp :accessor instans-writer-append-p :initarg :appendp)
   (name :accessor instans-writer-name :initarg :name)))

;; Select writers

(define-class instans-select-writer (instans-writer) ())

(define-class instans-csv-writer (instans-select-writer)
  ((csv-output :accessor instans-csv-writer-csv-output :initarg :csv-output)))

(define-class instans-sparql-query-results-writer (instans-select-writer)
  ((results :accessor instans-sparql-query-results-writer-results :initarg :results)))

(define-class instans-srx-writer (instans-sparql-query-results-writer)
  ((stream :accessor instans-srx-writer-stream :initarg :stream)))

;; Construct writers

(define-class instans-construct-writer (instans-writer) ())

(define-class instans-stream-writer (instans-construct-writer)
  ((stream :accessor instans-stream-writer-stream :initarg :stream :initform nil)))

(define-class instans-agent-writer (instans-construct-writer)
  ((destinations :accessor instans-agent-writer-destinations :initarg :destinations :initform nil)))

;;; Output processors collect data that is later written by writers.

(define-class instans-output-processor ()
  ((instans :accessor instans-output-processor-instans :initarg :instans)
   (output-name :accessor instans-output-processor-output-name :initarg :output-name :initform nil)
   (writer :accessor instans-output-processor-writer :initarg :writer)))

;; Select output processors collect SELECT solutions

(define-class instans-select-output-processor (instans-output-processor)
  ((variables :accessor instans-select-output-processor-variables :initform nil)
   (variables-written-p :accessor instans-select-output-processor-variables-written-p :initform nil)
   (solutions :accessor instans-select-output-processor-solutions :initform nil)
   (solutions-tail :accessor instans-select-output-processor-solutions-tail :initform nil)))

;; Construct output processors collect CONSTRUCT statements

(define-class instans-construct-output-processor (instans-output-processor) ())

(define-class instans-n-statement-output-processor (instans-construct-output-processor)
  ((statements :accessor instans-n-statement-output-processor-statements :initform nil)
   (tail :accessor instans-n-statement-output-processor-tail :initform nil)))

(define-class instans-nt-output-processor (instans-n-statement-output-processor) ())
(define-class instans-nq-output-processor (instans-n-statement-output-processor) ())

(define-class instans-trig-output-processor (instans-construct-output-processor)
  ((current-graph :accessor instans-trig-output-processor-current-graph :initform nil)
   (current-subject :accessor instans-trig-output-processor-current-subject :initform nil)
   (prefixes-written-p :accessor instans-trig-output-processor-prefixes-written-p :initarg :prefixes-written-p :initform nil)
   (graph-subject-predicate-object-trie :accessor instans-trig-output-processor-graph-subject-predicate-object-trie :initform (make-instance 'trie))))

(define-class instans-turtle-output-processor (instans-trig-output-processor) ())

;;; Print-object

(defmethod print-object ((this instans-input-processor) stream)
  (format stream "#<~A ~S>" (type-of this) (instans-input-processor-name this)))

(defmethod print-object ((this instans-writer) stream)
  (format stream "#<~A ~S>" (type-of this) (instans-writer-name this)))

(defmethod print-object ((this instans-output-processor) stream)
  (format stream "#<~A ~S>" (type-of this) (instans-output-processor-output-name this)))

;;; Input processing is mostly defined in instan/src/parser/*.lisp files.

(defgeneric instans-input-processor-runnable-p (input-processor)
  (:method ((this instans-input-processor))
    (eq (instans-input-processor-status this) :runnable)))

(defgeneric instans-runnable-p (instans)
  (:method ((this instans))
    (some #'instans-input-processor-runnable-p (instans-input-processors this))))

;;; Creating output processors

(defun make-instans-output-processor-output-stream (output-name appendp)
  (cond ((or (null output-name) (string= "-" output-name))
	 *standard-output*)
	(appendp
	 (open output-name :direction :output :if-exists :append))
	(t
	 (open output-name :direction :output :if-exists :supersede))))

(defun create-select-output-processor (instans output-name output-type &key appendp)
;  (inform "create-select-output-processor ~A ~A" output-name appendp)
  (setf appendp (not (null (and appendp output-name (not (string= "-" output-name)) (probe-file output-name)))))
;  (inform "appendp now ~A" appendp)
  (let ((writer (case output-type
		  (:csv
		   (make-instance 'instans-csv-writer
				  :name output-name
				  :appendp appendp
				  :csv-output (make-instance
					       'csv-output
					       :require-headers-p (not appendp)
					       :stream (make-instans-output-processor-output-stream output-name appendp))))
		  (:sparql-results
		   (make-instance 'instans-sparql-query-results-writer
				  :name output-name
				  :results (make-instance 'sparql-query-results)))
		  (:srx
		   (make-instance 'instans-srx-writer
				  :name output-name
				  :results (make-instance 'sparql-query-results)
				  :appendp appendp
				  :stream (make-instans-output-processor-output-stream output-name appendp)))
		  (t (error* "Unknown select output writer type ~S" output-type)))))
    (make-instance 'instans-select-output-processor
		   :instans instans
		   :output-name output-name
		   :writer writer)))

;;; !!! Clean this !!!

(defun create-construct-output-processor (instans output-name output-type &key appendp)
  (create-construct-stream-output-processor instans output-name output-type :appendp appendp))

(defun create-construct-stream-output-processor (instans output-name output-type &key appendp)
  (setf appendp (and appendp (or (null output-name) (string= "-" output-name) (probe-file output-name))))
  (let* ((stream (make-instans-output-processor-output-stream output-name appendp))
	 (writer (make-instance 'instans-stream-writer :name output-name :stream stream)))
    (case output-type
      ((:ttl :turtle) (make-instance 'instans-turtle-output-processor :instans instans :output-name output-name :writer writer))
      (:trig (make-instance 'instans-trig-output-processor :instans instans :output-name output-name :writer writer))
      (:nt (make-instance 'instans-nt-output-processor :instans instans :output-name output-name :writer writer))
      (:nq (make-instance 'instans-nq-output-processor :instans instans :output-name output-name :writer writer))
      (t (error* "Unknown construct output processor type ~S" output-type)))))

(defun create-construct-agent-output-processor (instans output-name output-type destinations)
  (let ((writer (make-instance 'instans-agent-writer :name output-name :destinations destinations)))
    (case output-type
      ((:ttl :turtle) (make-instance 'instans-turtle-output-processor :instans instans :output-name output-name :writer writer))
      (:trig (make-instance 'instans-trig-output-processor :instans instans :output-name output-name :writer writer))
      (:nt (make-instance 'instans-nt-output-processor :instans instans :output-name output-name :writer writer))
      (:nq (make-instance 'instans-nq-output-processor :instans instans :output-name output-name :writer writer))
      (t (error* "Unknown construct output processor type ~S" output-type)))))

;;; Flushing output processors

(defgeneric flush-output-processor (instans-output-processor)
  (:method ((this instans-n-statement-output-processor))
    (write-statements (instans-output-processor-writer this) (instans-n-statement-output-processor-statements this))
    (setf (instans-n-statement-output-processor-statements this) nil)
    (setf (instans-n-statement-output-processor-tail this) nil))
  (:method ((this instans-trig-output-processor))
    (let ((instans (instans-output-processor-instans this))
	  (writer (instans-output-processor-writer this)))
      (maybe-write-prefixes instans this)
      (write-graph-subject-predicate-object-trie writer instans (instans-trig-output-processor-graph-subject-predicate-object-trie this))
      (setf (instans-trig-output-processor-graph-subject-predicate-object-trie this) (make-instance 'trie))))
  (:method ((this instans-select-output-processor))
    (let ((writer (instans-output-processor-writer this)))
      (unless (instans-select-output-processor-variables-written-p this)
	(write-variables writer (instans-select-output-processor-variables this))
	(setf (instans-select-output-processor-variables-written-p this) t))
      (loop for solution in (instans-select-output-processor-solutions this)
	    do (write-solution writer solution))
      (setf (instans-select-output-processor-solutions this) nil))))

;;; Closing output processors

(defgeneric close-output-processor (instans-output-processor)
  (:method ((this instans-output-processor))
    (let ((writer (instans-output-processor-writer this)))
      (flush-output-processor this)
      (when (typep writer 'instans-agent-writer)
	(loop for agent in (instans-agent-writer-destinations writer)
	      do (agent-send agent :eof)))
      (close-writer writer))))

(defgeneric close-writer (instans-writer)
  (:method ((this instans-stream-writer))
    (close-stream-not-stdout-stderr (instans-stream-writer-stream this)))
  (:method ((this instans-csv-writer))
    (close-stream-not-stdout-stderr (csv-output-stream (instans-csv-writer-csv-output this))))
  (:method ((this instans-srx-writer))
    (when (open-stream-p (instans-srx-writer-stream this))
      (output-srx (instans-sparql-query-results-writer-results this) (instans-srx-writer-stream this))
      (close-stream-not-stdout-stderr (instans-srx-writer-stream this))))
  (:method ((this instans-writer))
    (declare (ignore this))
    nil))

;;; Doing SELECT output

(defgeneric select-output (instans-select-output-processor node token)
  (:method ((this instans-select-output-processor) node token)
    (multiple-value-bind (vars values) (solution-bindings node token)
      (when (null (instans-select-output-processor-variables this))
	(setf (instans-select-output-processor-variables this)
	      (mapcar #'(lambda (var) (reverse-resolve-binding (node-instans node) var)) vars)))
      (let ((newtail (list values)))
	(cond ((null (instans-select-output-processor-solutions this))
	       (setf (instans-select-output-processor-solutions this) newtail)
	       (setf (instans-select-output-processor-solutions-tail this) newtail))
	      (t
	       (setf (cdr (instans-select-output-processor-solutions-tail this)) newtail)
	       (setf (instans-select-output-processor-solutions-tail this) newtail)))))))

(defgeneric write-variables (instans-writer variables)
  (:method ((this instans-csv-writer) variables)
    (unless (instans-writer-append-p this)
      (write-csv-headers (instans-csv-writer-csv-output this)
			 (mapcar #'(lambda (var)
				     (format nil "~(~A~)" (subseq (uniquely-named-object-name var) 1)))
				 variables))))
  (:method ((this instans-sparql-query-results-writer) variables)
    (set-query-variables (instans-sparql-query-results-writer-results this) variables)))

(defgeneric write-solution (instans-writer values)
  (:method ((this instans-csv-writer) values)
    (write-csv-record (instans-csv-writer-csv-output this)
		      (mapcar #'(lambda (value)
				  (cond ((rdf-term-p value) (rdf-term-as-string value))
					((typep value 'xsd-boolean-value) (if value "true" "false"))
					((typep value 'xsd-datetime-value) (datetime-canonic-string value))
					(t value)))
			      values)))
  (:method ((this instans-sparql-query-results-writer) values)
    (add-sparql-result-values (instans-sparql-query-results-writer-results this) values)))

(defun solution-bindings (node token)
  (let* ((vars (solution-modifiers-project-vars node))
	 (values (mapcar #'(lambda (var) (token-value node token var)) vars)))
    (values vars values)))

(defun solution-bindings-pretty (node token)
  (let* ((vars (solution-modifiers-project-vars node))
	 (values (mapcar #'(lambda (var)
			     (let ((value (token-value node token var)))
			       (cond ((rdf-term-p value) (rdf-term-as-string value))
				     ((typep value 'xsd-boolean-value) (if value "true" "false"))
				     ((typep value 'xsd-datetime-value) (datetime-canonic-string value))
				     (t value))))
			 vars)))
    (values vars values)))

;;; Doing CONSTRUCT output

(defgeneric construct-output (instans-construct-output-processor s p o &optional g)
  (:method ((this instans-n-statement-output-processor) s p o &optional g)
    (when (typep this 'instans-nt-output-processor)
      (assert* (null g) "Non-null graph in N-Triples output: ~A" g))
    (cond ((null (instans-n-statement-output-processor-statements this))
	   (setf (instans-n-statement-output-processor-statements this) (list (list s p o g)))
	   (setf (instans-n-statement-output-processor-tail this) (instans-n-statement-output-processor-statements this)))
	  (t
	   (setf (cdr (instans-n-statement-output-processor-tail this)) (list (list s p o g)))
	   (setf (instans-n-statement-output-processor-tail this) (cdr (instans-n-statement-output-processor-tail this))))))
  (:method ((this instans-trig-output-processor) s p o &optional g)
    (when (typep this 'instans-turtle-output-processor)
      (assert* (null g) "Non-null graph in Turtle output: ~A" g))
    (trie-add-path (instans-trig-output-processor-graph-subject-predicate-object-trie this) (list g s p o) #'sparql-value-equal)
;    (inform "~&~A~%" (trie-paths (instans-trig-output-processor-graph-subject-predicate-object-trie this)))
))

(defgeneric write-statements (instans-writer statements)
  (:method ((this instans-stream-writer) statements)
    (loop for (s p o g) in statements
	 do (format (instans-stream-writer-stream this) "~&~A ~A ~A~@[ ~A~]~%" s p o g)))
  (:method ((this instans-agent-writer) statements)
    (loop for agent in (instans-agent-writer-destinations this)
	  do (agent-send agent statements))))

(defgeneric write-graph-subject-predicate-object-trie (instans-writer instans trie &optional wrap-default-p)
  (:method ((this instans-stream-writer) (instans instans) trie &optional (wrap-default-p t))
;    (trie-print trie *error-output*)
    (let ((stream (instans-stream-writer-stream this))
	  (indent 0)
	  (init-sep ""))
;      (inform "stream = ~S~%" stream)
      (loop for (g . s-trie) in (trie-level trie)
;	    do (inform "g = ~S, s-trie = ~S" g s-trie)
	    do (let (g-string)
		 (cond (g
			(setf g-string (sparql-value-to-string g :instans instans))
			(format stream "~A {" g-string)
			(incf indent (+ (length g-string) 3))
			(setf init-sep " "))
		       (wrap-default-p
			(format stream "{")
			(incf indent 2)
			(setf init-sep " ")))
		 (loop for (s . p-trie) in (trie-level s-trie)
		       for s-sep = init-sep then (format nil "~%~V@T" indent)
		       for s-string = (sparql-value-to-string s :instans instans)
;		       do (inform "s = ~S, p-trie = ~S" s p-trie)
		       do (format stream "~A~A" s-sep s-string)
		       do (incf indent (length s-string))
		       do (loop for (p . o-trie) in (trie-level p-trie)
				for p-string = (sparql-value-to-string p :instans instans)
				for p-sep = "" then (format nil ";~%~V@T" indent)
;				do (inform "p = ~S, o-trie = ~S" p o-trie)
				do (format stream "~A ~A" p-sep p-string)
				do (incf indent (length p-string))
				do (loop for (o) in (trie-level o-trie)
					 for o-sep = "" then (format nil ",~%~V@T" indent)
;					 do (inform "o = ~S" o)
					 do (format stream "~A ~A" o-sep (sparql-value-to-string o :instans instans)))
				do (incf indent (- (length p-string))))
			 do (incf indent (- (length s-string)))
			 do (format stream " .~%"))
		 (cond (g
			(format stream "}~%")
			(decf indent (+ (length g-string) 3)))
		       (wrap-default-p
			(format stream "}~%")
			(decf indent 2)))
;		 (finish-output stream)
		 ))))
  (:method ((this instans-agent-writer) (instans instans) trie &optional wrap-default-p)
    (declare (ignorable wrap-default-p))
    (let* ((msg (list nil))
	   (tail msg))
      (trie-map trie #'(lambda (path)
			 (setf (cdr tail) (list (list (second path) (third path) (fourth path) (first path))))
			 (setf tail (cdr tail))))
      ;; (trie-print trie)
      ;; (inform "trie paths = ~S" (trie-paths trie))
      (pop msg)
      ;; (inform "About to send ~S~%" msg)
      (loop for agent in (instans-agent-writer-destinations this)
	    do (agent-send agent msg)))))

(defgeneric maybe-write-prefixes (instans instans-output-processor)
  (:method ((instans instans) (processor instans-trig-output-processor))
    (let ((writer (instans-output-processor-writer processor)))
      (when (and (typep writer 'instans-stream-writer)
		 (instans-encode-prefixes-p instans)
		 (not (instans-trig-output-processor-prefixes-written-p processor)))
	(loop for (k . v) in (instans-prefixes instans)
	      when (string-equal k "BASE")
	      do (format (instans-stream-writer-stream writer) "BASE <~A>~%" v)
	      else
	      do (format (instans-stream-writer-stream writer) "PREFIX ~A: <~A>~%" k v))
	(setf (instans-trig-output-processor-prefixes-written-p processor) t))))
  (:method ((instans instans) (processor instans-output-processor))
    (declare (ignore instans processor))))

(defgeneric add-input-processor (instans processor)
  (:method ((this instans) processor)
    (push-to-end processor (instans-input-processors this))))
