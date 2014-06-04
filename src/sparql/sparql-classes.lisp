;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Type conventions:
;;; - All string literals with no lang tag are always converted to xsd-string-value's. That is, no sparql function should ever expect to get an rdf-literal with
;;;   type = xsd:string or with type = nil and lang = nil
;;; - All number literals are always converted to some xsd-number-value type.

(defvar *rdf-first*)
(defvar *rdf-rest*)
(defvar *rdf-nil*)
(defvar *rdf-type*)
(defvar *xsd-value-type-descriptors*)
(defvar *rdf-lang-string-iri*)
(defvar *sparql-standard-op-library*)
(defvar *sparql-xsd-op-library*)
(defvar *sparql-ops*)
(defvar *instans-op-library*)
(defvar *instans-math-extension-op-library*)
(defvar *instans-datetime-extension-op-library*)

(define-class sparql-error ()
  ((format :accessor sparql-error-format :initarg :format :initform nil)
   (arguments :accessor sparql-error-message-arguments :initarg :arguments :initform nil)))

(define-class hashkeyed () ((hashkey :accessor hashkeyed-hashkey :initform nil)))

(define-class rdf-term (hashkeyed) ())

;;; Should we canonize IRIs?
(define-class rdf-iri (rdf-term)
  ((string :accessor rdf-iri-string :initarg :string)
   (scheme :accessor rdf-iri-scheme :initarg :scheme :initform nil)
   (authority :accessor rdf-iri-authority :initarg :authority :initform nil)
   (path :accessor rdf-iri-path :initarg :path :initform nil)
   (query :accessor rdf-iri-query :initarg :query :initform nil)
   (fragment :accessor rdf-iri-fragment :initarg :fragment :initform nil)
;   (had-dot-segments-p :accessor rdf-iri-had-dot-segments-p :initarg :had-dot-segments-p :initform nil)
))

(define-class rdf-literal (rdf-term)
  ((string :accessor rdf-literal-string :initarg :string :initform nil)
   (type :accessor rdf-literal-type :initarg :type :initform nil)
   (lang :accessor rdf-literal-lang :initarg :lang :initform nil)
   (value :accessor rdf-literal-value :initarg :value)))

(define-class uniquely-named-object ()
  ((name :accessor uniquely-named-object-name :initarg :name)))

(define-class rdf-blank-node (rdf-term uniquely-named-object) ())

(define-class sparql-var (uniquely-named-object hashkeyed) ())

(define-class internal-var (sparql-var) ())

(define-class sparql-unbound (rdf-term) ())

(define-class sparql-distinct () ())

(define-class type-descriptor ()
  ((iri :accessor type-descriptor-iri :initarg :iri)
   (iri-string :accessor type-descriptor-iri-string :initarg :iri-string)
   (lisp-type :accessor type-descriptor-lisp-type :initarg :lisp-type)
   (value-parser :accessor type-descriptor-value-parser :initarg :value-parser)))

(define-class type-descriptors ()
  ((string-map :accessor type-descriptors-string-map :initform (make-hash-table :test #'equal))))

(define-class uniquely-named-object-factory ()
  ((object-type :initarg :object-type)
   (name-counter :initform -1)
   (objects-by-name :initform (make-hash-table))))

(define-class sparql-op ()
  ((name :accessor sparql-op-name :initarg :name)
   (lisp-name :accessor sparql-op-lisp-name :initarg :lisp-name)
   (arguments :accessor sparql-op-arguments :initarg :arguments)
   (containing-library :accessor sparql-op-containing-library :initarg :containing-library)
   (returns :accessor sparql-op-returns :initarg :returns)
   (body :accessor sparql-op-body :initarg :body)
   (hiddenp :accessor sparql-op-hiddenp :initarg :hiddenp :initform nil)))

(define-class sparql-function (sparql-op) ())

;; (define-class sparql-form (sparql-op) ())

(define-class sparql-macro (sparql-op) ())

(define-class sparql-op-library ()
  ((prefix :accessor sparql-op-library-prefix :initarg :prefix)
   (iri-string :accessor sparql-op-library-iri-string :initarg :iri-string)
   (ops :accessor sparql-op-library-ops :initform (make-hash-table :test #'equal))))

(define-class sparql-ops ()
  ((ops :accessor sparql-ops-ops :initform (make-hash-table :test #'equal))
   (libraries :accessor sparql-ops-libraries :initform (make-hash-table :test #'equal))))

(define-class sparql-runtime-exception ()
  ((runtime-exception :accessor sparql-runtime-exception-data :initarg :data)))

(define-class sparql-results ()
  ((results)
   (tail)))

(defmethod initialize-instance :after ((this sparql-results) &key &allow-other-keys)
  (setf (slot-value this 'results) (list nil))
  (setf (slot-value this 'tail) (slot-value this 'results)))

(defgeneric add-sparql-result (sparql-results result)
  (:method ((this sparql-results) result)
    (setf (cdr (slot-value this 'tail)) (list result))
    (setf (slot-value this 'tail) (cdr (slot-value this 'tail)))))

(defgeneric sparql-results (sparql-results)
  (:method ((this sparql-results))
    (cdr (slot-value this 'results))))

(define-class sparql-query-results ()
  ((variables :accessor sparql-query-results-variables :initarg :variables :initform nil)
   (links :accessor sparql-query-results-links :initarg :links :initform nil)
   (results :accessor sparql-query-results-results :initarg :results)
   (triples :accessor sparql-query-results-triples :initarg :triples)
   (boolean :accessor sparql-query-results-boolean :initarg :boolean)))

(define-class sparql-abstract-result ()
  ((rule :accessor sparql-result-rule :initarg :rule :initform nil)))

(define-class sparql-result (sparql-abstract-result)
  ((bindings :accessor sparql-result-bindings :initarg :bindings)))

(define-class sparql-binding ()
  ((variable :accessor sparql-binding-variable :initarg :variable)
   (value :accessor sparql-binding-value :initarg :value)))

(define-class sparql-link (sparql-abstract-result)
  ((href :accessor sparql-link-href :initarg :href)))

(define-class sparql-boolean-result (sparql-abstract-result)
  ((value :accessor sparql-boolean-result-value :initarg :value)))

(define-class sparql-triples-result (sparql-abstract-result)
  ((triples :accessor sparql-triples-result-triples :initarg :triples)))

;;; BEGIN initialize-instance :after

(defmethod initialize-instance :after ((this rdf-literal) &key (value nil value-present-p) &allow-other-keys)
  (declare (ignorable value))
  (unless value-present-p
    (let ((type (rdf-literal-type this)))
      (when type
	(let ((descriptor (find-type-descriptor (rdf-iri-string type))))
	  (when descriptor
	    (setf (rdf-literal-value this) (funcall (type-descriptor-value-parser descriptor) (rdf-literal-string this)))))))))

;;; END initialize-instance :after

;;; BEGIN print-object

(defmethod print-object ((this sparql-error) stream)
  (format stream "#<~A \"~A\">" (type-of this) (apply #'format nil (sparql-error-format this)  (sparql-error-message-arguments this))))

(defmethod print-object ((this rdf-iri) stream)
  (format stream "#<~A ~A>" (type-of this) (rdf-iri-string this)))

(defmethod print-object ((this rdf-literal) stream)
  (format stream "#<~A \"~A\"" (type-of this) (rdf-literal-string this))
  (when (rdf-literal-type this)
    (format stream "^^~A" (rdf-iri-string (rdf-literal-type this))))
  (when (rdf-literal-lang this)
    (format stream "@\"~A\"" (rdf-literal-lang this)))
  (format stream ">"))

(defmethod print-object ((this uniquely-named-object) stream)
  (format stream "#<~A ~A>" (type-of this) (uniquely-named-object-name this)))

(defmethod print-object ((this sparql-op) stream)
  (format stream "#<~A ~:[~;hidden ~]\"~A\" (~{~A~^ ~}) returns ~A>"
	  (type-of this) (sparql-op-hiddenp this) (sparql-op-name this) (sparql-op-arguments this) (sparql-op-returns this)))

(defmethod print-object ((this sparql-runtime-exception) stream)
  (format stream "#<~A format=~S>" (type-of this) (sparql-runtime-exception-data this)))

(defmethod print-object ((this sparql-binding) stream)
  (format stream "#<~A ~A = ~S>" (type-of this) (uniquely-named-object-name (sparql-binding-variable this)) (sparql-binding-value this)))

;;; END print-object

(defgeneric compute-hashkey (hashkeyed)
  (:method ((this rdf-iri))
    (sxhash (rdf-iri-string this)))
  (:method ((this rdf-literal))
    (mix (mix (sxhash (rdf-literal-string this)) (sxhash (rdf-literal-lang this))) (get-hashkey (rdf-literal-type this))))
  (:method ((this uniquely-named-object))
    (sxhash (uniquely-named-object-name this)))
  (:method ((this sparql-unbound))
    (sxhash this)))

(defun get-hashkey (x)
  (cond ((typep x 'hashkeyed)
	 (when (null (hashkeyed-hashkey x))
	   (setf (hashkeyed-hashkey x) (compute-hashkey x)))
	 (hashkeyed-hashkey x))
	(t
	 (sxhash x))))

(defgeneric rdf-plain-literal-p (term)
  (:method ((this rdf-literal))
    (not (slot-boundp this 'type)))
  (:method ((this rdf-term)) nil))

(defgeneric rdf-simple-literal-p (term)
  (:method ((this rdf-literal))
    (not (or (slot-boundp this 'type) (slot-boundp this 'lang))))
  (:method ((this rdf-term)) nil))

(defgeneric rdf-term-as-string (term)
  (:method ((this rdf-iri)) (format nil "~A" (rdf-iri-string this)))
  (:method ((this rdf-literal)) (rdf-literal-to-string this))
  (:method ((this rdf-blank-node)) (uniquely-named-object-name this))
  (:method ((this sparql-unbound)) "UNBOUND")
  (:method ((this rdf-term)) (format nil "~A" this)))

(defun create-rdf-literal-with-type (string type-iri)
  (let ((type-descriptor (find-type-descriptor (rdf-iri-string type-iri))))
;    (inform "create-rdf-literal-with-type ~S ~S" string type-iri)
;    (describe type-descriptor)
    (cond ((null type-descriptor)
	   (make-instance 'rdf-literal :string string :type type-iri))
	  (t (funcall (type-descriptor-value-parser type-descriptor) string)))))

(defun create-rdf-literal-with-lang (string lang)
  (make-instance 'rdf-literal :string string :lang lang :type *rdf-lang-string-iri*))

(defgeneric rdf-literal-to-string (rdf-literal)
  (:method ((this rdf-literal))
    (coerce (append (cons #\" (coerce (rdf-literal-string this) 'list))
		    '(#\")
		    (if (rdf-literal-lang this) (cons #\@ (coerce (rdf-literal-lang this) 'list))
			(if (rdf-literal-type this) (append '(#\^ #\^ #\<) (coerce (rdf-iri-string (rdf-literal-type this)) 'list) '(#\>)))))
    'string)))

(defgeneric uniquely-named-object-equal (o1 o2)
  (:method ((o1 uniquely-named-object) (o2 uniquely-named-object))
    (or (eq o1 o2)
	(equal (uniquely-named-object-name o1) (uniquely-named-object-name o2)))))

(defgeneric make-uniquely-named-object (factory name &rest keys &key &allow-other-keys)
  (:method ((factory uniquely-named-object-factory) name &rest keys &key &allow-other-keys)
    (or (gethash name (slot-value factory 'objects-by-name))
	(let ((object (apply #'make-instance (slot-value factory 'object-type) :name name keys)))
	  (setf (gethash name (slot-value factory 'objects-by-name)) object)
	  object))))

(defgeneric generate-object-with-unique-name (factory &rest keys &key name-prefix &allow-other-keys)
  (:method ((factory uniquely-named-object-factory) &rest keys &key name-prefix &allow-other-keys)
    (let ((generated-name (format nil "~@[~A~]-~D" name-prefix (incf (slot-value factory 'name-counter)))))
      (cond ((null (gethash generated-name (slot-value factory 'objects-by-name)))
	     (remf keys :name-prefix)
	     (let ((object (apply #'make-instance (slot-value factory 'object-type) :name generated-name keys)))
	       (setf (gethash generated-name (slot-value factory 'objects-by-name)) object)))
	    (t
	     (error* "Object with name ~A already exists" generated-name))))))

(defun create-sparql-binding (var value)
  (make-instance 'sparql-binding :variable var :value value))

(defun create-sparql-result (bindings)
  (make-instance 'sparql-result :bindings bindings))

(defgeneric sparql-result-equal (r1 r2)
  (:method ((r1 sparql-result) (r2 sparql-result))
    (let ((bl1 (sparql-result-bindings r1))
	  (bl2 (sparql-result-bindings r2)))
      (and (every #'(lambda (b1) (or (sparql-unbound-p (sparql-binding-value b1)) (find b1 bl2 :test #'sparql-binding-equal))) bl1)
	   (every #'(lambda (b2) (or (sparql-unbound-p (sparql-binding-value b2)) (find b2 bl1 :test #'sparql-binding-equal))) bl2)))))

(defgeneric sparql-binding-equal (b1 b2)
  (:method ((b1 sparql-binding) (b2 sparql-binding))
    (and (sparql-var-equal (sparql-binding-variable b1) (sparql-binding-variable b2))
	 (sparql-value-equal (sparql-binding-value b1) (sparql-binding-value b2)))))

(defun create-sparql-link (href)
  (make-instance 'sparql-link :href href))

(defun find-type-descriptor (iri-string &optional (type-descriptors *xsd-value-type-descriptors*))
  (gethash iri-string (type-descriptors-string-map type-descriptors)))

(defun add-sparql-op-library (&key prefix iri-string (sparql-ops *sparql-ops*))
  (setf prefix (string-downcase prefix))
  (setf iri-string (string-downcase iri-string))
  (setf (gethash prefix (sparql-ops-libraries sparql-ops)) (make-instance 'sparql-op-library :prefix prefix :iri-string iri-string)))

(defun find-sparql-op-library (library-name &key (sparql-ops *sparql-ops*))
  (gethash (string-downcase library-name) (sparql-ops-libraries sparql-ops)))

(defun add-sparql-op (&key (sparql-ops *sparql-ops*) kind prefixed-name-string lisp-name arguments returns body hiddenp)
  (when prefixed-name-string
    (setf prefixed-name-string (string-downcase prefixed-name-string)))
  (multiple-value-bind (library-name op-name)
      (split-sparql-op-prefixed-name prefixed-name-string)
    (let ((library (find-sparql-op-library library-name :sparql-ops sparql-ops)))
      (cond ((null library)
	     (error* "Undefined SPARQL operation library ~A" library-name))
	    (t
	     (let ((sparql-op (make-instance kind :name prefixed-name-string :lisp-name lisp-name :arguments arguments :returns returns :body body :hiddenp hiddenp
					     :containing-library library)))
	       (setf (gethash op-name (sparql-op-library-ops library)) sparql-op)
	       (setf (gethash prefixed-name-string (sparql-ops-ops sparql-ops)) sparql-op)
	       (setf (gethash (concatenate 'string (sparql-op-library-iri-string library) op-name) (sparql-ops-ops sparql-ops)) sparql-op)))))))

(defun find-sparql-op (name &key (sparql-ops *sparql-ops*))
  (gethash (string-downcase (if (rdf-iri-p name) (rdf-iri-string name) name)) (sparql-ops-ops sparql-ops)))

(defun list-sparql-ops (&key library-name (sparql-ops *sparql-ops*))
  (cond ((null library-name)
	 (maphash #'(lambda (key value) (inform "~A -> ~A" key value)) (sparql-ops-ops sparql-ops)))
	(t
	 (maphash #'(lambda (key value) (inform "~A -> ~A" key value)) (sparql-op-library-ops (find-sparql-op-library library-name :sparql-ops sparql-ops))))))

(defun initialize-globals ()
  (setf *sparql-unbound* (make-instance 'sparql-unbound))
  (setf *sparql-distinct* (make-instance 'sparql-distinct))
  (setf *rdf-first* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
  (setf *rdf-rest* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))
  (setf *rdf-nil* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
  (setf *rdf-type* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  (setf *rdf-lang-string-iri* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"))
  (setf *xsd-value-type-descriptors* (make-instance 'type-descriptors))
  (setf *sparql-ops* (make-instance 'sparql-ops))
  (setf *sparql-standard-op-library* (add-sparql-op-library :prefix "" :iri-string "http://www.w3.org/TR/sparql11-query/#SparqlOps#"))
  (setf *sparql-xsd-op-library* (add-sparql-op-library :prefix "xsd" :iri-string "http://www.w3.org/2001/XMLSchema#"))
  (setf *instans-math-extension-op-library* (add-sparql-op-library :prefix "math" :iri-string "http://instans.org/extensions/math#"))
  (setf *instans-datetime-extension-op-library* (add-sparql-op-library :prefix "datetime" :iri-string "http://instans.org/extensions/datetime#"))
  (setf *instans-op-library* (add-sparql-op-library :prefix "instans" :iri-string "http://instans.org/extensions/instans#")))

(eval-when (:load-toplevel :execute)
  (initialize-globals))

;;; Misc

(defgeneric sparql-query-results-solutions (r)
  (:method ((this sparql-query-results))
    (cond ((slot-boundp this 'boolean)
	   (sparql-query-results-boolean this))
	  (t
	   (loop with variables = (sparql-query-results-variables this)
		 for result in (sparql-query-results-results this)
		 collect (loop for var in variables
			       for binding = (find-if #'(lambda (b) (sparql-var-equal var (sparql-binding-variable b))) (sparql-result-bindings result))
			       when binding collect binding))))))

(defgeneric sparql-results-compare (query-results1 query-results2 &key verbosep output-stream result-label1 result-label2)
  (:method ((query-results1 sparql-query-results) (query-results2 sparql-query-results) &key verbosep (output-stream *standard-output*) (result-label1 "") (result-label2 ""))
    (cond ((and (slot-boundp query-results1 'boolean) (slot-boundp query-results2 'boolean))
	   (cond ((eq (sparql-query-results-boolean query-results1) (sparql-query-results-boolean query-results2))
		  (when verbosep (format output-stream "~%~:(~A~) solutions ~S and ~(~A~) solutions ~S are equal" result-label1 query-results1 result-label2 query-results2))
		  (values t t))
		 (t
		  (when verbosep (format output-stream "~%~:(~A~) solutions ~S and ~(~A~) solutions ~S are not equal" result-label1 query-results1 result-label2 query-results2))
		  (values nil nil))))
	  ((and (slot-boundp query-results1 'triples) (slot-boundp query-results2 'triples))
	   (let ((triple-list1 (sparql-query-results-triples query-results1))
		 (triple-list2 (sparql-query-results-triples query-results2)))
	     (cond ((and (= (length triple-list1) (length triple-list2))
			 (every #'(lambda (tr1 tr2) (equal-triples tr1 tr2)) triple-list1 triple-list2))
		    (when verbosep (format output-stream "~%~:(~A~) and ~(~A~) are equal" result-label1 result-label2))
		    (values t t))
		   ((not (= (length triple-list1) (length triple-list2)))
		    (when verbosep (format output-stream "~%~D ~:(~A~) triples, ~(~A~) ~D triples" result-label1 (length triple-list1) result-label2 (length triple-list2)))
		    (values nil nil))
		   (t
		    (flet ((show-triples (triples) (loop for triple in triples do (format output-stream "~%Triple: [~{~A~^ ~}]" triple))))
		      (let ((observed-minus-expected (set-difference triple-list1 triple-list2 :test #'equal-triples))
			    (expected-minus-observed (set-difference triple-list2 triple-list1 :test #'equal-triples)))
			(cond ((and (null observed-minus-expected) (null expected-minus-observed))
			       (when verbosep
				 (format output-stream "~%~:(~A~) triples and ~(~A~) triples are same, but in a different order~%" result-label1 result-label2)
				 (format output-stream "~%~:(~A~):" result-label1)
				 (show-triples triple-list1)
				 (format output-stream "~%~:(~A~):" result-label2)
				 (show-triples triple-list2)
				 (values t nil)))
			      (t
			       (format output-stream "~%~:(~A~) triples not in ~(~A~):" result-label1 result-label2)
			       (show-triples expected-minus-observed)
			       (format output-stream "~%~:(~A~) triples not ~(~A~):" result-label2 result-label1)
			       (show-triples observed-minus-expected)
			       (values nil nil)))))))))
	  ((and (slot-boundp query-results1 'results) (slot-boundp query-results2 'results))
	   (let ((result-list1 (sparql-query-results-results query-results1))
		 (result-list2 (sparql-query-results-results query-results2)))
	     (cond ((and (= (length result-list1) (length result-list2))
			 (every #'(lambda (r1 r2) (sparql-result-equal r1 r2)) result-list1 result-list2))
		    (when verbosep (format output-stream "~%~:(~A~) solutions and ~(~A~) solutions are equal" result-label1 result-label2))
		    (values t t))
		   ((not (= (length result-list1) (length result-list2)))
		    (when verbosep (format output-stream "~%~D ~:(~A~) solutions, ~(~A~) ~D solutions" result-label1 (length result-list1) result-label2 (length result-list2)))
		    (values nil nil))
		   (t
		    (flet ((show-solutions (sl) (loop for s in sl do (format output-stream "~%Solution: ~{~A~^ ~}" (sparql-result-bindings s)))))
		      (let ((observed-minus-expected (set-difference result-list1 result-list2 :test #'sparql-result-equal))
			    (expected-minus-observed (set-difference result-list2 result-list1 :test #'sparql-result-equal)))
			(cond ((and (null observed-minus-expected) (null expected-minus-observed))
			       (when verbosep
				 (format output-stream "~%~:(~A~) solutions and ~(~A~) solutions are same, but in a different order~%" result-label1 result-label2)
				 (format output-stream "~%~:(~A~):" result-label1)
				 (show-solutions result-list1)
				 (format output-stream "~%~:(~A~):" result-label2)
				 (show-solutions result-list2)
				 (values t nil)))
			      (t
			       (format output-stream "~%~:(~A~) solutions not in ~(~A~):" result-label1 result-label2)
			       (show-solutions expected-minus-observed)
			       (format output-stream "~%~:(~A~) solutions not ~(~A~):" result-label2 result-label1)
			       (show-solutions observed-minus-expected)
			       (values nil nil)))))))))
	  (t
	   (when verbosep (format output-stream "~%Cannot compare ~:(~A~) ~S and ~(~A~) ~S" result-label1 query-results1 result-label2 query-results2))
	   (values nil nil)))))

(defun sparql-var-equal (v1 v2)
  (and (sparql-var-p v1) (sparql-var-p v2)
       (uniquely-named-object-equal v1 v2)))

(defun sparql-var-list-difference (l1 l2)
  (list-difference l1 l2 :test #'sparql-var-equal))

(defun sparql-var-list-union (l1 l2)
  (list-union l1 l2 :test #'sparql-var-equal))

(defun sparql-var-list-intersection (l1 l2)
  (list-intersection l1 l2 :test #'sparql-var-equal))

(defun find-sparql-var (v vlist)
  (find v vlist :test #'sparql-var-equal))

(defun sparql-value-equal (v1 v2)
;  (inform "v1 = ~S (of type ~S), v2 = ~S (of type ~S), (eq (type-of v1) (type-of v2)) = ~S" v1 (type-of v1) v2 (type-of v2) (eq (type-of v1) (type-of v2)))
  (and (equal (type-of v1) (type-of v2))
       (sparql-call "=" v1 v2)))

(defun equal-triples (tr1 tr2)
  (and (consp tr1) (consp tr2)
       (= (length tr1) (length tr2) 3)
       (sparql-value-equal (first tr1) (first tr2))
       (sparql-value-equal (second tr1) (second tr2))
       (sparql-value-equal (third tr1) (third tr2))))

(defun equal-quads (q1 q2)
  (and (consp q1) (consp q2)
       (= (length q1) (length q2))
       (every #'sparql-value-equal q1 q2)))
