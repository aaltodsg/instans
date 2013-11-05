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
(defvar *rdf-blank-node-factory*)
(defvar *sparql-var-factory*)
(defvar *sparql-ops*)
(defvar *sparql-standard-op-library*)
(defvar *instans-op-library*)
(defvar *instans-math-extension-op-library*)
(defvar *instans-datetime-extension-op-library*)

(define-class sparql-error ()
  ((format :accessor sparql-error-format :initarg :format :initform nil)
   (arguments :accessor sparql-error-message-arguments :initarg :arguments :initform nil)))

(define-class rdf-term () ())

;;; Should we canonize IRIs?
(define-class rdf-iri (rdf-term)
  ((string :accessor rdf-iri-string :initarg :string)
   (scheme :accessor rdf-iri-scheme :initarg :scheme :initform nil)
   (authority :accessor rdf-iri-authority :initarg :authority :initform nil)
   (path :accessor rdf-iri-path :initarg :path :initform nil)
   (query :accessor rdf-iri-query :initarg :query :initform nil)
   (fragment :accessor rdf-iri-fragment :initarg :fragment :initform nil)
   (had-dot-segments-p :accessor rdf-iri-had-dot-segments-p :initarg :had-dot-segments-p :initform nil)))

(define-class rdf-literal (rdf-term)
  ((string :accessor rdf-literal-string :initarg :string :initform nil)
   (type :accessor rdf-literal-type :initarg :type :initform nil)
   (lang :accessor rdf-literal-lang :initarg :lang :initform nil)
   (value :accessor rdf-literal-value :initarg :value)))

(define-class uniquely-named-object ()
  ((name :accessor uniquely-named-object-name :initarg :name)))

(define-class rdf-blank-node (rdf-term uniquely-named-object) ())

(define-class sparql-var (uniquely-named-object) ())

(define-class sparql-unbound (rdf-term) ())

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

(define-class sparql-form (sparql-op) ())

(define-class sparql-op-library ()
  ((prefix :accessor sparql-op-library-prefix :initarg :prefix)
   (iri-string :accessor sparql-op-library-iri-string :initarg :iri-string)
   (ops :accessor sparql-op-library-ops :initform (make-hash-table :test #'equal))))

(define-class sparql-ops ()
  ((ops :accessor sparql-ops-ops :initform (make-hash-table :test #'equal))
   (libraries :accessor sparql-ops-libraries :initform (make-hash-table :test #'equal))))

(define-class sparql-runtime-exception ()
  ((runtime-exception :accessor sparql-runtime-exception-data :initarg :data)))

(define-class sparql-query-results ()
  ((variables :accessor sparql-query-results-variables :initarg :variables :initform nil)
   (links :accessor sparql-query-results-links :initarg :links :initform nil)
   (results :accessor sparql-query-results-results :initarg :results)
   (boolean :accessor sparql-query-results-boolean :initarg :boolean)))

(define-class sparql-binding ()
  ((variable :accessor sparql-binding-variable :initarg :variable)
   (value :accessor sparql-binding-value :initarg :value)))

(define-class sparql-link ()
  ((href :accessor sparql-link-href :initarg :href)))

(define-class sparql-result ()
  ((bindings :accessor sparql-result-bindings :initarg :bindings)))

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
  (format stream "#<~A format=~S args=~A>" (type-of this) (sparql-error-format this)  (sparql-error-message-arguments this)))

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

(defgeneric rdf-iri-to-string (iri)
  (:method ((this rdf-iri))
    (cond ((slot-boundp this 'string) (rdf-iri-string this))
	  (t
	   (setf (rdf-iri-string this)
		 (apply #'concatenate 'string
			(and (rdf-iri-scheme this) (list (rdf-iri-scheme this) ":"))
			(and (rdf-iri-authority this) (list "//" (rdf-iri-authority this)))
			(list (rdf-iri-path this))
			(and (rdf-iri-query this) (list "?" (rdf-iri-query this)))
			(and (rdf-iri-fragment this) (list "#" (rdf-iri-fragment this)))))))))

(defgeneric rdf-plain-literal-p (term)
  (:method ((this rdf-literal))
    (not (slot-boundp this 'type)))
  (:method ((this rdf-term)) nil))

(defgeneric rdf-simple-literal-p (term)
  (:method ((this rdf-literal))
    (not (or (slot-boundp this 'type) (slot-boundp this 'lang))))
  (:method ((this rdf-term)) nil))

(defun create-rdf-literal-with-type (string type-iri)
  (let ((type-descriptor (find-type-descriptor (rdf-iri-string type-iri))))
    (cond ((null type-descriptor)
	   (make-instance 'rdf-literal :string string :type type-iri))
	  (t (funcall (type-descriptor-value-parser type-descriptor) string)))))

(defun create-rdf-literal-with-lang (string lang)
  (make-instance 'rdf-literal :string string :lang lang :type *rdf-lang-string-iri*))

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

(defun make-rdf-blank-node (name)
  (make-uniquely-named-object *rdf-blank-node-factory* name))

(defun generate-rdf-blank-node (&optional (name-prefix "_:!"))
  (generate-object-with-unique-name *rdf-blank-node-factory* :name-prefix name-prefix))

(defun make-sparql-var (name)
  (make-uniquely-named-object *sparql-var-factory* name))

(defun generate-sparql-var (&optional name-prefix)
  (generate-object-with-unique-name *sparql-var-factory* :name-prefix name-prefix))

(defun create-sparql-binding (var value)
  (make-instance 'sparql-binding :variable var :value value))

(defun create-sparql-result (bindings)
  (make-instance 'sparql-result :bindings bindings))

(defun create-sparql-link (href)
  (make-instance 'sparql-link :href href))

(defun find-type-descriptor (iri-string &optional (type-descriptors *xsd-value-type-descriptors*))
  (gethash iri-string (type-descriptors-string-map type-descriptors)))

(defun add-sparql-op-library (&key prefix iri-string (sparql-ops *sparql-ops*))
  (setf (gethash prefix (sparql-ops-libraries sparql-ops)) (make-instance 'sparql-op-library :prefix prefix :iri-string iri-string)))

(defun find-sparql-op-library (library-name &key (sparql-ops *sparql-ops*))
  (gethash library-name (sparql-ops-libraries sparql-ops)))

(defun add-sparql-op (&key (sparql-ops *sparql-ops*) kind name lisp-name arguments returns body hiddenp)
  (multiple-value-bind (library-name op-name)
      (split-sparql-op-prefixed-name name)
    (let ((library (find-sparql-op-library library-name :sparql-ops sparql-ops)))
      (cond ((null library)
	     (error* "Undefined SPARQL operation library ~A" library-name))
	    (t
	     (let ((sparql-op (make-instance kind :name name :lisp-name lisp-name :arguments arguments :returns returns :body body :hiddenp hiddenp
					     :containing-library library)))
	       (setf (gethash name (sparql-ops-ops sparql-ops)) sparql-op)
	       (setf (gethash op-name (sparql-op-library-ops library)) sparql-op)))))))

(defun find-sparql-op (name &key (sparql-ops *sparql-ops*))
  (gethash name (sparql-ops-ops sparql-ops)))

(defun list-sparql-ops (&key library-name (sparql-ops *sparql-ops*))
  (cond ((null library-name)
	 (maphash #'(lambda (key value) (inform "~A -> ~A" key value)) (sparql-ops-ops sparql-ops)))
	(t
	 (maphash #'(lambda (key value) (inform "~A -> ~A" key value)) (sparql-op-library-ops (find-sparql-op-library library-name :sparql-ops sparql-ops))))))

(eval-when (:load-toplevel :execute)
  (setf *sparql-unbound* (make-instance 'sparql-unbound))
  (setf *rdf-first* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
  (setf *rdf-rest* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))
  (setf *rdf-nil* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
  (setf *rdf-type* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  (setf *rdf-lang-string-iri* (make-instance 'rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"))
  (setf *xsd-value-type-descriptors* (make-instance 'type-descriptors))
  (setf *sparql-ops* (make-instance 'sparql-ops))
  (setf *sparql-standard-op-library* (add-sparql-op-library :prefix "" :iri-string "http://www.w3.org/TR/sparql11-query/#SparqlOps#"))
  (setf *instans-math-extension-op-library* (add-sparql-op-library :prefix "math" :iri-string "http://instans.org/extensions/math#"))
  (setf *instans-datetime-extension-op-library* (add-sparql-op-library :prefix "datetime" :iri-string "http://instans.org/extensions/datetime#"))
  (setf *instans-op-library* (add-sparql-op-library :prefix "instans" :iri-string "http://instans.org/extensions/instans#")))

(defun initialize-uniquely-named-object-factories ()
  (setf *rdf-blank-node-factory* (make-instance 'uniquely-named-object-factory :object-type 'rdf-blank-node))
  (setf *sparql-var-factory* (make-instance 'uniquely-named-object-factory :object-type 'sparql-var)))

;;; Misc

(defgeneric sparql-query-results-solutions (r)
  (:method ((this sparql-query-results))
    (cond ((slot-boundp this 'boolean)
	   (sparql-query-results-boolean this))
	  (t
	   (loop with variables = (sparql-query-results-variables this)
		 for result in (sparql-query-results-results this)
		 collect (loop for var in variables
			       for binding = (find-if #'(lambda (b) (uniquely-named-object-equal var (sparql-binding-variable b))) (sparql-result-bindings result))
			       when binding collect binding))))))

