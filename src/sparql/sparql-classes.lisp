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

;(define-class hashkeyed () ((hashkey :accessor hashkeyed-hashkey :initform nil)))

;(define-class rdf-term (hashkeyed) ())

;;; Should we canonize IRIs?
(defstruct (rdf-iri :named (:predicate rdf-iri-p) (:type list))
  (string)
  (hashkey)
  (scheme)
  (authority)
  (path)
  (query)
  (fragment)
;   (had-dot-segments-p :accessor rdf-iri-had-dot-segments-p :initarg :had-dot-segments-p :initform nil)
)

;(defun make-rdf-iri (&key string) (make-instance 'rdf-iri :string string))

;; (define-class rdf-literal ()
;;   ((string :accessor rdf-literal-string :initarg :string :initform nil)
;;    (hashkey :accessor rdf-literal-hashkey :initform nil)
;;    (type :accessor rdf-literal-type :initarg :type :initform nil)
;;    (lang :accessor rdf-literal-lang :initarg :lang :initform nil)
;;    (value :accessor rdf-literal-value :initarg :value)))

(defstruct (rdf-literal :named (:predicate rdf-literal-p) (:type list))
  (string)
  (hashkey)
  (type)
  (lang))

(defun rdf-lang-literal-p (x)
  (and (rdf-literal-p x) (not (null (rdf-literal-lang x)))))

(defun rdf-typed-literal-p (x)
  (and (rdf-literal-p x) (not (null (rdf-literal-type x)))))

(define-class uniquely-named-object ()
  ((name :accessor uniquely-named-object-name :initarg :name)
   (pretty-name :accessor uniquely-named-object-pretty-name :initarg :pretty-name :initform nil)))

(define-class rdf-blank-node (uniquely-named-object) ())

(define-class rdf-anonymous-blank-node (rdf-blank-node) ())

(define-class rdf-named-blank-node (rdf-blank-node) ())

(define-class sparql-var (uniquely-named-object) ())

(define-class internal-var (sparql-var) ())

(define-class sparql-unbound () ())

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

;;; BEGIN initialize-instance :after

;; (defmethod initialize-instance :after ((this rdf-literal) &key (value nil value-present-p) &allow-other-keys)
;;   (declare (ignorable value))
;;   (unless value-present-p
;;     (let ((type (rdf-literal-type this)))
;;       (when type
;; 	(let ((descriptor (find-type-descriptor (rdf-iri-string type))))
;; 	  (when descriptor
;; 	    (setf (rdf-literal-value this) (funcall (type-descriptor-value-parser descriptor) (rdf-literal-string this)))))))))

(defmethod initialize-instance :after ((this uniquely-named-object) &key pretty-name &allow-other-keys)
  (unless pretty-name (setf (uniquely-named-object-pretty-name this) (uniquely-named-object-name this))))

;;; END initialize-instance :after

;;; BEGIN print-object

(defmethod print-object ((this sparql-error) stream)
  (format stream "#<~A \"~A\">" (type-of this) (apply #'format nil (sparql-error-format this)  (sparql-error-message-arguments this))))

;; (defmethod print-object ((this rdf-iri) stream)
;;   (format stream "#<~A ~A>" (type-of this) (rdf-iri-string this)))

;; (defmethod print-object ((this rdf-literal) stream)
;;   (format stream "#<~A \"~A\"" (type-of this) (rdf-literal-string this))
;;   (when (rdf-literal-lang this)
;;     (format stream "@\"~A\"" (rdf-literal-lang this)))
;;   (when (rdf-literal-type this)
;;     (format stream "^^~A" (rdf-iri-string (rdf-literal-type this))))
;;   (format stream ">"))

(defmethod print-object ((this uniquely-named-object) stream)
  (format stream "#<~A ~A>" (type-of this) (uniquely-named-object-pretty-name this)))

(defmethod print-object ((this sparql-op) stream)
  (format stream "#<~A ~:[~;hidden ~]\"~A\" (~{~A~^ ~}) returns ~A>"
	  (type-of this) (sparql-op-hiddenp this) (sparql-op-name this) (sparql-op-arguments this) (sparql-op-returns this)))

;;; END print-object

(defun compute-hashkey (this)
  (cond ((rdf-iri-p this) (sxhash (rdf-iri-string this)))
	((rdf-literal-p this)
	 (mix (mix (sxhash (rdf-literal-string this)) (sxhash (string-upcase (rdf-literal-lang this))))
	      (get-hashkey (rdf-literal-type this))))
	((uniquely-named-object-p this) (sxhash (uniquely-named-object-name this)))
	((sparql-unbound-p this) (sxhash this))
	(t (error* "Unexpected value for hashing ~S" this))))

;; (defun get-hashkey (x)
;;   (cond ((typep x 'hashkeyed)
;; 	 (when (null (hashkeyed-hashkey x))
;; 	   (setf (hashkeyed-hashkey x) (compute-hashkey x)))
;; 	 (hashkeyed-hashkey x))
;; 	(t
;; 	 (sxhash x))))

(defun get-hashkey (x)
  (cond ((rdf-iri-p x)
	 (when (null (rdf-iri-hashkey x))
	   (setf (rdf-iri-hashkey x) (compute-hashkey x)))
	 (rdf-iri-hashkey x))
	((rdf-literal-p x)
	 (when (null (rdf-literal-hashkey x))
	   (setf (rdf-literal-hashkey x) (compute-hashkey x)))
	 (rdf-literal-hashkey x))
	(t
	 (sxhash x))))

(defun rdf-plain-literal-p (term)
  (and (rdf-literal-p term)
       (null (rdf-literal-type term))))
;;; Note: we do not have any literals with no lang or type

;; (defun rdf-simple-literal-p (term)
;;   (and (rdf-literal-p term)
;;        (not (or (slot-boundp term 'type) (slot-boundp term 'lang)))))

(defun rdf-term-p (x) (typep x 'rdf-term))

(defun rdf-term-as-string (term)
  (cond ((rdf-iri-p term) (format nil "~A" (rdf-iri-string term)))
	((rdf-literal-p term) (rdf-literal-to-string term))
	((rdf-blank-node-p term) (uniquely-named-object-name term))
	((sparql-unbound-p term) "UNBOUND")
	((rdf-term-p term) (format nil "~A" term))
	(t (error* "Not an rdf-term ~A" term))))

;(defun rdf-blank-node-p (x) (typep x 'rdf-blank-node))

;;; Note! This is not SPARQL operation RDFterm-equal
(defun rdf-term-equal (t1 t2)
  (cond ((rdf-iri-p t1) (and (rdf-iri-p t2) (rdf-iri= t1 t2)))
	((rdf-blank-node-p t1) (and (rdf-blank-node-p t2) (string= (uniquely-named-object-name t1) (uniquely-named-object-name t2))))
	((rdf-literal-p t1) (equal t1 t2))
	(t nil)))

(defun create-rdf-literal-with-type (string type-iri)
  (let ((type-descriptor (find-type-descriptor (rdf-iri-string type-iri))))
;    (inform "create-rdf-literal-with-type ~S ~S, ~S" string type-iri (and type-descriptor (type-descriptor-value-parser type-descriptor)))
;    (describe type-descriptor)
    (cond ((null type-descriptor)
	   (make-rdf-literal :string string :type type-iri :lang nil))
	  (t (multiple-value-bind (value msg)
		 (funcall (type-descriptor-value-parser type-descriptor) string :errorp nil)
	       (cond ((null msg) value)
		     (t nil)))))))

(defun create-rdf-literal-with-lang (string lang)
  (make-rdf-literal :string string :lang lang :type *rdf-lang-string-iri*))

(defun rdf-literal-to-string (this)
  (coerce (append (cons #\" (coerce (rdf-literal-string this) 'list))
		  '(#\")
		  (if (rdf-literal-lang this) (cons #\@ (coerce (rdf-literal-lang this) 'list))
		      (if (rdf-literal-type this) (append '(#\^ #\^ #\<) (coerce (rdf-iri-string (rdf-literal-type this)) 'list) '(#\>)))))
	  'string))

(defgeneric uniquely-named-object-equal (o1 o2)
  (:method ((o1 uniquely-named-object) (o2 uniquely-named-object))
    (or (eq o1 o2)
	(equal (uniquely-named-object-name o1) (uniquely-named-object-name o2)))))

(defgeneric make-uniquely-named-object (factory name &rest keys &key pretty-name &allow-other-keys)
  (:method ((factory uniquely-named-object-factory) name &rest keys &key pretty-name &allow-other-keys)
    (or (gethash name (slot-value factory 'objects-by-name))
	(let ((object (apply #'make-instance (slot-value factory 'object-type) :name name :pretty-name pretty-name keys)))
	  (setf (gethash name (slot-value factory 'objects-by-name)) object)
	  object))))

(defgeneric generate-object-with-unique-name (factory &rest keys &key name-prefix &allow-other-keys)
  (:method ((factory uniquely-named-object-factory) &rest keys &key name-prefix &allow-other-keys)
    (let ((generated-name (format nil "~@[~A~]~D" name-prefix (incf (slot-value factory 'name-counter)))))
      (cond ((null (gethash generated-name (slot-value factory 'objects-by-name)))
	     (remf keys :name-prefix)
	     (let ((object (apply #'make-instance (slot-value factory 'object-type) :name generated-name keys)))
	       (setf (gethash generated-name (slot-value factory 'objects-by-name)) object)))
	    (t
	     (error* "Object with name ~A already exists" generated-name))))))

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
  (setf *rdf-first* (make-rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
  (setf *rdf-rest* (make-rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))
  (setf *rdf-nil* (make-rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
  (setf *rdf-type* (make-rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  (setf *rdf-lang-string-iri* (make-rdf-iri :string "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"))
  (setf *xsd-value-type-descriptors* (make-instance 'type-descriptors))
  (setf *sparql-ops* (make-instance 'sparql-ops))
  (setf *sparql-standard-op-library* (add-sparql-op-library :prefix "" :iri-string "http://www.w3.org/TR/sparql11-query/#SparqlOps#"))
  (setf *sparql-xsd-op-library* (add-sparql-op-library :prefix "xsd" :iri-string "http://www.w3.org/2001/XMLSchema#"))
  (setf *instans-math-extension-op-library* (add-sparql-op-library :prefix "math" :iri-string "http://instans.org/extensions/math#"))
  (setf *instans-datetime-extension-op-library* (add-sparql-op-library :prefix "datetime" :iri-string "http://instans.org/extensions/datetime#"))
  (setf *instans-op-library* (add-sparql-op-library :prefix "instans" :iri-string "http://instans.org/extensions/instans#")))

(defun create-initial-prefix-alist ()
  (list (cons "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	(cons "xsd" "http://www.w3.org/2001/XMLSchema#")))

(eval-when (:load-toplevel :execute)
  (initialize-globals))

;;; Misc

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

(defun sparql-value-same-term (v1 v2)
;  (inform "v1 = ~S (of type ~S), v2 = ~S (of type ~S), (eq (type-of v1) (type-of v2)) = ~S" v1 (type-of v1) v2 (type-of v2) (eq (type-of v1) (type-of v2)))
  (and (equal (type-of v1) (type-of v2))
       (sparql-call "sameTerm" v1 v2)))

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

		   
