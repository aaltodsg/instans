;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defmacro error-safe (form)
  `(handler-case
       (catch :sparql-error ,form)
     (t (e) (make-instance 'sparql-error :format "~S" :arguments (list e)))))

(defmacro error-safe-ebv (v)
  `(error-safe (sparql-call "ebv" ,v)))

(defmacro eval-sparql-filter (filter args)
  (let ((v (gensym)))
    `(let ((,v (error-safe-ebv (apply ,filter ,args))))
       (and ,v (not (sparql-error-p ,v))))))

(defun make-sparql-op-name (library-name op-name)
  (cond ((alpha-char-p (char (string op-name) 0))
	 (fmt-intern "~:@(%~A-~A%~)" library-name op-name))
	(t
	 (fmt-intern "~:@(%~A~A%~)" library-name op-name))))

(defun make-sparql-function-lambda-list (args)
  (loop for item in args collect (cond ((symbolp item) item) (t (first item)))))

(defun collect-ignorable (lambda-list)
  (loop for item in lambda-list
	for prev-optional-p = nil then (or optionalp prev-optional-p)
	for optionalp = (eq item '&optional)
        when prev-optional-p collect (if (symbolp item) item (car item))))

(defun arg-names-from-sparql-function-args-spec (lambda-list)
  (loop for item in lambda-list
	unless (member item '(&optional &rest))
	collect (if (symbolp item) item (car item))))

(defun sparql-function-inner-and-outer-args-spec-compatible-p (inner-args-spec outer-args-spec)
  (and (= (length inner-args-spec) (length outer-args-spec))
       (every #'(lambda (outer-item inner-item)
		  (or (equal outer-item inner-item)
		      (and (consp outer-item) (consp inner-item)
			   (= (length outer-item) (length inner-item))
			   (subtypep (second inner-item) (second outer-item)))))
	      outer-args-spec inner-args-spec)))

(defun make-sparql-function-body (name outer-args-spec specs)
  (let ((outer-arg-names (arg-names-from-sparql-function-args-spec outer-args-spec))
	(true-condition-found-p nil)
	(default-body nil)
	(default-body-present-p nil))
    ;; (inform "make-sparql-function-body ~S outer-arg-names = ~S" name outer-arg-names)
    (when (eq (car (car (last specs))) :default)
      (setf default-body-present-p t)
      (setf default-body (cdr (car (last specs))))
      (setf specs (butlast specs)))
    (list `(cond ,@(loop for (method-sym inner-args-spec . body) in specs
			 for inner-lambda-list = (make-sparql-function-lambda-list inner-args-spec)
			 unless (eq method-sym :method)
			 do (error* "Malformed method spec ~A" specs)
			 unless (sparql-function-inner-and-outer-args-spec-compatible-p inner-args-spec outer-args-spec)
			 do (error* "~A: Incompatible lambda lists ~A and ~A" name outer-args-spec inner-args-spec)
			 collect (loop with test-lambda-list = nil
				       with test-forms = nil
				       with test-actuals = nil
				       with body-lambda-list = nil
				       for optional-seen-p = nil then (or optional-seen-p (eq inner-item '&optional))
				       for rest-seen-p = nil then (or rest-seen-p (eq inner-item '&rest))
				       for inner-item in inner-args-spec
				       for outer-item in outer-args-spec
				       do (cond ((member inner-item '(&optional))
						 (push-to-end inner-item test-lambda-list)
						 (push-to-end inner-item body-lambda-list))
						((symbolp inner-item)
						 (push-to-end inner-item body-lambda-list))
						((and optional-seen-p (not rest-seen-p))
						 (push-to-end (first inner-item) test-lambda-list)
						 (push-to-end `(or (not ,(first inner-item)) (typep ,(first inner-item) ',(second inner-item))) test-forms)
						 (push-to-end (first inner-item) body-lambda-list)
						 (push-to-end (first outer-item) test-actuals))
						(t
						 (push-to-end (first inner-item) test-lambda-list)
						 (push-to-end `(typep ,(first inner-item) ',(second inner-item)) test-forms)
						 (push-to-end (first inner-item) body-lambda-list)
						 (push-to-end (first outer-item) test-actuals)))
				       finally (progn
						 (when (null test-forms)
						   (cond ((not (null default-body-present-p))
							  (error* "~A: cannot have a :default definition and a null test in the same op" name))
							 ((not true-condition-found-p)
							  (setf true-condition-found-p t))
							 (t
							  (error* "~A: Two methods yield an empty argument type test (T)!" name))))
						 (let ((op-lambda-call `(,@(if rest-seen-p '(apply))
									   (lambda (,@body-lambda-list) ,@body) ,@outer-arg-names)))
						   (return (if (null test-forms)
							       `(t ,op-lambda-call)
							       `((,@(if rest-seen-p '(apply))
								    (lambda (,@test-lambda-list)
								      (and ,@test-forms))
								    ,@test-actuals)
								 ,op-lambda-call)))))))
		 ,@(cond ((not (null default-body-present-p))
			  (list `(t ,@default-body)))
			 ((not true-condition-found-p)
			  (list `(t
				  ,(let ((format (case (length outer-arg-names)
						   (0 (format nil "~A: Cannot apply to zero arguments" name))
						   (1 (format nil "~A: Cannot apply to argument ~~S" name))
						   (t (format nil "~A: Cannot apply to arguments ~{~A~^ ~}" name (loop repeat (length outer-arg-names) collect "~S"))))))
					`(signal-sparql-error ,format ,@outer-arg-names))))))))))

(defmacro define-xsd-value-type (short-name-string spec)
  (let* ((short-name (intern-instans (string-downcase short-name-string)))
	 (xsd-value-type-prefix "http://www.w3.org/2001/XMLSchema#")
	 (lisp-type (fmt-intern "~:@(XSD-~A-VALUE~)" short-name))
	 (value-parser (fmt-intern "~:@(PARSE-XSD-~A~)" short-name))
	 (descriptor-var (fmt-intern "*~:@(XSD-~A-VALUE-TYPE-DESCRIPTOR~)*" short-name))
	 (iri-var (fmt-intern "*~:@(XSD-~A-IRI~)*" short-name))
	 (iri-string-var (fmt-intern "*~:@(XSD-~A-IRI-STRING~)*" short-name))
	 (iri-string (format nil "~A~A" xsd-value-type-prefix short-name-string)))
    `(progn 
       (deftype ,lisp-type () ',spec)
       (defvar ,iri-string-var)
       (defvar ,iri-var)
       (defvar ,descriptor-var)
       (setf ,iri-string-var ,iri-string)
       (setf ,iri-var (make-instance 'rdf-iri :string ,iri-string-var))
       (setf ,descriptor-var (make-instance 'type-descriptor :iri ,iri-var :iri-string ,iri-string-var :lisp-type ',lisp-type :value-parser #',value-parser))
       (setf (gethash ,iri-string-var (type-descriptors-string-map *xsd-value-type-descriptors*)) ,descriptor-var)
       )))

(defun split-sparql-op-prefixed-name (name)
  (let ((index (position #\: name)))
    (cond ((null index) (values "" name))
	  (t (values (subseq name 0 index) (subseq name (1+ index)))))))

(defmacro define-sparql-op (kind prefixed-name-string (&key arguments returns hiddenp) &body body)
  (multiple-value-bind (library-name op-name)
      (split-sparql-op-prefixed-name prefixed-name-string)
    (let* ((lisp-name (make-sparql-op-name library-name op-name))
	   (lambda-list (make-sparql-function-lambda-list arguments))
	   (ignorable (collect-ignorable lambda-list)))
      `(progn
	 (defun ,lisp-name (,@lambda-list)
	   (declare (special *instans*))
	   ,@(if ignorable (list `(declare (ignorable ,@ignorable))))
	   ,@body)
	 (add-sparql-op :kind ',kind :prefixed-name-string ,prefixed-name-string :lisp-name ',lisp-name :arguments ',arguments :returns ',returns :body ',body :hiddenp ,hiddenp)))))

(defmacro define-sparql-function (prefixed-name-string (&key arguments returns hiddenp) &body methods)
  `(define-sparql-op sparql-function ,prefixed-name-string (:arguments ,arguments :returns ,returns :hiddenp ,hiddenp)
					;     (inform "(~S ~{~A~^ ~})" ,prefixed-name-string (list ,@(loop for arg in arguments when (consp arg) collect (first arg) else when (not (member arg '(&rest &optional))) collect arg)))
     ;; (inform "~A ~A" ,prefixed-name-string (list ,@(loop for arg in arguments when (and (symbolp arg) (not (member arg '(&optional &rest)))) collect arg
     ;; 							else when (not (symbolp arg)) collect (first arg))))
     ,@(make-sparql-function-body prefixed-name-string arguments methods)))

;; (defmacro define-sparql-form (prefixed-name-string (&key arguments returns hiddenp) &body body)
;;   `(define-sparql-op sparql-form ,prefixed-name-string (:arguments ,arguments :returns ,returns :hiddenp ,hiddenp) ,@body))

(defmacro define-sparql-macro (prefixed-name-string (&rest lambda-list) &body body)
  (multiple-value-bind (library-name op-name)
      (split-sparql-op-prefixed-name prefixed-name-string)
    (let* ((lisp-name (make-sparql-op-name library-name op-name)))
      `(progn
	 (defmacro ,lisp-name (,@lambda-list)
	   ,@body)
	 (add-sparql-op :kind 'sparql-macro :prefixed-name-string ,prefixed-name-string :lisp-name ',lisp-name :arguments ',lambda-list :body ',body :hiddenp nil)))))

(defmacro define-sparql-two-string-function (op-name return-type string-operation)
  (let ((prefixed-name-string (format nil "~(~A~)" op-name)))
    `(define-sparql-function ,prefixed-name-string (:arguments ((arg1 literal-or-string) (arg2 literal-or-string)) :returns ,return-type)
       (:method ((arg1 xsd-string-value) (arg2 xsd-string-value)) (,string-operation arg1 arg2))
       (:method ((arg1 xsd-string-value) (arg2 rdf-literal))
	 (cond ((not (rdf-literal-lang arg2)) (signal-sparql-error "~A: Arg2 type ~A not compatible" ',op-name (rdf-literal-type arg2)))
	       (t (,string-operation arg1 (rdf-literal-string arg2)))))
       (:method ((arg1 rdf-literal) (arg2 xsd-string-value))
	 (cond ((not (rdf-literal-lang arg1))
		(signal-sparql-error "~A: Arg1 type ~A not compatible" ',op-name (rdf-literal-type arg1)))
	       (t 
		,(if (eq return-type 'literal-or-string)
		     `(make-instance 'rdf-literal :string (,string-operation (rdf-literal-string arg1) arg2) :lang (rdf-literal-lang arg1))
		     `(,string-operation (rdf-literal-string arg1) arg2)))))
       (:method ((arg1 rdf-literal) (arg2 rdf-literal))
	 (cond ((not (rdf-literal-lang arg1))
		(signal-sparql-error "~A: Arg1 type ~A not compatible" ',op-name (rdf-literal-type arg1)))
	       ((not (rdf-literal-lang arg2))
		(signal-sparql-error "~A: Arg2 type ~A not compatible" ',op-name (rdf-literal-type arg2)))
	       ((not (string= (rdf-literal-lang arg1) (rdf-literal-lang arg2)))
		(signal-sparql-error "~A: Incompatible lang tags in ~A and ~A" ',op-name arg1 arg2))
	       (t
		,(if (eq return-type 'literal-or-string)
		     `(make-instance 'rdf-literal :string (,string-operation (rdf-literal-string arg1) (rdf-literal-string arg2)) :lang (rdf-literal-lang arg1))
		     `(,string-operation (rdf-literal-string arg1) (rdf-literal-string arg2)))))))))

(defmacro current-instans ()
  `*instans*)

(defmacro sparql-call (name &rest args)
  (multiple-value-bind (library-name op-name)
      (split-sparql-op-prefixed-name name)
    `(,(make-sparql-op-name library-name op-name) ,@args)))

(defmacro incompatible-types-error (op a b)
  `(signal-sparql-error "~S: Types not compatible: ~S and ~S" ,(format nil "~A" op) ,a ,b))

(defvar *sparql-unbound*)

(defmacro sparql-unbound ()
  `*sparql-unbound*)

(defvar *sparql-distinct*)

(defmacro sparql-distinct ()
  `*sparql-distinct*)
