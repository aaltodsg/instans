;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; This should be cleaned and made more compatible with other parsing facilities.

(defun parse-results (instans input &rest keys &key &allow-other-keys)
  (cond ((rdf-iri-p input)
	 (apply #'parse-results-from-url instans input keys))
	((pathnamep input)
	 (apply #'parse-results-file instans (namestring input) keys))
	((stringp input)
	 (cond ((http-or-file-iri-string-p input)
		(apply #'parse-results-from-url instans (parse-iri input) keys))
	       (t
		(apply #'parse-results-file instans input keys))))
	((streamp input)
	 (apply #'parse-results-stream instans input nil keys))
	(t
	 (error* "Cannot parse input from ~A" input))))
	 
(defun parse-results-file (instans filename &rest keys &key &allow-other-keys)
  (with-open-file (stream filename)
    (apply #'parse-results-stream instans stream filename keys)))

(defun parse-results-from-url (instans iri &rest keys &key &allow-other-keys)
  (cond ((string= (rdf-iri-scheme iri) "file")
	 (let ((filename (rdf-iri-path iri)))
	   (inform "Filename ~S" filename)
	   (apply #'parse-results-file instans filename keys)))
	(t
	 (let* ((input-name (rdf-iri-string iri))
		(data (drakma:http-request input-name))
		(string (cond ((stringp data) data) (t (coerce (mapcar #'code-char (coerce data 'list)) 'string)))))
	   (with-input-from-string (stream string)
	     (apply #'parse-results-stream instans stream (rdf-iri-string iri) keys))))))

(defun parse-results-stream (instans stream input-name &rest keys &key &allow-other-keys)
  (let* ((pos (position #\. input-name :from-end t :test #'char=))
	 (input-type (and pos (intern-keyword (string-upcase (subseq input-name (1+ pos)))))))
    (apply (case input-type
	     (:srx #'parse-srx-stream)
;	     (:srj #'parse-srj-stream)
	     (t (error* "Cannot parse ~S. Unknown file type" input-type)))
	   instans stream input-name keys)))

(defun json-object-to-sparql-object (instans jo)
  (let ((type (gethash "type" jo))
	(value (gethash "value" jo)))
    (cond ((equal type "uri")
	   (parse-iri value))
	  ((or (equal type "literal") (equal type "typed-literal"))
	   (let ((lang (gethash "xml:lang" jo))
		 (datatype (gethash "datatype" jo)))
	     (cond ((not (null lang))
		    (create-rdf-literal-with-lang value lang))
		   ((not (null datatype))
		    (create-rdf-literal-with-type value (parse-iri datatype)))
		   (t
		    value))))
	  ((equal type "bnode")
	   (make-named-blank-node instans (concatenate 'string "_:" value)))
	  (t
	   (error* "Cannot transform ~S to a Sparql object" jo)))))

;; (defun parse-srj-stream (instans stream input-name &rest keys &key &allow-other-keys)
;;   (declare (ignorable instans input-name keys))
;;   (let* ((object (yason:parse stream))
;; 	 (head (gethash "head" object))
;; 	 (results (gethash "results" object))
;; 	 (boolean (gethash "boolean" object))
;; 	 (query-results (make-instance 'sparql-query-results)))
;;     (when head
;;       (inform "vars = ~S"
;; 	      (setf (sparql-query-results-variables query-results)
;; 		    (mapcar #'(lambda (name) (make-sparql-var instans (string-upcase (concatenate 'string "?" name)))) (gethash "vars" head)))))
;;     (when boolean
;;       (inform "boolean = ~S"
;; 	      (setf (sparql-query-results-boolean query-results) boolean)))
;;     (when results
;;       (inform "results = ~S" 
;; 	      (setf (sparql-query-results-results query-results)
;; 		    (mapcar #'(lambda (result)
;; 				(maph #'(lambda (k v)
;; 					  (make-instance 'sparql-binding :variable (make-sparql-var instans (string-upcase (concatenate 'string "?" k))) :value (json-object-to-sparql-object instans v)))
;; 				      result))
;; 			    (gethash "bindings" results)))))
;;     query-results))

;; (defun parse-srx-stream (instans stream input-name &rest keys &key &allow-other-keys)
;;   (declare (ignorable instans stream input-name keys))
;;   (error* "Not implemented"))

(defun parse-srx-stream (instans stream input-name &rest keys &key (errors-in-literal-allowed-p t) &allow-other-keys)
  (declare (special *sparql-var-factory*))
  (declare (ignorable keys))
  (let ((sparql-iri (parse-iri "http://www.w3.org/2005/sparql-results#sparql"))
	(head-iri (parse-iri "http://www.w3.org/2005/sparql-results#head"))
	(variable-iri (parse-iri "http://www.w3.org/2005/sparql-results#variable"))
	(link-iri (parse-iri "http://www.w3.org/2005/sparql-results#link"))
	(results-iri (parse-iri "http://www.w3.org/2005/sparql-results#results"))
	(result-iri (parse-iri "http://www.w3.org/2005/sparql-results#result"))
	(binding-iri (parse-iri "http://www.w3.org/2005/sparql-results#binding"))
	(bnode-iri (parse-iri "http://www.w3.org/2005/sparql-results#bnode"))
	(uri-iri (parse-iri "http://www.w3.org/2005/sparql-results#uri"))
	(literal-iri (parse-iri "http://www.w3.org/2005/sparql-results#literal"))
	(boolean-iri (parse-iri "http://www.w3.org/2005/sparql-results#boolean")))
    (labels ((srx-error (fmt &rest args) (apply #'error* (concatenate 'string "~A: " fmt) input-name args))
	     (tr (x)
;	       (inform "tr ~S" x)
	       (cond ((consp x)
		      (cond ((and (stringp (car x)) (stringp (cdr x)))
			     (parse-iri (concatenate 'string (cdr x) (car x))))
			    (t
			     (let ((tr-car (tr (car x)))
				   (tr-cdr (tr (cdr x))))
			       (cond ((rdf-iri-equal tr-car sparql-iri)
				      (translate-sparql-query-results tr-cdr x))
				     ((rdf-iri-equal tr-car head-iri)
				      (translate-head tr-cdr x))
				     ((rdf-iri-equal tr-car variable-iri)
				      (translate-variable tr-cdr x))
				     ((rdf-iri-equal tr-car link-iri)
				      (translate-link tr-cdr x))
				     ((rdf-iri-equal tr-car results-iri)
				      (translate-results tr-cdr x))
				     ((rdf-iri-equal tr-car result-iri)
				      (translate-result tr-cdr x))
				     ((rdf-iri-equal tr-car binding-iri)
				      (translate-binding tr-cdr x))
				     ((rdf-iri-equal tr-car bnode-iri)
				      (translate-bnode tr-cdr x))
				     ((rdf-iri-equal tr-car uri-iri)
				      (translate-iri tr-cdr x))
				     ((rdf-iri-equal tr-car literal-iri)
				      (translate-literal tr-cdr x))
				     ((rdf-iri-equal tr-car boolean-iri)
				      (translate-boolean tr-cdr x))
				     (t
				      (cons tr-car tr-cdr)))))))
		     (t x)))
	     (translate-sparql-query-results (args orig-form)
	       (unless ;(and (= (length args) 3)
			    (every #'(lambda (x) (or (sparql-var-p x) (sparql-link-p x))) (second args))
		 (srx-error "Illegal query results:~%translated ~S~%parsed     ~S" args orig-form))
	       (let* ((variables (filter #'sparql-var-p (second args)))
		      (links (filter #'sparql-link-p (second args)))
		      (head-args nil))
		 (when variables (setf head-args (list :variables variables)))
		 (when links (setf head-args (append head-args (list :links links) head-args)))
					;		 (inform "head-args = ~S" head-args)
		 (cond ((sparql-boolean-result-p (third args))
			(when variables (srx-error "Variables not allowed with a boolean result: ~S" orig-form))
			(apply #'make-instance 'sparql-query-results :boolean (third args) head-args))
		       ((every #'sparql-result-p (third args))
			(let ((results (third args)))
			  (loop for result in results
				for bindings = (sparql-result-bindings result)
				do (loop for binding in bindings
					 for var = (sparql-binding-variable binding)
					 unless (find-sparql-var var variables)
					 do (srx-error "Undefined variable ~A in results" var)))
			  (let ((res (apply #'make-instance 'sparql-query-results :results results head-args)))
			    (setf (sparql-query-results-tail res) (last (sparql-query-results-results res)))
			    res)))
		       (t (srx-error "Illegal query results:~%translated ~S~%parsed     ~S" args orig-form)))))
	     (translate-head (args orig-form)
	       (unless (every #'(lambda (x) (or (sparql-var-p x) (sparql-link-p x))) (rest args))
		 (srx-error "Illegal head:~%translated ~S~%parsed     ~S" args orig-form))
	       (rest args))
	     (translate-variable (args orig-form)
	       (let ((name-elem (first (first args))))
		 (unless (string= (first name-elem) "name") (srx-error "Illegal variable:~%translated ~S~%parsed     ~S" args orig-form))
		 (make-sparql-var instans (format nil "?~A" (string-upcase (second name-elem))))))
	     (translate-link (args orig-form)
	       (let ((href-elem (first (first args))))
		 (unless (string= (first href-elem) "href") (srx-error "Illegal link:~%translated ~S~%parsed     ~S" args orig-form))
		 (create-sparql-link (second href-elem))))
	     (translate-results (args orig-form)
	       (unless (and (null (first args)) (every #'sparql-result-p (rest args))) (srx-error "Illegal results:~%translated ~S~%parsed     ~S" args orig-form))
	       (rest args))
	     (translate-result (args orig-form)
	       (unless (and (null (first args)) (every #'sparql-binding-p (rest args))) (srx-error "Illegal bindings:~%translated ~S~%parsed     ~S" args orig-form))
	       (create-sparql-result (rest args)))
	     (translate-binding (args orig-form)
	       (let ((name-elem (first (first args))))
		 (unless (string= (first name-elem) "name") (srx-error "Illegal binding:~%translated ~S~%parsed     ~S" args orig-form))
		 (let ((name (second name-elem))
		       (value (second args)))
		   (create-sparql-binding (make-sparql-var instans (format nil "?~A" (string-upcase name))) value))))
	     (translate-bnode (args orig-form)
	       (declare (ignorable orig-form))
	       (cond ((null (first args))
		      (make-named-blank-node instans (second args)))
		     (t (srx-error "Illegal format bnode ~S" args))))
	     (translate-iri (args orig-form)
	       (cond ((null (first args))
		      (parse-iri (second args)))
		     (t (srx-error "Illegal format uri:~%translated ~S~%parsed     ~S" args orig-form))))
	     (translate-literal (args orig-form)
	       (declare (ignorable orig-form))
;	       (inform "translate-literal ~A ~A" args orig-form)
	       (let ((datatype/lang-item (first args))
		     (value (second args)))
		 (cond ((null datatype/lang-item)
			(multiple-value-bind (v msg) (parse-xsd-value value)
			  (cond ((null msg)
				 (cond ((and errors-in-literal-allowed-p (equal v "SPARQL-ERROR"))
					(make-instance 'sparql-error :format "PARSED from SRX file"))
				       (t (or v ""))))
				((not errors-in-literal-allowed-p)
				 (srx-error msg))
				(t (make-instance 'sparql-error :format msg :arguments nil)))))
		       ((equal (first (first datatype/lang-item)) "datatype")
			(let ((literal (catch :sparql-error
					 (create-rdf-literal-with-type (or value "") (parse-iri (second (first datatype/lang-item)))))))
			  (cond ((not (sparql-error-p literal)) literal)
				((not errors-in-literal-allowed-p)
				 (srx-error "Illegal format literal: ~S" args))
				(t (make-instance 'sparql-error :format "Illegal format literal ~S" :arguments (list args))))))
		       ((equal (first (first datatype/lang-item)) "lang")
			(create-rdf-literal-with-lang (or value "") (second (first datatype/lang-item))))
		       ((not errors-in-literal-allowed-p)
			(srx-error "Illegal format literal: ~S" args))
		       (t (make-instance 'sparql-error :format "Illegal format literal ~S" :arguments (list args))))))
	     (translate-boolean (args orig-form)
	       (cond ((null (first args))
		      (create-sparql-boolean-result (parse-xsd-boolean (second args))))
		     (t (srx-error "Illegal format boolean:~%translated ~S~%parsed     ~S" args orig-form)))))
      ;; (let* ((parsed (let ((p (xmls:parse stream))) (inform "~%parsed = ~S~%" p) p))
      (let* ((parsed (xmls:parse stream))
	     (translated (tr parsed)))
	translated))))
