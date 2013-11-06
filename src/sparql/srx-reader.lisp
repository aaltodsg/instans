;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun parse-srx-from-url (iri)
  (let* ((input-name (rdf-iri-string iri))
	 (data (drakma:http-request input-name))
	 (string (cond ((stringp data) data) (t (coerce (mapcar #'code-char (coerce data 'list)) 'string)))))
    (with-input-from-string (stream string)
      (parse-srx-stream stream :filename (rdf-iri-string iri)))))

(defun parse-srx-file (filename)
  (with-open-file (stream filename)
    (parse-srx-stream stream :filename filename)))

(defun parse-srx-stream (stream &key filename)
  (declare (special *sparql-var-factory*))
  (unless (boundp '*sparql-var-factory*) ;; For testing
    (initialize-uniquely-named-object-factories))
  (let ((sparql-iri (parse-iri "http://www.w3.org/2005/sparql-results#sparql"))
	(head-iri (parse-iri "http://www.w3.org/2005/sparql-results#head"))
	(variable-iri (parse-iri "http://www.w3.org/2005/sparql-results#variable"))
	(link-iri (parse-iri "http://www.w3.org/2005/sparql-results#link"))
	(results-iri (parse-iri "http://www.w3.org/2005/sparql-results#results"))
	(result-iri (parse-iri "http://www.w3.org/2005/sparql-results#result"))
	(binding-iri (parse-iri "http://www.w3.org/2005/sparql-results#binding"))
	(uri-iri (parse-iri "http://www.w3.org/2005/sparql-results#uri"))
	(literal-iri (parse-iri "http://www.w3.org/2005/sparql-results#literal"))
	(boolean-iri (parse-iri "http://www.w3.org/2005/sparql-results#boolean")))
    (labels ((srx-error (fmt &rest args)
	       (error* (format nil (concatenate 'string "~A: " fmt)) filename args))
	     (tr (x)
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
	       (unless (and (= (length args) 3)
			    (every #'(lambda (x) (or (sparql-var-p x) (sparql-link-p x))) (second args)))
		 (srx-error "Illegal query results:~%translated ~S~%parsed     ~S" args orig-form))
	       (let* ((variables (filter #'sparql-var-p (second args)))
		      (links (filter #'sparql-link-p (second args)))
		      (head-args nil))
		 (when variables (setf head-args (list :variables variables)))
		 (when links (setf head-args (append head-args (list :links links) head-args)))
		 (cond ((member (third args) '(T NIL))
			(apply #'make-instance 'sparql-query-results :boolean (third args) head-args))
		       ((every #'sparql-result-p (third args))
			(apply #'make-instance 'sparql-query-results :results (third args) head-args))
		       (t (srx-error "Illegal query results:~%translated ~S~%parsed     ~S" args orig-form)))))
	     (translate-head (args orig-form)
	       (unless (every #'(lambda (x) (or (sparql-var-p x) (sparql-link-p x))) (rest args)) (srx-error "Illegal head:~%translated ~S~%parsed     ~S" args orig-form))
	       (rest args))
	     (translate-variable (args orig-form)
	       (let ((name-elem (first (first args))))
		 (unless (string= (first name-elem) "name") (srx-error "Illegal variable:~%translated ~S~%parsed     ~S" args orig-form))
		 (make-sparql-var (format nil "?~A" (string-upcase (second name-elem))))))
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
		   (create-sparql-binding (make-sparql-var (format nil "?~A" (string-upcase name))) value))))
	     (translate-iri (args orig-form)
	       (cond ((null (first args))
		      (parse-iri (second args)))
		     (t (srx-error "Illegal format uri:~%translated ~S~%parsed     ~S" args orig-form))))
	     (translate-literal (args orig-form)
	       (let ((datatype/lang-item (first args))
		     (value (second args)))
		 (cond ((null datatype/lang-item)
			value)
		       ((equal (first (first datatype/lang-item)) "datatype")
			(create-rdf-literal-with-type value (parse-iri (second (first datatype/lang-item)))))
		       ((equal (first (first datatype/lang-item)) "lang")
			(create-rdf-literal-with-lang value (second datatype/lang-item)))
		       (t (srx-error "Illegal format literal:~%translated ~S~%parsed     ~S" args orig-form)))))
	     (translate-boolean (args orig-form)
	       (cond ((null (first args))
		      (parse-xsd-boolean (second args)))
		     (t (srx-error "Illegal format boolean:~%translated ~S~%parsed     ~S" args orig-form)))))
      (let* ((parsed (xmls:parse stream))
	     (translated (tr parsed)))
	translated))))

(defun parse-srx-files (pathname-pattern)
  (loop for file in (directory pathname-pattern)
	for tr = (parse-srx-file file)
        do (inform "File ~A" file)
        when tr
	do (cond ((slot-boundp tr 'boolean)
		  (inform "Boolean value ~A" (sparql-query-results-solutions tr)))
		 ((slot-boundp tr 'results)
		  (loop for s in (sparql-query-results-solutions tr)
			do (inform "Solution ~A" s)))
		 (t (inform "No boolean nor results")))))
