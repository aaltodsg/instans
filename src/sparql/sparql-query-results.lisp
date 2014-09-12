;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class sparql-query-results ()
  ((variables :accessor sparql-query-results-variables)
   (links :accessor sparql-query-results-links)
   (results :accessor sparql-query-results-results)
   (tail :accessor sparql-query-results-tail)
   (boolean :accessor sparql-query-results-boolean)))

(defgeneric set-query-variables (query-results variables)
  (:method ((this sparql-query-results) variables)
    (when (slot-boundp this 'variables)
      (unless (every #'uniquely-named-object-equal (sparql-query-results-variables this) variables)
	(error* "Trying to set different variables ~S to ~S; variables were ~S" variables this (sparql-query-results-variables this))))
    (setf (sparql-query-results-variables this) variables)))

(defgeneric add-sparql-result (query-results &key variables values)
  (:method ((this sparql-query-results) &key (variables nil variables-present-p) values)
    (when variables-present-p (set-query-variables this variables))
    (cond ((slot-boundp this 'results)
	   (setf (cdr (sparql-query-results-tail this)) (list values))
	   (setf (sparql-query-results-tail this) (cdr (sparql-query-results-tail this))))
	  (t
	   (setf (sparql-query-results-results this) (list values))
	   (setf (sparql-query-results-tail this) (sparql-query-results-results this))))))

(defgeneric output-srx (query-results stream)
  (:method ((this sparql-query-results) stream)
    (let ((variables (sparql-query-results-variables this)))
      (xml-emitter:with-xml-output (stream)
	(xml-emitter:with-tag ("sparql" '(("xmlns" "http://www.w3.org/2005/sparql-results#")))
	  (xml-emitter:with-tag ("head")
	    (loop for variable in variables
		  do (xml-emitter:with-simple-tag ("variable" `(("name" ,(format nil "~(~A~)" (subseq (uniquely-named-object-name variable) 1)))))))
	    (when (slot-boundp this 'links)
	      (loop for link in (sparql-query-results-links this)
		    do (xml-emitter:with-simple-tag ("link" `(("href" ,link)))))))
	  (cond ((slot-boundp this 'boolean)
		 (xml-emitter:simple-tag "boolean" (if (sparql-query-results-boolean this) "true" "false")))
		((slot-boundp this 'results)
		 (xml-emitter:with-tag ("results")
		   (loop for values in (sparql-query-results-results this)
			 do (xml-emitter:with-tag ("result")
			      (loop for variable in variables
				    for value in values
				    do (xml-emitter:with-tag ("binding" `(("name" ,(format nil "~(~A~)" (subseq (uniquely-named-object-name variable) 1)))))
					 (cond ((rdf-iri-p value)
						(xml-emitter:simple-tag "uri" (rdf-iri-string value)))
					       ((rdf-literal-p value)
						(cond ((rdf-literal-type value)
						       (xml-emitter:with-tag ("literal") (rdf-literal-string value) (list (list "datatype" (rdf-literal-type value)))))
						      ((rdf-literal-lang value)
						       (xml-emitter:with-tag ("literal") (rdf-literal-string value) (list (list "xml:lang" (rdf-literal-lang value)))))
						      (t
						       (xml-emitter:with-tag ("literal") (rdf-literal-string value)))))
					       ((rdf-blank-node-p value)
						(xml-emitter:with-tag ("bnode") (uniquely-named-object-name value)))
					       (t
						(xml-emitter:with-tag ("literal") (sparql-value-to-string value))))))))))))))))

			     
      
      
    
