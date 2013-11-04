;;; -*- Mode: Lisp -*-

(in-package #:instans)

(defmacro with-ll1-parser ((parser-name) (&rest rules) &body body)
  (initialize-nonterminal-generator :named-linear)
  (let* ((parser (generate-ll1-parser parser-name rules :warn-about-transformations-p t))
	 (result-ops (loop for p in (parser-productions parser)
			   collect (and (production-result-func-name p) `(,(production-result-func-name p) ,@(cdr (production-result-func-lambda p))))))
	 (parser-var (gensym (format nil "~A-PARSER" parser-name)))
	 (lexer-var (gensym (format nil "~A-LEXER" parser-name)))
	 (parsing-var (gensym (format nil "~A-PARSING" parser-name))))
    `(labels ,(loop for op in result-ops when op collect op)
       (let ((,parser-var
       	      ,(serialize-parser
       		parser
       		:serialize-production #'(lambda (p) (serialize-production p
       									  :ignoring-fields '(:properties :result-arglist :result-func-lambda :result-func-name)
       									  :field-functions (list (list :result-func #'(lambda (p field func)
       															(declare (ignorable field func))
															(let ((fn (production-result-func-name p)))
															  (and fn `#',fn))))))))))
	 (labels ((,parser-name (,lexer-var &rest keys &key &allow-other-keys)
		    (let ((,parsing-var (apply #'make-instance 'parsing
					       :parser ,parser-var
					       :lexer ,lexer-var
					  keys)))
	 	    (ll-parse ,parsing-var))))
	   ,@body)))))
