;;; -*- Mode: Lisp -*-

(in-package #:instans)

(defmacro with-ll1-rules ((name) (&rest rules) &body body)
  (initialize-nonterminal-generator :named-linear)
  (let* ((parser (generate-ll1-grammar name 'll-parser rules :warn-about-transformations-p t))
	 (result-ops (loop for p in (grammar-productions parser)
			   collect (and (production-result-func-name p) `(,(production-result-func-name p) ,@(cdr (production-result-func-lambda p))))))
	 (parser-var (gensym (format nil "~A-PARSER" name)))
	 (lexer-var (gensym (format nil "~A-LEXER" name))))
;    (describe parser)
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
	 (labels ((,name (,lexer-var &rest keys &key &allow-other-keys)
		    (declare (ignorable keys))
		    (setf (ll-parser-lexer ,parser-var) ,lexer-var)
	 	    (ll-parse ,parser-var)))
	   ,@body)))))
    
(defmacro generate-ll1-parser (name &body rules)
  (initialize-nonterminal-generator :named-linear)
  (let* ((parser (generate-ll1-grammar name 'll-parser rules :warn-about-transformations-p t))
	 (result-ops (loop for p in (grammar-productions parser)
			   collect (and (production-result-func-name p) `(,(production-result-func-name p) ,@(cdr (production-result-func-lambda p)))))))
;    (describe parser)
    `(labels ,(loop for op in result-ops when op collect op)
       ,(serialize-parser
	 parser
	 :serialize-production #'(lambda (p) (serialize-production p
								   :ignoring-fields '(:properties :result-arglist :result-func-lambda :result-func-name)
								   :field-functions (list (list :result-func #'(lambda (p field func)
														 (declare (ignorable field func))
														 (let ((fn (production-result-func-name p)))
														   (and fn `#',fn)))))))))))
