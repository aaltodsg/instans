;;; -*- Mode: Lisp -*-

(in-package #:instans)

(defmacro generate-ll1-parser (name (&rest keys &key &allow-other-keys) &body rules)
  (initialize-nonterminal-generator :named-linear)
  (let* ((parser (apply #'generate-ll1-grammar name 'll-parser rules :warn-about-transformations-p t keys))
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
