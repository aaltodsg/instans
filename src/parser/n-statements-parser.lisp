;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun make-n-statements-parser (instans input-stream &key base graph input-type triple-callback block-callback document-callback subscribe)
  (declare (ignorable base))
  (assert* (member input-type '(:n-triples :n-quads)) "Unknown type ~A" input-type)
  ;; (when base (warn "make-n-statements-parser: Ignoring base"))
  ;; (when (and graph (eq input-type :nquads)) (warn "make-n-statements-parser: Ignoring graph with N-Quads"))
  (when block-callback
    (setf triple-callback #'(lambda (&rest args) (funcall block-callback (list args))))
    (setf block-callback nil))
  (let* ((lexer (make-instance 'n-statements-lexer :input-stream input-stream :instans instans))
	 (document-statements (list nil))
	 (document-statements-last document-statements))
    (flet ((emit (s p o &optional g)
;	     (inform "emit ~S ~S ~S ~S" s p o g)
	     (incf *triple-count*)
	     (let ((new (case input-type
			  (:n-triples
			   (when g
			     (ll-parser-failure "~A parser does not allow any quad ~S ~S ~S ~S" input-type s p o g))
			   (list s p o graph))
			  (:n-quads (list s p o g)))))
	       (when document-callback
		 (setf (cdr document-statements-last) (list new))
		 (setf document-statements-last (cdr document-statements-last)))
	       (when triple-callback (apply triple-callback new)))))
      (let ((parser (generate-ll1-parser statement ()
		      (nquadsDoc  ::= ((:OPT statement) (:REP0 (EOL-TOKEN statement)) :RESULT (if document-callback (funcall document-callback (rest document-statements))))) ;;; Note: we eat the last EOL-TOKEN at the same time with :eof
		      (statement  ::= (subject predicate object (:OPT graphLabel) .-TERMINAL :RESULT (if (opt-yes $3) (emit $0 $1 $2 (opt-value $3)) (emit $0 $1 $2))))
		      (graphLabel ::= (:OR IRIREF-TERMINAL BlankNode))
		      (subject    ::= (:OR IRIREF-TERMINAL BlankNode))
		      (predicate  ::= (IRIREF-TERMINAL))
		      (object     ::= (:OR IRIREF-TERMINAL BlankNode literal))
		      (BlankNode  ::= (BLANK_NODE_LABEL-TERMINAL :RESULT (make-rdf-blank-node instans $0)))
		      (literal    ::= (STRING_LITERAL_QUOTE-TERMINAL
				       (:OPT (:OR (LANGTAG-TERMINAL :RESULT #'(lambda (s) (create-rdf-literal-with-lang s (subseq $0 1))))
						  (^^-TERMINAL IRIREF-TERMINAL :RESULT #'(lambda (s) (nth-value 0 (create-rdf-literal-with-type s $1))))))
				       :RESULT (if (opt-yes-p $1) (funcall (opt-value $1) $0) $0))))))
	(setf (ll-parser-lexer parser) lexer)
	(setf (ll-parser-subscribe parser) subscribe)
	parser))))

(defun testaus (&rest keys &key &allow-other-keys)
  (initialize-nonterminal-generator :named-linear)
  (let* ((rules
	  '((doc ::= ((:OPT statement) (:REP0 (EOL-TOKEN statement))))))
	 (parser (apply #'generate-ll1-grammar 'testaus 'll-parser rules :warn-about-transformations-p t keys)))
    parser))

(defun make-n-triples-parser (instans input-stream &rest keys &key base graph input-type triple-callback block-callback document-callback subscribe)
  (declare (ignorable  base graph input-type triple-callback block-callback document-callback subscribe))
  (apply #'make-n-statements-parser instans input-stream :input-type :n-triples keys))

(defun make-n-quads-parser (instans input-stream &rest keys &key base graph input-type triple-callback block-callback document-callback subscribe)
  (declare (ignorable  base graph input-type triple-callback block-callback document-callback subscribe))
  (apply #'make-n-statements-parser instans input-stream :input-type :n-quads keys))
