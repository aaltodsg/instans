;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; New version starts

(define-class rete-input-processor ()
  ((instans :accessor rete-input-processor-instans :initarg :instans)
   (operations :accessor rete-input-processor-operations :initarg :operations)
   (base :accessor rete-input-processor-base :initarg :base)
   (graph :accessor rete-input-processor-graph :initarg :graph)
   (subscribe :accessor rete-input-processor-subscribe :initarg :subscribe :initform nil)))

(define-class rete-stream-input-processor (rete-input-processor)
  ((input-policy :accessor rete-stream-input-processor-input-policy :initarg :input-policy)
   (parser :accessor rete-stream-input-processor-parser :initarg :parser)))

(define-class rete-agent-input-processor (rete-input-processor)
  ((agent :accessor rete-agent-input-agent :initarg :agent)))

(define-class rete-output-processor () 
  ((output-name :accessor rete-output-processor-output-name :initarg :output-name :initform nil)
   (output-writers :accessor rete-output-processor-output-writers :initarg :output-writers)))

(define-class rete-output-writer () ())

(define-class rete-stream-output-writer (rete-output-writer)
  ((stream :accessor rete-stream-output-writer-stream :initarg :stream :initform nil)))

(define-class rete-agent-output-writer (rete-output-writer)
  ((agents :accessor rete-agent-output-writer-agents :initarg :agents :initform nil)))

(define-class rete-construct-output-processor (rete-output-processor) ())

(define-class rete-n-statement-output-processor (rete-construct-output-processor)
  ((statements :accessor rete-n-statement-output-processor-statements :initform nil)
   (tail :accessor rete-n-statement-output-processor-tail :initform nil)))

(define-class rete-nt-output-processor (rete-n-statement-output-processor) ())
(define-class rete-nq-output-processor (rete-n-statement-output-processor) ())

(define-class rete-trig-output-processor (rete-construct-output-processor)
  ((current-graph :accessor rete-trig-output-processor-current-graph :initform nil)
   (current-subject :accessor rete-trig-output-processor-current-subject :initform nil)
   (spol-trie :accessor rete-trig-output-processor-spol-trie :initform nil)))

(define-class rete-turtle-output-processor (rete-trig-output-processor) ())

(defgeneric add-statement (rete-construct-output-processor s p o &optional g)
  (:method ((this rete-n-statement-output-processor) s p o &optional g)
    (when (typep this 'rete-nt-output-processor)
      (assert* (null g) "Non-null graph in N-Triples output: ~A" g))
    (cond ((null (rete-n-statement-output-processor-statements this))
	   (setf (rete-n-statement-output-processor-statements this) (list (list s p o g)))
	   (setf (rete-n-statement-output-processor-tail this) (rete-n-statement-output-processor-statements this)))
	  (t
	   (setf (cdr (rete-n-statement-output-processor-tail this)) (list (list s p o g)))
	   (setf (rete-n-statement-output-processor-tail this) (cdr (rete-n-statement-output-processor-tail this))))))
  (:method ((this rete-trig-output-processor) s p o &optional g)
    (when (typep this 'rete-turtle-output-processor)
      (assert* (null g) "Non-null graph in Turtle output: ~A" g))
    (when (null (rete-trig-output-processor-spol-trie this))
      (setf (rete-trig-output-processor-spol-trie this) (make-instance 'trie)))
    (trie-add-path (rete-trig-output-processor-spol-trie this) (list g s p o))))

(defgeneric flush-construct-output (rete-construct-output-processor)
  (:method ((this rete-n-statement-output-processor))
    (loop for writer in (rete-output-processor-output-writers this)
	  do (write-rete-output-statements writer (rete-n-statement-output-processor-statements this)))
    (setf (rete-n-statement-output-processor-statements this) nil)
    (setf (rete-n-statement-output-processor-tail this) nil))
  (:method ((this rete-trig-output-processor))
    (loop for writer in (rete-output-processor-output-writers this)
	  do (write-rete-output-trie writer (rete-trig-output-processor-spol-trie this)))
    (setf (rete-trig-output-processor-spol-trie this) nil)))

(defun agent-send (&rest args)
  (declare (ignorable args))
  nil)

(defgeneric write-rete-output-statements (rete-output-writer statements)
  (:method ((this rete-stream-output-writer) statements)
    (loop for (s p o g) in statements
	 do (format (rete-stream-output-writer-stream this) "~&~A ~A ~A~@[ ~A~]~%" s p o g)))
  (:method ((this rete-agent-output-writer) statements)
    (loop for agent in (rete-agent-output-writer-agents this)
	  do (agent-send agent statements))))

(defgeneric write-rete-output-trie (rete-output-writer trie)
  (:method ((this rete-stream-output-writer) trie)
    (let ((stream (rete-stream-output-writer-stream this))
	  (indent 0)
	  (init-sep ""))
      (loop for (g . s-trie) in (trie-level trie)
	    do (let (g-string)
		 (when g
		   (setf g-string (sparql-value-to-string g))
		   (format stream "~%~A {" g-string)
		   (incf indent (+ (length g-string) 3))
		   (setf init-sep " "))
		 (loop for (s . p-trie) in (trie-level s-trie)
		       for s-sep = init-sep then (format nil "~%~V@T" indent)
		       for s-string = (sparql-value-to-string s)
		       do (format stream "~A~A" s-sep s-string)
		       do (incf indent (length s-string))
		       do (loop for (p . o-level) in (trie-level p-trie)
				for p-string = (sparql-value-to-string p)
				for p-sep = "" then (format nil ";~%~V@T" indent)
				do (format stream "~A ~A" p-sep p-string)
				do (incf indent (length p-string))
				do (loop for (o) in o-level
					 for o-sep = "" then (format nil ",~%~V@T" indent)
					 do (format stream "~A ~A" o-sep (sparql-value-to-string o)))
				do (incf indent (- (length p-string))))
			 do (incf indent (- (length s-string)))
			 do (format stream " .~%"))))))
  (:method ((this rete-agent-output-writer) trie)
    (let* ((msg (list nil))
	   (tail msg))
      (trie-map trie #'(lambda (path) (setf (cdr tail) (list (second path) (third path) (fourth path) (first path)))))
      (loop for agent in (rete-agent-output-writer-agents this)
	    do (agent-send agent msg)))))
;;; New version ends

