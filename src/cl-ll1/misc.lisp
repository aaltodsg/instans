;;; -*- Mode: Lisp -*-

(in-package #:instans)

(defmacro ignoreall (&body body)
  (declare (ignore body))
  nil)

(defun result-var (i)
  (intern (format nil "$~D" i)))

(defvar *generated-nonterminals* nil)
(defvar *gen-counter* 0)
(defvar *numbering-scheme* :prime)

(defun initialize-nonterminal-generator (&optional scheme)
  (setf *generated-nonterminals* nil)
  (setf *gen-counter* 0)
  (when scheme
    (setf *numbering-scheme* scheme)))

(defun make-nonterminal (base &optional item)
  (case *numbering-scheme*
    (:named-linear
     (incf *gen-counter*)
     (let ((name (intern (format nil "~A-~D" base *gen-counter*))))
       (when (member name *generated-nonterminals*)
	 (error* "Something wrong in make-nonterminal: generating the same nonteraminal twice"))
       (push name *generated-nonterminals*)
       name))
    (:linear
     (incf *gen-counter*)
     (let ((name (intern (format nil "_~D" *gen-counter*))))
       (when (member name *generated-nonterminals*)
	 (error* "Something wrong in make-nonterminal: generating the same nonteraminal twice"))
       (push name *generated-nonterminals*)
       name))
    (:composed
     (let ((extension (cond ((member (car item) '(:OR :REP0 :REP1 :OPT)) (car item))
			    ((eq (car (last (butlast item))) :result) :RESULT)
			    (t :SUBSEQ)))
	   (base-previous (assoc base *generated-nonterminals*)))
       (when (null base-previous)
	 (setf base-previous (list base))
	 (push base-previous *generated-nonterminals*))
       (let ((extension-previous (assoc extension (cdr base-previous))))
	 (when (null extension-previous)
	   (setf extension-previous (list extension))
	   (push extension-previous (cdr base-previous)))
	 (let ((name (intern (format nil "~A-~D-in-~A" extension (1- (length extension-previous)) base))))
	   (push name extension-previous)
	   name))))
    (:prime
     (let ((base-previous (assoc base *generated-nonterminals*)))
       (when (null base-previous)
	 (setf base-previous (list base))
	 (push base-previous *generated-nonterminals*))
       (let* ((prime-count (length base-previous))

	      (name (intern (format nil "~A~{~A~}~@['~]" base (loop for i from 1 to (floor prime-count 2) collect #\") (oddp prime-count)))))
	 (when (member name *generated-nonterminals*)
	   (error* "Something wrong in make-nonterminal: generating the same nonterminal twice"))
	 (push name (cdr base-previous))
	 name)))))
