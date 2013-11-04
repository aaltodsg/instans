;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-output-function inform)

(defun error* (fmt &rest args)
  (error (apply #'format nil fmt args)))

(defun quotify-list (l)
  (mapcar #'(lambda (x) (if (symbolp x) (list 'quote x) x)) l))

(defun list-union (list1 list2 &key (test #'eql))
  (cond ((null list1) list2)
	((null list2) list1)
	(t
	 (append list1 (filter #'(lambda (x) (not (member x list1 :test test))) list2)))))

(defun list-intersection (list1 list2 &key (test #'eql))
  (cond ((or (null list1) (null list2)) nil)
	(t
	 (filter #'(lambda (x) (member x list2 :test test)) list1))))

(defun list-difference (list1 list2 &key (test #'eql))
  (filter #'(lambda (x) (not (member x list2 :test test))) list1))

(defun maph (func hash-table)
  (let ((result nil)
	(last nil))
    (maphash #'(lambda (k v)
		 (let ((val (funcall func k v)))
		   (cond ((null result)
			  (setf result (list val))
			  (setf last result))
			 (t
			  (setf (cdr last) (list val))
			  (setf last (cdr last))))))
	     hash-table)
    result))

(defun predicate-name (name)
  (cond ((find #\- (coerce (string name) 'list))
	 (fmt-intern "~:@(~A-p~)" name))
	(t
	 (fmt-intern "~:@(~Ap~)" name))))

(defun camel-case (string)
  (coerce (loop with humpp = nil
		for ch in (coerce string 'list)
		when (alpha-char-p ch)
		collect (cond ((not humpp) (char-downcase ch))
			      (t (setf humpp nil) (char-upcase ch)))
	        else when (char= ch #\-) 
		do (setf humpp t)
		else 
		collect ch)
	  'string))

;;; Char ops accepting nil
(defun char-code* (char-or-code) (if (characterp char-or-code) (char-code char-or-code) char-or-code))

(defun char=* (x y) (and x y (= (char-code* x) (char-code* y))))

(defun digit-char-p* (ch &optional (radix 10)) (and ch (digit-char-p ch radix)))

#+sbcl
(defun shell-script (script &rest args)
  (let ((process (sb-ext:run-program "/bin/sh" (cons script args) :output t :error :output)))
					;(barf "status=~A, exit code=~A~&" (sb-ext:process-status process) (sb-ext:process-exit-code process))
    process))
