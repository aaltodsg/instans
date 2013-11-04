;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defvar *chbuf* nil)

;;;  "A character buffer for the lexical analysis"
(define-class chbuf ()
  ((contents :accessor chbuf-contents :initarg :contents)
   (size :accessor chbuf-size :initarg :size)
   (index :accessor chbuf-index :initarg :index)))

(defmethod print-object ((this chbuf) stream)
  (format stream "#<~A ~D/~D: \"~{~C~}\">" (type-of this) (chbuf-index this) (chbuf-size this)
	  (loop for i from 0 below (chbuf-index this) collect (aref (chbuf-contents this) i))))

(defun create-chbuf (&optional (initial-size 200))
  (make-instance 'chbuf :index 0 :size initial-size :contents (make-array initial-size :element-type 'character)))

(defun chbuf-put-char (buf ch)
  (let ((size (chbuf-size buf)))
    (when (= size (chbuf-index buf))
      (let* ((new-size (* 2 size))
	     (contents (chbuf-contents buf))
	     (new-contents (make-array new-size :element-type 'character)))
	(loop for i from 0 below size do (setf (aref new-contents i) (aref contents i)))
	(setf (chbuf-size buf) new-size)
	(setf (chbuf-contents buf) new-contents))))
  (setf (aref (chbuf-contents buf) (chbuf-index buf)) ch)
  (incf (chbuf-index buf))
  buf)

(defun chbuf-put-chars (buf &rest chars)
  (loop for char in chars do (chbuf-put-char buf char))
  buf)

(defun chbuf-drop-last-char (buf)
  (decf (chbuf-index buf)))

(defun chbuf-init (buf &rest chars)
  (setf (chbuf-index buf) 0)
  (apply #'chbuf-put-chars buf chars)
  buf)

(defun empty-chbuf (&rest chars)
  (when (null *chbuf*)
    (setf *chbuf* (create-chbuf)))
  (apply #'chbuf-init *chbuf* chars)
  *chbuf*)

(defun chbuf-string (buf &optional (start 0) end)
  (loop with length = (- (or end (chbuf-index buf)) start)
	with string = (make-string length)
	for i from 0 below length
	do (setf (char string i) (aref (chbuf-contents buf) (+ start i)))
	finally (progn ;(incf *chbuf-string-length* length)
		  (return string))))
