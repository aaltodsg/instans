;;; -*- Mode: Lisp -*-

;;; -------------------- Debug messages --------------------

(in-package #:instans)

(proclaim '(optimize safety))

(defvar *dbg-stream* *error-output*)

(defvar *dbg-indent* 0)

(defun dbginc (inc)
  (incf *dbg-indent* inc))

(defvar *dbgp* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *dbgp* nil))

(defmacro dbg (&rest args)
  (cond ((not *dbgp*)
	 nil)
	(t
	 `(format *dbg-stream* (concatenate 'string "~%~V@T" ,(first args)) *dbg-indent* ,@(rest args)))))

(defmacro dbgblock (&body body)
  (cond ((not *dbgp*)
	 nil)
	(t
	 `(progn ,@body))))
