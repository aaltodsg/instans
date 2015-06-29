;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun my-hash-table-test (a b) (eql a b))

(defun my-hash-function (x) (cond ((numberp x) 0)
				  ((listp x) 1)
				  (t 2)))

(sb-ext:define-hash-table-test my-hash-table-test my-hash-function)

(defun my-hash-table-experiment (n)
  (let ((table (make-hash-table :test #'my-hash-table-test)))
    (loop for i from 0 below n
	  for item = (gethash i table)
	  when item
	  do (format t "~&!!!Got item = ~S~%" item)
	  else do (setf (gethash i table) (list i)))
    (loop for i from 0 below n
	  for item = (gethash i table)
	  do (format t "~&Got item = ~S~%" item)
	  do (setf (cdr item) (list i)))
    (loop for i from 0 below n
	  for item = (gethash i table)
	  do (format t "~&Got item = ~S~%" item))))
    

