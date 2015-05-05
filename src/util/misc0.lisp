;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun error* (fmt &rest args)
  (error (apply #'format nil fmt args)))

(defun quotify-list (l)
  (mapcar #'(lambda (x) (if (symbolp x) (list 'quote x) x)) l))

(defun intern-instans (name)
  (intern name :instans))

(defun intern-keyword (name)
  (intern name :keyword))

(defun fmt-intern (fmt &rest args)
  (intern (apply #'format nil fmt args) :instans))

;;; Used by define-class
(defun predicate-name (name)
  (cond ((find #\- (coerce (string name) 'list))
	 (fmt-intern "~:@(~A-p~)" name))
	(t
	 (fmt-intern "~:@(~Ap~)" name))))

