;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defvar *version* "0.4.0.8")
(defvar *version-datetime* "2016-07-08T00:35:02")

(defun instans-version ()
  *version*)
  ;; (let ((hit (asdf::system-registered-p "instans")))
  ;;   (and hit (slot-value (cdr hit) 'asdf::version))))

(defun instans-version-datetime ()
  *version-datetime*)

(defun instans-license ()
  (let ((hit (asdf::system-registered-p "instans")))
    (and hit (slot-value (cdr hit) 'asdf::licence))))  ;;; Licence!

(defun instans-author ()
  (let ((hit (asdf::system-registered-p "instans")))
    (and hit (slot-value (cdr hit) 'asdf::author))))

(defun instans-description ()
  (let ((hit (asdf::system-registered-p "instans")))
    (and hit (slot-value (cdr hit) 'asdf::description))))

