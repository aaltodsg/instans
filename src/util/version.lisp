;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defvar *version* "0.4.0.5")
(defvar *version-datetime* "2016-07-08T00:26:22")

(defun instans-version ()
  (let ((hit (asdf::system-registered-p "instans")))
    (and hit (slot-value (cdr hit) 'asdf::version))))

(defun instans-license ()
  (let ((hit (asdf::system-registered-p "instans")))
    (and hit (slot-value (cdr hit) 'asdf::licence))))  ;;; Licence!

(defun instans-author ()
  (let ((hit (asdf::system-registered-p "instans")))
    (and hit (slot-value (cdr hit) 'asdf::author))))

(defun instans-description ()
  (let ((hit (asdf::system-registered-p "instans")))
    (and hit (slot-value (cdr hit) 'asdf::description))))

