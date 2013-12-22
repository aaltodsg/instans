;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

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

