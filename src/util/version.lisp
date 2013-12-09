;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun instans-version ()
  (let ((hit (asdf::system-registered-p "instans")))
    (and hit (slot-value (cdr hit) 'asdf::version))))
