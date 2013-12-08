;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun find-instans-root-directory ()
  (loop for path in asdf:*central-registry*
        for parts = (pathname-directory path)
        for last-two = (nthcdr (- (length parts) 2) parts)
	when (equalp last-two (list "instans" "src"))
	return (probe-file (format nil "~A/.." (namestring path)))
	finally (return nil)))

(defun find-make-rete-html-script ()
  (let ((root (find-instans-root-directory)))
    (and root (probe-file (concatenate 'string (namestring root) "scripts/make-rete-html-page.sh")))))
