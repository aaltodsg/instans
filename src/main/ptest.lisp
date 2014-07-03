;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun ptest ()
  (let ((engine1 (instans-add-rules (parse-iri "http://instans.org/p1") "../tests/input/p1.rq" :encode-prefixes-p t))
	(engine2 (instans-add-rules (parse-iri "http://instans.org/p2") "../tests/input/p2.rq" :encode-prefixes-p t)))
    (
    
