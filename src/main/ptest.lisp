;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun ptest (&key base graph subscribe)
  (let* ((input-iri 
	  (engine1-iri (parse-iri "http://instans.org/p1"))
	 (engine2-iri "http://instans.org/p2")
	 (engine1 (instans-add-rules engine1-iri "../tests/input/p1.rq" :encode-prefixes-p t))
	 (engine2 (instans-add-rules engine2-iri "../tests/input/p2.rq" :encode-prefixes-p t)))
    (instans-add-query-input-processor engine1-iri 

						     
