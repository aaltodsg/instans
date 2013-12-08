;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun main ()
  (flet ((usage (stream)
	   (format stream "Usage: instans [options]~%")
	   (format stream "Common options:~%")
	   (format stream "-h or --help                     Print this message and exit.~%")
	   (format stream "-v or --version                  Print version information and exit.~%")
	   (format stream "-m <file> or --manifest <file>   Run instans using the configuration in <file>~%")))
    (usage t)
    (format t "Note: this is not yet implemented~%")))

;(save-lisp-and-die "executable" :toplevel 'main :executable t)