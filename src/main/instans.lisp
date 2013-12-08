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
    (format t "Not implemented yet~%~%")
    (format t "You provided the arguments ~S~%~%" sb-ext:*posix-argv*)
    (usage t)))

;(save-lisp-and-die "executable" :toplevel 'main :executable t)
