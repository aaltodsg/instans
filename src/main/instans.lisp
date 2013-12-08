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
    (loop for arg in sb-ext:*posix-argv*
	  when (member arg '("-h" "--help") :test #'equalp)
	  do (progn
	       (usage t)
	       (return-from main nil))
	  when (member arg '("-v" "--version") :test #'equalp)
	  do (format t "INSTANS version ~A~%" (instans-version)))))

;(save-lisp-and-die "executable" :toplevel 'main :executable t)
