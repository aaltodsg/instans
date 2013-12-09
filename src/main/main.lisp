;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;(save-lisp-and-die "executable" :toplevel 'main :executable t)

(defun main ()
  (flet ((usage (stream)
	   (format stream "Usage: instans [options]~%")
	   (format stream "Common options:~%")
	   (format stream "-h or --help                                           Print this message and exit.~%")
	   (format stream "-v or --version                                        Print version information and exit.~%")
	   (format stream "-r <file or url> or --rules <file or url>              Use rules in <file or url>.~%")
	   (format stream "-t <file or url> or --triples <file or url>            Input all triples in <file or url>.~%")
	   (format stream "-i <url> or --input-stream <url>                       Input triples contiuously from stream.~%")
	   (format stream "-o <file or url> or --output-stream <file or url>      Write output to stream.~%")
	   (format stream "-m <file or url> or --manifest <file or url>           Run instans using the configuration in <file or url>~%")))
    (loop for arg in sb-ext:*posix-argv*
	  when (member arg '("-h" "--help") :test #'equalp)
	  do (progn
	       (usage t)
	       (return-from main nil))
	  when (member arg '("-v" "--version") :test #'equalp)
	  do (format t "INSTANS version ~A~%" (instans-version)))))

