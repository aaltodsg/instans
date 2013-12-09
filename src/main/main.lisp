;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;(save-lisp-and-die "executable" :toplevel 'main :executable t)

(defun parse-manifest (manifest)
  (declare (ignorable manifest))
  (format *error-output* "~%Reading manifest ~A~%" manifest)
  nil)

(defun main ()
  (let ((args sb-ext:*posix-argv*)
	(configuration nil)
	(notifications *error-output*))
    (labels ((usage ()
	       (format notifications "Usage: instans [ -h | --help | -v | --version | ( -m | --manifest <file or url> ) | { <configuration-option> } ]~%~%")
	       (format notifications "Options:~%")
	       (format notifications "  -h or --help                                           Print this message and exit.~%")
	       (format notifications "  -v or --version                                        Print version information and exit.~%")
	       (format notifications "  -m <file or url> or --manifest <file or url>           Run instans using the configuration in <file or url>~%")
	       (format notifications "~%Configuration options:~%")
	       (format notifications "  -r <file or url> or --rules <file or url>              Use rules in <file or url>.~%")
	       (format notifications "  -t <file or url> or --triples <file or url>            Input all triples in <file or url>.~%")
	       (format notifications "  -i <url> or --input-stream <url>                       Input triples contiuously from stream.~%")
	       (format notifications "  -o <file or url> or --output-stream <file or url>      Write output to <file or url>.~%")
	       (return-from main nil))
	     (simple-option (&rest values)
	       ;; (inform "simple-option arg=~S, values=~S" (first args) values)
	       (if (member (first args) values :test #'equalp)
		   (progn (pop args) t)))
	     (value-option (&rest values)
	       (if (member (first args) values :test #'equalp)
		   (progn (pop args) (or args (usage))))))
      (pop args) ; Program path
      (when (equalp (first args) "--end-toplevel-options") (pop args)) ; Inserted by wrapper script
      ;; (inform "args = ~S~%" args)
      (cond ((or (null args) (simple-option "-h" "--help"))
	     (usage))
	    ((simple-option "-v" "--version")
	     (format t "INSTANS version ~A~%" (instans-version))
	     (return-from main nil))
	    ((value-option "-m" "--manifest")
	     (when (cddr args) (usage))
	     (setf configuration (parse-manifest (first args))))
	    (t
	     (loop while args
		   do (cond ((value-option "-r" "--rules")
			     (push-to-end (list :rules (pop args)) configuration))
			    ((value-option "-t" "--triples")
			     (push-to-end (list :triples (pop args)) configuration))
			    ((value-option "-i" "--input-stream")
			     (push-to-end (list :input-stream (pop args)) configuration))
			    ((value-option "-o" "--output-stream")
			     (push-to-end (list :output-stream (pop args)) configuration))
			    (t
			     (format notifications "Illegal option ~A~%" (first args))
			     (usage))))))
      (format notifications "Configuration:~%~{~{~S~^ ~}~^~%~}~%" configuration))))
	 
	  

