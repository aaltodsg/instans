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

(defun process-configuration (configuration)
  (multiple-value-bind (instans instans-iri) (create-instans)
    (loop with base = nil
	  with graph = nil
	  with verbosep = nil
	  with rete-html-page-dir = nil
	  for (key value) in configuration
	  do (case key
	       (:name (setf (instans-name instans) value))
	       (:base (setf base (parse-iri value)))
	       (:graph (if (string= (string-downcase value) "default") nil (setf graph (parse-iri value))))
	       (:rete-html-page-dir (setf rete-html-page-dir value))
	       (:verbosep (setf verbosep t))
	       (:expect (inform "Expect not implemented yet!"))
	       (:policy (setf policy 
	       (:rules (instans-add-rules instans-iri value :base base :rete-html-page-dir rete-html-page-dir :silentp (not verbosep)))
	       (:triples (instans-add-triples instans-iri value :graph graph :base base))
	       (:input-stream (inform "Input streams not implemented yet!"))
	       (:output-stream (inform "Output streams not implemented yet!"))))))

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
	       (format notifications "  -b <url> or --base <url>                               Use <url> as the base.~%")
	       (format notifications "  -g <dataset> or --graph <dataset>                      If <dataset> is 'default' add the following triple~%")
	       (format notifications "                                                         inputs to the default graph. If it is <url> add them~%")
	       (format notifications "                                                         to the graph named by <url>~%")
	       (format notifications "  -r <file or url> or --rules <file or url>              Use rules in <file or url>.~%")
	       (format notifications "  -t <file or url> or --triples <file or url>            Input all triples in <file or url>.~%")
	       (format notifications "  -i <url> or --input-stream <url>                       Input triples contiuously from stream.~%")
	       (format notifications "  -o <file or url> or --output-stream <file or url>      Write output to <file or url>.~%")
	       (format notifications "  -e <file or url> or --expect <file or url>             Expect the execution to yield the results in <file or url>.~%")
	       (format notifications "  --rete-html-page-dir <dir>                             Create an HTML page presenting the Rete network.~%")
	       (format notifications "  --verbose <true or false>                              Whether to produce lots of diagnostic information.~%")
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
		   do (cond ((value-option "-n" "--name")
			     (push-to-end (list :name (pop args)) configuration))
			    ((value-option "-b" "--base")
			     (push-to-end (list :base (pop args)) configuration))
			    ((value-option "-g" "--graph")
			     (push-to-end (list :graph (pop args)) configuration))
			    ((value-option "-r" "--rules")
			     (push-to-end (list :rules (pop args)) configuration))
			    ((value-option "-t" "--triples")
			     (push-to-end (list :triples (pop args)) configuration))
			    ((value-option "-i" "--input-stream")
			     (push-to-end (list :input-stream (pop args)) configuration))
			    ((value-option "-o" "--output-stream")
			     (push-to-end (list :output-stream (pop args)) configuration))
			    ((value-option "-e" "--expect")
			     (push-to-end (list :expect (pop args)) configuration))
			    ((value-option "--rete-html-page-dir")
			     (push-to-end (list :rete-html-page-dir (pop args)) configuration))
			    ((value-option "-V" "--verbose")
			     (let ((bool (cond ((string= (first args) "true") t) ((string= (first args) "false") nil) (t (usage)))))
			       (push-to-end (list :verbosep bool) configuration)))
			    (t
			     (format notifications "Illegal option ~A~%" (first args))
			     (usage))))))
      (format notifications "Configuration:~%~{~{~S~^ ~}~^~%~}~%" configuration))))
