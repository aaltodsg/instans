;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;(save-lisp-and-die "executable" :toplevel 'main :executable t)

(defun process-configuration (configuration)
  (multiple-value-bind (instans instans-iri) (create-instans)
    (loop with base = nil
	  with graph = nil
	  with verbose = nil
	  with rete-html-page-dir = nil
	  for (key value) in configuration
	  do (case key
	       (:name (setf (instans-name instans) value))
	       (:base (setf base (parse-iri value)))
	       (:graph (if (string= (string-downcase value) "default") nil (setf graph (parse-iri value))))
	       (:rules (instans-add-rules instans-iri value :base base :rete-html-page-dir rete-html-page-dir :silentp (not verbose)))
	       (:triples (instans-add-triples instans-iri value :graph graph :base base))
	       (:input-stream (inform "Input streams not implemented yet!"))
	       (:output-stream (inform "Output streams not implemented yet!"))
	       (:expect (inform "Expect not implemented yet!"))
	       (:verbose (setf verbose (equalp (string-downcase value) "true")))
	       (:rete-html-page-dir (setf rete-html-page-dir value))))))

(defun main ()
  (let* ((args sb-ext:*posix-argv*)
	 (configuration nil)
	 (notifications *error-output*)
	 (info-options nil)
	 (configuration-options nil))
    (labels ((usage ()
	       (format notifications "Usage: instans [ ~{~A~^ | ~} | { <configuration-option> } ]~%~%" (loop for option in info-options append (second option)))
	       (format notifications "Options:~%")
	       (loop for option in info-options do (format notifications "  ~{~A~^ | ~}~52T~A~%" (second option) (format nil (third option))))
	       (format notifications "~%Configuration options:~%")
	       (loop for option in configuration-options
		     do (format notifications "  ~{~:[~A~;~{~A~^ ~}~]~^~19T| ~}~52T~A~%"
				(loop for o in (second option) nconc (list (consp o) o))
				(format nil (third option)))))
	     (parse-arg (options not-found-error-p)
	       (loop with arg = (first args)
		     for option in options
		     when (find-if #'(lambda (item) (string= arg (if (consp item) (first item) item))) (second option))
		     do (progn
			  (pop args)
			  (let ((value (if (not (consp (first (second option)))) t (if args (pop args) (usage)))))
			    (cond ((fourth option)
				   (funcall (fourth option) arg value))
				  (t
				   (push-to-end (list (first option) value) configuration)))
			    (return option)))
		     finally (cond (not-found-error-p
				    (format notifications "Illegal argument ~A~%" arg)
				    (return-from main nil))
				   (t
				    (return nil)))))
	     (read-args-from-file (file)
	       (with-open-file (stream file)
		 (loop for line = (read-line stream nil nil)
		       while line
		       do (setf line (string-trim '(#\Space #\Tab) line))
		       nconc (let* ((key-end (position-if #'whitespace-char-p line)))
			       (if (null key-end) 
				   (list (format nil "--~(~A~)" line))
				   (list (format nil "--~(~A~)" (subseq line 0 key-end))
					 (string-left-trim '(#\Space #\Tab) (subseq line key-end)))))))))
    (setf info-options `((:help ("-h" "--help") "Print this message and exit." ,#'(lambda (&rest ignore) (declare (ignore ignore)) (usage)))
			 (:version ("-v" "--version") "Print version information and exit."
				   ,#'(lambda (&rest ignore) (declare (ignore ignore)) (format t "INSTANS version ~A~%" (instans-version))))))
    (setf configuration-options `((:name          (("-n" "<string>") ("--name" "<string>")) "Use <string> as the name of the system.")
				  (:base          (("-b" "<url>") ("--base" "<url>")) "Use <url> as the base.")
				  (:graph         (("-g" "<dataset>") ("--graph" "<dataset>")) "If <dataset> is 'default' add the following triple~%~
                                                                                                ~52Tinputs to the default graph. If it is <url> add them~%~
                                                                                                ~52Tto the graph named by <url>")
				  (:rules         (("-r" "<file or url>") ("--rules" "<file or url>")) "Use rules in <file or url>.")
				  (:triples       (("-t" "<file or url>") ("--triples" "<file or url>")) "Input all triples in <file or url>.")
				  (:input-stream  (("-i" "<url>")         ("--input-stream" "<url>")) "Input triples contiuously from stream.")
				  (:output-stream (("-o" "<file or url>") ("--output-stream" "<file or url>")) "Write output to <file or url>.")
				  (:expect        (("-e" "<file or url>") ("--expect" "<file or url>")) "Expect the execution to yield the results in <file or url>.")
				  (:file          (("-f" "<file or url>") ("--file" "<file or url>")) "Read options from <file or url>."
				   ,#'(lambda (arg value) (declare (ignore arg)) (setf args (append (read-args-from-file value) args))))
				  (:verbose       (("--verbose" "<true or false>")) "Whether to produce lots of diagnostic information.")
				  (:rete-html-page-dir (("--rete-html-page-dir" "<dir>")) "Create an HTML page presenting the Rete network.")))
      (pop args) ; Program path
      (when (equalp (first args) "--end-toplevel-options") (pop args)) ; Inserted by wrapper script
      (when (null args) (usage))
      (let ((option (parse-arg info-options nil)))
	(when (null option)
	  (loop while args
		unless (parse-arg configuration-options t)
	        do (pop args))
	  (when configuration (format notifications "Configuration:~%~{~{~S~^ ~}~^~%~}~%" configuration)))))))


