;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun epa (rdf-source-file &key (report '(:select :construct :modify :all :rete-add :rete-remove :queue :rdf-operations :execute)))
  (let* ((encode-prefixes-p t)
	 (epa0 (make-instance 'event-processing-engine :name 'epa0 :rdf-operations '(:add :execute :flush) :allow-rule-instance-removal-p nil))
	 (epa1 (make-instance 'event-processing-engine :name 'epa1 :rdf-operations '(:add :execute :remove :execute :flush) :allow-rule-instance-removal-p nil))
	 (epa2 (make-instance 'event-processing-engine :name 'epa2 :rdf-operations '(:add :execute :remove :execute :flush) :allow-rule-instance-removal-p nil))
	 (epa3 (make-instance 'event-processing-engine :name 'epa3 :rdf-operations '(:add :execute :remove :execute :flush)))
	 (epa4 (make-instance 'event-processing-engine :name 'epa4 :rdf-operations '(:add :execute :remove :execute :flush)))
	 (epa5 (make-instance 'event-processing-engine :name 'epa5 :rdf-operations '(:add :execute :remove :execute :flush)))
	 (epa6 (make-instance 'event-processing-engine :name 'epa6 :rdf-operations '(:add :execute :remove :execute :flush)))
	 (agents (list epa0 epa1 epa2 ; epa3 epa4 epa5 epa6
		       ))
	 )
    (loop for agent in agents do (setf (instans-report-operation-kinds agent) report))
    (instans-add-rules epa0 "../tests/input/cep2sparql-testing/phased-queries/construct-EPA0.rq" :encode-prefixes-p encode-prefixes-p)
    (instans-add-rules epa1 "../tests/input/cep2sparql-testing/phased-queries/construct-EPA1.rq" :encode-prefixes-p encode-prefixes-p)
    (instans-add-rules epa2 "../tests/input/cep2sparql-testing/phased-queries/construct-EPA2.rq" :encode-prefixes-p encode-prefixes-p)
    (instans-add-rules epa3 "../tests/input/cep2sparql-testing/phased-queries/construct-EPA3.rq" :encode-prefixes-p encode-prefixes-p)
    (instans-add-rules epa4 "../tests/input/cep2sparql-testing/phased-queries/construct-EPA4.rq" :encode-prefixes-p encode-prefixes-p)
    (instans-add-rules epa5 "../tests/input/cep2sparql-testing/phased-queries/construct-EPA5.rq" :encode-prefixes-p encode-prefixes-p)
    (instans-add-rules epa6 "../tests/input/cep2sparql-testing/phased-queries/construct-EPA6.rq" :encode-prefixes-p encode-prefixes-p)
    (loop for agent in agents
	  unless (instans-find-status agent 'instans-rule-translation-succeeded)
	  do (let ((status (first (instans-status agent))))
	       (inform "Adding rules to ~A failed: ~A~{~%~A~}~%" agent (type-of status) (instans-status-messages status))
	       (return-from epa nil)))

    (instans-add-stream-input-processor epa0 rdf-source-file :input-type (intern-keyword (string-upcase (pathname-type (parse-namestring rdf-source-file)))))
    (setf (instans-construct-output-processor epa0) (create-construct-agent-output-processor epa0 :source :trig (list epa1 epa6)))

    (instans-add-agent-input-processor epa1 :name 'epa1)
    (setf (instans-construct-output-processor epa1) (create-construct-agent-output-processor epa1 :poststateless :trig (list epa2)))
;    (setf (instans-construct-output-processor epa1) (create-construct-stream-output-processor epa1 "poststateless.trig" :trig))

    (instans-add-agent-input-processor epa2 :name 'epa2)
;    (setf (instans-construct-output-processor epa2) (create-construct-agent-output-processor epa2 :poststateful :trig (list epa3 epa5)))
    (setf (instans-construct-output-processor epa2) (create-construct-stream-output-processor epa2 "poststateful.trig" :trig))

    ;; (instans-add-agent-input-processor epa3 :name 'epa3)
    ;; (setf (instans-construct-output-processor epa3) (create-construct-agent-output-processor epa3 :translated :trig (list epa4)))

    ;; (instans-add-agent-input-processor epa4 :name 'epa4)
    ;; (setf (instans-construct-output-processor epa4) (create-construct-stream-output-processor epa4 "projected.trig" :trig))

    ;; (instans-add-agent-input-processor epa5 :name 'epa5)
    ;; (setf (instans-construct-output-processor epa5) (create-construct-stream-output-processor epa5 "geo-and-time-events.trig" :trig))

    ;; (instans-add-agent-input-processor epa6 :name 'epa6)
    ;; (setf (instans-construct-output-processor epa6) (create-construct-stream-output-processor epa6 "eventcounts.trig" :trig))

    (run-instanses agents)))





