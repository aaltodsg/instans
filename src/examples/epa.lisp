;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun epa (rdf-source-file &key (report '(:select :construct :modify :all :rete-add :rete-remove :queue :rdf-operations :execute)))
  (let* ((epn (create-sequential-event-processing-network))
	 (epa0 (create-agent epn
			     :configuration (append (list :report report)
						    `(:name :epa0 :rdf-operations (:add :execute :flush) :allow-rule-instance-removal-p nil
						      :rules "../tests/input/cep2sparql-testing/phased-queries/construct-EPA0.rq"
						      :input-blocks ,rdf-source-file
						      :construct-agent-output (:name :source :type :trig :destinations (:epa1))))))
	 (common-configuration (append (list :report report)
				       '(:encode-prefixes-p t :rdf-operations (:add :execute :remove :execute :flush) :allow-rule-instance-removal-p nil)))
	 (epa1 (create-agent epn :configuration (append common-configuration
							'(:name :epa1
							  :rules "../tests/input/cep2sparql-testing/phased-queries/construct-EPA1.rq"
							  :agent-input :epa1-input
							  :construct-agent-output (:name :poststateless :type :trig :destinations (:epa2))))))
	 (epa2 (create-agent epn :configuration (append common-configuration
							'(:name :epa2
							  :rules "../tests/input/cep2sparql-testing/phased-queries/construct-EPA2.rq"
							  :agent-input :epa2-input
							  :construct-output "../tests/input/cep2sparql-testing/phased-queries/poststateful.trig"))))
	 (agents (list epa0 epa1 epa2 ; epa3 epa4 epa5 epa6
		       ))
	 )
;    (setf (instans-construct-output-processor epa2) (create-construct-agent-output-processor epa2 :poststateful :trig (list epa3 epa5)))

    ;; (instans-add-agent-input-processor epa3 :name 'epa3)
    ;; (setf (instans-construct-output-processor epa3) (create-construct-agent-output-processor epa3 :translated :trig (list epa4)))

    ;; (instans-add-agent-input-processor epa4 :name 'epa4)
    ;; (setf (instans-construct-output-processor epa4) (create-construct-stream-output-processor epa4 "projected.trig" :trig))

    ;; (instans-add-agent-input-processor epa5 :name 'epa5)
    ;; (setf (instans-construct-output-processor epa5) (create-construct-stream-output-processor epa5 "geo-and-time-events.trig" :trig))

    ;; (instans-add-agent-input-processor epa6 :name 'epa6)
    ;; (setf (instans-construct-output-processor epa6) (create-construct-stream-output-processor epa6 "eventcounts.trig" :trig))

    (run-instanses agents)))





