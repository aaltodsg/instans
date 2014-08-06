;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(require 'sb-bsd-sockets)

(defun ptest (&key subscribe)
  (let* ((input-iri (parse-iri "file:///Users/enu/instans/tests/input/pdata.trig"))
	 (output-name "pdata-out.trig")
	 (engine1-iri (parse-iri "http://instans.org/p1"))
	 (engine2-iri (parse-iri "http://instans.org/p2"))
	 (engine1 (create-instans engine1-iri))
	 (engine2 (create-instans engine2-iri))
	 (mbox (sb-concurrency:make-mailbox))
	 (mbox-processor nil)
	 thread1
	 thread2
	 )
    (instans-add-rules engine1-iri "../tests/input/p1.rq" :create-instans-p nil :encode-prefixes-p t)
    (instans-add-query-input-processor engine1-iri input-iri :subscribe subscribe :input-type :trig)
    (setf (instans-rdf-input-unit engine1) :block)
;    (setf (instans-construct-output-processor engine1) (create-construct-output-processor "out.trig" :trig))
    (setf mbox-processor (create-construct-output-processor nil :mbox))
    (setf (instans-construct-output-processor engine1) mbox-processor)
    (push mbox (mailbox-output-processor-mailboxes mbox-processor))
    (inform "Created ~A" mbox-processor)
    (describe mbox-processor)
    (setf (instans-select-output-processor engine1) (create-select-output-processor nil :csv))
;    (setf (instans-report-operation-kinds engine1) '(:rete-add :rete-remove :queue :rdf-operations :select :construct :modify))
    (instans-add-rules engine2-iri "../tests/input/p2.rq" :create-instans-p nil :encode-prefixes-p t)
    (instans-add-mailbox-query-input-processor engine2-iri mbox :subscribe subscribe)
    (setf (instans-rdf-input-unit engine2) :block)
    (setf (instans-construct-output-processor engine2) (create-construct-output-processor output-name :trig))
    (setf (instans-select-output-processor engine2) (create-select-output-processor nil :csv))
    (setf (instans-report-operation-kinds engine2) '(:rete-add :rete-remove :queue :rdf-operations :select :construct :modify))
    (setf thread1 (sb-thread:make-thread #'(lambda ()
					     (unwind-protect
						  (run-query-input-processors engine1)
					       (sb-concurrency:send-message mbox :end-of-input)
					       (instans-close-open-streams engine1)))
					 :name "Engine1"))
    (setf thread2 (sb-thread:make-thread #'(lambda ()
					     ;; (unwind-protect
					     ;; 	  (run-query-input-processors engine2)
					     ;;   (instans-close-open-streams engine2))
					     (unwind-protect
						  (loop for msg = (sb-concurrency:receive-message mbox)
							do (inform "got message ~A" msg)
							when (eq msg :end-of-input)
							do (return)
							do (process-query-input engine2 (rest msg)))
					       (instans-close-open-streams engine2)))
    					 :name "Engine2"))
    (sb-thread:join-thread thread1)
    (sb-thread:join-thread thread2)
))
    
						  
(defvar *msgs* nil)

(defun ptest2 (n)
  (let* ((mbox1 (sb-concurrency:make-mailbox))
	 (mbox2 (sb-concurrency:make-mailbox))
	 thread1 thread2
	 (counts (make-array 5 :element-type 'SB-EXT:WORD)))
    (setf (aref counts 0) 0)
    (setf (aref counts 1) 0)
    (setf thread2 (sb-thread:make-thread #'(lambda ()
					     (loop for i from 0 to n
						   do (let* ((msg (sb-concurrency:receive-message mbox2)))
							(sb-ext:atomic-incf (aref counts 1) msg))
							(sb-concurrency:send-message mbox1 i)))
					 :name "Thread2"))
    (setf thread1 (sb-thread:make-thread #'(lambda ()
					     (loop for i from 0 to n
						   do (progn
							(sb-concurrency:send-message mbox2 i)
							(let ((msg (sb-concurrency:receive-message mbox1)))
							  (sb-ext:atomic-incf (aref counts 0) msg)))))
					 :name "Thread1"))
    (sb-thread:join-thread thread1)
    (sb-thread:join-thread thread2)
    (inform "count = ~A, ~A" (aref counts 0) (aref counts 1))))

(defun ptest3 ()
  (let* (;(mbox (sb-concurrency:make-mailbox))
	 thread1 thread2
	 (counts (make-array 5 :element-type 'SB-EXT:WORD)))
    (setf (aref counts 0) 0)
    (setf (aref counts 1) 0)
    (setf thread1 (sb-thread:make-thread #'(lambda ()
					     (sb-ext:atomic-incf (aref counts 0) 2)
					     :name "Increase")))
    (setf thread2 (sb-thread:make-thread #'(lambda ()
					     (sb-ext:atomic-incf (aref counts 1) 10)
					     :name "Decrease")))
    (sb-thread:join-thread thread1)
    (sb-thread:join-thread thread2)
    (inform "count = ~A, ~A" (aref counts 0) (aref counts 1))))

