;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun ptest (&key subscribe)
  (let* ((input-iri (parse-iri "file:///Users/enu/instans/tests/input/pdata.trig"))
	 (output-name "-")
	 (engine1-iri (parse-iri "http://instans.org/p1"))
	 (engine2-iri (parse-iri "http://instans.org/p2"))
	 (engine1 (create-instans engine1-iri))
	 (engine2 (create-instans engine2-iri))
	 (mbox-processor (create-construct-output-processor nil :mbox))
	 (mbox (sb-concurrency:make-mailbox))
	 thread1 thread2)
    (setf *seen-threads* nil)
    (instans-add-rules engine1-iri "../tests/input/p1.rq" :encode-prefixes-p t)
    (instans-add-rules engine2-iri "../tests/input/p2.rq" :encode-prefixes-p t)
    (instans-add-query-input-processor engine1-iri input-iri :subscribe subscribe :input-type :trig)
    (push mbox (mailbox-output-processor-mailboxes mbox-processor))
    (setf (instans-construct-output-processor engine1) mbox-processor)
    (setf (instans-select-output-processor engine1) (create-select-output-processor nil :csv))
    (setf (instans-construct-output-processor engine2) (create-construct-output-processor output-name :trig))
    (setf (instans-select-output-processor engine2) (create-select-output-processor nil :csv))
    (inform "1: threads: ~S~%seen-threads: ~S" (sb-thread:list-all-threads) *seen-threads*)
    (setf thread2 (sb-thread:make-thread #'(lambda ()
					     (push (list sb-thread:*current-thread* :inside-lambda2) *seen-threads*)
					     (loop with continuep = t
						   while continuep
						   do (let ((inputs (sb-concurrency:receive-message mbox)))
							(process-query-input engine2 inputs))))
					 :name "Engine2"))
    (inform "2: threads: ~S~%seen-threads: ~S" (sb-thread:list-all-threads) *seen-threads*)
    (setf thread1 (sb-thread:make-thread #'(lambda ()
					     (push (list sb-thread:*current-thread* :inside-lambda1) *seen-threads*)
					     (loop while t
						  do (run-query-input-processors engine1))
					     ;(sb-thread:terminate-thread thread2)
					     )
					 :name "Engine1"))
    (inform "3: threads: ~S~%seen-threads: ~S" (sb-thread:list-all-threads) *seen-threads*)
))
    
						  