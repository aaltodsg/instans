;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class event-processing-engine (instans agent) ())

(define-class event-processing-network ()
  ((engines :accessor event-processing-network-engines :initarg :engines)
   (connections :accessor event-processing-network-connections :initarg :connections)
   (inputs :accessor event-processing-network-inputs :initarg :inputs)
   (outputs :accessor event-processing-network-outputs :initarg :outputs)))

(define-class mailbox-output-processor (construct-output-processor)
  ((mailboxes :accessor mailbox-output-processor-mailboxes :initarg :mailboxes :initform nil)
   (quads :accessor mailbox-output-processor-quads :initform nil)
   (quads-tail :accessor mailbox-output-processor-quads-tail :initform nil)))

(defmethod write-construct-output ((this mailbox-output-processor) instans s p o &optional g)
  (loop for mailbox in (mailbox-output-processor-mailboxes this)
	do (sb-concurrency:send-message mailbox (list s p o g))))

(defmacro define-event-processing-network (name &body definitions)
  (declare (ignorable name definitions))
  nil)

(define-event-processing-network example (:directory "../tests/input/CEP2SPARQL/" :base "http://example.org/")
  (engine stateless 	 :rules "stateless.rq")
  (engine stateful 	 :rules "stateful.rq")
  (engine translate 	 :rules "translate.rq")
  (engine project   	 :rules "project.rq")
  (engine split     	 :rules "split.rq")
  (engine aggregate      :rules "aggregate.rq")
  (engine compose        :rules "compose.rq")
  (engine transform      :rules "transform.rq")
  (engine pattern-detect :rules "pattern-detect.rq")
  (input-trig "/dev/stdin" stateless aggregate transform)
  (connection stateless "<poststateless>" stateful)
  (connection stateful "<poststateful>" translate split)
  (connection translate "<translated"> project)
  (output-trig project "projected.trig")
  (connection split "<geoEvents>" compose)
  (connection split "<timeEvents>" compose)
  (output-trig compose "combined.trig")
  (input-ttl "pattern.ttl" pattern-detect)
  (connect transform "<directions>" pattern-detect)
  (output-trig pattern-detect "patterndetect.trig"))

