;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class agent ()
  ((name :accessor agent-name :initarg :name)
   (function :accessor agent-function :initarg :function)
   (arguments :accessor agent-arguments :initarg :arguments :initform nil)
   (thread :accessor agent-thread)
   (mbox :accessor agent-mbox :initarg :mbox)
   (receivers :accessor agent-receivers :initarg :receivers)))

(defmethod initialize-instance :after ((this agent) &key name mbox &allow-other-keys)
  (when (null mbox)
    (setf (agent-mbox this) (sb-concurrency:make-mailbox :name (format nil "~A-mbox" name)))))

(defgeneric agent-start (agent)
  (:method ((this agent))
    (setf (agent-thread this) (sb-thread:make-thread (agent-function this) :arguments (agent-arguments this) :name (format nil "~A-thread" (agent-name this))))))

(defgeneric agent-send (agent message)
  (:method ((this agent) message)
    (loop for receiver in (agent-receivers this) do (sb-concurrency:send-message (agent-mbox receiver) message))))

(defgeneric agent-receive (agent)
  (:method ((this agent)) (sb-concurrency:receive-message (agent-mbox this))))

(defgeneric agent-yield (agent)
  (:method ((this agent)) (sb-thread:thread-yield)))

(defgeneric agent-terminate (agent)
  (:method ((this agent)) (sb-thread:terminate-thread (agent-thread this))))

(define-class event-processing-engine (instans agent) ())

(define-class event-processing-network ()
  ((engines :accessor event-processing-network-engines :initarg :engines)
   (connections :accessor event-processing-network-connections :initarg :connections)
   (inputs :accessor event-processing-network-inputs :initarg :inputs)
   (outputs :accessor event-processing-network-outputs :initarg :outputs)))
