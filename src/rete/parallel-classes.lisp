;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class agent ()
  ((name :accessor agent-name :initarg :name)
   (function :accessor agent-function :initarg :function)
   (arguments :accessor agent-arguments :initarg :arguments :initform nil)
   (messages :accessor agent-messages :initarg :messages)))

(define-class sequential-agent (agent)
  ((status :accessor sequential-agent-status :initform :runnable)))

(define-class threaded-agent (agent)
  ((thread :accessor threaded-agent-thread)))

(define-class message-container ()
  ((name :accessor message-container-name :initarg :name)))

(define-class simple-message-container (message-container)
  ((messages :accessor simple-message-container-messages :initform nil)
   (tail :accessor simple-message-container-tail :initform nil)))

(define-class mailbox-container (message-container)
  ((mbox :accessor mailbox-container-mbox :initarg :mbox)))

(defgeneric add-message (message-container msg)
  (:method ((this simple-message-container) msg)
    (let ((newtail (list msg)))
      (cond ((null (simple-message-container-messages this))
	     (setf (simple-message-container-messages this) newtail)
	     (setf (simple-message-container-tail this) newtail))
	    (t
	     (setf (cdr (simple-message-container-tail this)) newtail)
	     (setf (simple-message-container-tail this) newtail)))))
  (:method ((this mailbox-container) msg)
    (sb-concurrency:send-message (mailbox-container-mbox this) msg)))

(defgeneric remove-first-message (message-container hangp)
  (:method ((this simple-message-container) hangp)
    (cond ((null (simple-message-container-messages this))
	   (cond ((not hangp)
		  (values nil nil))
		 (t
		  (error* "Trying to remove a message from an empty message container would deadlock the system" this))))
	  (t
	   (let ((result (pop (simple-message-container-messages this))))
	     (when (null (simple-message-container-messages this))
	       (setf (simple-message-container-tail this) nil))
	     (values result t)))))
  (:method ((this mailbox-container) hangp)
    (cond ((not hangp)
	   (sb-concurrency:receive-message-no-hang (mailbox-container-mbox this)))
	  (t
	   (values (sb-concurrency:receive-message (mailbox-container-mbox this)) t)))))

(defgeneric has-messages-p (message-container)
  (:method ((this simple-message-container))
    (not (null (simple-message-container-messages this))))
  (:method ((this mailbox-container))
    (not (sb-concurrency:mailbox-empty-p (mailbox-container-mbox this)))))

(defgeneric agent-terminated-p (agent)
  (:method ((this sequential-agent))
    (eq (sequential-agent-status this) :terminated))
  (:method ((this threaded-agent))
    (not (sb-thread:thread-alive-p (threaded-agent-thread this)))))

(defgeneric agent-start (agent)
  (:method ((this sequential-agent))
    (setf (sequential-agent-status this) :runnable)
    (funcall (agent-function this) (agent-arguments this)))
  (:method ((this threaded-agent))
    (setf (threaded-agent-thread this) (sb-thread:make-thread (agent-function this) :arguments (agent-arguments this) :name (format nil "~A-thread" (agent-name this))))))

(defgeneric agent-send (agent message)
  (:method ((this agent) message)
    (add-message (agent-messages this) message)))

(defgeneric agent-receive (agent hangp)
  (:method ((this agent) hangp)
    (remove-first-message (agent-messages this) hangp)))

(defgeneric agent-yield (agent)
  (:method ((this sequential-agent))
    (declare (ignorable this))
    nil)
  (:method ((this threaded-agent))
    (declare (ignorable this))
    (sb-thread:thread-yield)))

(defgeneric agent-terminate (agent)
  (:method ((this sequential-agent))
    (setf (sequential-agent-status this) :terminated))
  (:method ((this threaded-agent))
    (declare (ignorable this))
    (sb-thread:terminate-thread (threaded-agent-thread this))))

(define-class event-processing-agent (instans agent) ())

(define-class sequential-event-processing-agent (event-processing-agent) ())

(define-class threaded-event-processing-agent (event-processing-agent) ())

(define-class event-processing-network ()
  ((agent-class :accessor event-processing-network-agent-class :initarg :agent-class)
   (message-container-class :accessor event-processing-network-message-container-class :initarg :message-container-class)
   (agents :accessor event-processing-network-agents :initarg :agents)
;   (connections :accessor event-processing-network-connections :initarg :connections)
;   (inputs :accessor event-processing-network-inputs :initarg :inputs)
;   (outputs :accessor event-processing-network-outputs :initarg :outputs))
  ))

(defun create-threaded-event-processing-network ()
  (make-instance 'threaded-event-processing-agent :agent-class 'threaded-agent :message-container-class 'mailbox-container))

(defun create-sequential-event-processing-network ()
  (make-instance 'sequential-event-processing-agent :agent-class 'sequential-agent :message-container-class 'simple-message-container))

(defgeneric create-agent (event-processing-network &key name configuration)
  (:method ((this event-processing-network) &key name configuration)
    (let ((agent (make-instance (event-processing-network-agent-class this)
				:name name
				:messages (make-instance (event-processing-network-message-container-class this) :name name))))
      (instans-configure agent configuration)
      agent)))
