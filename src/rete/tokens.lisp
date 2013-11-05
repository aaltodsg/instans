;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defvar *singleton-token* (list (list nil (sxhash nil))))
(setf *singleton-token* (list (list nil (sxhash nil))))

;;; Note: key-item (nil key) is used instead of just the key to be able to call (assoc var token) to retrieve the value of a variable (nil never matches).
(defgeneric make-token (node prev-token new-vars new-values)
  (:method ((this existence-start-node) prev-token new-vars new-values)
    (let* ((key-item (or (car prev-token) (list (sxhash nil)))) ; We get the prev key here. It is either in an item (nil key), or if none, we use (nil (sxhash nil))
	   (new-token prev-token)) ; We leave the key of prev-token in place to be able to retrieve the full prev-token in start-node-token
      (loop for var in new-vars for value in new-values
	 do (push (list var value) new-token))
      (cons key-item new-token))) ; We boldly use the key of prev-token as the key of new-token!
  ;;; We use the key of prev-token if any, or (sxhash nil). New-token, however, contains the new values for the previous-value-var, and a pair (nil key) of prev-token
  (:method ((this filter-memory) prev-token new-vars new-values)
    (let* ((key-item (or (car prev-token) (list nil (sxhash nil))))
	   (new-token prev-token)) ; We leave the key of prev-token in place to be able to retrieve the full prev-token when calling the successors.
      (loop for var in new-vars for value in new-values
	 do (push (list var value) new-token))
      (cons key-item new-token))) ; We boldly use the key of prev-token as the key of new-token!
  ;;; We compute the key for new-token using the key of prev-token and the sxhash values of the new bindings.
  (:method ((node node) prev-token new-vars new-values)
    (let* ((key (or (second (car prev-token)) (sxhash nil))) ; We retrieve the key of prev-token and use it as a basis for the key of new-token
	   (new-token (cdr prev-token))) ; We eliminate the key of prev-token
      (loop for var in new-vars for value in new-values
	 do (push (list var value) new-token)
	 do (setf key (mix (sxhash (list (hashable-key var) (hashable-key value))) key)))
      (cons (list nil key) new-token))))

;;; We can freely skip the first item of token, since it should be (nil key)!
(defun token-value (node token var)
  (declare (ignore node))
;  (second (assoc var (cdr token) :test #'equal)))
  (loop for item in (cdr token)
	when (equal var (car item))
	return (second item)
	finally (return (sparql-unbound))))

(defsetf token-value (node token var) (new-value)
  `(progn ,node
	  (setf (second (assoc ,var (cdr ,token) :test #'equal)) ,new-value)
	  ,new-value))

(defgeneric start-node-token (node token)
  (:method ((this exists-end-node) token)
    (loop with counter-var = (existence-counter-var (subgraph-start-node this))
       for items on token
       for item = (cadr items)
       while (not (equal (car item) counter-var))
	 ;;; (car items) should be (nil [key of the contained token]). Thus, this should be the token as it was in existence-start-node!
       finally (return items)))
  (:method ((this optional-end-node) token)
    (loop with active-p-var = (existence-active-p-var (subgraph-start-node this))
       for items on (cdr token)
       for item = (car items)
       while (not (equal (car item) active-p-var))
	 ;;; (car items) should be (nil [key of the contained token]). Thus, this should be the token as it was in existence-start-node!
       finally (return (cdr items)))))
