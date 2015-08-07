;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun make-singleton-token ()
  (list (list nil (sxhash nil))))

;;; Note: the new-vars and new-values are in reverse order, i.e., the new bindings are pushed to the old token!
;;; Note: key-item (nil key) is used instead of just the key to be able to call (assoc var token) to retrieve the value of a variable (nil never matches).
(defgeneric make-token (node prev-token new-vars new-values)
  (:method ((this existence-start-node) prev-token new-vars new-values)
    (let* ((key-item (or (car prev-token) (list nil (sxhash nil)))) ; We get the prev key here. It is either in an item (nil key), or if none, we use (nil (sxhash nil))
	   (new-token prev-token)) ; We leave the key of prev-token in place to be able to retrieve the full prev-token in start-node-token
      (loop for var in new-vars for value in new-values
	    do (push (list var value) new-token))
      (cons key-item new-token))) ; We boldly use the key of prev-token as the key of new-token!
  (:method ((this service-node) prev-token new-vars new-values) ; Like existence-start-node
    (let* ((key-item (or (car prev-token) (list nil (sxhash nil)))) ; We get the prev key here. It is either in an item (nil key), or if none, we use (nil (sxhash nil))
	   (new-token prev-token)) ; We leave the key of prev-token in place to be able to retrieve the full prev-token in start-node-token
      (loop for var in new-vars for value in new-values
	    do (push (list var value) new-token))
      (cons key-item new-token))) ; We boldly use the key of prev-token as the key of new-token!
  ;;; We use the key of prev-token if any, or (sxhash nil). New-token, however, contains the new values for the previous-value-var, and a pair (nil key) of prev-token
  (:method ((this filter-with-previous-value) prev-token new-vars new-values)
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
	    unless (or (typep value 'term-or-value)
		       (and (typep value 'group) (typep node 'aggregate-join-node)))
	 do (error* "Trying to bind an illegal value ~S to variable ~A" value var)
	    do (push (list var value) new-token)
	    do (setf key (mix (mix (get-hashkey var) (get-hashkey value)) key)))
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
    ;;; Order in the token is (... (counter-var ...) (active-p ...) ...)
    (loop with counter-var = (existence-counter-var (subgraph-start-node this))
	  for items on token
	  for item = (first items)
;	  do (inform "items = ~A, item = ~A" items item)
	  while (not (equal (car item) counter-var))
	  ;;; This comment seems to be wrong: (car items) should be (nil [key of the contained token]). Thus, this should be the token as it was in existence-start-node!
	  finally ;(progn (inform "return ~A" (cons (third items) items))
	 (return (cons (third items) items))))
					;)
  (:method ((this optional-end-node) token)
    (loop with active-p-var = (existence-active-p-var (subgraph-start-node this))
	  for items on (cdr token)
	  for item = (car items)
	  while (not (equal (car item) active-p-var))
	  ;;; (car items) should be (nil [key of the contained token]). Thus, this should be the token as it was in existence-start-node!
	  finally (return (cdr items)))))

(defun token-equal (t1 t2)
  (and (equal (length t1) (length t2))
       (loop for (var1 value1) in t1
	     for (var2 value2) in t2
	     unless (and (or (and (null var1) (null var2))
			     (sparql-var-equal var1 var2))
			 (or (eq value1 value2) (sparql-call "=" value1 value2)))
	     do (return nil)
	     finally (return t))))

(defun token-pretty-string (node token &optional indent)
  (let ((fmt (if indent (format nil "~~{~~A~~^~~%~V@T~~}" indent) "~~{~~A~~^~~%~~}")))
    (format nil fmt
	    (loop for item in token
		  collect (cond ((consp item)
				 (destructuring-bind (var value) item
				   (let ((varstr (cond ((sparql-var-p var) (uniquely-named-object-name (reverse-resolve-binding (node-instans node) var)))
						       ((null var) "key")
						       (t var))))
				     (format nil "~A -> ~A" varstr (sparql-value-to-string value :instans (node-instans node))))))
				(t
				 (format nil "~A" item)))))))
