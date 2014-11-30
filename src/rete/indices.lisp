;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;; (defmethod initialize-instance :after ((this hash-token-index) &key &allow-other-keys)
;;   (let ((key (token-index-key this)))
;;     (setf (hash-token-index-table this)
;; 	  (case (length key)
;; 	    (0 nil)
;; 	    ;;; Change this, if the iris are coded in other way
;; 	    (t (make-hash-table :test #'equal))))))
;; 	    ;; (1 (make-hash-table))
;; 	    ;; ((2 3) (make-hash-table))
;; 	    ;; (t (error* "Illegal key ~A" key))))))

(defmethod initialize-instance :after ((this hash-token-index) &key &allow-other-keys)
  (setf (hash-token-index-table this)
	(make-hash-table :test #'equal)))

(defgeneric index-get-tokens-and-defined-p (index key)
  (:method ((this hash-token-index) key)
    (assert key)
    (multiple-value-bind (value definedp)
	(gethash key (hash-token-index-table this))
      (values (cdr value) definedp))))
      
(defgeneric index-get-tokens (index key)
  (:method ((this hash-token-index) key)
    (assert key)
    (cdr (gethash key (hash-token-index-table this)))))

(defgeneric index-tokens (index)
  (:method ((this hash-token-index))
    (let ((r nil))
      (maphash #'(lambda (k v) (push (list k v) r)) (hash-token-index-table this))
      (nreverse r))))

;;; Returns t if this is the first token with this key
(defgeneric index-put-token (index key token)
  (:method ((this hash-token-index) key token)
    (assert key)
    (let ((item (gethash key (hash-token-index-table this))))
	(cond ((null item)
	       (setf (gethash key (hash-token-index-table this)) (list nil token))
	       t)
	      (t
	       (setf (cdr item) (cons token (cdr item)))
	       nil)))))

;;; Returns T if this was the last token with this key
(defgeneric index-remove-token (index key token)
  (:method ((this hash-token-index) key token)
    (assert key)
    (let ((item (gethash key (hash-token-index-table this))))
      (when (null item)
	(loop for (k v) in (index-tokens this)
	      when (eq k key)
	      do (error* "Something really wrong, key ~S in table is eq to key ~S" k key)
	      else when (equal k key)
	      do (error* "Trying to remove key ~S that is equal, but not eq to key ~S in the table" key k)))
;      (inform "~&index-remove-token ~A ~A ~A" key token item)
      (loop named delete-token
	    with hashkey1 = (second (car token))
	    for prev on item
	    for rest = (cdr prev)
	    for hashkey2 = (second (car (car rest)))
	    while rest
;	    do (inform "hashkey1 = ~A, hashkey2 = ~A" hashkey1 hashkey2)
	    when (= hashkey1 hashkey2)
	    do (progn
		 (setf (cdr prev) (cdr rest))
		 (return-from delete-token)))
      (cond ((null (cdr item))
	     (remhash key (hash-token-index-table this))
	     t)
	    (t
	     nil)))))

(defgeneric index-count (index)
  (:method ((this hash-token-index))
    (let ((table (hash-token-index-table this))
	  (count 0))
      (unless (null table)
	(maphash #'(lambda (k v) k (incf count (length (cdr v)))) table))
      count)))

(defgeneric index-clear (index)
  (:method ((this hash-token-index))
    (let ((table (hash-token-index-table this)))
      (unless (null table)
	(clrhash table)))))

(defun join-alpha-key (join alpha-token)
  (pop alpha-token) ;;; Get rid of the hash key
  (loop with key = (sxhash nil)
	for var in (node-use join)
	do (setf key (mix key (get-hashkey (second (assoc var alpha-token)))))
	finally (return key)))

;; (defun join-alpha-key (join alpha-token)
;;   (pop alpha-token) ;;; Get rid of the hash key
;;   (loop for var in (node-use join)
;;         for index = (position var (node-def-preceq (join-alpha join)))
;; 	do (checkit index "Missing var ~S in alpha-memory ~S" var (join-alpha join))
;; 	collect (nth index alpha-token)))

(defun join-beta-key (join beta-token)
  (loop with key = (sxhash nil)
	for var in (node-use join)
	do (setf key (mix key (get-hashkey (token-value join beta-token var))))
	finally (return key)))

(defun service-node-index-key (node token)
  (loop with key = (sxhash nil)
	for var in (service-node-index-key-vars node)
	do (setf key (mix key (get-hashkey (token-value node token var))))
	finally (return key)))
