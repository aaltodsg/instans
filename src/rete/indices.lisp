;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defmethod initialize-instance :after ((this hash-token-index) &key &allow-other-keys)
  (let ((key (token-index-key this)))
    (setf (hash-token-index-table this)
	  (case (length key)
	    (0 nil)
	    ;;; Change this, if the iris are coded in other way
	    (1 (make-hash-table :test #'equal))
	    ((2 3) (make-hash-table :test #'equal))
	    (t (error* "Illegal key ~A" key))))))

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
      (checkit item "trying to remove a non-existent key ~S" key)
      (loop named delete-token
	 with hashkey1 = (second (car token))
	 for prev on item
	 for rest = (cdr prev)
	 for hashkey2 = (second (car (car rest)))
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
