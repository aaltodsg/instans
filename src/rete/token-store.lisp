;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defgeneric token-store-count (token-store)
  (:method ((this token-store))
    (let ((store (token-store-hash-table this)))
      (cond ((null store) 0)
	    (t (hash-table-count (token-store-hash-table this)))))))

;; (defgeneric inner-token (token-store token)
;;   (:method ((this existence-start-node) token) (cdddr token))
;;   (:method ((this filter-with-previous-value) token) (cddr token))
;;   (:method ((this token-store) token) token))

;; (defgeneric token-store-get (token-store token)
;;   (:method ((this token-store) token)
;;     (let* ((key (second (car token)))
;; 	   (stored-token (gethash key (token-store-hash-table this)))
;; 	   (stored-token-inner (when-checkit (inner-token this stored-token)))
;; 	   )
;;       (declare (ignorable stored-token-inner))
;;       (checkit (numberp key) "key ~S not a number" key)
;;       (checkit (or (null stored-token) (token-equal token stored-token-inner)) "token~%~A and inner-token~%~A not equal~%~A" token stored-token-inner stored-token)
;;       stored-token)))

(defgeneric token-store-get (token-store token)
  (:method ((this token-store) token)
    (gethash token (token-store-hash-table this))))

(defgeneric token-store-put (token-store token)
  (:method ((this token-store) token)
    (let ((ht (token-store-hash-table this)))
      (checkit (not (gethash token ht)) "token ~S already in token-store ~S" token this)
      (setf (gethash token ht) token)
      (incf (token-store-put-count this)))))

(defgeneric token-store-put-if-missing (token-store token)
  (:method ((this token-store) token)
    (let ((ht (token-store-hash-table this)))
      (cond ((gethash token ht) nil)
	    (t 
	     (setf (gethash token ht) token)
	     (incf (token-store-put-count this))
	     t)))))

(defgeneric token-store-remove (token-store token)
  (:method ((this token-store) token)
    (let ((ht (token-store-hash-table this)))
      (checkit (gethash token ht) "token ~S not in token-store ~S" token this)
      (remhash token ht)
      (incf (token-store-remove-count this)))))

(defgeneric token-store-remove-if-exists (token-store token)
  (:method ((this token-store) token)
    (let ((ht (token-store-hash-table this)))
      (cond ((remhash token ht)
	     (incf (token-store-remove-count this))
	     t)
	    (t nil)))))

(defgeneric token-store-tokens (token-store)
  (:method ((token-store token-store))
    (maph #'(lambda (k v)
	      (declare (ignore k))
	      v)
	  (token-store-hash-table token-store))))

(defgeneric token-store-clear (token-store)
  (:method ((token-store token-store))
    (let ((table (token-store-hash-table token-store)))
      (unless (null table)
	(clrhash table)))))
