;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;; (defgeneric store-count (memory)
;;   (:method ((this memory))
;;     (let ((store (memory-store this)))
;;       (cond ((null store) 0)
;; 	    (t (hash-table-count (memory-store this)))))))

(defgeneric inner-token (memory token)
  (:method ((this existence-start-node) token) (cdddr token))
  (:method ((this filter-memory) token) (cddr token))
  (:method ((this memory) token) token))

(defgeneric store-get-token (memory token)
  (:method ((this memory) token)
    (let ((key (second (car token))))
      (checkit (numberp key) "key ~S not a number" key)
      (let ((item (gethash key (memory-store this))))
	(and item
	     (let ((stored-token (find token (cdr item) :test #'token-equal)))
	       (and stored-token
		    (let ((stored-token-inner (when-checkit (inner-token this stored-token))))
		      (declare (ignorable stored-token-inner))
		      (checkit (token-equal token stored-token-inner) "token~%~A and inner-token~%~A not equal~%~A" token stored-token-inner stored-token)
		      stored-token))))))))

(defgeneric store-put-token (memory token)
  (:method ((this memory) token)
    (let ((key (second (car token))))
      (checkit (numberp key) "key ~S not a number" key)
      (let ((item (gethash key (memory-store this))))
	(cond ((null item)
	       (setf (gethash key (memory-store this)) (list key token)))
	      (t
	       (checkit (not (find token (cdr item) :test #'token-equal)) "token ~S already in memory ~S" token this))
	       (setf (cdr item) (cons token (cdr item)))))
      (incf (store-count this))
      (incf (memory-store-put-count this)))))

(defgeneric store-remove-token (memory token)
  (:method ((this memory) token)
    (let ((key (second (car token))))
      (checkit (numberp key) "key ~S not a number" key)
      (let ((item (gethash key (memory-store this))))
	(checkit item "token ~S not in memory ~S" token this)
	(loop for rest on item
	      for other-token = (second rest)
	      while other-token
	      when (token-equal other-token token)
	      do (progn
		   (cond ((= 2 (length item))
			  (remhash key (memory-store this)))
			 (t
			  (setf (cdr rest) (cddr rest))))
		   (decf (store-count this))
		   (return))
	     finally (checkit nil "token ~S not in memory ~S" token this)))
      (incf (memory-store-remove-count this)))))

;;; Do not descrutively modify the result!
(defgeneric store-tokens (memory)
  (:method ((memory memory))
    (mapcan #'(lambda (v) (copy-list v))
	    (maph #'(lambda (k v)
		      (declare (ignore k))
		      (cdr v))
		  (memory-store memory)))))
    
;; (defgeneric store-tokens (memory)
;;   (:method ((memory memory))
;;     (let ((result nil)
;; 	  (last nil))
;;       (maphash #'(lambda (k v)
;; 		   (declare (ignore k))
;; 		   (let ((tokens (cdr v)))
;; 		     (assert* tokens "Tokens should not be NIL!")
;; 		     (cond ((null result)
;; 			    (setf result tokens))
;; 			   (t
;; 			    (setf (cdr last) tokens)))
;; 		     (setf last (last tokens))))
;; 	       (memory-store memory))
;;       result)))

(defgeneric store-clear (memory)
  (:method ((memory memory))
    (let ((table (memory-store memory)))
      (unless (null table)
	(clrhash table)))))
