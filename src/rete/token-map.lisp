;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;; (defgeneric token-map-put (token-map token value)
;;   (:method ((this token-map) token value)
;;     (let ((a (assoc token (token-map-map this) :test #'token-equal)))
;;       (cond ((null a)
;; 	     (setf (token-map-map this) (cons (cons token value) (token-map-map this))))
;; 	    (t
;; 	     (setf (cdr a) value))))
;;     value))

(defgeneric token-map-contents (token-map)
  (:method ((this token-map))
    (let ((result nil))
      (maphash #'(lambda (k v) (push (list k v) result)) (token-map-map this))
      result)))

(defgeneric token-map-put (token-map token value)
  (:method ((this token-map) token value)
    (let* ((ht (token-map-map this)))
      (setf (gethash token ht) value))))

;; (defgeneric token-map-get (token-map token)
;;   (:method ((this token-map) token)
;;     (let ((a (assoc token (token-map-map this) :test #'token-equal)))
;;       (cond ((null a)
;; 	     (inform "Help: ~S not in ~S~%Contents is ~S" token this (token-map-map this))
;; 	     (error* "token-map-get: Missing token ~A in token map ~A" token this))
;; 	    (t
;; 	     (cdr a))))))

(defgeneric token-map-get (token-map token)
  (:method ((this token-map) token)
    (let ((hit (gethash token (token-map-map this))))
      (cond ((null hit)
	     (inform "Help: ~S not in ~S~%Contents is ~S" token this (token-map-map this))
	     (error* "token-map-get: Missing token ~A in token map ~A" token this))
	    (t hit)))))

;; (defgeneric token-map-remove (token-map token)
;;   (:method ((this token-map) token)
;;     (let ((a (assoc token (token-map-map this) :test #'token-equal)))
;;       (cond ((null a)
;; 	     (error* "token-map-remove: Missing token ~A in token map ~A" token this))
;; 	    (t
;; 	     (setf (token-map-map this) (delete a (token-map-map this))))))
;;     nil))

(defgeneric token-map-remove (token-map token)
  (:method ((this token-map) token)
    (let ((ht (token-map-map this)))
      (checkit (gethash token ht) "token ~S not in token-map ~S" token this)
      (remhash token (token-map-map this)))))
