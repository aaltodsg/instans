;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class table-binding () 
  ((key :accessor table-binding-key :initarg :key)
   (case-sensitive-p :accessor table-binding-case-sensitive-p :initarg :case-sensitive-p)
   (value :accessor table-binding-value :initarg :value)))

(define-class bindings-table ()
  ((name :accessor bindings-table-name :initarg :name)
   (case-sensitive-p :accessor bindings-table-case-sensitive-p :initarg :case-sensitive-p)
   (hash-table :accessor bindings-table-hash-table :initform (make-hash-table))))

(defun hash-characters (characters &key (start 0) (end (length characters)) (case-sensitive-p nil))
  (loop with hv = (sxhash nil)
	for i from start below end
	for ch = (if case-sensitive-p (elt characters i) (char-upcase (elt characters i)))
	do (setf hv (mix (sxhash ch) hv))
	finally (return hv)))

(defun characters-to-string (characters &optional (start 0) (end (length characters)))
  (let* ((length (- end start))
	 (string (make-string length)))
    (loop for i from 0 below length do (setf (aref string i) (elt characters (+ start i))))
    string))

(defun table-binding-key-matches-p (binding characters &optional (start 0) (end (length characters)))
  (let* ((length (- end start))
	 (key (table-binding-key binding)))
    (loop for i from 0 below length unless (char= (char key i) (elt characters (+ start i))) do (return nil)
	  finally (return t))))

(defgeneric add-table-binding (table characters &key start end case-sensitive-p)
  (:method ((table bindings-table) characters &key (start 0) (end (length characters)) (case-sensitive-p (bindings-table-case-sensitive-p table)))
    (when (and (bindings-table-case-sensitive-p table) (not case-sensitive-p))
      (error "Cannot add a case insensitive binding ~S to a case sensitive table ~S" (subseq characters start end) table)) 
    (let* ((hv (hash-characters characters :start start :end end :case-sensitive-p (bindings-table-case-sensitive-p table)))
	   (binding (gethash hv (bindings-table-hash-table table))))
;      (inform "add hv = ~S, binding = ~S, binding-value = ~S, binding-case = ~S" hv binding (and binding (table-binding-value binding)) (and binding (table-binding-case-sensitive-p binding)))
      (cond ((null binding)
	     (setf binding (make-instance 'table-binding :key (characters-to-string characters start end) :case-sensitive-p case-sensitive-p))
	     (setf (gethash hv (bindings-table-hash-table table)) binding))
	    (t
	     (error* "~S already bound in ~S" (characters-to-string characters start end) table))))))

(defgeneric get-table-binding (table characters &key start end case-sensitive-p)
  (:method ((table bindings-table) characters &key (start 0) (end (length characters)) (case-sensitive-p (bindings-table-case-sensitive-p table)))
;    (inform "get-table-binding ~S (~D ~D) of (~S)" (characters-to-string characters start end) start end characters)
    (let* ((hv (hash-characters characters :start start :end end :case-sensitive-p case-sensitive-p))
	   (binding (gethash hv (bindings-table-hash-table table))))
;      (inform "get hv = ~S, binding = ~S, binding-value = ~S, binding-case = ~S" hv binding (and binding (table-binding-value binding)) (and binding (table-binding-case-sensitive-p binding)))
      (cond ((null binding)
	     (values nil :not-exists))
	    ((table-binding-case-sensitive-p binding)
	     (cond ((table-binding-key-matches-p binding characters start end) binding)
		   (t (values nil :case-mismatch))))
	    (t binding)))))

;; (defgeneric remove-table-binding (table characters &key start end)
;;   (:method ((table bindings-table) characters &key (start 0) (end (length characters)))
;;     (let* ((hv (hash-characters characters :start start :end end))
;; 	   (binding (gethash hv (bindings-table-hash-table table))))
;;       (cond ((null binding)
;; 	     (values nil :not-exists))
;; 	    ((table-binding-case-sensitive-p binding)
;; 	     (cond ((table-binding-key-matches-p binding characters start end)
;; 		    (setf (gethash hv (bindings-table-hash-table table)) nil))
;; 		   (t
;; 		    (values nil :case-mismatch))))))))
