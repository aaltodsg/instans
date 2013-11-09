;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defmethod initialize-instance :after ((this quad-store) &rest keys &key &allow-other-keys)
  (let ((initial-quads (getf keys :initial-quads)))
    (loop for quad in initial-quads do (add-quad (instans-quad-store this) quad))))

(defgeneric add-quad (quad-store quad)
  (:method ((this list-quad-store) quad)
    (pushnew quad (list-quad-store-quads this) :test #'equal)))

(defgeneric remove-quad (quad-store quad)
  (:method ((this list-quad-store) quad)
    (setf (list-quad-store-quads this) (delete quad (list-quad-store-quads this) :test #'equal :count 1))))

(defgeneric quad-store-quads (quad-store)
  (:method ((this list-quad-store))
    (list-quad-store-quads this)))


