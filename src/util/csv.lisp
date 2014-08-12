;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class csv-output ()
  ((stream :accessor csv-output-stream :initarg :stream)
   (separator :accessor csv-output-separator :initarg :separator :initform (format nil "~C" #\linefeed))
   (headers :accessor csv-output-headers)
   (require-headers-p :accessor csv-output-require-headers-p :initarg :require-headers-p :initform t)))

(defgeneric write-csv-headers (csv-output headers)
  (:method ((this csv-output) headers)
    (when (slot-boundp this 'headers)
      (error* "Trying to write headers twice in ~S" this))
    (setf (csv-output-headers this) headers)
    (write-csv-record this headers)))

(defgeneric write-csv-record (csv-output record)
  (:method ((this csv-output) record)
    (unless (null record)
      (when (and (csv-output-require-headers-p this) (not (slot-boundp this 'headers)))
	(error* "Trying to write record without writing headers first"))
      (let* ((cr (code-char #x0D))
	     (lf (code-char #x0A))
	     (dquote (code-char #x22))
	     (comma (code-char #x2C))
	     (escape-chars (list cr lf dquote comma))
	     (stream (csv-output-stream this)))
	(loop for field in record
	      for separator = nil then comma
	      do (progn
		   (when separator (princ separator stream))
		   (cond ((and (stringp field) (some #'(lambda (x) (char-in-set-p* x escape-chars)) field))
			  (princ dquote stream)
			  (loop for i from 0 below (length field)
				for char = (char field i)
				when (char= char dquote) do (princ dquote stream)
				do (princ char stream))
			  (princ dquote stream))
			 (t
			  (princ field stream)))))
	(princ (csv-output-separator this) stream)))))
