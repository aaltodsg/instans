;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(defvar *minimum-sbcl-version-for-instans* "1.1.12")

(defun parse-version-number (version-string)
  (let* ((major-end-pos (position #\. version-string))
	 (minor-end-pos (and major-end-pos (position #\. version-string :start (1+ major-end-pos)))))
    (values (and major-end-pos (parse-integer version-string :start 0 :end major-end-pos))
	    (and minor-end-pos (parse-integer version-string :start (1+ major-end-pos) :end minor-end-pos))
	    (and minor-end-pos (parse-integer version-string :start (1+ minor-end-pos))))))

(defun version< (v1 v2)
  (multiple-value-bind (major1 minor1 revision1)
      (parse-version-number v1)
    (multiple-value-bind (major2 minor2 revision2)
	(parse-version-number v2)
      (and revision1 revision2
	   (or (< major1 major2)
	       (and (= major1 major2)
		    (or (< minor1 minor2)
			(and (= minor1 minor2)
			     (< revision1 revision2)))))))))

(defun output-configuration (&optional (target "LISP-CONFIGURATION"))
  (flet ((output (stream)
	   (format stream "minimum-sbcl-version-for-instans: ~A" *minimum-sbcl-version-for-instans*)
	   (format stream "~%lisp-implementation-type:         ~A" (lisp-implementation-type))
	   (format stream "~%lisp-implementation-version:      ~A" (lisp-implementation-version))
	   (format stream "~%consistent-sbcl-version:          ~:[no~;yes~]" (and (string= (lisp-implementation-type) "SBCL") (not (version< (lisp-implementation-version) *minimum-sbcl-version-for-instans*))))
	   (format stream "~%quicklisp:                        ~:[no~;yes~]" (find-package '#:ql))
	   (let ((asdf-present-p (handler-case
				  (progn
				    (require '#:asdf)
				    (member "ASDF" *modules* :test #'equalp))
				  (t () nil))))
	     (format stream "~%asdf:                             ~:[no~;yes~]~%" asdf-present-p))))
    (cond ((null target)
	   (with-output-to-string (stream)
	     (output stream)))
	  ((or (stringp target) (pathnamep target))
	   (with-open-file (stream target :direction :output :if-exists :supersede)
	     (output stream)))
	  ((or (eq target T) (and (streamp target) (output-stream-p target)))
	   (output target))
	  (t
	   (error "Don't know how to output to ~S" target)))))

;; (eval-when (:load-toplevel :execute)
;;   (output-configuration))
