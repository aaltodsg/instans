;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-output-function inform)

(defun error* (fmt &rest args)
  (error (apply #'format nil fmt args)))

(defun quotify-list (l)
  (mapcar #'(lambda (x) (if (symbolp x) (list 'quote x) x)) l))

;;; This retains the order and possible duplicates of the arguments.
(defun list-union (list1 list2 &key (test #'eql))
  (cond ((null list1) list2)
	((null list2) list1)
	(t
	 (append list1 (filter #'(lambda (x) (not (member x list1 :test test))) list2)))))

;;; The result is in the same order as list1 and it contains the matching duplicate elements in list1.
(defun list-intersection (list1 list2 &key (test #'eql))
  (cond ((or (null list1) (null list2)) nil)
	(t
	 (filter #'(lambda (x) (member x list2 :test test)) list1))))

;;; The result is in the same order as list1 and it contains the matching duplicate elements in list1.
(defun list-difference (list1 list2 &key (test #'eql))
  (filter #'(lambda (x) (not (member x list2 :test test))) list1))

(defun maph (func hash-table)
  (let ((result nil)
	(last nil))
    (maphash #'(lambda (k v)
		 (let ((val (funcall func k v)))
		   (cond ((null result)
			  (setf result (list val))
			  (setf last result))
			 (t
			  (setf (cdr last) (list val))
			  (setf last (cdr last))))))
	     hash-table)
    result))

;;; Used by define-class
(defun predicate-name (name)
  (cond ((find #\- (coerce (string name) 'list))
	 (fmt-intern "~:@(~A-p~)" name))
	(t
	 (fmt-intern "~:@(~Ap~)" name))))

(defun camel-case (string)
  (coerce (loop with humpp = nil
		for ch in (coerce string 'list)
		when (alpha-char-p ch)
		collect (cond ((not humpp) (char-downcase ch))
			      (t (setf humpp nil) (char-upcase ch)))
	        else when (char= ch #\-) 
		do (setf humpp t)
		else 
		collect ch)
	  'string))

(defun http-iri-string-p (str)
  (and (stringp str)
       (or (and (>= (length str) 5) (string= (subseq str 0 5) "http:"))
	   (and (>= (length str) 6) (string= (subseq str 0 6) "https:")))))

(defun file-iri-string-p (str)
  (and (stringp str) (>= (length str) 7) (string= (subseq str 0 7) "file://")))

(defun file-iri-string-path (str)
  (and (file-iri-string-p str)
       (subseq str 7)))

(defun http-or-file-iri-string-p (str)
  (or (http-iri-string-p str) (file-iri-string-p str)))

;;; Char ops accepting nil
(defun char-code* (char-or-code) (if (characterp char-or-code) (char-code char-or-code) char-or-code))

(defun char=* (x y) (and x y (= (char-code* x) (char-code* y))))

(defun digit-char-p* (ch &optional (radix 10)) (and ch (digit-char-p ch radix)))

;;;

(defun split-string (string delimiter &optional (start 0))
  (if (>= start (length string)) nil
      (let ((end (or (search delimiter string :start2 start) (length string))))
	(cons (subseq string start end) (split-string string delimiter (+ end (length delimiter)))))))

(defun split-string-with-backslash-escapes (string delimiter &optional (start 0))
  (if (>= start (length string)) nil
      (let ((end (loop with i = start
		       do (cond ((>= i (length string)) (return i))
				((char= (char string i) #\\) (incf i 2))
				((eql i (search delimiter string :start2 i)) (return i))
				(t (incf i))))))
	(cons (subseq string start end) (split-string-with-backslash-escapes string delimiter (+ end (length delimiter)))))))

(defun stream-contents-to-string (stream)
  (with-output-to-string (output)
    (loop for line = (read-line stream nil nil)
	  while line do (format output "~A~%" line))))

(defun http-get-to-string (iri-string)
  (let ((data (drakma:http-request iri-string)))
    (cond ((stringp data) data) (t (coerce (mapcar #'code-char (coerce data 'list)) 'string)))))

(defun parse-colon-separated-values (string)
  (loop while (> (length string) 0)
        collect (let ((pos (or (position #\: string) (length string))))
		  (prog1 (intern (string-upcase (subseq string 0 pos)) :keyword) (setf string (subseq string (min (length string) (1+ pos))))))))

(defun parse-spec-string (string)
  (let ((directives (split-string-with-backslash-escapes string "&")))
    (loop for directive in directives
	  for pieces = (split-string-with-backslash-escapes directive "=")
	  when (not (= (length pieces) 2))
	  do (return-from parse-spec-string (values nil (format nil "Illegal form in specification: ~S" pieces)))
	  else collect (list (intern (string-upcase (first pieces)) :keyword) (second pieces)))))


(defun create-temp-file-name (&key (directory ".") (name-prefix "tmp") type)
  (let ((dir-path (namestring (probe-file directory))))
    (loop for i from 0
	  for file-name = (format nil "~A~A~D~@[.~(~A~)~]" dir-path name-prefix i type)
	  for path = (probe-file file-name)
	  unless path
	  return file-name)))

(defun create-empty-temp-file (&rest keys &key (directory ".") (name-prefix "tmp") type)
  (declare (ignorable directory name-prefix type))
  (let ((file-name (apply #'create-temp-file-name keys)))
    (with-open-file (str file-name :direction :output)
      file-name)))

(defun expand-dirname (directory)
  (let ((chars (remove-dot-segments (coerce (namestring (format nil "~A~A" (namestring (probe-file ".")) directory)) 'list))))
    (unless (char= (first (last chars)) #\/)
      (setf (cdr (last chars)) (list #\/)))
    (coerce chars 'string)))

#+sbcl
(defun shell-script (script &rest args)
  (let ((process (sb-ext:run-program "/bin/sh" (cons script args) :output t :error :output)))
    process))
