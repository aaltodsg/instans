;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-output-function inform)

;; (defun replace-by (subseq1 subseq2 sequence)
;;   (let* ((ls1 (length subseq1))
;; ;	 (ls2 (length subseq2))
;; 	 (ls (length sequence))
;; 	 (steps (loop for index = 0 then (+ hit ls1)
;; 		      for prevhit = nil then hit
;; 		      for hit = (and (< index ls) (search subseq1 sequence :start2 index))
;; 		      while hit
;; 		      collect (if prevhit (- hit prevhit) hit))))
;;     (inform "steps = ~A" steps)
;;     (cond ((null steps)
;; 	   sequence)
;; 	  (t
;; 	   (let ((to nil)
;; 		 (from (coerce sequence 'list))
;; 		 (replacement (coerce subseq2 'list)))
;; 	     (inform "from = ~A, to = ~A" from to)
;; 	     (loop for step in steps
;; 		   for popafter = 0 then ls1
;; 		   do (inform "step = ~a" step)
;; 		   do (loop repeat (- step popafter) do (push (pop from) to))
;; 		   do (loop for item in replacement do (push item to))
;; 		   do (setf from (nthcdr ls1 from))
;; 		   do (inform "from = ~A, to = ~A" from to))
;; 	     (loop while from do (push (pop from) to))
;; 	     (inform "from = ~A, to = ~A" from to)
;; 	     (coerce (nreverse to) (type-of sequence)))))))

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

(defun list-subset (sub super &key (test #'eql))
  (loop for x in sub
        unless (member x super :test test)
        return nil)
  t)

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

(defun file-iri-or-filename-as-string (x)
  (cond ((pathnamep x)
	 (namestring x))
	((rdf-iri-p x)
	 (cond ((equal (rdf-iri-scheme x) "file")
		(rdf-iri-path x))
	       (t
		nil)))
	((file-iri-string-p x)
	 (file-iri-string-path x))
	((stringp x) x)
	(t nil)))

;;; Char ops accepting nil
(defun char-code* (char-or-code) (if (characterp char-or-code) (char-code char-or-code) char-or-code))

(defun char=* (x y) (and x y (= (char-code* x) (char-code* y))))

(defun digit-char-p* (ch &optional (radix 10)) (and ch (digit-char-p ch radix)))

;;;

(defun hex-char-to-int (ch)
  (let ((cc (char-code ch)))
    (cond ((<= (char-code #\0) cc (char-code #\9))
	   (- (char-code ch) (char-code #\0)))
	  ((<= (char-code #\A) cc (char-code #\F))
	   (+ 10 (- (char-code ch) (char-code #\A))))
	  ((<= (char-code #\a) cc (char-code #\f))
	   (+ 10 (- (char-code ch) (char-code #\a))))
	  (t
	   (error* "Not a hex digit ~S" ch)))))

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

(defun file-contents-to-string (file)
  (with-open-file (stream file)
    (stream-contents-to-string stream)))

(defun http-get-to-string (iri-string)
  (let ((data (drakma:http-request iri-string)))
    (cond ((stringp data) data) (t (coerce (mapcar #'code-char (coerce data 'list)) 'string)))))

(defun intern-colon-separated-keywords (string)
  (loop while (> (length string) 0)
        collect (let ((pos (or (position #\: string) (length string))))
		  (prog1 (intern-keyword (string-upcase (subseq string 0 pos)))
		    (setf string (subseq string (min (length string) (1+ pos))))))))

(defun parse-colon-separated-strings (string)
  (loop while (> (length string) 0)
        collect (let ((pos (or (position #\: string) (length string))))
		  (prog1 (subseq string 0 pos)
		    (setf string (subseq string (min (length string) (1+ pos))))))))

(defun parse-spec-string (string)
  (let ((directives (split-string-with-backslash-escapes string "&")))
    (loop for directive in directives
	  for pieces = (split-string-with-backslash-escapes directive "=")
	  when (not (= (length pieces) 2))
	  do (return-from parse-spec-string (values nil (format nil "Illegal form in specification: ~S" pieces)))
	  else collect (list (intern-keyword (string-upcase (first pieces))) (second pieces)))))


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
  (setf directory (namestring directory))
  (let ((chars (remove-dot-segments (coerce (if (char= (char directory 0) #\/) directory (namestring (format nil "~A~A" (namestring (probe-file ".")) directory))) 'list))))
    (unless (char= (first (last chars)) #\/)
      (setf (cdr (last chars)) (list #\/)))
    (coerce chars 'string)))

(defun directoryp (name)
  (let ((dn (probe-file (expand-dirname name))))
    (cond ((null dn) nil)
	  (t
	   (setf dn (namestring dn))
	   (char= (char dn (1- (length dn))) #\/)))))

(defun split-path-to-name-and-type-strings (path)
  (cond ((pathnamep path)
	 (split-path-to-name-and-type-strings (namestring path)))
	(t
	 (let* ((fn (file-namestring path))
		(last-dot-index (position #\. fn :from-end t)))
	   (cond ((null last-dot-index) (error* "Missing file type in ~A" path))
		 (t
		  (values (directory-namestring path) (subseq fn 0 last-dot-index) (subseq fn (1+ last-dot-index)))))))))

(defvar *stream-open-close-report-output* nil)

(defun close-stream (stream &key (message "closing ~A") (report-stream *stream-open-close-report-output*))
  (declare (ignorable message))
  (when report-stream (format report-stream "~&~A~%" (format nil message stream)))
  (close stream))

(defun close-stream-not-stdout-stderr (stream &key (message "closing ~A") (report-stream *stream-open-close-report-output*))
  (unless (or (eq stream *standard-output*) (eq stream *stream-open-close-report-output*) ); (not (open-stream-p stream)))
    (close-stream stream :message message :report-stream report-stream)))

(defun open-file (file &rest keys &key (message "opening ~{~A~^ ~}") (report-stream *stream-open-close-report-output*) &allow-other-keys)
  (declare (ignorable message))
  (remf keys :message)
  (when report-stream
    (setf keys (copy-list keys))
    (format report-stream "~&~A~%" (format nil message (list file keys))))
  (apply #'open file keys))

;;;

(defun debugp (subscribe &rest publish)
  (and (not (null publish))
       (or (eq subscribe t)
	   (eq subscribe :all)
	   (some #'(lambda (key) (member key subscribe)) publish))))

;;;



#+sbcl
(defun shell-script (script &rest args)
  (let ((process (sb-ext:run-program "/bin/sh" (cons script args) :output t :error :output)))
    process))

(defun shell-cmd (cmd &rest args)
  (let ((process nil)
	(output-string nil)
	(error-string nil))
    (setf error-string (with-output-to-string (error-stream)
			 (setf output-string 
			       (with-output-to-string (output-stream)
				 (setf process (sb-ext:run-program cmd args :output output-stream :error error-stream :search t))))))
    (values (zerop (sb-ext:process-exit-code process)) output-string error-string))) 

;;;

;; (define-class insert-ordered-multivalue-table ()
;;   ((data :accessor insert-ordered-multivalue-table-data :initform (make-hash-table :test #'equal))
;;    (order :accessor insert-ordered-multivalue-table-order :initform nil)
;;    (order-tail :accessor insert-ordered-multivalue-table-order-tail :initform nil)))

;; (defgeneric add-to-ordered-table (table key value &optional new-level-if-does-not-exist-p)
;;   (:method ((this insert-ordered-multivalue-table) key value)
;;     (let* ((table (insert-ordered-multivalue-table-data this))
;; 	   (item (gethash key table)))
;;       (cond ((null item)
;; 	     (setf item (setf (gethash key table) (make-instance 'insert-ordered-multivalue-table)))
;; 	     (cond ((null (insert-ordered-multivalue-table-order this))
;; 		    (setf (insert-ordered-multivalue-table-order this) (list key))
;; 		    (setf (insert-ordered-multivalue-table-order-tail this) (insert-ordered-multivalue-table-order this)))
;; 		   (t
;; 		    (setf (cdr (insert-ordered-multivalue-table-order-tail this)) (list key))
;; 		    (setf (insert-ordered-multivalue-table-order-tail this) (cdr (insert-ordered-multivalue-table-order-tail this))))))
;; 	    (t
;; 	     (

;; (defgeneric get-ordered-multivalue-table-values (table key)
;;   (:method ((this insert-ordered-multivalue-table) key)
;;     (gethash key (insert-ordered-multivalue-table-data this))))
