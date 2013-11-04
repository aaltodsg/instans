;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class datetime ()
  ((time :initarg :time :accessor datetime-time)
   (usec :initarg :usec :accessor datetime-usec)
   (year :initform nil :initarg :year)
   (month :initform nil :initarg :month)
   (day :initform nil :initarg :day)
   (hours :initform nil :initarg :hours)
   (minutes :initform nil :initarg :minutes)
   (seconds :initform nil :initarg :seconds)
   (tz :initform nil :initarg :tz)
   (tz-str :initform nil :initarg :tz-str)))

(defmethod print-object ((this datetime) stream)
  (format stream "#<~S ~S>" (type-of this) (datetime-canonic-string this)))

(defun datetime-fill-slots (x)
  (multiple-value-bind (seconds minutes hours day month year day-of-week dst tz)
      (decode-universal-time (+ (datetime-time x) sb-impl::unix-to-universal-time))
    (declare (ignore day-of-week))
    (when dst (incf tz))
    (setf (slot-value x 'year) year)
    (setf (slot-value x 'month) month)
    (setf (slot-value x 'day) day)
    (setf (slot-value x 'hours) hours)
    (setf (slot-value x 'minutes) minutes)
    (setf (slot-value x 'seconds) seconds)
    (setf (slot-value x 'tz) tz)))

(defmacro define-datetime-lazy-getter (slot-name)
  (let ((getter-name (intern (format nil "~A-~A" 'datetime slot-name))))
    `(defun ,getter-name (x)
       (unless (slot-value x 'year)
	 (datetime-fill-slots x))
       (slot-value x ',slot-name))))

(define-datetime-lazy-getter year)
(define-datetime-lazy-getter month)
(define-datetime-lazy-getter day)
(define-datetime-lazy-getter hours)
(define-datetime-lazy-getter minutes)
(define-datetime-lazy-getter seconds)
(define-datetime-lazy-getter tz)

(defun datetime-tz-string (dt)
  (or (slot-value dt 'tz-str)
      (let* ((tz (datetime-tz dt))
	     (tz-str (cond ((or (null tz) (zerop tz)) "")
			   (t
			    (multiple-value-bind (tz-h tz-m) (floor (abs tz) 1)
			      (concatenate 'string (if (< tz 0) "-" "+") (format nil "~2,'0D:~2,'0D" tz-h (* 60 tz-m))))))))
	(setf (slot-value dt 'tz-str) tz-str)
	tz-str)))

(defun datetime-compare (dt1 dt2)
  (let ((ut1 (datetime-time dt1))
	(ut2 (datetime-time dt2)))
    (cond ((< ut1 ut2) -1)
	  ((> ut1 ut2) 1)
	  (t
	   (signum (- (datetime-usec dt1) (datetime-usec dt2)))))))

(defun datetime= (dt1 dt2)
  (= (datetime-compare dt1 dt2) 0))

(defun datetime< (dt1 dt2)
  (< (datetime-compare dt1 dt2) 0))

(defun datetime<= (dt1 dt2)
  (<= (datetime-compare dt1 dt2) 0))

(defun datetime> (dt1 dt2)
  (> (datetime-compare dt1 dt2) 0))

(defun datetime>= (dt1 dt2)
  (>= (datetime-compare dt1 dt2) 0))

(defun datetime-plain ()
  (make-instance 'datetime :time 0 :usec 0))

(defun datetime-now ()
  (multiple-value-bind (time-t suseconds-t)
      (sb-unix::get-time-of-day)
    (make-instance 'datetime :time time-t :usec suseconds-t)))

(defun datetime-from-string (str)
  (let (year month day hours minutes seconds usec tz-string tz)
    (catch 'parse-error
      (let ((strlen (length str))
	    (error-msg nil)
	    (current-position 0))
	(labels ((looking-at (ch) (and (< current-position strlen) (char= (char str current-position) ch)))
		 (eat-int (&optional length delim)
		   (loop with result = 0
			 with start = current-position
			 with max = (if (null length) strlen (min strlen (+ start length)))
			 while (and (< current-position max)
				    (let ((ch (char str current-position)))
				      (if (digit-char-p ch)
					  (progn 
					    (setf result (+ (* 10 result) (+ (- (char-code ch) (char-code #\0)))))
					    (incf current-position))
					  nil)))
			 finally (return (progn
					   (cond ((null length)
						  (when (= current-position start) (parse-error "Could not parse an int in ~S starting at index ~D" str start)))
						 (t
						  (when (not (= current-position (+ start length))) (parse-error "Could not parse an int of length ~D in ~S starting at index ~D" length str start))))
					   (when delim (if (looking-at delim) (incf current-position) (parse-error "Missing delimiter '~A' in ~S at ~D" delim str current-position)))
					   result))))
		 (parse-error (fmt &rest args)
		   (setf error-msg (apply #'format nil fmt args))
		   (throw 'parse-error (values nil error-msg))))
	  (let* ((year-sign (if (looking-at #\-) (progn (incf current-position) -1) 1))
	  	 (cp current-position))
	    (setf year (eat-int nil #\-))
	    (when (zerop year) (parse-error "No year zero allowed in ~S" str))
	    (when (and (> (- current-position cp) 4) (char= (char str cp) #\0))
	      (parse-error "Leading '0' not allowed in year in ~S" str))
	    (setf year (* year-sign year)))
	  (setf month (eat-int 2 #\-))
	  (setf day (eat-int 2 #\T))
	  (setf hours (eat-int 2 #\:))
	  (setf minutes (eat-int 2 #\:))
	  (setf seconds (eat-int 2))
	  (cond ((looking-at #\.)
		 (incf current-position)
		 (let ((cp current-position))
		   (setf usec (eat-int))
		   (loop for i from 1 to (- 6 (- current-position cp))
			 do (setf usec (* 10 usec)))))
		(t
		 (setf usec 0)))
					;(setf tz-string (subseq str current-position))
	  (cond ((< current-position strlen)
		 (let (tz-sign tz-hours tz-minutes)
		   (cond ((looking-at #\Z)
			  (incf current-position)
			  (setf tz 0)
			  (setf tz-string "Z"))
			 ((or (looking-at #\+) (looking-at #\-))
			  (setf tz-sign (if (looking-at #\+) 1 -1))
			  (incf current-position)
			  (setf tz-hours (eat-int 2 #\:))
			  (setf tz-minutes (eat-int 2))
			  (setf tz (* tz-sign (+ tz-hours (/ tz-minutes 60)))))
			 (t (parse-error "Illegal timezone in ~S" str)))
		   (when (< current-position strlen)
		     (parse-error "Garbage after timezone in ~S" str))))
		(t (setf tz nil))))
	;;nil
	(cond ((null error-msg)
	       (make-instance 'datetime
			      :time (- (encode-universal-time seconds minutes hours day month year tz) sb-impl::unix-to-universal-time) :usec usec
			      :year year :month month :day day :hours hours :minutes minutes :seconds seconds :tz-str tz-string :tz tz))
	      (t
	       (list nil error-msg)))
	))))

(defun datetime-canonic-string (dt)
  (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~{~C~}~A"
	  (datetime-year dt)
	  (datetime-month dt)
	  (datetime-day dt)
	  (datetime-hours dt)
	  (datetime-minutes dt)
	  (datetime-seconds dt)
	  (let ((num (datetime-usec dt)))
	    (and (> num 0)
		 (loop with characters = nil
		       with digit
		       for i from 1 to 6
		       do (progn
			    (multiple-value-setq (num digit) (floor num 10))
			    (when (or (> digit 0) (not (null characters)))
			      (push (code-char (+ digit (char-code #\0))) characters)))
		       finally (return (cons #\. characters)))))
	  (datetime-tz-string dt)))

(defun datetime-in-seconds (dt)
  (encode-universal-time (datetime-seconds dt)
			 (datetime-minutes dt)
			 (datetime-hours dt)
			 (datetime-day dt)
			 (datetime-month dt)
			 (datetime-year dt)
			 (datetime-tz dt)))
