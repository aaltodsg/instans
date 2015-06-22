;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defstruct (datetime :named (:predicate datetimep) (:type list))
  (time)
  (usec)
  (year)
  (month)
  (day)
  (hours)
  (minutes)
  (seconds)
  (tz)
  (tz-string))

(defun datetime-fill-tz-string (dt)
  (let ((tz (datetime-tz dt)))
    (setf (datetime-tz-string dt)
	  (cond ((or (null tz) (zerop tz)) "")
		(t
		 (multiple-value-bind (tz-h tz-m) (floor (abs tz) 1)
		   (concatenate 'string (if (< tz 0) "-" "+") (format nil "~2,'0D:~2,'0D" tz-h (* 60 tz-m)))))))))

(defun datetime-fill-slots (dt)
  (multiple-value-bind (seconds minutes hours day month year day-of-week dst tz)
      (decode-universal-time (+ (datetime-time dt) sb-impl::unix-to-universal-time))
    (declare (ignore day-of-week))
    (when dst (incf tz))
    (setf (datetime-year dt) year)
    (setf (datetime-month dt) month)
    (setf (datetime-day dt) day)
    (setf (datetime-hours dt) hours)
    (setf (datetime-minutes dt) minutes)
    (setf (datetime-seconds dt) seconds)
    (setf (datetime-tz dt) tz)
    (datetime-fill-tz-string dt))
    dt)

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
  (make-datetime :time 0 :usec 0))

(defun datetime-now ()
  (multiple-value-bind (time-t suseconds-t)
      (sb-unix::get-time-of-day)
    (let ((dt (make-datetime :time time-t :usec suseconds-t)))
      (datetime-fill-slots dt)
      dt)))

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
	       (let ((dt (make-datetime
			  :time (- (encode-universal-time seconds minutes hours day month year tz) sb-impl::unix-to-universal-time) :usec usec
			  :year year :month month :day day :hours hours :minutes minutes :seconds seconds :tz-string tz-string :tz tz)))
		 (unless tz-string (datetime-fill-tz-string dt))
		 dt))
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
