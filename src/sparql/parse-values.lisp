;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun parse-xsd-integer (string &key (start 0) (end (length string)) (errorp t) (error-value nil))
  (flet ((parse-error (msg) (cond (errorp (signal-sparql-error msg))
				  (t (values error-value msg)))))
    (handler-case
	(multiple-value-bind (num ended)
	    (parse-integer string :start start :end end)
	  (cond ((= ended end)
		 num)
		(t
		 (parse-error "Could not parse an integer"))))
    (t (e) (parse-error (format nil "~A" e))))))

(defun parse-float-old (string &key (start 0) (end (length string)) (type 'double-float) (allow-exponent-p t))
  (let ((i 0)
	(length (- end start))
	(mantissa-sign 1)
	(mantissa-int 0)
	(mantissa-frac-length 0)
	(mantissa-frac 0)
	(exponent-sign 1)
	(exponent 0))
    (block inner
      (macrolet ((return-error (fmt &rest args) `(return-from inner (signal-sparql-error ,fmt ,@args)))
		 (getch () `(prog1 (char string (+ start i)) (incf i)))
		 (peekch () `(char string (+ start i)))
		 (looking-at (&rest chars) (if (null (cdr chars)) `(char= (peekch) ,(car chars)) `(member (peekch) ',chars :test #'char=))))
	(handler-case 
	    (progn
	      (cond ((looking-at #\+) (getch) (setf mantissa-sign 1))
		    ((looking-at #\-) (getch) (setf mantissa-sign -1)))
	      (when (or (= i length) (not (digit-char-p (peekch))))
		(return-error "Missing digits in ~S" string))
	      (loop while (and (< i length) (looking-at #\0))
		 do (getch))
	      (loop while (< i length)
		 for digit = (digit-char-p (peekch))
		 while digit
		 do (progn (getch) (setf mantissa-int (+ (* 10 mantissa-int) digit))))
	      (when (< i length)
		(when (looking-at #\.)
		  (getch)
		  (when (< i length)
		    (loop while (< i length)
		       for digit = (digit-char-p (peekch))
		       while digit
		       do (progn (getch)
				 (incf mantissa-frac-length)
				 (setf mantissa-frac (+ (* 10 mantissa-frac) digit))))))
		(when (and allow-exponent-p (< i length))
		  (when (looking-at #\e #\E)
		    (getch)
		    (when (= i length) (return-error "Missing exponent digits in ~A" string))
		    (cond ((looking-at #\+) (getch) (setf exponent-sign 1))
			  ((looking-at #\-) (getch) (setf exponent-sign -1)))
		    (when (or (= i length) (not (digit-char-p (peekch))))
		      (return-error "Missing digits in exponent of ~S" string))
		    (loop while (< i length)
		       for digit = (digit-char-p (peekch))
		       while digit
		       do (progn (getch) (setf exponent (+ (* 10 exponent) digit)))))))
	      (cond ((< i length)
		     (return-error "Junk after ~A at ~D" string i))
		    (t
		     (let ((mantissa (* mantissa-sign (+ (coerce mantissa-int type) (/ (coerce mantissa-frac type) (expt 1.0d1 mantissa-frac-length)))))
			   (exp (* (expt 1.0d1 (* exponent-sign exponent)))))
		       (coerce (* mantissa exp) type)))))
	  (t (e) (return-error "~A" e)))))))

(defun parse-float (string &key (start 0) (end (length string)) (type 'double-float) (allow-exponent-p t) (errorp t) (error-value nil))
  (let ((i 0)
	(length (- end start))
	(mantissa-sign 1)
	(mantissa-int 0)
	(mantissa-frac-length 0)
	(mantissa-frac 0)
	(exponent-sign 1)
	(exponent 0))
    (block inner
      (macrolet ((getch () `(prog1 (char string (+ start i)) (incf i)))
		 (peekch () `(char string (+ start i)))
		 (looking-at (&rest chars) (if (null (cdr chars)) `(char= (peekch) ,(car chars)) `(member (peekch) ',chars :test #'char=))))
	(flet ((return-error (fmt &rest args) (if errorp 
						  (return-from inner (apply #'signal-sparql-error fmt args))
						  (return-from inner (values error-value (apply #'format nil fmt args))))))
	  (handler-case 
	      (progn
		(cond ((looking-at #\+) (getch) (setf mantissa-sign 1))
		      ((looking-at #\-) (getch) (setf mantissa-sign -1)))
		(when (or (= i length) (not (or (digit-char-p (peekch)) (looking-at #\.))))
		  (return-error "Missing digits in ~A" (subseq string start end)))
		(loop while (and (< i length) (looking-at #\0))
		      do (getch))
		(loop while (< i length)
		      for digit = (digit-char-p (peekch))
		      while digit
		      do (progn (getch) (setf mantissa-int (+ (* 10 mantissa-int) digit))))
		(when (< i length)
		  (when (looking-at #\.)
		    (getch)
		    (when (< i length)
		      (loop while (< i length)
			    for digit = (digit-char-p (peekch))
			    while digit
			    do (progn (getch)
				      (incf mantissa-frac-length)
				      (setf mantissa-frac (+ (* 10 mantissa-frac) digit))))))
		  (when (and allow-exponent-p (< i length))
		    (when (looking-at #\e #\E)
		      (getch)
		      (when (= i length) (return-error "Missing exponent digits in ~A" (subseq string start end)))
		      (cond ((looking-at #\+) (getch) (setf exponent-sign 1))
			    ((looking-at #\-) (getch) (setf exponent-sign -1)))
		      (when (or (= i length) (not (digit-char-p (peekch))))
			(return-error "Missing digits in exponent of ~A" (subseq string start end)))
		      (loop while (< i length)
			    for digit = (digit-char-p (peekch))
			    while digit
			    do (progn (getch) (setf exponent (+ (* 10 exponent) digit)))))))
		(cond ((< i length)
		       (return-error "Junk after ~A at ~D" (subseq string start end) i))
		      (t
		       (let ((mantissa (* mantissa-sign (+ (coerce mantissa-int type) (/ (coerce mantissa-frac type) (expt 1.0d1 mantissa-frac-length)))))
			     (exp (* (expt 1.0d1 (* exponent-sign exponent)))))
			 (coerce (* mantissa exp) type)))))
	    (t (e) (return-error "~A" e))))))))

(defun parse-xsd-decimal (string &key (start 0) (end (length string)) (errorp t) (error-value nil))
  (parse-float string :start start :end end :type 'single-float :allow-exponent-p nil :errorp errorp :error-value error-value))

(defun parse-xsd-float (string &key (start 0) (end (length string)) (errorp t) (error-value nil))
  (parse-float string :start start :end end :type 'single-float :errorp errorp :error-value error-value))

(defun parse-xsd-double (string &key (start 0) (end (length string)) (errorp t) (error-value nil))
  (parse-float string :start start :end end :type 'double-float :errorp errorp :error-value error-value))

(defun parse-xsd-boolean (string &key (errorp t) (error-value nil))
  (cond ((string-equal string "true") t)
	((string-equal string "false") nil)
	(errorp
	 (signal-sparql-error "Unable to parse a boolean from ~S" string))
	(t
	 (values error-value (format nil  "Unable to parse a boolean from ~S" string)))))

(defun parse-xsd-datetime (string &key (errorp t) (error-value nil))
  (multiple-value-bind (dt error-msg) (datetime-from-string string)
    (cond ((null dt)
	   (cond ((not errorp)
		  (values error-value error-msg))
		 (t
		  (signal-sparql-error error-msg))))
	  (t dt))))

(defun parse-xsd-string (string) string)

(defun parse-xsd-value (string)
  (let (value msg)
;    (inform "about to parse ~S as an integer" string)
    (multiple-value-setq (value msg) (parse-xsd-boolean string :errorp nil))
    (cond ((null msg) value)
	  (t
	   (multiple-value-setq (value msg) (parse-xsd-integer string :errorp nil))
	   (cond ((null msg) value)
		 (t
		  (multiple-value-setq (value msg) (parse-xsd-decimal string :errorp nil))
		  (cond ((null msg) value)
			(t
			 (multiple-value-setq (value msg) (parse-xsd-double string :errorp nil))
			 (cond ((null msg) value)
			       (t
				(multiple-value-setq (value msg) (parse-xsd-datetime string :errorp nil))
				(cond ((null msg) value)
				      (t
				       string))))))))))))
		      
    
