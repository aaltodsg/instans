;;; -*- Mode: Lisp -*-

(in-package #:instans)

(define-class abstract-lexer ()
  ((input-stream :accessor lexer-input-stream :initarg :input-stream)
   (current-position :accessor lexer-current-position :initform -1)
   (newline-positions :accessor lexer-newline-positions :initarg :newline-positions :initform (list nil))
   (char :accessor lexer-char :initform nil)
   (unget-chars :accessor lexer-unget-chars :initform nil)))

(defmethod print-object ((this abstract-lexer) stream)
  (format stream "#<LEXER ~:[~A~;~C~] (~{~C~^ ~})>" (characterp (lexer-char this)) (lexer-char this) (lexer-unget-chars this)))

(defgeneric peekch (lexer)
  (:method ((lexer abstract-lexer))
    (when (null (lexer-char lexer))
      (setf (lexer-char lexer)
	    (cond ((null (lexer-unget-chars lexer))
		   (let ((ch (read-char (lexer-input-stream lexer) nil nil)))
		     (when (and ch (char= ch #\newline))
		       (push (lexer-current-position lexer) (cdr (lexer-newline-positions lexer))))
		     ch))
		  (t
		   (pop (lexer-unget-chars lexer)))))
      (incf (lexer-current-position lexer)))
    (lexer-char lexer)))

(defgeneric get-char (lexer)
  (:method ((lexer abstract-lexer))
    (prog1 (peekch lexer) (setf (lexer-char lexer) nil))))

(defgeneric unget-char (lexer ch)
  (:method ((lexer abstract-lexer) ch)
    (when (lexer-char lexer)
      (push (lexer-char lexer) (lexer-unget-chars lexer)))
    (decf (lexer-current-position lexer))
    (setf (lexer-char lexer) ch)))

(defgeneric get-input-token (lexer))

;;; nl-positions = (nil pos_n pos_n-1 ... pos1)
(defun position-to-row-and-column (position newline-positions)
  (let ((nl-positions (reverse (cdr newline-positions))))
    (cond ((or (null nl-positions) (null position))
	   (values 0 0))
	  (t
	   (loop for prev-nl-position = 0 then nl-position
		 for nl-position in nl-positions
		 for row from 0
		 when (> nl-position position)
		 return (values row (1- (- position prev-nl-position)))
		 finally (return (values (1+ row) (1- (- position nl-position)))))))))

(defgeneric lexer-position-to-row-and-column (lexer position)
  (:method ((lexer abstract-lexer) position)
    (position-to-row-and-column position (lexer-newline-positions lexer))))

