;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun whitespace-char-p (ch) (char-in-set-p* ch '(#x20 #x09 #x0D #x0A)))

(defun eol-char-p (ch) (or (char=* ch #x0A) (char=* ch #x0D)))

(defun spacing-char-p (ch) (or (char=* ch #x20) (char=* ch #x09)))

(defun char-in-set-p* (char-or-code chars-or-codes)
  (cond ((stringp chars-or-codes)
	 (loop for i from 0 below (length chars-or-codes)
	       when (char=* (char chars-or-codes i) char-or-code)
	       return t))
	(t
	 (loop for x in chars-or-codes when (char=* x char-or-code) return t))))

(defmacro char-in-ranges-p* (char &rest ranges)
  (let ((code (gensym))
	(ch (gensym)))
    `(let ((,ch ,char))
       (and ,ch (let ((,code (char-code ,ch)))
		  (or ,@(loop for (start . end) in ranges
			      collect `(<= ,(if (characterp start) (char-code start) start) ,code ,(if (characterp end) (char-code end) end)))))))))

;; (defmacro char-in-ranges-new-p* (char &rest ranges-or-chars-or-codes)
;;   (let ((code (gensym))
;; 	(ch (gensym))
;; 	(items (loop for x in ranges-or-chars-or-codes append (cond ((consp x) (if (listp (cdr x)) x (list x))) (t (list x))))))
;;     `(let ((,ch ,char))
;;        (and ,ch (let ((,code (char-code ,ch)))
;; 		  (or ,@(loop for item in items
;; 			      do (inform "item = ~A" item)
;; 			      collect (cond ((consp item)
;; 					     (destructuring-bind (start . end) item
;; 					       `(<= ,(if (characterp start) (char-code start) start) ,code ,(if (characterp end) (char-code end) end))))
;; 					    ((characterp item)
;; 					     `(= ,(char-code item) ,code))
;; 					    (t
;; 					     `(= ,item ,code))))))))))

;; (defconst *pn-chars-base-ranges* '((#\A . #\Z) (#\a . #\z) (#x00C0 . #x00D6) (#x00D8 . #x00F6) (#x00F8 . #x02FF) (#x0370 . #x037D)
;; 				   (#x037F . #x1FFF) (#x200C . #x200D) (#x2070 . #x218F) (#x2C00 . #x2FEF) (#x3001 . #xD7FF)
;; 				   (#xF900 . #xFDCF) (#xFDF0 . #xFFFD) (#x10000 . #xEFFFF)))

;; (defconst *digit-char-range* '(#\0 . #\9))

;; (defconst *pn-chars-u-ranges* (append *pn-chars-base-ranges* '(#\_)))

;; (defconst *pn-chars-u-digit-ranges* (append *pn-chars-u-ranges* (list *digit-char-range*)))

;; (defconst *pn-chars-ranges* (append *pn-chars-u-digit-ranges* '(#\- #x0087 (#x0300 . #x036F) (#x203F . #x2040))))

;; (defconst *pn-chars-dot-ranges* (append *pn-chars-ranges* '(#\.)))

;; (defconst *var-name-start-char-ranges* *pn-chars-u-digit-ranges*)

;; (defconst *var-name-other-char-ranges* (append *pn-chars-u-digit-ranges* '(#x0087 (#x0300 . #x036F) (#x203F . #x2040))))

(defun pn-chars-base-p (ch) (char-in-ranges-p* ch
					       (#\A . #\Z) (#\a . #\z) (#x00C0 . #x00D6) (#x00D8 . #x00F6) (#x00F8 . #x02FF) (#x0370 . #x037D)
					       (#x037F . #x1FFF) (#x200C . #x200D) (#x2070 . #x218F) (#x2C00 . #x2FEF) (#x3001 . #xD7FF)
					       (#xF900 . #xFDCF) (#xFDF0 . #xFFFD) (#x10000 . #xEFFFF)))

(defun pn-chars-u-p (ch) (or (pn-chars-base-p ch) (char=* ch #\_)))

(defun pn-chars-u-digit-p (ch) (or (pn-chars-u-p ch) (digit-char-p* ch)))

(defun pn-chars-p (ch) (or (pn-chars-u-digit-p ch) (char=* ch #\-) (char=* ch (code-char #xb7)) (char-in-ranges-p* ch (#x0300 . #x036F) (#x203F . #x2040))))

(defun pn-chars-dot-p (ch) (or (pn-chars-p ch) (char=* ch #\.)))


;; (defun pn-chars-p (ch) (or (pn-chars-u-digit-p ch) (char=* ch #\-) (char=* ch #x00B7)
;; 			   (char-in-ranges-p* ch )))

(defun var-name-start-char-p (ch) (pn-chars-u-digit-p ch))

(defun var-name-other-char-p (ch) (or (var-name-start-char-p ch) (char=* ch #x0087) (char-in-ranges-p* ch (#x0300 . #x036F) (#x203F . #x2040))))

(defun iri-char-p (ch) (not (or (char-in-set-p* ch "<>\"{}|^`\\") (char-in-ranges-p* ch (#x0 . #x20)))))

(defun show-ranges (predicate from to &key (stream *standard-output*) (separator " | "))
  (let ((sep nil))
    (flet ((show-range (start end)
	     (if sep (format stream "~A" sep))
	     (cond ((< #x20 end 128)
		    (cond ((= start end) (format stream "~C" (code-char end)))
			  (t (format stream "[~C-~C]" (code-char start) (code-char end)))))
		   (t
		    (cond ((= start end) (format stream "#~4,'0x" end))
			  (t (format stream "[#~4,'0x-#~4,'0x]" start end)))))
	     (if (not sep) (setf sep separator))))
      (loop with inside-range-p = nil
	    with start = nil
	    for i from from to to
	    for satisfiedp = (funcall predicate (code-char i))
	    do (cond ((and inside-range-p (not satisfiedp))
		      (show-range start (1- i))
		      (setf inside-range-p nil))
		     ((and (not inside-range-p) satisfiedp)
		      (setf start i)
		      (setf inside-range-p t)))
	    finally (if inside-range-p (show-range start to))))))

(defun decode-string-escape-char (ch)
  (case ch
    (#\t (code-char #x0009))
    (#\b (code-char #x0008))
    (#\n (code-char #x000A))
    (#\r (code-char #x000D))
    (#\f (code-char #x000C))
    (#\" (code-char #x0022))
    (#\' (code-char #x0027))
    (#\\ (code-char #x005C))
    (t (error* "Illegal string escape char ~C" ch))))

