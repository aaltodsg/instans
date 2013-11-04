;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun whitespace-char-p (ch) (char-in-set-p* ch '(#x20 #x9 #xD #xA)))

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

(defun pn-chars-base-p (ch)
  (char-in-ranges-p* ch
		     (#\A . #\Z) (#\a . #\z) (#x00C0 . #x00D6) (#x00D8 . #x00F6) (#x00F8 . #x02FF) (#x0370 . #x037D)
		     (#x037F . #x1FFF) (#x200C . #x200D) (#x2070 . #x218F) (#x2C00 . #x2FEF) (#x3001 . #xD7FF)
		     (#xF900 . #xFDCF) (#xFDF0 . #xFFFD) (#x10000 . #xEFFFF)))

(defun pn-chars-u-digit-p (ch) (or (pn-chars-base-p ch) (digit-char-p* ch) (char=* ch #\_)))

(defun pn-chars-p (ch) (or (pn-chars-u-digit-p ch) (char=* ch #\-) (char=* ch #x00B7)
			   (char-in-ranges-p* ch (#x0300 . #x036F) (#x203F . #x2040))))

(defun iri-char-p (ch) (not (or (char-in-set-p* ch "<>\"{}|^`\\") (char-in-ranges-p* ch (#x0 . #x20)))))

(defun var-name-start-char-p (ch) (or (pn-chars-u-digit-p ch) (char=* ch #\_)))

(defun var-name-char-p (ch) (or (pn-chars-u-digit-p ch) (char-in-set-p* ch '(#\_ #x00B7)) (char-in-ranges-p* ch (#x0300 . #x036F) (#x203F . #x2040))))
