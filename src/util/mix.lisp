;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; The code below (function mix), is from SBCL version sbcl-1.1.13,
;;; file src/code/target-sxhash.lisp
;;; =================================================================
;;; 

(in-package :instans)

;;;; hashing functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; [ We skip a lot of unnexessary code]

(defconstant mpf most-positive-fixnum)
;; (declaim (inline mix))
(defun mix (x y)
  ;; (declare (optimize (speed 3)))
  ;; (declare (type (and fixnum unsigned-byte) x y))
  ;; the ideas here:
  ;;   * Bits diffuse in both directions (shifted arbitrarily left by
  ;;     the multiplication in the calculation of XY, and shifted
  ;;     right by up to 5 places by the ASH).
  ;;   * The #'+ and #'LOGXOR operations don't commute with each other,
  ;;     so different bit patterns are mixed together as they shift
  ;;     past each other.
  ;;   * The arbitrary constant XOR used in the LOGXOR expression is
  ;;     intended to help break up any weird anomalies we might
  ;;     otherwise get when hashing highly regular patterns.
  ;; (These are vaguely like the ideas used in many cryptographic
  ;; algorithms, but we're not pushing them hard enough here for them
  ;; to be cryptographically strong.)
  ;;
  ;; note: 3622009729038463111 is a 62-bit prime such that its low 61
  ;; bits, low 60 bits and low 29 bits are all also primes, thus
  ;; giving decent distributions no matter which of the possible
  ;; values of most-positive-fixnum we have.  It is derived by simple
  ;; search starting from 2^60*pi.  The multiplication should be
  ;; efficient no matter what the platform thanks to modular
  ;; arithmetic.
  ;; (let* ((mul (logand 3622009729038463111 sb!xc:most-positive-fixnum))
  ;;        (xor (logand 608948948376289905 sb!xc:most-positive-fixnum))
  ;;        (xy (logand (+ (* x mul) y) sb!xc:most-positive-fixnum)))
  ;;   (logand (logxor xor xy (ash xy -5)) sb!xc:most-positive-fixnum)))

;;; We change sb!xc:most-positive-fixnum to most-positive-fixnum
  (let* ((mul (logand 3622009729038463111 mpf))
         (xor (logand 608948948376289905 mpf))
         (xy (logand (+ (* x mul) y) mpf)))
    (logand (logxor xor xy (ash xy -5)) mpf)))

