;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-sparql-function "math:expt" (:arguments ((x xsd-number-value) (y xsd-number-value)) :returns xsd-number-value)
  (:method ((x xsd-number-value) (y xsd-number-value)) (expt x y)))

(define-sparql-function "math:sqrt" (:arguments ((x xsd-number-value)) :returns xsd-number-value)
  (:method ((x xsd-number-value)) (sqrt x)))

(define-sparql-function "datetime:datetime_in_seconds" (:arguments ((x xsd-datetime-value)) :returns xsd-number-value)
  (:method ((x xsd-datetime-value)) (datetime-in-seconds x)))

(define-sparql-function "instans:add_rules" (:arguments ((instans-iri rdf-iri) (rules iri-or-string)) :returns rdf-iri)
  (:method ((instans-iri rdf-iri) (rules rdf-iri))
    (let ((instans (instans-add-rules instans-iri rules)))
      (and instans (instans-find-status instans 'instans-rule-translation-succeeded) t))))

(define-sparql-function "instans:status" (:arguments ((instans-iri rdf-iri)) :returns xsd-string)
  (:method ((instans-iri rdf-iri))
    (let ((status (first (instans-status (get-instans instans-iri)))))
      (if status (string-downcase (string (type-of status))) ""))))

(define-sparql-function "instans:error_message" (:arguments ((instans-iri rdf-iri)) :returns xsd-string)
  (:method ((instans-iri rdf-iri))
    (let ((status (first (instans-status (get-instans instans-iri)))))
      (if status (substitute "," "&comma;" (html-entities:encode-entities (format nil "~{~A~^~%~}" (instans-status-messages status)))) ""))))

(define-sparql-function "instans:has_status" (:arguments ((instans-iri rdf-iri) (status xsd-string-value)) :returns xsd-string)
  (:method ((instans-iri rdf-iri) (status xsd-string-value))
    (let* ((key (intern (string-upcase status) 'instans)) ;;; Note: always us package when interning something in run-time
	   (instans (get-instans instans-iri)))
      (and instans (instans-has-status instans key)))))

(define-sparql-function "instans:add_triples" (:arguments ((instans-iri rdf-iri) (triples iri-or-string)
							   &optional (graph-iri rdf-iri) (base rdf-iri))
							  :returns xsd-boolean)
  (:method ((instans-iri rdf-iri) (triples iri-or-string) &optional (graph-iri rdf-iri) (base rdf-iri))
    (let ((instans (instans-add-triples instans-iri triples :graph graph-iri :base base)))
      (and instans (instans-find-status instans 'instans-execution-succeeded)))))

(define-sparql-function "instans:execute_system" (:arguments ((rules iri-or-string) &optional (triples iri-or-string) (expected-results iri-or-string) (graph-iri iri-or-string) (base iri-or-string)) :returns xsd-boolean)
  (:method ((rules iri-or-string) &optional (triples iri-or-string) (expected-results iri-or-string) (graph-iri rdf-iri) (base rdf-iri))
;    (error-safe
    (let ((instans (instans-execute-system rules :triples triples :expected-results expected-results :graph graph-iri :base base)))
      (if instans (parse-iri (instans-name instans)) (signal-sparql-error "Execution failed"))
;)
)))

(define-sparql-function "instans:dynamic_call" (:arguments ((func rdf-iri) &rest args) :returns t)
  (:method ((func rdf-iri) &rest args)
    (let ((sparql-op (find-sparql-op (rdf-iri-string func))))
      (cond ((null sparql-op)
	     (signal-sparql-error "~A does not name a Sparql function or form" (rdf-iri-string func)))
	    (t
	     (apply (sparql-op-lisp-name sparql-op) args))))))

(define-sparql-function "instans:printing" (:arguments ((msg xsd-string-value) (x t)) :returns t)
  (:method ((msg xsd-string-value) (x t))
    (inform "~A~S" msg x)
    x))

(define-sparql-function "instans:compare_rdf_files" (:arguments ((instans-iri rdf-iri) (input1 iri-or-string) (input2 t)) :returns xsd-boolean-value)
  (:method ((instans-iri rdf-iri) (input1 iri-or-string) (input2 t))
    (instans-compare-rdf-files instans-iri input1 input2)))

(define-sparql-function "instans:parse_rdf_file" (:arguments ((instans-iri rdf-iri) (input-iri iri-or-string)) :returns xsd-boolean-value)
  (:method ((instans-iri rdf-iri) (input-iri iri-or-string))
    (let ((instans (or (get-instans instans-iri) (create-instans instans-iri))))
      (instans-parse-rdf-file instans-iri input-iri)
	(instans-has-status instans 'instans-rdf-parsing-succeeded))))

;; outer-arg-spec ((instans-iri rdf-iri) (triples iri-or-string) &optional (graph-iri rdf-iri) base &rest args)
;; outer-lambda (instans-iri triples &optional graph-iri base &rest args)
;; outer-ignorable (instans-iri triples graph-iri base args)
;; inner-arg-spec ((instans-iri1 rdf-iri) (triples1 iri-or-string) &optional (graph-iri1 rdf-iri) base1 &rest args)
;; inner-test-lambda (instans-iri1 triples1 &optional (graph-iri1 nil graph-iri1-present-p))
;; inner-body-lambda (instans-iri1 triples1 &optional graph-iri1 base1 &rest args)


