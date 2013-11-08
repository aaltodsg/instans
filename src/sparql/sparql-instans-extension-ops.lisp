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

;; (define-sparql-function "instans:create_rete" (:arguments () :returns rdf-iri)
;;   (:method ()
;;     (instans-create-rete)))

;; (define-sparql-function "instans:add_rules" (:arguments ((rete-iri rdf-iri) (rules iri-or-string)) :returns rdf-iri)
;;   (:method ((rete-iri rdf-iri) (rules rdf-iri))
;;     (instans-add-rules rete-iri rules)))

;; (define-sparql-function "instans:add_triples_from_url" (:arguments ((rete-iri rdf-iri) (triples iri-or-string) &optional (graph-iri rdf-iri)) :returns xsd-boolean)
;;   (:method ((rete-iri rdf-iri) (triples iri-or-string) &optional (graph-iri rdf-iri))
;;     (instans-add-triples-from-url rete-iri triples graph-iri)))

(define-sparql-function "instans:execute_system" (:arguments ((rules iri-or-string) (triples iri-or-string) &optional (expected-results iri-or-string) (graph-iri rdf-iri)) :returns xsd-boolean)
  (:method ((rules iri-or-string) (triples iri-or-string) &optional (expected-results iri-or-string) (graph-iri rdf-iri))
    (instans-execute-system rules triples expected-results graph-iri)))

			





