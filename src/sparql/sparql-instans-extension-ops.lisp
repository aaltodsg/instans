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
    (instans-add-rules instans-iri rules)))

(define-sparql-function "instans:add_triples" (:arguments ((instans-iri rdf-iri) (triples iri-or-string)
							   &optional (expected-results iri-or-string) (graph-iri rdf-iri) (base rdf-iri))
							  :returns xsd-boolean)
  (:method ((instans-iri rdf-iri) (triples iri-or-string) &optional (expected-results iri-or-string) (graph-iri rdf-iri) (base rdf-iri))
    (instans-add-triples-from-url instans-iri triples :expected-results expected-results :graph graph-iri :base base)))

(define-sparql-function "instans:execute_system" (:arguments ((rules iri-or-string) &optional (triples iri-or-string) (expected-results iri-or-string) (graph-iri rdf-iri) (base rdf-iri)) :returns xsd-boolean)
  (:method ((rules iri-or-string) &optional (triples iri-or-string) (expected-results iri-or-string) (graph-iri rdf-iri) (base rdf-iri))
    (instans-execute-system rules :triples triples :expected-results expected-results :graph graph-iri :base base)))

			





