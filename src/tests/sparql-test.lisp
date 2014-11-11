;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; PREFIX dawgt: <http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#>
;;; PREFIX ent: <http://www.w3.org/ns/entailment/>
;;; PREFIX mf: <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#>
;;; PREFIX mfx: <http://jena.hpl.hp.com/2005/05/test-manifest-extra#>
;;; PREFIX pr: <http://www.w3.org/ns/owl-profile/>
;;; PREFIX qt: <http://www.w3.org/2001/sw/DataAccess/tests/test-query#>
;;; PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
;;; PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
;;; PREFIX sd: <http://www.w3.org/ns/sparql-service-description#>
;;; PREFIX sparql: <http://www.w3.org/ns/sparql#>
;;; PREFIX ut: <http://www.w3.org/2009/sparql/tests/test-update#>
;;; <>
;;; dawgt:approval
;;; dawgt:approvedBy
;;; mf:action
;;; mf:action	qt:data
;;; mf:action	qt:graphData
;;; mf:action	qt:query
;;; mf:action	qt:serviceData
;;; mf:action	qt:serviceData		qt:data
;;; mf:action	qt:serviceData		qt:endpoint
;;; mf:action	sd:EntailmentProfile
;;; mf:action	sd:EntailmentProfile	<>
;;; mf:action	sd:entailmentRegime
;;; mf:action	sd:entailmentRegime	<>
;;; mf:action	ut:data
;;; mf:action	ut:graphData
;;; mf:action	ut:graphData		rdfs:label
;;; mf:action	ut:graphData		ut:graph
;;; mf:action	ut:request
;;; mf:description
;;; mf:entries
;;; mf:feature
;;; mf:name
;;; mf:notable
;;; mf:requires
;;; mf:result
;;; mf:result	ut:data
;;; mf:result	ut:graphData
;;; mf:result	ut:graphData	rdfs:label
;;; mf:result	ut:graphData	ut:graph
;;; mf:result	ut:result
;;; mf:resultCardinality
;;; qt:data
;;; qt:endpoint
;;; qt:graphData
;;; qt:query
;;; qt:queryForm
;;; qt:serviceData
;;; qt:serviceData	qt:data
;;; qt:serviceData	qt:endpoint
;;; rdf:type
;;; rdfs:comment
;;; rdfs:label
;;; rdfs:seeAlso
;;; sd:EntailmentProfile
;;; sd:entailmentRegime
;;; sd:EntailmentProfile	<>	<>	<>
;;; sd:entailmentRegime		<>	<>	<>
;;; ut:data
;;; ut:graph
;;; ut:graphData
;;; ut:request
;;; ut:result
;;; ut:graphData	rdfs:label
;;; ut:graphData	ut:graph

;; (define-class sparql-test ()
;;   ((
;;     )))


