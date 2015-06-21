(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/FIspace/eem#ObjectEvent")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasEventID") (PARSE-XSD-STRING "eve0")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
 (PARSE-XSD-STRING "An ObjectEvent with event ID eve0")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventOccurredAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:32:08.187+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventTimeZoneOffset")
 (CREATE-RDF-LITERAL-WITH-TYPE "0"
                               (PARSE-IRI
                                "http://www.w3.org/2001/XMLSchema#long"))
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/FIspace/eem#Reader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://purl.org/FIspace/eem#logicalID")
 (PARSE-XSD-STRING "reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://purl.org/FIspace/eem#recordedByReader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://purl.org/FIspace/eem#action")
 (PARSE-IRI "http://purl.org/FIspace/eem#ADD")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasBusinessStepType")
 (PARSE-IRI "http://purl.org/FIspace/cbv#commissioning")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasDisposition")
 (PARSE-IRI "http://purl.org/FIspace/cbv#active")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:32:08.203+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#comment")
 (PARSE-XSD-STRING "The object event for commissoning of pharma products")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve0")
 (PARSE-IRI "http://purl.org/FIspace/eem#associatedWithEPCList")
 (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/FIspace/eem#SetOfEPCs")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333323")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333324")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333325")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/FIspace/eem#ObjectEvent")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasEventID") (PARSE-XSD-STRING "eve1")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
 (PARSE-XSD-STRING "An ObjectEvent with event ID eve1")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventOccurredAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:32:11.157+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventTimeZoneOffset")
 (CREATE-RDF-LITERAL-WITH-TYPE "0"
                               (PARSE-IRI
                                "http://www.w3.org/2001/XMLSchema#long"))
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://purl.org/FIspace/eem#recordedByReader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://purl.org/FIspace/eem#action")
 (PARSE-IRI "http://purl.org/FIspace/eem#ADD")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasBusinessStepType")
 (PARSE-IRI "http://purl.org/FIspace/cbv#commissioning")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasDisposition")
 (PARSE-IRI "http://purl.org/FIspace/cbv#active")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:32:11.158+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#comment")
 (PARSE-XSD-STRING "The object event for commissoning of pharma products")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve1")
 (PARSE-IRI "http://purl.org/FIspace/eem#associatedWithEPCList")
 (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/FIspace/eem#SetOfEPCs")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333326")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333327")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/FIspace/eem#AggregationEvent")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasEventID") (PARSE-XSD-STRING "eve2")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
 (PARSE-XSD-STRING "An AggregationEvent with event ID eve2")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventOccurredAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:32:36.279+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventTimeZoneOffset")
 (CREATE-RDF-LITERAL-WITH-TYPE "0"
                               (PARSE-IRI
                                "http://www.w3.org/2001/XMLSchema#long"))
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasAggregationURI")
 (PARSE-IRI
  "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sscc/344.444.555523")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#recordedByReader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#action")
 (PARSE-IRI "http://purl.org/FIspace/eem#ADD")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasBusinessStepType")
 (PARSE-IRI "http://purl.org/FIspace/cbv#packing")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasDisposition")
 (PARSE-IRI "http://purl.org/FIspace/cbv#in_progress")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:32:36.28+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve2")
 (PARSE-IRI "http://purl.org/FIspace/eem#associatedWithEPCList")
 (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/FIspace/eem#SetOfEPCs")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333323")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333324")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333325")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/FIspace/eem#AggregationEvent")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasEventID") (PARSE-XSD-STRING "eve3")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
 (PARSE-XSD-STRING "An AggregationEvent with event ID eve3")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventOccurredAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:32:36.294+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventTimeZoneOffset")
 (CREATE-RDF-LITERAL-WITH-TYPE "0"
                               (PARSE-IRI
                                "http://www.w3.org/2001/XMLSchema#long"))
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasAggregationURI")
 (PARSE-IRI
  "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sscc/344.444.555524")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#recordedByReader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#action")
 (PARSE-IRI "http://purl.org/FIspace/eem#ADD")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasBusinessStepType")
 (PARSE-IRI "http://purl.org/FIspace/cbv#packing")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasDisposition")
 (PARSE-IRI "http://purl.org/FIspace/cbv#in_progress")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:32:36.296+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve3")
 (PARSE-IRI "http://purl.org/FIspace/eem#associatedWithEPCList")
 (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/FIspace/eem#SetOfEPCs")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333326")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/233.3344.333327")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/FIspace/eem#ObjectEvent")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasEventID") (PARSE-XSD-STRING "eve4")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
 (PARSE-XSD-STRING "An ObjectEvent with event ID eve4")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventOccurredAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:32:39.403+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventTimeZoneOffset")
 (CREATE-RDF-LITERAL-WITH-TYPE "0"
                               (PARSE-IRI
                                "http://www.w3.org/2001/XMLSchema#long"))
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://purl.org/FIspace/eem#action")
 (PARSE-IRI "http://purl.org/FIspace/eem#OBSERVE")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasBusinessStepType")
 (PARSE-IRI "http://purl.org/FIspace/cbv#shipping")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasDisposition")
 (PARSE-IRI "http://purl.org/FIspace/cbv#in_transit")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://purl.org/FIspace/eem#associatedWithTransactionList")
 (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/FIspace/eem#SetOfTransactions")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI
  "http://fispace.aston.ac.uk/pharmaCo1/data/transactions/inv/bt/id/0300011111116:A123")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/FIspace/eem#Transaction")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI
  "http://fispace.aston.ac.uk/pharmaCo1/data/transactions/inv/bt/id/0300011111116:A123")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasTransactionType")
 (PARSE-IRI "http://purl.org/FIspace/cbv#btt/inv")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/transactions/inv/bt/id/0300011111116:A123")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI
  "http://fispace.aston.ac.uk/pharmaCo1/data/transactions/po/bt/id/0399999999991:XYZ567")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/FIspace/eem#Transaction")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI
  "http://fispace.aston.ac.uk/pharmaCo1/data/transactions/po/bt/id/0399999999991:XYZ567")
 (PARSE-IRI "http://purl.org/FIspace/eem#hasTransactionType")
 (PARSE-IRI "http://purl.org/FIspace/cbv#btt/po")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/transactions/po/bt/id/0399999999991:XYZ567")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve4")
 (PARSE-IRI "http://purl.org/FIspace/eem#associatedWithEPCList")
 (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/FIspace/eem#SetOfEPCs")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sscc/344.444.555523")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sscc/344.444.555524")
      (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve5")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:37:01+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve6")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:37:02+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve7")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:37:03+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve8")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:00+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve9")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:00+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve10")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:01+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve11")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:01+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve12")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:02+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve13")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:02+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve14")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:03+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve15")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:04+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve16")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:04+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve17")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:05+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve18")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:06+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve19")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:06+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/eve20")
 (PARSE-IRI "http://purl.org/FIspace/eem#eventRecordedAt")
 (PARSE-XSD-DATETIME "2014-04-04T17:38:09+02:00")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/events/x"))
(LIST (PARSE-IRI "http://instans.org/this") (PARSE-IRI "http://instans.org/is")
      (PARSE-IRI "http://instans.org/last"))
