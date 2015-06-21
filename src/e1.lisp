(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#ObjectEvent")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://purl.org/eem#hasEventID") (PARSE-XSD-STRING "eve0")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
      (PARSE-XSD-STRING "An ObjectEvent with event ID eve0")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://purl.org/eem#eventOccurredAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:29.825+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://purl.org/eem#eventTimeZoneOffset")
      (CREATE-RDF-LITERAL-WITH-TYPE "0"
                                    (PARSE-IRI
                                     "http://www.w3.org/2001/XMLSchema#long"))
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/eem#Reader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://purl.org/eem#logicalID") (PARSE-XSD-STRING "reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://purl.org/eem#recordedByReader")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://purl.org/eem#action")
      (PARSE-IRI "http://purl.org/eem#ADD")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://purl.org/eem#hasBusinessStepType")
      (PARSE-IRI "http://purl.org/cbv#commissioning")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://purl.org/eem#hasDisposition")
      (PARSE-IRI "http://purl.org/cbv#active")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://purl.org/eem#eventRecordedAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:29.825+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#comment")
      (PARSE-XSD-STRING
       "The object event for commissioning of pharma products")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve0")
      (PARSE-IRI "http://purl.org/eem#associatedWithEPCList")
      (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#SetOfEPCs")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001001")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001002")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001003")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001004")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001005")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001006")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001007")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001008")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001009")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010010")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010011")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010012")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010013")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010014")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010015")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010016")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010017")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010018")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010019")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:0")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010020")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event0"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#ObjectEvent")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://purl.org/eem#hasEventID") (PARSE-XSD-STRING "eve1")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
      (PARSE-XSD-STRING "An ObjectEvent with event ID eve1")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://purl.org/eem#eventOccurredAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:30.658+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://purl.org/eem#eventTimeZoneOffset")
      (CREATE-RDF-LITERAL-WITH-TYPE "0"
                                    (PARSE-IRI
                                     "http://www.w3.org/2001/XMLSchema#long"))
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/eem#Reader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://purl.org/eem#logicalID") (PARSE-XSD-STRING "reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://purl.org/eem#recordedByReader")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://purl.org/eem#action")
      (PARSE-IRI "http://purl.org/eem#ADD")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://purl.org/eem#hasBusinessStepType")
      (PARSE-IRI "http://purl.org/cbv#commissioning")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://purl.org/eem#hasDisposition")
      (PARSE-IRI "http://purl.org/cbv#active")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://purl.org/eem#eventRecordedAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:30.658+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#comment")
      (PARSE-XSD-STRING
       "The object event for commissioning of pharma products")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve1")
      (PARSE-IRI "http://purl.org/eem#associatedWithEPCList")
      (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#SetOfEPCs")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010021")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010022")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010023")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010024")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010025")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010026")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010027")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010028")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010029")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010030")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010031")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010032")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010033")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010034")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010035")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010036")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010037")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010038")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010039")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:1")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010040")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event1"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#ObjectEvent")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://purl.org/eem#hasEventID") (PARSE-XSD-STRING "eve2")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
      (PARSE-XSD-STRING "An ObjectEvent with event ID eve2")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://purl.org/eem#eventOccurredAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:31.177+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://purl.org/eem#eventTimeZoneOffset")
      (CREATE-RDF-LITERAL-WITH-TYPE "0"
                                    (PARSE-IRI
                                     "http://www.w3.org/2001/XMLSchema#long"))
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/eem#Reader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://purl.org/eem#logicalID") (PARSE-XSD-STRING "reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://purl.org/eem#recordedByReader")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://purl.org/eem#action")
      (PARSE-IRI "http://purl.org/eem#ADD")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://purl.org/eem#hasBusinessStepType")
      (PARSE-IRI "http://purl.org/cbv#commissioning")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://purl.org/eem#hasDisposition")
      (PARSE-IRI "http://purl.org/cbv#active")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://purl.org/eem#eventRecordedAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:31.177+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#comment")
      (PARSE-XSD-STRING
       "The object event for commissioning of pharma products")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve2")
      (PARSE-IRI "http://purl.org/eem#associatedWithEPCList")
      (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#SetOfEPCs")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010041")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010042")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010043")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010044")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010045")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010046")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010047")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010048")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010049")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010050")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010051")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010052")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010053")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010054")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010055")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010056")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010057")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010058")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010059")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:2")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010060")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event2"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#ObjectEvent")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://purl.org/eem#hasEventID") (PARSE-XSD-STRING "eve3")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
      (PARSE-XSD-STRING "An ObjectEvent with event ID eve3")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://purl.org/eem#eventOccurredAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:31.702+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://purl.org/eem#eventTimeZoneOffset")
      (CREATE-RDF-LITERAL-WITH-TYPE "0"
                                    (PARSE-IRI
                                     "http://www.w3.org/2001/XMLSchema#long"))
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/eem#Reader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://purl.org/eem#logicalID") (PARSE-XSD-STRING "reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://purl.org/eem#recordedByReader")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://purl.org/eem#action")
      (PARSE-IRI "http://purl.org/eem#ADD")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://purl.org/eem#hasBusinessStepType")
      (PARSE-IRI "http://purl.org/cbv#commissioning")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://purl.org/eem#hasDisposition")
      (PARSE-IRI "http://purl.org/cbv#active")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://purl.org/eem#eventRecordedAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:31.702+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#comment")
      (PARSE-XSD-STRING
       "The object event for commissioning of pharma products")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve3")
      (PARSE-IRI "http://purl.org/eem#associatedWithEPCList")
      (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#SetOfEPCs")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010061")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010062")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010063")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010064")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010065")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010066")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010067")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010068")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010069")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010070")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010071")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010072")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010073")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010074")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010075")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010076")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010077")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010078")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010079")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:3")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010080")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event3"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#ObjectEvent")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://purl.org/eem#hasEventID") (PARSE-XSD-STRING "eve4")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
      (PARSE-XSD-STRING "An ObjectEvent with event ID eve4")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://purl.org/eem#eventOccurredAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:32.224+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://purl.org/eem#eventTimeZoneOffset")
      (CREATE-RDF-LITERAL-WITH-TYPE "0"
                                    (PARSE-IRI
                                     "http://www.w3.org/2001/XMLSchema#long"))
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/eem#Reader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
 (PARSE-IRI "http://purl.org/eem#logicalID") (PARSE-XSD-STRING "reader102")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://purl.org/eem#recordedByReader")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader102")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://purl.org/eem#action")
      (PARSE-IRI "http://purl.org/eem#ADD")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://purl.org/eem#hasBusinessStepType")
      (PARSE-IRI "http://purl.org/cbv#commissioning")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://purl.org/eem#hasDisposition")
      (PARSE-IRI "http://purl.org/cbv#active")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://purl.org/eem#eventRecordedAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:32.224+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#comment")
      (PARSE-XSD-STRING
       "The object event for commissioning of pharma products")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve4")
      (PARSE-IRI "http://purl.org/eem#associatedWithEPCList")
      (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#SetOfEPCs")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010081")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010082")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010083")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010084")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010085")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010086")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010087")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010088")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010089")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010090")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010091")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010092")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010093")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010094")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010095")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010096")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010097")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010098")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010099")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:4")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.1000000100100")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event4"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#AggregationEvent")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#hasEventID") (PARSE-XSD-STRING "eve5")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://www.w3.org/2000/01/rdf-schema#label")
      (PARSE-XSD-STRING "An AggregationEvent with event ID eve5")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#eventOccurredAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:33.741+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#eventTimeZoneOffset")
      (CREATE-RDF-LITERAL-WITH-TYPE "0"
                                    (PARSE-IRI
                                     "http://www.w3.org/2001/XMLSchema#long"))
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#hasAggregationURI")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sscc/030001.1012345.2222222333")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader103")
 (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
 (PARSE-IRI "http://purl.org/eem#Reader")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader103")
 (PARSE-IRI "http://purl.org/eem#logicalID") (PARSE-XSD-STRING "reader103")
 (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#recordedByReader")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/readers/id/reader103")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#action")
      (PARSE-IRI "http://purl.org/eem#ADD")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#hasBusinessStepType")
      (PARSE-IRI "http://purl.org/cbv#packing")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#hasDisposition")
      (PARSE-IRI "http://purl.org/cbv#in_progress")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#eventRecordedAt")
      (PARSE-XSD-DATETIME "2015-04-25T23:15:33.741+01:00")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (PARSE-IRI "http://fispace.aston.ac.uk/pharmaCo1/data/events/id/eve5")
      (PARSE-IRI "http://purl.org/eem#associatedWithEPCList")
      (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      (PARSE-IRI "http://purl.org/eem#SetOfEPCs")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001001")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001002")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001003")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001004")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001005")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001006")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001007")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001008")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.10000001009")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010010")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010011")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010012")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010013")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010014")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010015")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010016")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010017")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010018")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010019")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010020")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010021")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010022")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010023")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010024")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010025")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010026")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010027")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010028")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010029")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010030")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010031")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010032")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010033")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010034")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010035")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010036")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010037")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010038")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010039")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010040")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010041")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010042")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010043")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010044")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010045")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010046")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010047")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010048")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010049")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010050")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010051")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010052")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010053")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010054")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010055")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010056")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010057")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010058")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010059")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010060")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010061")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010062")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010063")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010064")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010065")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010066")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010067")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010068")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010069")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010070")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010071")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010072")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010073")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010074")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010075")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010076")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010077")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010078")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010079")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010080")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010081")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010082")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010083")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010084")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010085")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010086")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010087")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010088")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010089")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010090")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010091")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0012345.100000010092")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0054321.100000010081")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0054321.100000010082")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0054321.100000010083")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0054321.100000010084")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0054321.100000010085")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0054321.100000010086")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0054321.100000010087")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
(LIST (MAKE-NAMED-BLANK-NODE (CURRENT-INSTANS) "_:5")
      (PARSE-IRI "http://purl.org/co#element")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/epc/id/sgtin/030001.0054321.100000010088")
      (PARSE-IRI
       "http://fispace.aston.ac.uk/pharmaCo1/data/eventGraphID/event5"))
