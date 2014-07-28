# Insert the pattern to match with incoming events in EPA 8

PREFIX :<http:example.org/default#>
PREFIX geo:<http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX ssn:<http://purl.oclc.org/NET/ssnx/ssn#>
PREFIX ep:<http://www.ontologydesignpatterns.org/cp/owl/eventprocessing.owl#>

INSERT DATA {
GRAPH <http://example.org/pattern/> {
  [] :index 1 ;
     :value "NW" .

  [] :index 2 ;
     :value "NE" .
} }
