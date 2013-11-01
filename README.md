Instans
=======

Incremental Engine for Standing SPARQL

INSTANS is an incremental SPARQL query engine based on the Rete algorithm, targeted to be a flexible
event processing agent. The main characteristics are:

* Continuous incremental processing of multiple SPARQL queries. No repetition of windows by
default. Every incoming triple is processed when available. When a query is completely matched, the
result is immediately available.

* SPARQL Update support to INSERT triples to a local triplestore. Can be used e.g. for storage of
intermediate results or to pass information between queries.

INSTANS supports most parts of SPARQL 1.1 Query and SPARQL 1.1 Update specifications. 


