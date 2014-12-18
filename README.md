INSTANS
=======
[Incremental Engine for Standing SPARQL](http://instans.org/)

INSTANS is an incremental SPARQL query engine based on the Rete algorithm, targeted to be a flexible event processing agent. The main characteristics are:
* Continuous incremental processing of a network of SPARQL queries. No
repetition of windows by default.
* Unit of processing selectable as triple, block or document. When a
query is completely matched, the result is immediately available.
* Configurable execution policy to support different types of query
networks.
* SPARQL Update support to INSERT triples locally to memory-resident graphs. Can be used e.g. for storage of intermediate results or to pass information between queries.

INSTANS supports most parts of SPARQL 1.1 Query and SPARQL 1.1 Update
specifications. The current version has been developed and tested on
Mac OS X. Installation instructions are available also for (Debian)
Linux, e.g. Ubuntu. 

Documentation is available in the
[INSTANS github wiki](https://github.com/aaltodsg/instans/wiki).
