CEP2SPARQL Example
==================

This example considers all the types of event processing agents listed in [1] and shows a translation of each type into
SPARQL 1.1, forming an interconnected SPARQL-based event processing network.

To run the example, instans needs to be installed as instructed on the root level of this (INSTANS) repository.

The example can be run with the one-line shell script "cep2sparql.sh":

$ ./cep2sparql.sh

The example does not run a remote service query in EPA 3, but uses a fixed local binding instead.
All operations between named graphs are printed out by the final query:

SELECT * WHERE { GRAPH ?g { ?s ?p ?o } }

[1] Etzion, Luckham, Niblett: Event Processing in Action. Manning Publications (Jul 2010)
