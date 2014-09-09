CEP2SPARQL Example
==================

This example considers all the types of event processing agents listed in [1] and shows a translation of each type into
SPARQL 1.1, forming an interconnected SPARQL-based event processing network.

To run the example, instans needs to be installed as instructed on the root level of this (INSTANS) repository.

The command line to run the example is in the one-line shell script "cep2sparql.sh":

$ ./cep2sparql.sh

The example does not run a remote service query in EPA 3, but uses a
fixed local binding instead.

The following files are involved:
* _EPA-All.rq:_ SPARQL implementation of eight types of event processing agents (EPA), interconnected.
* _cep2sparql.sh:_ The command line to run a verbose version with a small
number of test events.
* _construct-event-output.rq:_ An auxiliary SPARQL query to CONSTRUCT
output of events passing between different EPAs.
* _CEP2SPARQL_SampleEvents.trig:_ The sample events in TriG format.
* _CEP2SPARQL_SamplePattern.trig:_ A sample pattern for pattern detection
(EPA 8) in TriG format.

The background article is scheduled for publication in ODBASE 2014:
http://onthemove-conferences.org/index.php/otm-program-2013/program-odbase2014

[1] Etzion, Luckham, Niblett: Event Processing in Action. Manning Publications (Jul 2010)
