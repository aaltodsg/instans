echo '------------------------------'
echo '100 event warm-up'
../../../../bin/instans --construct-output-ttl=/dev/null -r EPA1.rq --rdf-operations=add:execute-snapshot:remove:execute:flush -g http://instans.org/source --time=- --input-blocks=../data/i5sensors100events.ttl
echo '------------------------------'
echo '1 Event'
../../../../bin/instans -r EPA1.rq --rdf-operations=add:execute-snapshot:remove:execute:flush -g http://instans.org/source --time=- --input-blocks=../data/i5sensors1event.ttl
echo
echo '------------------------------'
echo '100 Events'
../../../../bin/instans -r EPA1.rq --rdf-operations=add:execute-snapshot:remove:execute:flush -g http://instans.org/source --time=- --input-blocks=../data/i5sensors100events.ttl
echo
echo '------------------------------'
echo '1,000 Events'
../../../../bin/instans -r EPA1.rq --rdf-operations=add:execute-snapshot:remove:execute:flush -g http://instans.org/source --time=- --input-blocks=../data/i5sensors1000events.ttl
echo
echo '------------------------------'
echo '10,000 Events'
../../../../bin/instans -r EPA1.rq --rdf-operations=add:execute-snapshot:remove:execute:flush -g http://instans.org/source --time=- --input-blocks=../data/i5sensors10000events.ttl

