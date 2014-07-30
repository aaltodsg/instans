echo '------------------------------'
echo '1 Event (100 event warm-up)'
../../../../bin/instans -r EPA3.rq --rdf-operations=add:execute-snapshot:remove:execute -g http://instans.org/poststateful --input-blocks=../data/i5sensors100events.ttl --time=- --input-blocks=../data/i5sensors1event.ttl
echo
echo '------------------------------'
echo '100 Events (100 event warm-up)'
../../../../bin/instans -r EPA3.rq --rdf-operations=add:execute-snapshot:remove:execute -g http://instans.org/poststateful --input-blocks=../data/i5sensors100events.ttl --time=- --input-blocks=../data/i5sensors100events.ttl
echo
echo '------------------------------'
echo '1,000 Events (100 event warm-up)'
../../../../bin/instans -r EPA3.rq --rdf-operations=add:execute-snapshot:remove:execute -g http://instans.org/poststateful --input-blocks=../data/i5sensors100events.ttl --time=- --input-blocks=../data/i5sensors1000events.ttl

