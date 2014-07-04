echo '------------------------------'
echo '1 Event (100 event warm-up)'
../../../../bin/instans --construct-output=epa1.trig -r EPA1.rq -g http://instans.org/source --input-blocks=../data/5sensors100events.ttl --time=- --input-blocks=../data/5sensors1event.ttl
echo
echo '------------------------------'
echo '100 Events (100 event warm-up)'
../../../../bin/instans --construct-output=epa100.trig -r EPA1.rq -g http://instans.org/source --input-blocks=../data/5sensors100events.ttl --time=- --input-blocks=../data/5sensors100events.ttl
echo
echo '------------------------------'
echo '1,000 Events (100 event warm-up)'
../../../../bin/instans --construct-output=epa1000.trig -r EPA1.rq -g http://instans.org/source --input-blocks=../data/5sensors100events.ttl --time=- --input-blocks=../data/5sensors1000events.ttl
echo
echo '------------------------------'
echo '10,000 Events (100 event warm-up)'
../../../../bin/instans --construct-output=epa10000.trig -r EPA1.rq -g http://instans.org/source --input-blocks=../data/5sensors100events.ttl --time=- --input-blocks=../data/5sensors10000events.ttl

