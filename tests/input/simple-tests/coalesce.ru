PREFIX ex:<http://example.org/#>

SELECT *
WHERE { 
  ?s ex:value ?x
  BIND(COALESCE(?x, 1/0) AS ?RESULT1)
  BIND(COALESCE(1/0, ?x) AS ?RESULT2)
  BIND(COALESCE(5, ?x) AS ?RESULT3)
#  BIND(COALESCE(?y, 3) AS ?RESULT4)
#  BIND(COALESCE(?y) AS ?RESULT5)
}