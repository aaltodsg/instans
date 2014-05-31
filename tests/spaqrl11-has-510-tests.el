(let ((tests
       '(
    ( 
    :add01
    :add02
    :add03
    :add04
    :add05
    :add06
    :add07
    :add08
    )
    
    ( 
    :agg01
    :agg02
    :agg03
    :agg04
    :agg05
    :agg06
    :agg07
    :agg08
    :agg08b
    :agg09
    :agg10
    :agg11
    :agg12
    :agg-groupconcat-01
    :agg-groupconcat-02
    :agg-groupconcat-03
    :agg-sum-01
    :agg-sum-02
    :agg-avg-01
    :agg-avg-02
    :agg-min-01
    :agg-min-02
    :agg-max-01
    :agg-max-02
    :agg-sample-01 
    :agg-err-01
    :agg-err-02
    :agg-empty-group
)
    
    ( 
      :insert-data-spo1
      :insert-data-spo-named1
      :insert-data-spo-named2
      :insert-data-spo-named3
      :insert-where-01
      :insert-where-02
      :insert-where-03
      :insert-where-04
      :insert-using-01
      :insert-05a
      :insert-data-same-bnode
      :insert-where-same-bnode
      :insert-where-same-bnode2
    )
    
    ( 
    :bind01
    :bind02
    :bind03
    :bind04
    :bind05
    :bind06
    :bind07
    :bind08
    :bind10
    :bind11
    )
    
    ( 
        :values1
        :values2
        :values3
        :values4
        :values5
        :values6
        :values7
        :values8
        :inline1
        :inline2
    )
    
    (
	:dawg-clear-default-01
	:dawg-clear-graph-01
	:dawg-clear-named-01
	:dawg-clear-all-01
   )
    
    ( 
    :constructwhere01
    :constructwhere02
    :constructwhere03
    :constructwhere04
    :constructwhere05
    :constructwhere06
    )
    
    ( 
    :copy01
    :copy02
    :copy03
    :copy04
    :copy06
    :copy07
    )
    
    ( 
    :csv01
    :tsv01
    :csv02
    :tsv02
    :csv03
    :tsv03
    )
    
    (
	:dawg-delete-data-01
	:dawg-delete-data-02
	:dawg-delete-data-03
	:dawg-delete-data-04
	:dawg-delete-data-05
	:dawg-delete-data-06
   )
    
    (
	:dawg-delete-insert-01
	:dawg-delete-insert-01b
	:dawg-delete-insert-01c
	:dawg-delete-insert-02
	:dawg-delete-insert-03
	:dawg-delete-insert-03b
	:dawg-delete-insert-04
	:dawg-delete-insert-04b
	:dawg-delete-insert-05
	:dawg-delete-insert-05b
	:dawg-delete-insert-06
	:dawg-delete-insert-06b
	:dawg-delete-insert-07
	:dawg-delete-insert-07b
	:dawg-delete-insert-08
	:dawg-delete-insert-09
   )
    
    (
	:dawg-delete-where-01
	:dawg-delete-where-02
	:dawg-delete-where-03
	:dawg-delete-where-04
	:dawg-delete-where-05
	:dawg-delete-where-06
   )
    
    (
 	:dawg-delete-01
	:dawg-delete-02
	:dawg-delete-03
	:dawg-delete-04
	:dawg-delete-05
	:dawg-delete-06
	:dawg-delete-07
	:dawg-delete-with-01
	:dawg-delete-with-02
	:dawg-delete-with-03
	:dawg-delete-with-04
	:dawg-delete-with-05
	:dawg-delete-with-06
	:dawg-delete-using-01
	:dawg-delete-using-02a
	:dawg-delete-using-03
	:dawg-delete-using-04
	:dawg-delete-using-05
    :dawg-delete-using-06a
   )
    
    (
	:dawg-drop-default-01
	:dawg-drop-graph-01
	:dawg-drop-named-01
	:dawg-drop-all-01
   )
    
    ( 
      :rdf01
      :rdf02
      :rdf03
      :rdf04

      :rdfs01
      :rdfs02
      :rdfs03
      :rdfs04
      :rdfs05
      :rdfs06
      :rdfs07
      :rdfs08
      :rdfs09
      :rdfs10
      :rdfs11
      :rdfs12
      :rdfs13

      :d-ent-01

      :owlds01
      :owlds02
      :paper-sparqldl-Q1
      :paper-sparqldl-Q1-rdfs
      :paper-sparqldl-Q2
      :paper-sparqldl-Q3
      :paper-sparqldl-Q4
      :paper-sparqldl-Q5
      :plainLit
      
      :bind01
      :bind02
      :bind03
      :bind04
      :bind05
      :bind06
      :bind07
      :bind08

      :sparqldl-01 
      :sparqldl-02 
      :sparqldl-03
      :sparqldl-04
      :sparqldl-05
      :sparqldl-06
      :sparqldl-07
      :sparqldl-08
      :sparqldl-09
      :sparqldl-10
      :sparqldl-11
      :sparqldl-12
      :sparqldl-13

      :lang
      :parent2
      :parent3
      :parent4
      :parent5	
      :parent6
      :parent7
      :parent8
      :parent9
      :parent10
      :simple1
      :simple2
      :simple3
      :simple4
      :simple5	
      :simple6
      :simple7
      :simple8
    )
    
    ( 
    :exists01
    :exists02
    :exists03
    :exists04
    :exists05
  )
    
    ( 
	:strdt01
	:strdt02
	:strdt03
	:strlang01
	:strlang02
	:strlang03
	:isnumeric01
	:abs01
	:ceil01
	:floor01
	:round01
	:concat01
	:concat02
	:substring01
	:substring02
	:length01
	:ucase01
	:lcase01
	:encode01
	:contains01
	:starts01
	:ends01
	:plus-1
	:plus-2
	:md5-01
	:md5-02
	:sha1-01
	:sha1-02
	:sha256-01
	:sha256-02
	:sha512-01
	:sha512-02
	:minutes
	:seconds
	:hours
	:month
	:year
	:day
	:timezone
	:tz
	:bnode01
	:bnode02
	:in01
	:in02
	:notin01
	:notin02
	:now01
	:rand01
	:iri01
	:if01
	:if02
	:coalesce01
	:strbefore01a
	:strbefore02
	:strafter01a
	:strafter02
	:replace01
	:replace02
	:replace03
	:uuid01
	:struuid01
 )
    
    ( 
    :group01
    :group03
    :group04
    :group05
    :group06
    :group07
    )
    
    ( 
    :jsonres01
    :jsonres02
    :jsonres03
    :jsonres04
    )
    
    ( 
    :move01
    :move02
    :move03
    :move04
    :move06
    :move07
    )
    
    ( 
    :subset-by-exclusion-nex-1
    :subset-by-exclusion-minus-1
    :temporal-proximity-by-exclusion-nex-1
    :subset-01
    :subset-02
    :set-equals-1
    :subset-03
    :exists-01
    :exists-02
    :full-minuend
    :partial-minuend
   )
    
    ( 
    :projexp01
    :projexp02
    :projexp03
    :projexp04
    :projexp05
    :projexp06
    :projexp07
 )
    
    ( 
    :pp01
    :pp02
    :pp03
    :pp06
    :pp07
    :pp08
    :pp09
    :pp10
    :pp11
    :pp12
    :pp14
    :pp16

    :pp21
    :pp23
    :pp25
    
    :pp28a

    :pp30
    :pp31
    :pp32
    :pp33

    :pp34
    :pp35
    :pp36
    :pp37
 )
    
     (
;; 		:query_post_form
;; 		:query_dataset_default_graphs_get
;; 		:query_dataset_default_graphs_post
;; 		:query_dataset_named_graphs_post
;; 		:query_dataset_named_graphs_get
;; 		:query_dataset_full
;; 		:query_multiple_dataset
;; 		:query_get
;; 		:query_content_type_select
;; 		:query_content_type_ask
;; 		:query_content_type_describe
;; 		:query_content_type_construct
;; 		:update_dataset_default_graph
;; 		:update_dataset_default_graphs
;; 		:update_dataset_named_graphs
;; 		:update_dataset_full
;; 		:update_post_form
;; 		:update_post_direct
;; 		:update_base_uri
;; 		:query_post_direct
;; 		:bad_query_method
;; 		:bad_multiple_queries
;; 		:bad_query_wrong_media_type
;; 		:bad_query_missing_form_type
;; 		:bad_query_missing_direct_type
;; 		:bad_query_non_utf8
;; 		:bad_query_syntax
;; 		:bad_update_get
;; 		:bad_multiple_updates
;; 		:bad_update_wrong_media_type
;; 		:bad_update_missing_form_type
;; 		:bad_update_non_utf8
;; 		:bad_update_syntax
;; 		:bad_update_dataset_conflict
 	)
    
     (
;; 		:has-endpoint-triple
;; 		:returns-rdf
;; 		:conforms-to-schema
 	)
    
    ( 
:service1
:service2
:service3
:service4a
:service5
:service6
:service7
)
    
    ( 
    :subquery01
    :subquery02
    :subquery03
    :subquery04
    :subquery05
    :subquery06
    :subquery07
    :subquery08
    :subquery09
    :subquery10
    :subquery11
    :subquery12
    :subquery13
    :subquery14
   )
    
    ( 
:test_1
:test_2
:test_3
)
    
    ( 

:test_1
:test_2
:test_3
:test_4
:test_5
:test_6
:test_7
:test_8
:test_9
:test_10
:test_11
:test_12
:test_13
:test_14
:test_15
:test_16
:test_17
:test_18
:test_19
:test_20
:test_21
:test_22
:test_23
:test_24
:test_25
:test_26
:test_27
:test_28
:test_29
:test_30
:test_31
:test_32
:test_33
:test_34
:test_35a
:test_36a
:test_38a
:test_40
:test_41
:test_42
:test_43
:test_44
:test_45
:test_46
:test_47
:test_48
:test_49
:test_50
:test_51

:test_53
:test_54
:test_55
:test_56
:test_57
:test_58
:test_59
:test_60
:test_61a
:test_62a
:test_63
:test_64
:test_65
:test_66

:test_pn_01
:test_pn_02
:test_pn_03
:test_pn_04
:test_pn_05
:test_pn_06
:test_pn_07
:test_pn_08
:test_pn_09

:test_pn_bad_01
:test_pn_bad_02
:test_pn_bad_03
:test_pn_bad_04
:test_pn_bad_05
:test_pn_bad_06
:test_pn_bad_07
:test_pn_bad_08
:test_pn_bad_09
:test_pn_bad_10
:test_pn_bad_11
:test_pn_bad_12
:test_pn_bad_13

:test_pp_coll
)
    
    ( 

:test_1
:test_2
:test_3
:test_4
:test_5
:test_6
:test_7
:test_8
:test_9
:test_10
:test_11
:test_12
:test_13
:test_14
:test_15
:test_16
:test_17
:test_18
:test_19
:test_20
:test_21
:test_22
:test_23
:test_24
:test_25
:test_26
:test_27
:test_28
:test_29
:test_30
:test_31
:test_32
:test_33
:test_34
:test_35
:test_36
:test_37
:test_38
:test_39
:test_40
:test_41
:test_42
:test_43
:test_44
:test_45
:test_46
:test_47
:test_48
:test_49
:test_50
:test_51
:test_52
:test_53
:test_54
)
	
    (
		:syntax-update-other-01
	)
    
    ( 
      :load-silent
      :load-into-silent
      :clear-silent
      :clear-default-silent
      :create-silent
      :drop-silent
      :drop-default-silent
      :copy-silent
      :copy-to-default-silent
      :move-silent
      :move-to-default-silent
      :add-silent
      :add-to-default-silent
    ))))
  (dolist (test tests)
    (princ (format "%2d\n" (length test))))
  (apply #'+ (mapcar #'length tests)))
 8
28
13
10
10
 4
 6
 6
 6
 6
16
 6
19
 4
66
 5
61
 6
 4
 6
11
 7
24
 0
 0
 7
14
 3
86
54
 1
13
510
