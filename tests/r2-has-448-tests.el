(let ((tests
       '((:nested-opt-1 :nested-opt-2 :opt-filter-1 :opt-filter-2 :opt-filter-3 :filter-place-1 :filter-place-2 :filter-place-3 :filter-nested-1 :filter-nested-2 :filter-scope-1 :join-scope-1 :join-combo-1 :join-combo-2)
	 ( 
	       :ask-1
	       :ask-4
	       :ask-7
	       :ask-8
	       )
    
	      ( 
	       :base-prefix-1
	       :base-prefix-2
	       :base-prefix-3
	       :base-prefix-4
	       :base-prefix-5

	       :list-1
	       :list-2
	       :list-3
	       :list-4

	       :quotes-1
	       :quotes-2
	       :quotes-3
	       :quotes-4

	       :term-1
	       :term-2
	       :term-3
	       :term-4
	       :term-5
	       :term-6
	       :term-7
	       :term-8
	       :term-9

	       :var-1
	       :var-2

	       :bgp-no-match
	       :spoo-1

	       :prefix-name-1
	       )
    
	      ( :dawg-bnode-coref-001)
    
	      ( :dawg-boolean-literal :dawg-bev-1 :dawg-bev-2 :dawg-bev-3 :dawg-bev-4 :dawg-bev-5 :dawg-bev-6 )
     
	      (:dawg-bound-query-001)
    
	      (   
	       :cast-str
	       :cast-flt
	       :cast-dbl
	       :cast-dec
	       :cast-int
	       :cast-dT
	       :cast-bool
	       )
    
	      ( 
	       :construct-1
	       :construct-2
	       :construct-3
	       :construct-4
	       :construct-5
	       )
    
	      (
	       :dawg-dataset-01
	       :dawg-dataset-02
	       :dawg-dataset-03
	       :dawg-dataset-04
	       :dawg-dataset-05
	       :dawg-dataset-06
	       :dawg-dataset-07
	       :dawg-dataset-08
	       :dawg-dataset-09
	       :dawg-dataset-10
	       :dawg-dataset-11
	       :dawg-dataset-12
	       :dawg-dataset-09b
	       :dawg-dataset-10b
	       :dawg-dataset-12b
	       )
    
	      ( 
	       :no-distinct-1
	       :distinct-1
	       :no-distinct-2
	       :distinct-2
	       :no-distinct-3
	       :distinct-3
	       :no-distinct-4
	       :distinct-4
	       :no-distinct-9
	       :distinct-9
	       :distinct-star-1
	       )
    
	      ( :dawg-str-1 :dawg-str-2 :dawg-str-3 :dawg-str-4
						    :dawg-isBlank-1 :dawg-isLiteral-1 :dawg-datatype-1 :dawg-datatype-2 :dawg-datatype-3
						    :dawg-lang-1 :dawg-lang-2 :dawg-lang-3 :dawg-isURI-1
						    :dawg-isIRI-1 :dawg-langMatches-1 :dawg-langMatches-2 
						    :dawg-langMatches-3 :dawg-langMatches-4 :dawg-langMatches-basic
						    :lang-case-insensitive-eq
						    :lang-case-insensitive-ne
						    :sameTerm-simple :sameTerm-eq :sameTerm-not-eq
						    )
    
	      ( 
	       :eq-1 :eq-2 :eq-3 :eq-4 :eq-5 :eq-2-1 :eq-2-2
	       :eq-graph-1 :eq-graph-2 :eq-graph-3 :eq-graph-4 :eq-graph-5
	       )
    
	      ( 
	       :ge-1 :le-1 :mul-1 :plus-1 :minus-1 :unplus-1 :unminus-1
	       )
    
	      (
	       :dawg-graph-01
	       :dawg-graph-02
	       :dawg-graph-03
	       :dawg-graph-04
	       :dawg-graph-05
	       :dawg-graph-06
	       :dawg-graph-07
	       :dawg-graph-08
	       :dawg-graph-09
	       :dawg-graph-10
	       :dawg-graph-10b
	       :dawg-graph-11
	       )
    
	      (
	       :kanji-1
	       :kanji-2
	       :normalization-1
	       :normalization-2
	       :normalization-3
	       )
    
	      (
	       :open-eq-01 :open-eq-02 :open-eq-03 :open-eq-04 :open-eq-05 :open-eq-06
	       :open-eq-07 :open-eq-08 :open-eq-09 :open-eq-10 :open-eq-11 :open-eq-12
	       :date-1 :date-2 :date-3 :date-4
	       :open-cmp-01 :open-cmp-02
	       )
    
	      (:dawg-optional-filter-001 :dawg-optional-filter-002 :dawg-optional-filter-003 :dawg-optional-filter-004 :dawg-optional-filter-005-simplified :dawg-optional-filter-005-not-simplified)
    
	      (:dawg-optional-001 
	       :dawg-optional-002 
	       :dawg-union-001 
	       :dawg-optional-complex-1
	       :dawg-optional-complex-2
	       :dawg-optional-complex-3
	       :dawg-optional-complex-4 )
    
	      ( 
	       :reduced-1
	       :reduced-2
	       )
	      (
	       :dawg-regex-001 :dawg-regex-002 :dawg-regex-003 :dawg-regex-004
	       )
    
	      ( 
	       :limit-1
	       :limit-2
	       :limit-3
	       :limit-4
	       :offset-1
	       :offset-2
	       :offset-3
	       :offset-4
	       :slice-1
	       :slice-2
	       :slice-3
	       :slice-4
	       :slice-5
	       )
    
	      ( :dawg-sort-1 :dawg-sort-2 :dawg-sort-3 :dawg-sort-4
						       :dawg-sort-5 :dawg-sort-6 :dawg-sort-7 :dawg-sort-8
						       :dawg-sort-9 :dawg-sort-10 
						       :dawg-sort-numbers
						       :dawg-sort-builtin
						       :dawg-sort-function
						       )
    
	      ( :syntax-basic-01 :syntax-basic-02 :syntax-basic-03
						  :syntax-basic-04 :syntax-basic-05 :syntax-basic-06
						  :syntax-qname-01 :syntax-qname-02 :syntax-qname-03
						  :syntax-qname-04 :syntax-qname-05 :syntax-qname-06
						  :syntax-qname-07 :syntax-qname-08 :syntax-lit-01
						  :syntax-lit-02 :syntax-lit-03 :syntax-lit-04 :syntax-lit-05
						  :syntax-lit-06 :syntax-lit-07 :syntax-lit-08 :syntax-lit-09
						  :syntax-lit-10 :syntax-lit-11 :syntax-lit-12 :syntax-lit-13
						  :syntax-lit-14 :syntax-lit-15 :syntax-lit-16 :syntax-lit-17
						  :syntax-lit-18 :syntax-lit-19 :syntax-lit-20 :syntax-struct-01
						  :syntax-struct-02 :syntax-struct-03 :syntax-struct-05
						  :syntax-struct-06 :syntax-struct-07 :syntax-struct-08
						  :syntax-struct-09 :syntax-struct-10 :syntax-struct-11
						  :syntax-struct-12 :syntax-struct-13 :syntax-struct-14
						  :syntax-lists-01 :syntax-lists-02 :syntax-lists-03
						  :syntax-lists-04 :syntax-lists-05 :syntax-bnodes-01
						  :syntax-bnodes-02 :syntax-bnodes-03 :syntax-bnodes-04
						  :syntax-bnodes-05 :syntax-forms-01 :syntax-forms-02
						  :syntax-union-01 :syntax-union-02 :syntax-expr-01
						  :syntax-expr-02 :syntax-expr-03 :syntax-expr-04
						  :syntax-expr-05 :syntax-order-01 :syntax-order-02
						  :syntax-order-03 :syntax-order-04 :syntax-order-05
						  :syntax-order-06 :syntax-order-07 :syntax-limit-offset-01
						  :syntax-limit-offset-02 :syntax-limit-offset-03
						  :syntax-limit-offset-04 :syntax-pat-01 :syntax-pat-02
						  :syntax-pat-03 :syntax-pat-04 
						  )
    
	      ( :syntax-general-01 :syntax-general-02 :syntax-general-03
						      :syntax-general-04 :syntax-general-05 :syntax-general-06
						      :syntax-general-07 :syntax-general-08 :syntax-general-09
						      :syntax-general-10 :syntax-general-11 :syntax-general-12
						      :syntax-general-13 :syntax-general-14 :syntax-keywords-01
						      :syntax-keywords-02 :syntax-keywords-03 :syntax-lists-01
						      :syntax-lists-02 :syntax-lists-03 :syntax-lists-04
						      :syntax-lists-05 :syntax-bnode-01 :syntax-bnode-02
						      :syntax-bnode-03 :syntax-function-01 :syntax-function-02
						      :syntax-function-03 :syntax-function-04 :syntax-form-select-01
						      :syntax-form-select-02 :syntax-form-ask-02
						      :syntax-form-construct01 :syntax-form-construct02
						      :syntax-form-construct03 :syntax-form-construct04
						      :syntax-form-construct06 :syntax-form-describe01
						      :syntax-form-describe02 :syntax-dataset-01 :syntax-dataset-02
						      :syntax-dataset-03 :syntax-dataset-04 :syntax-graph-01
						      :syntax-graph-02 :syntax-graph-03 :syntax-graph-04
						      :syntax-graph-05 :syntax-esc-01 :syntax-esc-02 :syntax-esc-03
						      :syntax-esc-04 :syntax-esc-05 )
    
	      ( :syn-01 :syn-02 :syn-03 :syn-04 :syn-05 :syn-06
							:syn-07 :syn-08 :syn-bad-01 :syn-bad-02 :syn-bad-03
							:syn-bad-04 :syn-bad-05 :syn-bad-06 :syn-bad-07 :syn-bad-08
							:syn-bad-09 :syn-bad-10 :syn-bad-11 :syn-bad-12 :syn-bad-13
							:syn-bad-14 :syn-bad-15 :syn-bad-16 :syn-bad-17 :syn-bad-18
							:syn-bad-19 :syn-bad-20 :syn-bad-21 :syn-bad-22 :syn-bad-23
							:syn-bad-24 :syn-bad-25 :syn-bad-26 :syn-bad-27 :syn-bad-28
							:syn-bad-29 :syn-bad-30 :syn-bad-31 :bnode-dot
							:bnodes-missing-pvalues-01 :bnodes-missing-pvalues-02
							:empty-optional-01 :empty-optional-02 :filter-missing-parens
							:lone-list :lone-node
							:blabel-cross-filter :blabel-cross-graph-bad
							:blabel-cross-optional-bad :blabel-cross-union-bad )
    
	      ( :syn-09 :syn-10 :syn-11
				:syn-bad-34 :syn-bad-35 :syn-bad-36 :syn-bad-37 :syn-bad-38
				:syn-bad-OPT-breaks-BGP :syn-bad-UNION-breaks-BGP :syn-bad-GRAPH-breaks-BGP
				:syn-leading-digits-in-prefixed-names)
    
	      ( :syntax-reduced-01 :syntax-reduced-02
				   )
    
	      (
	       :dawg-triple-pattern-001 
	       :dawg-triple-pattern-002 
	       :dawg-triple-pattern-003 
	       :dawg-triple-pattern-004 
	       )
    
	      (
	       :type-promotion-01
	       :type-promotion-02
	       :type-promotion-03
	       :type-promotion-04
	       :type-promotion-05
	       :type-promotion-06
	       :type-promotion-07
	       :type-promotion-08
	       :type-promotion-09
	       :type-promotion-10
	       :type-promotion-11
	       :type-promotion-12
	       :type-promotion-13
	       :type-promotion-14
	       :type-promotion-15
	       :type-promotion-16
	       :type-promotion-17
	       :type-promotion-18
	       :type-promotion-19
	       :type-promotion-20
	       :type-promotion-21
	       :type-promotion-22
	       :type-promotion-23
	       :type-promotion-24
	       :type-promotion-25
	       :type-promotion-26
	       :type-promotion-27
	       :type-promotion-28
	       :type-promotion-29
	       :type-promotion-30
	       )
	      )))
  (dolist (test tests)
    (princ (format "%2d\n" (length test))))
  (apply #'+ (mapcar #'length tests)))
14
 4
27
 1
 7
 1
 7
 5
15
11
24
12
 7
12
 5
18
 6
 7
 2
 4
13
13
81
53
51
12
 2
 4
30
448
