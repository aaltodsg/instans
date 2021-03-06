;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class node ()
  ((name :initarg :name :accessor node-name :initform nil)
   (number :initarg :number :accessor node-number :initform nil)
   (instans :initarg :instans :accessor node-instans)
   (graph :initarg :graph :accessor node-graph :initform nil)
   (prev :initarg :prev :accessor node-prev :initform nil)
   (succ :initform nil :accessor node-succ)
   (def :accessor node-def)
   (use :accessor node-use)
   (kill :accessor node-kill :initarg :kill :initform nil)
   (def-prec :accessor node-def-prec)
;   (use-succ :accessor node-use-succ)
   (all-vars-in :accessor node-all-vars-in)
   (all-vars-out :accessor node-all-vars-out)
   (visible-vars-in :accessor node-visible-vars-in)
   (visible-vars-out :accessor node-visible-vars-out)
   (algebra-expr :accessor node-algebra-expr :initarg :algebra-expr :initform nil)
   (tracep :accessor node-trace-p :initform nil)))

(define-class memory (node)
  ((store :accessor memory-store :initform nil)
   (store-count-report-limit :accessor memory-store-count-report-limit :initform 0)
   (store-put-count :accessor memory-store-put-count :initform 0)
   (store-remove-count :accessor memory-store-remove-count :initform 0)))

(define-class subgraph-start-node (node)
  ((end-node :accessor subgraph-end-node :initarg :end-node)))

(define-class subgraph-end-node (node)
  ((start-node :accessor subgraph-start-node :initarg :start-node)))

(define-class existence-start-node (subgraph-start-node beta-memory)
  ((counter-var :accessor existence-counter-var :initarg :counter-var)
   (active-p-var :accessor existence-active-p-var :initarg :active-p-var)))

(define-class existence-end-node (subgraph-end-node) ())

;;;

(define-class alpha-node (node)
  ((variables :accessor alpha-node-variables :initarg :variables)))

(define-class triple-pattern-node (alpha-node)
  ((triple-pattern :accessor triple-pattern-node-triple-pattern :initarg :triple-pattern)
   (dataset :accessor triple-pattern-node-dataset :initarg :dataset :initform nil)))

(define-class datablock-node (alpha-node)
  ((values :accessor datablock-values :initarg :values)))

(define-class alpha-memory (memory) ())

(define-class beta-memory (memory) ())

(define-class join-node (node)
  ((alpha :accessor join-alpha :initarg :alpha)
   (beta :accessor join-beta :initarg :beta)
   (alpha-minus-beta-vars :accessor join-alpha-minus-beta-vars :initarg :alpha-minus-beta-vars) 
   (beta-minus-alpha-vars :accessor join-beta-minus-alpha-vars :initarg :beta-minus-alpha-vars) 
   (has-dummy-beta-p :accessor join-has-dummy-beta-p :initarg :has-dummy-beta-p)
   (alpha-index :accessor join-alpha-index :initform nil)
   (beta-index :accessor join-beta-index :initform nil)))

(define-class filter-node (node)
  ((test :accessor filter-test :initarg :test)
   (test-parameters :accessor filter-test-parameters :initarg :test-parameters)
   (test-lambda :accessor filter-test-lambda :initarg :test-lambda)
   (test-func :accessor filter-test-func)))

(define-class bind-node (node)
  ((variable :accessor bind-variable :initarg :variable)
   (form :accessor bind-form :initarg :form)
   (form-parameters :accessor bind-form-parameters :initarg :form-parameters)
   (form-lambda :accessor bind-form-lambda :initarg :form-lambda)
   (form-func :accessor bind-form-func)))

(define-class union-start-node (subgraph-start-node) ())

(define-class union-end-node (subgraph-end-node) 
  ((prev1 :accessor union-end-prev1 :initarg :prev1)
   (prev2 :accessor union-end-prev2 :initarg :prev2)))

(define-class exists-start-node (existence-start-node)
  ((kind :accessor exists-kind :initarg :kind :initform nil)))

(define-class exists-end-node (existence-end-node)
  ((kind :accessor exists-kind :initarg :kind :initform nil)))

(define-class filter-memory (memory filter-node) 
  ((prev-value-var :accessor filter-memory-prev-value-var :initarg :prev-value-var)))

(define-class optional-start-node (existence-start-node) ())

(define-class optional-end-node (existence-end-node) ())

(define-class minus-node (join-node) ())

(define-class aggregate-join-node (node)
  ((group :accessor aggregate-group :initarg :group)
   (group-var :accessor aggregate-join-group-var)
   (key-exprs :accessor aggregate-join-key-exprs)
   (key-vars :accessor aggregate-join-key-vars)
   (key-lambda :accessor aggregate-join-key-lambda)
   (key-func :accessor aggregate-join-key-func)
   (aggr-exprs :accessor aggregate-join-aggr-exprs :initarg :aggr-exprs)
   (aggr-vars :accessor aggregate-join-aggr-vars)
   (aggr-add-lambda :accessor aggregate-join-aggr-add-lambda)
   (aggr-add-func :accessor aggregate-join-aggr-add-func)
   (aggr-remove-lambda :accessor aggregate-join-aggr-remove-lambda)
   (aggr-remove-func :accessor aggregate-join-aggr-remove-func)
   (groups :accessor aggregate-join-groups)))

(define-class group ()
  ((aggregate-join :accessor group-aggregate-join :initarg :aggregate-join)
   (key :accessor group-key :initarg :key)
   (aggregates :accessor group-aggregates :initform nil)
   (token :accessor group-token :initarg :token)))

(define-class aggregate () ())

(define-class aggregate-with-history ()
  ((history :accessor aggregate-history :initform nil)))

(define-class aggregate-count (aggregate)
  ((count :accessor aggregate-count :initform 0)))

(define-class aggregate-sum (aggregate)
  ((sum :accessor aggregate-sum :initform 0)))

(define-class aggregate-avg (aggregate-count aggregate-sum) ())

(define-class aggregate-min (aggregate-with-history) ())

(define-class aggregate-max (aggregate-with-history) ())

(define-class aggregate-sample (aggregate-with-history) ())

(define-class aggregate-group-concat (aggregate-with-history)
  ((separator :accessor aggregate-group-concat-separator :initarg :separator)
   (distinctp :accessor aggregate-group-concat-distinct-p :initarg :distinctp)))

(define-class service-node (memory)
  ((endpoint :accessor service-node-endpoint :initarg :endpoint)
   (query-token-strings :accessor service-node-query-token-strings :initarg :query-token-strings)
   (query-vars :accessor service-node-query-vars :initarg :query-vars)
   (index :accessor service-node-index :initform nil)
   (index-key-vars :accessor service-node-index-key-vars :initform nil)
   (query-minus-index-key-vars :accessor service-node-query-minus-index-key-vars :initform nil)))

(define-class solution-modifiers-mixin (node)
  ((order-by :accessor solution-modifiers-order-by :initarg :order-by :initform nil)
;   (project :accessor solution-modifiers-project :initarg :project :initform nil)
   (distinctp :accessor solution-modifiers-distinct-p :initarg :distinctp :initform nil)
   (start :accessor solution-modifiers-start :initarg :start :initform nil)
   (length :accessor solution-modifiers-length :initarg :length :initform nil)
   (project-vars :accessor solution-modifiers-project-vars :initarg :project-vars :initform nil)
;   (project-as-exprs :accessor solution-modifiers-project-as-exprs :initarg :project-as-exprs :initform nil)
   (project-index :accessor solution-modifiers-project-index)))

(define-class rule-node (node) 
  ((rule-name :accessor rule-node-rule-name :initarg :rule-name :initform nil)
   (comment :accessor rule-node-comment :initarg :comment :initform nil)
   (annotations :accessor rule-node-annotations :initarg :annotations :initform nil)))

(define-class query-node (rule-node solution-modifiers-mixin) ())

(define-class update-node (rule-node) ())

(define-class select-node (query-node) ())

(define-class construct-node (query-node) 
  ((construct-template :accessor construct-template :initarg :construct-template)
   (construct-parameters :accessor construct-parameters :initarg :construct-parameters)
   (construct-lambda :accessor construct-lambda :initarg :construct-lambda)
   (construct-func :accessor construct-func)))

(define-class ask-node (query-node) ((satisfiedp :accessor ask-node-satisfied-p :initform nil)))

(define-class describe-node (query-node)
  ((var-or-iri-list :accessor describe-node-var-or-iri-list :initarg :var-or-iri-list)))

(define-class modify-node (update-node) 
  ((delete-template :accessor modify-delete-template :initarg :delete-template)
   (delete-parameters :accessor modify-delete-parameters :initarg :delete-parameters)
   (delete-lambda :accessor modify-delete-lambda :initarg :delete-lambda)
   (delete-func :accessor modify-delete-func)
   (insert-template :accessor modify-insert-template :initarg :insert-template)
   (insert-parameters :accessor modify-insert-parameters :initarg :insert-parameters)
   (insert-lambda :accessor modify-insert-lambda :initarg :insert-lambda)
   (insert-func :accessor modify-insert-func)))

(define-class load-node (update-node)
  ())

(define-class clear-node (update-node)
  ())

(define-class drop-node (update-node)
  ())

(define-class create-node (update-node)
  ())

(define-class add-node (update-node)
  ())

(define-class move-node (update-node)
  ())

(define-class copy-node (update-node)
  ())

(define-class quad-store ()
  ())

(define-class list-quad-store (quad-store)
  ((quads :accessor list-quad-store-quads :initform nil)))

(define-class token-index () 
  ((key :accessor token-index-key :initarg :key)))

(define-class hash-token-index (token-index)
  ((id :accessor hash-token-index-id :initarg :id :initform nil)
   (table :accessor hash-token-index-table)))

(define-class triple-pattern-matcher ()
  ((instans :accessor triple-pattern-matcher-instans :initarg :instans)
   (xxx :accessor triple-pattern-matcher-xxx :initform nil)
   (sxx :accessor triple-pattern-matcher-sxx :initform nil)
   (xpx :accessor triple-pattern-matcher-xpx :initform nil)
   (xxo :accessor triple-pattern-matcher-xxo :initform nil)
   (spx :accessor triple-pattern-matcher-spx :initform nil)
   (sxo :accessor triple-pattern-matcher-sxo :initform nil)
   (xpo :accessor triple-pattern-matcher-xpo :initform nil)
   (spo :accessor triple-pattern-matcher-spo :initform nil)))

;;;
;;; Code generation classes
;;;

(define-class code-generator () ())

(define-class code-emitter () 
  ((code-generator :accessor emitter-code-generator)))

(define-class code-decorator () 
  ((code-generator :accessor decorator-code-generator)))

;;;
;;; Runtime classes
;;;

(define-class rule-instance-queue ()
  ((instans :accessor rule-instance-queue-instans :initarg :instans)
   (head :accessor rule-instance-queue-head :initform nil)
   (tail :accessor rule-instance-queue-tail :initform nil)
   (add-count :accessor rule-instance-queue-add-count :initform 0)
   (remove-count :accessor rule-instance-queue-remove-count :initform 0)
   (select-count :accessor rule-instance-queue-select-count :initform 0)
   (ask-count :accessor rule-instance-queue-ask-count :initform 0)
   (describe-count :accessor rule-instance-queue-describe-count :initform 0)
   (modify-count :accessor rule-instance-queue-modify-count :initform 0)
   (construct-count :accessor rule-instance-queue-construct-count :initform 0)))

; Rule rule instances and queue
(define-class rule-instance ()
  ((node :accessor rule-instance-node :initarg :node)
   (token :accessor rule-instance-token :initarg :token)))

;;; System
(define-class instans ()
  ((name :accessor instans-name :initarg :name)
   (nodes :accessor instans-nodes :initarg :nodes :initform nil)
   (node-id-counter :accessor instans-node-id-counter :initform 0)
   (node-color-alist :accessor instans-node-color-alist :initform nil)
   (var-factory :accessor instans-var-factory :initform (make-instance 'uniquely-named-object-factory :object-type 'sparql-var))
   (anonymous-blank-node-factory :accessor instans-anonymous-blank-node-factory :initform (make-instance 'uniquely-named-object-factory :object-type 'rdf-anonymous-blank-node))
   (named-blank-node-factory :accessor instans-named-blank-node-factory :initform (make-instance 'uniquely-named-object-factory :object-type 'rdf-named-blank-node))
   (bindings :accessor instans-bindings :initarg :bindings :initform nil)
   (triple-pattern-matcher :accessor instans-triple-pattern-matcher :initform nil)
   (quad-store :accessor instans-quad-store :initarg :quad-store :initform nil)
   (stores :accessor instans-stores :initform nil)
   (indices :accessor instans-indices :initform nil)
   (memory-sizes-report-interval :accessor instans-memory-sizes-report-interval :initform nil)
   (memory-sizes-report-counter :accessor instans-memory-sizes-report-counter :initform 0)
   (memory-sizes-report-delta-p :accessor instans-memory-sizes-report-delta-p :initform nil)
   (memory-sizes-report-stream :accessor instans-memory-sizes-report-stream :initform t)
   (memory-summaries-report-interval :accessor instans-memory-summaries-report-interval :initform nil)
   (memory-summaries-report-counter :accessor instans-memory-summaries-report-counter :initform 0)
   (store-sizes-alist :accessor instans-store-sizes-alist :initform 0)
   (index-sizes-alist :accessor instans-index-sizes-alist :initform 0)
   (use-quad-store-p :accessor instans-use-quad-store-p :initarg :use-quad-store-p :initform nil)
					;   (initial-data-ops :accessor instans-initial-data-ops :initform nil)
   (rule-instance-queue :accessor instans-rule-instance-queue)
   (input-processors :accessor instans-input-processors :initarg :input-processors :initform nil)
   (rdf-input-unit :accessor instans-rdf-input-unit :initarg :rdf-input-unit :initform :single)
   (allowed-rdf-input-units :accessor instans-allowed-rdf-input-units :allocation :class :initform '(:triple :quad :block :document))
   (rdf-operations :accessor instans-rdf-operations :initarg :rdf-operations :initform '(:add :execute))
   (allowed-rdf-operations :accessor instans-allowed-rdf-operations :allocation :class
			   :initform '(:add :remove :execute :execute-first :execute-snapshot :execute-repeat-first :execute-repeat-snapshot :flush))
   (allow-rule-instance-removal-p :accessor instans-allow-rule-instance-removal-p :initarg :allow-rule-instance-removal-p :initform t)
;   (allowed-rule-instance-removal-policies :accessor instans-allowed-rule-instance-removal-policies :allocation :class :initform '(:remove :keep))
   (queue-execution-policy :accessor instans-queue-execution-policy :initarg :queue-execution-policy :initform :repeat-first)
   (allowed-queue-execution-policies :accessor instans-allowed-queue-execution-policies :allocation :class :initform '(:first :snapshot :repeat-first :repeat-snapshot))
					;   (default-rete-input-op :accessor instans-default-rete-input-op :initarg :default-rete-input-op :initform :add)
   (ask-output-processor :accessor instans-ask-output-processor :initarg :ask-output-processor :initform nil)
   (select-output-processor :accessor instans-select-output-processor :initarg :select-output-processor :initform nil)
   (construct-output-processor :accessor instans-construct-output-processor :initarg :construct-output-processor :initform nil)
   (select-compare-function :accessor instans-select-compare-function :initarg :select-compare-function :initform nil)
   ;; (select-function-arguments :accessor instans-select-function-arguments :initarg :select-function-arguments :initform nil)
   ;; (modify-function :accessor instans-modify-function :initarg :modify-function :initform nil)
   ;; (modify-function-arguments :accessor instans-modify-function-arguments :initarg :modify-function-arguments :initform nil)
   (construct-function :accessor instans-construct-function :initarg :construct-function :initform nil)
   (construct-function-arguments :accessor instans-construct-function-arguments :initarg :construct-function-arguments :initform nil)
   (input-function :accessor instans-input-function :initarg :input-function)
   (input-function-arguments :accessor instans-input-function-arguments :initarg :input-function-arguments :initform nil)
   (constant-iri-var-alist :accessor instans-constant-iri-var-alist :initform nil)
   (constant-literal-var-alist :accessor instans-constant-literal-var-alist :initform nil)
   (add-quad-count :accessor instans-add-quad-count :initarg :add-quad-count)
   (remove-quad-count :accessor instans-remove-quad-count :initarg :remove-quad-count)
   (default-output :accessor instans-default-output :initarg :default-output :initform t)
   (status :accessor instans-status :initform nil)
   (current-op :accessor instans-current-op :initform nil)
   (prefixes :accessor instans-prefixes :initform nil)
   (prefixes-sorted :accessor instans-prefixes-sorted :initform nil)
   (encode-prefixes-p :accessor instans-encode-prefixes-p :initform nil)
   (print-prefix-encodings-p :accessor instans-print-prefix-encodings-p :initform t)
   (debug-topics :accessor instans-debug-topics :initform nil)
   (algebra-expr-list :accessor instans-algebra-expr-list :initform nil)
   (report-operation-kinds :accessor instans-report-operation-kinds :initarg :report-operation-kinds :initform nil)
   (canonic-algebra-expr-list :accessor instans-canonic-algebra-expr-list :initform nil)
   (colors :accessor instans-colors :initform nil)))

(define-class instans-status ()
  ((messages :accessor instans-status-messages :initarg :messages)))

(define-class instans-feature-not-implemented-yet (instans-status) ())
(define-class instans-rule-parsing-failed (instans-status) ())
(define-class instans-rule-parsing-succeeded (instans-status) ())
(define-class instans-rule-translation-failed (instans-status) ())
(define-class instans-rule-translation-succeeded (instans-status) ())
(define-class instans-rule-initialization-failed (instans-status) ())
(define-class instans-rule-initialization-succeeded (instans-status) ())
(define-class instans-rule-running-failed (instans-status) ())
(define-class instans-rule-running-succeeded (instans-status) ())
(define-class instans-rule-comparing-failed (instans-status) ())
(define-class instans-rule-comparing-succeeded (instans-status) ())
(define-class instans-rdf-parsing-failed (instans-status) ())
(define-class instans-rdf-parsing-succeeded (instans-status) ())
(define-class instans-rdf-compare-files-similar (instans-status) ())
(define-class instans-rdf-compare-files-not-similar (instans-status) ())
