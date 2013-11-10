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
   (def-prec :accessor node-def-prec)
   (use-succ :accessor node-use-succ)
   (vars-add :accessor node-vars-add)
   (vars-del :accessor node-vars-del)
   (vars-in :accessor node-vars-in)
   (vars-out :accessor node-vars-out)
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


(define-class aggregate-join-node (node)
  ((group :accessor aggregate-join-group :initarg :group)
   (group-form :accessor aggregate-join-group-form :initarg :group-form)
   (group-form-func :accessor aggregate-join-group-form-func :initarg :group-form-func)
   (var-aggr-list :accessor aggregate-join-var-aggr-list :initarg :var-aggr-list)
   (group-partition :accessor aggregate-join-group-partition)))

(define-class group-partition ()
  ((parts :accessor group-partition-parts :initform nil)))

(define-class group-partition-part ()
  ((key :accessor group-partition-part-key :initarg :key)
   (input-token-count :accessor group-partition-part-input-token-count :initform 0)
   (aggr-values :accessor group-partition-part-aggr-values :initform nil)
   (aggr-values-histories :accessor group-partition-part-aggr-values-histories :initform nil)
   (token :accessor group-partition-part-token)))

(define-class solution-modifiers-node (node)
  ((order-by :accessor solution-modifiers-order-by :initarg :order-by :initform nil)
   (project-vars :accessor solution-modifiers-project-vars :initarg :project-vars :initform nil)
   (project-index :accessor solution-modifiers-project-index)
   (distinctp :accessor solution-modifiers-distinct-p :initarg :distinctp :initform nil)
   (start :accessor solution-modifiers-start :initarg :start :initform nil)
   (length :accessor solution-modifiers-length :initarg :length :initform nil)))

(define-class rule-node (node) ())

(define-class select-node (rule-node) ())

(define-class construct-node (rule-node) 
  ((construct-template :accessor construct-template :initarg :construct-template)))

(define-class modify-node (rule-node) 
  ((delete-template :accessor modify-delete-template :initarg :delete-template)
   (delete-parameters :accessor modify-delete-parameters :initarg :delete-parameters)
   (delete-lambda :accessor modify-delete-lambda :initarg :delete-lambda)
   (delete-func :accessor modify-delete-func)
   (insert-template :accessor modify-insert-template :initarg :insert-template)
   (insert-parameters :accessor modify-insert-parameters :initarg :insert-parameters)
   (insert-lambda :accessor modify-insert-lambda :initarg :insert-lambda)
   (insert-func :accessor modify-insert-func)))

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
   (remove-policy :accessor rule-instance-queue-remove-policy :initarg :policy :initform nil)
   (queue-execute-policy :accessor rule-instance-queue-execute-policy :initarg :policy :initform :snapshot)
   (add-count :accessor rule-instance-queue-add-count :initform 0)
   (remove-count :accessor rule-instance-queue-remove-count :initform 0)
   (select-count :accessor rule-instance-queue-select-count :initform 0)
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
   (var-factory :accessor instans-var-factory :initform (make-instance 'uniquely-named-object-factory :object-type 'sparql-var))
   (blank-node-factory :accessor instans-blank-node-factory :initform (make-instance 'uniquely-named-object-factory :object-type 'rdf-blank-node))
   (bindings :accessor instans-bindings :initarg :bindings :initform nil)
   (triple-pattern-matcher :accessor instans-triple-pattern-matcher :initform nil)
   (quad-store :accessor instans-quad-store :initarg :quad-store :initform nil)
   (use-quad-store-p :accessor instans-use-quad-store-p :initarg :use-quad-store-p :initform nil)
   (active-p :accessor instans-active-p :initform t :initarg :activep)
   (rule-instance-queue :accessor instans-rule-instance-queue)
   (remove-rule-instances-p :accessor instans-remove-rule-instances-p :initform nil :initarg :remove-rule-instances-p)
   (execution-policy :accessor instans-execution-policy :initarg :execution-policy :initform :repeat-first)
   (default-rete-input-op :accessor instans-default-rete-input-op :initarg :default-rete-input-op :initform :add)
   (select-function :accessor instans-select-function :initarg :select-function :initform nil)
   (select-function-arguments :accessor instans-select-function-arguments :initarg :select-function-arguments :initform nil)
   (modify-function :accessor instans-modify-function :initarg :modify-function :initform nil)
   (modify-function-arguments :accessor instans-modify-function-arguments :initarg :modify-function-arguments :initform nil)
   (construct-function :accessor instans-construct-function :initarg :construct-function :initform nil)
   (construct-function-arguments :accessor instans-construct-function-arguments :initarg :construct-function-arguments :initform nil)
   (input-function :accessor instans-input-function :initarg :input-function)
   (input-function-arguments :accessor instans-input-function-arguments :initarg :input-function-arguments :initform nil)
   (input-count :accessor instans-input-count :initarg :input-count)
   (add-quad-count :accessor instans-add-quad-count :initarg :add-quad-count)
   (remove-quad-count :accessor instans-remove-quad-count :initarg :remove-quad-count)))
