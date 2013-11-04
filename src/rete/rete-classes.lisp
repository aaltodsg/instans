;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(define-class node ()
  ((name :initarg :name :accessor node-name :initform nil)
   (number :initarg :number :accessor node-number :initform nil)
   (network :initarg :network :accessor node-network)
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
   (form-lambda :accessor filter-form-lambda :initarg :form-lambda)
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
   (delete-func :accessor modify-delete-func)
   (insert-template :accessor modify-insert-template :initarg :insert-template)
   (insert-parameters :accessor modify-insert-parameters :initarg :insert-parameters)
   (insert-func :accessor modify-insert-func)))

;;;
;;; Rete network class
;;;

(define-class network ()
  ((name :accessor network-name :initarg :name :initform nil)
   (nodes :accessor network-nodes :initarg :nodes :initform nil)
   (node-id-counter :accessor network-node-id-counter :initform 0)
   (bindings :accessor network-bindings :initarg :bindings :initform nil)
   (triple-pattern-matcher :accessor network-triple-pattern-matcher :initform nil)
   (quad-store :accessor network-quad-store :initarg :quad-store :initform nil)
   (use-quad-store-p :accessor network-use-quad-store-p :initarg :use-quad-store-p :initform nil)
   (active-p :accessor network-active-p :initform t :initarg :activep)
   (rule-instance-queue :accessor network-rule-instance-queue)
   (remove-rule-instances-p :accessor network-remove-rule-instances-p :initform nil :initarg :remove-rule-instances-p)
   (execution-policy :accessor network-execution-policy :initarg :execution-policy :initform :repeat-first)
   (default-rete-input-op :accessor network-default-rete-input-op :initarg :default-rete-input-op :initform :add)
   (select-function :accessor network-select-function :initarg :select-function :initform nil)
   (select-function-arguments :accessor network-select-function-arguments :initarg :select-function-arguments :initform nil)
   (modify-function :accessor network-modify-function :initarg :modify-function :initform nil)
   (modify-function-arguments :accessor network-modify-function-arguments :initarg :modify-function-arguments :initform nil)
   (construct-function :accessor network-construct-function :initarg :construct-function :initform nil)
   (construct-function-arguments :accessor network-construct-function-arguments :initarg :construct-function-arguments :initform nil)
   (input-function :accessor network-input-function :initarg :input-function)
   (input-function-arguments :accessor network-input-function-arguments :initarg :input-function-arguments :initform nil)
   (input-count :accessor network-input-count :initarg :input-count)
   (add-quad-count :accessor network-add-quad-count :initarg :add-quad-count)
   (remove-quad-count :accessor network-remove-quad-count :initarg :remove-quad-count)))

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
  ((network :accessor triple-pattern-matcher-network :initarg :network)
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
  ((network :accessor rule-instance-queue-network :initarg :network)
   (head :accessor rule-instance-queue-head :initform nil)
   (tail :accessor rule-instance-queue-tail :initform nil)
   (remove-policy :accessor rule-instance-queue-remove-policy :initarg :policy :initform nil)
   (queue-execute-policy :accessor rule-instance-queue-execute-policy :initarg :policy :initform :snapshot)
   (add-count :accessor rule-instance-queue-add-count :initform 0)
   (remove-count :accessor rule-instance-queue-remove-count :initform 0)
   (select-count :accessor rule-instance-queue-select-count :initform 0)
   (modify-count :accessor rule-instance-queue-modify-count :initform 0)
   (construct-count :accessor rule-instance-queue-construct-count :initform 0)))

(define-class rete-system ()
  ((name :accessor rete-system-name :initarg :name)
   (init-func :accessor rete-system-init-func :initarg :init-func)
   (node-init-funcs :accessor rete-system-node-init-funcs :initarg :node-init-funcs)
   (node-add-funcs :accessor rete-system-node-add-funcs :initarg :node-add-funcs)
   (node-remove-funcs :accessor rete-system-node-remove-funcs :initarg :node-remove-funcs)
   (matcher-init-funcs :accessor rete-system-matcher-init-funcs :initarg :matcher-init-funcs)
   (matcher-add-funcs :accessor rete-system-matcher-add-funcs :initarg :matcher-add-funcs)
   (matcher-remove-funcs :accessor rete-system-matcher-remove-funcs :initarg :matcher-remove-funcs)
   (stores :accessor rete-system-stores :initarg :stores)
   (indices :accessor rete-system-indices :initarg :indices)
   (exec-count :accessor rete-system-exec-count :initform 0)
   (queue :accessor rete-system-queue :initarg :queue :initform nil)))

; Rule rule instances and queue
(define-class rule-instance ()
  ((node :accessor rule-instance-node :initarg :node)
   (token :accessor rule-instance-token :initarg :token)))

;;; Bindings. This is struct for easier debugging. Fix later

(defstruct (bindings (:type list) :named)
  (alist nil))



