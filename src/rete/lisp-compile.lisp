;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun sparql-expr-to-lisp (expr)
  (cond ((consp expr)
	 (let ((sparql-op (first expr))
	       (args-in-lisp (mapcar #'sparql-expr-to-lisp (rest expr))))
	   (cond ((sparql-form-p sparql-op)
		  (apply (sparql-op-lisp-name sparql-op) args-in-lisp))
		 (t
		  (cons (sparql-op-lisp-name sparql-op) args-in-lisp)))))
	((sparql-var-p expr)
	 (intern (string (uniquely-named-object-name expr)) :instans))
	(t expr)))

(defun lisp-compile-nodes (new-nodes)
  ;;; Take this in use at some point
  ;(inform "compiling ~S~%" algebra-expr)
  (loop for node in new-nodes
	;; do (inform "compiling ~A" node)
	do (cond ((filter-node-p node)
		  (let ((filter-lambda `(lambda ,(sparql-var-lisp-names (node-use node)) ;(let ((v 
					  (eq ,(sparql-expr-to-lisp (filter-test node)) t))))
		    (setf (filter-test-lambda node) filter-lambda)
		    (setf (filter-test-func node) (compile nil filter-lambda))))
		 ((bind-node-p node)
		  ;;		      (inform "bind-form ~A = ~A" node (bind-form node))
		  (let ((bind-lambda `(lambda ,(sparql-var-lisp-names (node-use node)) ,(sparql-expr-to-lisp (bind-form node)))))
		    ;;			(inform "bind-lambda = ~S" bind-lambda)
		    (setf (bind-form-lambda node) bind-lambda)
		    (setf (bind-form-func node) (compile nil bind-lambda))))
		 ((aggregate-join-node-p node)
		  (let* ((key-lambda `(lambda ,(sparql-var-lisp-names (aggregate-join-key-vars node))
					(list ,@(loop for key-expr in (aggregate-join-key-exprs node)
						      collect (sparql-expr-to-lisp key-expr)))))
			 (group-arg (gensym "GROUP"))
			 (instans-arg (gensym "INSTANS"))
			 (aggrs-temp (gensym "AGGRS"))
			 (aggr-temp (gensym "AGGR"))
			 (aggr-add-lambda `(lambda (,instans-arg ,group-arg ,@(sparql-var-lisp-names (aggregate-join-aggr-vars node)))
					     (declare (ignorable ,instans-arg))
					     ;; (inform "begin aggr-add~%")
					     ;; ,@(loop for arg in (sparql-var-lisp-names (aggregate-join-aggr-vars node))
					     ;;      collect `(inform "aggr-add ~A = ~A" ',arg ,arg))
					     ;; (inform "")
						 ;;; aggr-expr = (append (list sparql-op op-name group-var index) args)
					     (let ((,aggrs-temp (or (group-aggregates ,group-arg)
								    (setf (group-aggregates ,group-arg)
									  (list ,@(loop for aggr-expr in (aggregate-join-aggr-exprs node)
											;; do (inform "aggr-expr = ~A" aggr-expr)
											collect (cond ((eq (second aggr-expr) 'SAMPLE)
												       `(make-instance 'aggregate-sample))
												      ((eq (second aggr-expr) 'COUNT)
												       `(make-instance 'aggregate-count))
												      ((eq (second aggr-expr) 'SUM)
												       `(make-instance 'aggregate-sum))
												      ((eq (second aggr-expr) 'AVG)
												       `(make-instance 'aggregate-avg))
												      ((eq (second aggr-expr) 'MIN)
												       `(make-instance 'aggregate-min))
												      ((eq (second aggr-expr) 'MAX)
												       `(make-instance 'aggregate-max))
												      ((eq (second aggr-expr) 'GROUP_CONCAT)
												       (let* ((properties (nthcdr 5 aggr-expr))
													      (distinctp (getf properties :distinct))
													      (separator (getf properties :separator)))
;													 (inform "dist=~A,sep = ~A" distinctp separator)
													 `(make-instance 'aggregate-group-concat
															 :distinctp ,distinctp
															 :separator ,separator))))))))))
					       ,@(loop for aggr-expr in (aggregate-join-aggr-exprs node)
						       collect `(let ((,aggr-temp (pop ,aggrs-temp)))
					;								   (inform "add-value ~A to ~A" ',(sparql-expr-to-lisp (fifth aggr-expr)) ,aggr-temp)
								  (aggregate-add-value ,aggr-temp ,(sparql-expr-to-lisp (fifth aggr-expr)))))
					;						   (inform "end aggr-add~%")
					       )))
			 (aggr-remove-lambda `(lambda (,instans-arg ,group-arg ,@(sparql-var-lisp-names (aggregate-join-aggr-vars node)))
						(declare (ignorable ,instans-arg))
						;; ,@(loop for arg in (sparql-var-lisp-names (aggregate-join-aggr-vars node))
						;; 	 collect `(inform "aggr-remove ~A = ~A" ',arg ,arg))
						(let ((,aggrs-temp (group-aggregates ,group-arg)))
						  ,@(loop for aggr-expr in (aggregate-join-aggr-exprs node)
							  collect `(let ((,aggr-temp (pop ,aggrs-temp)))
					;								      (inform "remove-value ~A from ~A" ',(sparql-expr-to-lisp (fifth aggr-expr)) ,aggr-temp)
								     (aggregate-remove-value ,aggr-temp ,(sparql-expr-to-lisp (fifth aggr-expr)))))))))
					;			(inform "(aggregate-join-key-lambda node ~A) = ~%~A" node key-lambda)
					;			(inform "(aggregate-join-aggr-add-lambda node ~A) = ~%~A" node aggr-add-lambda)
					;			(inform "(aggregate-join-aggr-remove-lambda node ~A) = ~%~A" node aggr-remove-lambda)
		    (setf (aggregate-join-key-lambda node) key-lambda)
		    (setf (aggregate-join-key-func node) (compile nil key-lambda))
		    (setf (aggregate-join-aggr-add-lambda node) aggr-add-lambda)
		    (setf (aggregate-join-aggr-add-func node) (compile nil aggr-add-lambda))
		    (setf (aggregate-join-aggr-remove-lambda node) aggr-remove-lambda)
		    (setf (aggregate-join-aggr-remove-func node) (compile nil aggr-remove-lambda))
		    ))
		 ((modify-node-p node)
		  ;; (inform "compiling modify-delete-lambda ~S~%modify-delete-template = ~S" (modify-delete-lambda node) (modify-delete-template node))
		  ;; (inform "compiling modify-insert-lambda ~S~%modify-insert-template = ~S" (modify-insert-lambda node) (modify-insert-template node))
		  (setf (modify-delete-func node) (and (modify-delete-template node) (compile nil (modify-delete-lambda node))))
		  (setf (modify-insert-func node) (and (modify-insert-template node) (compile nil (modify-insert-lambda node))))))))

