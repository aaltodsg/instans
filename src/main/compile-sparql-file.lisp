;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

(defun compile-sparql-file (file &key output-dir sa-output-file dot-output-file network network-name
			    (mkhtmlp (not (null output-dir))) html-output-file (mkhtml-script "mk-html1")
			    (parser #'sparql-parse-file))
  (declare (special *node-color-alist*))
  (setf *node-color-alist* nil)
  (when (null network)
    (setf network (make-instance 'network :name network-name :bindings (make-bindings))))
  (let* ((parsing (funcall parser file))
	 (colors (list "Black" "Red" "Blue" "Green" "Orange"))
	 (*gen-var-counter* 0)
	 (algebra-expr-list nil))
    (cond ((not (parsing-succeeded-p parsing))
	   (inform "~%~A:~A~%" file (parsing-error-message parsing))
	   (return-from compile-sparql-file parsing))
	  (t
	   (inform "Parsed ~S" (first (parsing-result-stack parsing)))
	   (setf algebra-expr-list (filter-not #'(lambda (x) (member (car x) '(PREFIX BASE))) (first (parsing-result-stack parsing))))
	   ))
    (setf (rest (last colors)) colors)
    (loop for algebra-expr in algebra-expr-list
	  for color in colors
	  do (compile-sparql-algebra-expr network algebra-expr color))
    (unless (null output-dir)
      (let ((name-part (pathname-name file))
	    (truedirname (pathname-directory (truename output-dir))))
	(setf dot-output-file (make-pathname :directory truedirname :name name-part :type "dot"))
	(setf sa-output-file (make-pathname :directory truedirname :name name-part :type "sa"))
	(when mkhtmlp
	  (unless html-output-file
	    (setf html-output-file (make-pathname :directory truedirname :name name-part :type "html"))))))
    (if sa-output-file (with-open-file (out sa-output-file :direction :output :if-exists :supersede)
			 (loop for expr in algebra-expr-list
			       do (let* ((*print-circle* nil)
					 (*print-pretty* t)
					 (*print-right-margin* 110))
				    (print expr *error-output*)
				    (let ((string (with-output-to-string (str) (print expr str))))
				      (setf string (cl-ppcre:regex-replace-all ">" (cl-ppcre:regex-replace-all "<"  string "&lt;") "&gt;"))
				      (print string *error-output*)
				      (print string out))))))
    (when dot-output-file (print-dot-file network dot-output-file :html-labels-p nil))
    (when mkhtmlp
      (assert (probe-file mkhtml-script))
      (inform "Running ~S on ~S" mkhtml-script file)
      (shell-script mkhtml-script file))
    network))

(defun compile-sparql-algebra-expr (network algebra-expr &optional (color "Black"))
  (declare (special *node-color-alist*))
  (inform "compiling ~S" algebra-expr)
  (let* ((canonic (canonize-sparql-algebra-variables algebra-expr (network-bindings network)))
	 (new-nodes (translate-sparql-algebra-to-rete canonic network)))
    (compute-node-uses-defs-and-vars new-nodes)
    (loop for node in new-nodes
	  do (push-to-end (cons node color) *node-color-alist*)
	  do (cond ((filter-node-p node)
		    (let ((filter-lambda `(lambda ,(sparql-var-lisp-names (node-use node)) ;(let ((v 
					    (eq ,(sparql-expr-to-lisp (filter-test node)) t))))
		      (setf (filter-test-lambda node) filter-lambda)
		      (setf (filter-test-func node) (compile nil filter-lambda))))
		   ((bind-node-p node)
		    (let ((bind-lambda `(lambda ,(sparql-var-lisp-names (node-use node)) (sparql-expr-to-lisp (bind-form node)))))
		      (setf (bind-form-func node) (compile nil bind-lambda))))
		   ((aggregate-join-node-p node)
		    (setf (aggregate-join-group-form-func node) (compile nil `(lambda ,(sparql-var-lisp-names (node-use node)) ,(aggregate-join-group-form node)))))
		   ((modify-node-p node)
		    (setf (modify-delete-func node)
			  (and (rest (modify-delete-template node))
			       (compile nil `(lambda (,(first (modify-delete-template node))
						      ,@(mapcar #'second (modify-delete-parameters node)))
					       ,@(rest (modify-delete-template node))))))
		    (setf (modify-insert-func node)
			  (and (rest (modify-insert-template node))
			       (compile nil `(lambda (,(first (modify-insert-template node))
						      ,@(mapcar #'second (modify-insert-parameters node)))
					       ,@(rest (modify-insert-template node)))))))))
    (initialize-new-nodes network new-nodes)
    new-nodes))

(defun sparql-expr-to-lisp (expr)
  (cond ((consp expr)
	 (cons (sparql-op-lisp-name (first expr)) (mapcar #'sparql-expr-to-lisp (rest expr))))
	((sparql-var-p expr)
	 (intern (string (uniquely-named-object-name expr))))
	(t expr)))

(defun supply-defaults (overriding-keylist default-keylist)
  (loop for default-key in default-keylist by #'cddr
	for default-value in (cdr default-keylist) by #'cddr
	unless (getf overriding-keylist default-key)
	do (setf (getf overriding-keylist default-key) default-value))
  overriding-keylist)

(defun build-system-from-rules (&rest keys &key network rules-file (output-dir "parser/output") overriding-keys &allow-other-keys)
  (remf keys :rules-file)
  (remf keys :output-dir)
  (remf keys :overriding-keys)
  (when overriding-keys
    (setf keys (supply-defaults overriding-keys keys)))
  (when (null network)
    (setf network (apply #'make-instance 'network keys)))
  (inform "Adding rules from ~A~%" rules-file)
  (add-rules-from-file network rules-file :output-dir output-dir)
  network)

(defun report-execution (network)
  (let ((queue (network-rule-instance-queue network)))
    (inform "Read ~D inputs~&Added ~D quads~&Removed ~D quads~&Executed ~D rules: ~D select rules, ~D modify rules, and ~D construct rules"
	    (network-input-count network)
	    (network-add-quad-count network)
	    (network-remove-quad-count network)
	    (rule-instance-queue-execute-count queue)
	    (rule-instance-queue-select-count queue)
	    (rule-instance-queue-modify-count queue)
	    (rule-instance-queue-construct-count queue))
    network))
