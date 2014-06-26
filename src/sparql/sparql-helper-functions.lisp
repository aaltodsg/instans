;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;

(in-package #:instans)

;;; Errors

;(defvar *sparql-error-op* :inform-and-throw)
(defvar *sparql-error-op* :inform-and-throw)

(defun sparql-error-on-errors ()
  (setf *sparql-error-op* :error))

(defun sparql-inform-and-throw-on-errors ()
  (setf *sparql-error-op* :inform-and-throw))

(defun sparql-throw-on-errors ()
  (setf *sparql-error-op* :throw))

(defun sparql-silent-on-errors ()
  (setf *sparql-error-op* nil))

(defun signal-sparql-error (fmt &rest args)
  (let ((result (make-instance 'sparql-error :format fmt :arguments args)))
    (case *sparql-error-op*
      (:inform-and-throw (inform "; Warning: ~S~%" (apply #'format nil fmt args))
			 (throw :sparql-error result))
      (:throw (throw :sparql-error result))
      (:error (error* "~S" result)))
    result))

;;; This is a trick. Evaluating the var causes it to be bound to a sparql-unbound object
(defun sparql-var-boundp (x)
  (not (sparql-unbound-p x)))

;;; This is not OK
(defmacro ieee-nan-p (x)
  (declare (ignore x))
  nil)

(defun sparql-check-and-divide (a b)
  (if (zerop b) (signal-sparql-error "Divide by zero") (/ a b)))

(defun rdf-iri= (a b)
  (string= (rdf-iri-string a) (rdf-iri-string b)))

(defun rdf-iri-equal (a b)
  (and (rdf-iri-p a) (rdf-iri-p b)
       (string= (rdf-iri-string a) (rdf-iri-string b))))

;; (defun rdf-literal= (a b)
;;   (let ((type-a (getf (cddr a) :type))
;; 	(type-b (getf (cddr b) :type)))
;;     (cond ((null type-a)
;; 	   (cond ((null type-b)
;; 		  (let ((lang-a (getf (cddr a) :lang))
;; 			(lang-b (getf (cddr b) :lang)))
;; 		    (cond ((null lang-a)
;; 			   (cond ((null lang-b) (string= (second a) (second b)))
;; 				 (t (signal-sparql-error "Literals not compatible: ~S and ~S" a b))))
;; 			  (t
;; 			   (cond ((null lang-b)
;; 				  (signal-sparql-error "Literals not compatible: ~S and ~S" a b))
;; 				 (t
;; 				  (and (string= (second a) (second b)) (string= lang-a lang-b))))))))
;; 		 (t (signal-sparql-error "Literals not compatible: ~S and ~S" a b))))
;; 	  ((null type-b)
;; 	   (signal-sparql-error "Literals not compatible: ~S and ~S" a b))
;; 	  (t
;; 	   (cond ((getf (cddr a) :lang) (error* "Malformed RDF-literal ~S has both type and lang tag" a))
;; 		 ((getf (cddr b) :lang) (error* "Malformed RDF-literal ~S has both type and lang tag" b))
;; 		 (t (and (string= (second a) (second b)) (string= (second type-a) (second type-b)))))))))

(defun create-sparql-call (op-name &rest args)
  (let ((sparql-op (find-sparql-op (string-downcase op-name))))
    (cond ((null sparql-op)
	   nil)
	  ;; ((sparql-form-p sparql-op)
	  ;;  (let ((expanded (apply (sparql-op-lisp-name sparql-op) args)))
	  ;;    expanded))
	  (t
	   (cons sparql-op args)))))

;; (defun create-sparql-aggregate-call (group-var index op-name &rest args)
;;   (declare (ignorable group-var index))
;;   (let ((sparql-op (find-sparql-op (format nil "~(aggregate_~A~)" op-name))))
;;     (cond ((null sparql-op)
;; 	   (error* "Unknown aggregate operation ~A" op-name))
;; 	  (t
;; 	   ;(append (list sparql-op group-var index) args)
;; 	   (cons sparql-op args)))))

(defun sparql-var-lisp-name (var)
  (intern-instans (string (uniquely-named-object-name var))))

(defun sparql-var-lisp-names (var-list)
  (mapcar #'sparql-var-lisp-name var-list))

(defun rdf-quads-equal-p (quad-or-triple1 quad-or-triple2)
  (destructuring-bind (s1 p1 o1 &optional g1) quad-or-triple1
    (destructuring-bind (s2 p2 o2 &optional g2) quad-or-triple2
      (and (sparql-value-equal s1 s2)
	   (sparql-value-equal p1 p2)
	   (sparql-value-equal o1 o2)
	   (sparql-value-equal g1 g2)))))

(defun rdf-graphs-isomorphic-p-old (graph1 graph2)
  (and (= (length graph1) (length graph2))
       (every #'(lambda (q1) (find q1 graph2 :test #'rdf-quads-equal-p)) graph1)))

(defun quad-hash-key (quad)
  (destructuring-bind (s p o &optional g) quad
    (let ((key (sxhash nil)))
      (unless (rdf-blank-node-p s)
	(setf key (mix key (get-hashkey s))))
      (unless (rdf-blank-node-p p)
	(setf key (mix key (get-hashkey p))))
      (unless (rdf-blank-node-p o)
	(setf key (mix key (get-hashkey o))))
      (unless (rdf-blank-node-p g)
	(setf key (mix key (get-hashkey g))))
      key)))

(defun quad-contains-blanks-p (quad)
  (some #'rdf-blank-node-p quad))

(defun fill-missing-graph (quads-or-triples &optional graph)
  (loop for item in quads-or-triples
        collect (if (eq (length item) 3) (append item (list graph)) item)))

(defun partition-graph-to-constant-and-non-constant-quads (graph)
  (loop for quad in graph
        nconc (filter #'rdf-blank-node-p quad) into blanks
	when (quad-contains-blanks-p quad) collect quad into nonconstant
	else collect quad into constant
	finally (return (values constant nonconstant blanks))))

(defun rdf-graphs-isomorphic-p (graph1 graph2)
;  (inform "rdf-graphs-isomorphic-p~%graph1 ~{~{~S~^ ~}~^~%       ~}~%~%graph2 ~{~{~S~^ ~}~^~%       ~}~%" graph1 graph2)
  (setf graph1 (fill-missing-graph graph1))
  (setf graph2 (fill-missing-graph graph2))
  (flet ((hash-graph-items (graph)
	   (loop with table = (make-hash-table)
		 for item in graph
		 for items = (gethash (quad-hash-key item) table)
;		 do (inform "adding [~{~S~^ ~}]~%to     {~{[~{~S~^ ~}]~^~%        ~}}" item items)
		 when items do (push-to-end-new item (gethash (quad-hash-key item) table) :test #'rdf-quads-equal-p)
		 else do (setf (gethash (quad-hash-key item) table) (list item))
;		 do (inform "now    {~{[~{~S~^ ~}]~^~%        ~}}~%" (gethash (quad-hash-key item) table))
		 finally (return table))))
    (cond ((not (= (length graph1) (length graph2))) nil)
	  (t
	   (multiple-value-bind (constant-graph1 non-constant-graph1 blanks1) (partition-graph-to-constant-and-non-constant-quads graph1)
	     (multiple-value-bind (constant-graph2 non-constant-graph2 blanks2) (partition-graph-to-constant-and-non-constant-quads graph2)
	       (let ((table1 (hash-graph-items graph1)) (table2 (hash-graph-items graph2)))
		 (cond ((not (and (= (length constant-graph1) (length constant-graph2)) (= (length blanks1) (length blanks2)))) nil)
		       (t
			(loop for quad1 in constant-graph1
			      for matching-quads2 = (gethash (quad-hash-key quad1) table2)
;			      do (inform "quad1 = ~S, matching-quads2 = ~S" quad1 matching-quads2)
			      when (not (and (= 1 (length matching-quads2))  (equal-quads quad1 (first matching-quads2))))
			      do (progn ;(inform "constant matching failed")
				   (return-from rdf-graphs-isomorphic-p nil)))
			(let ((var-mappings12 nil)
			      (var-mappings21 nil))
			  (flet ((unify (graph1 table2 var-mappings12)
;				   (inform "enter unify ~{[~{~S~^ ~}]~^~%            ~}" graph1)
				   (loop with changedp = t
					 while changedp
					 do (progn
					      (setf changedp nil)
					      (loop for item1 in graph1
						    for item1-vars = (filter #'rdf-blank-node-p item1)
						    for items-from-table2 = (gethash (quad-hash-key item1) table2)
						    for items-from-table2-vars = (mapcar #'(lambda (item) (filter #'rdf-blank-node-p item)) items-from-table2)
						    for new-var-mappings = (apply #'mapcar #'list item1-vars items-from-table2-vars)
						    for old-var-mappings = (mapcar #'(lambda (v1) (assoc v1 var-mappings12 :test #'uniquely-named-object-equal)) item1-vars)
;						    do (inform "item1 = [~{~S~^ ~}]~%item1-vars = ~S,~%items-from-table2 = {~{[~{~S~^ ~}]~^~%                    ~}},~%items-from-table2-vars = ~S"
;							       item1 item1-vars items-from-table2 items-from-table2-vars)
;						    do (inform "new-var-mappings = ~S" new-var-mappings)
;						    do (inform "old-var-mappings = ~S" old-var-mappings)
						    do (cond ((null item1-vars)
;							      (inform "Comparing non-blank items")
							      (if (not (and (= (length items-from-table2) 1) (equal-quads item1 (first items-from-table2))))
								  (return-from rdf-graphs-isomorphic-p nil)))
							     (t
							      (loop for old-mapping in old-var-mappings
								    for new-mapping in new-var-mappings
;								    do (inform "  old-mapping ~S~%  new-mapping ~S" old-mapping new-mapping)
								    do (cond ((null old-mapping)
									      (setf changedp t)
									      (push new-mapping var-mappings12)
;									      (inform "  var-mappings12 = ~S" var-mappings12)
									      )
									     (t
									      (let ((merge (intersection (rest old-mapping) (rest new-mapping) :test #'uniquely-named-object-equal)))
;										(inform "  merge ~S" merge)
										(cond ((null merge)
										       (return-from rdf-graphs-isomorphic-p nil))
										      ((< (length merge) (length (rest old-mapping)))
										       (setf changedp t)
										       (setf (rest old-mapping) merge))))))))))))
;				   (inform "exit unify, mappings ~S~%" var-mappings12)
				   (cond ((or (null var-mappings12) (and (every #'(lambda (mapping) (= 2 (length mapping))) var-mappings12) var-mappings12)) t)
					 (t
					  (generate-mappings-and-test var-mappings12
								      #'(lambda (mappings)
;									  (inform "Testing with mappings ~S" mappings)
									  (loop for quad1 in graph1
									        for quad2 = (replace-quad-blanks quad1 mappings)
									        unless (find quad2 (gethash (quad-hash-key quad1) table2) :test #'equal-quads)
									        return nil)
									  t))))))
									        
			    (and (unify non-constant-graph1 table2 var-mappings12)
				 (unify non-constant-graph2 table1 var-mappings21)))))))))))))

(defun replace-quad-blanks (quad mappings)
  (loop for x in quad collect (if (rdf-blank-node-p x) (rest (assoc x mappings :test #'uniquely-named-object-equal)) x)))

(defun generate-mappings-and-test (mappings predicate)
  (labels ((generate (tail result)
	     (cond ((null tail)
		    (when (funcall predicate result)
		      (return-from generate-mappings-and-test t)))
		   (t
		    (let* ((mapping (first tail))
			   (v1 (first mapping)))
		      (loop for v2 in (rest mapping)
			    do (generate (rest tail) (cons (list v1 v2) result))))))))
    (generate mappings nil)
    nil))

;; _:a1 :p :o
;; _:a2 :p :o
;; :s :p2 _:a2
;; :s :p2 _:a1

;; _:b1 :p :o
;; _:b2 :p :o

;; :s :p2 _:b1
;; :s :p2 _:b2

;; map _:b1 :p :o
;; get t2 _:b1 :p :o => _:a1 :p :o, _:a2 :p :o
;; => (_:b1 => _:a1 _:a2)
;; get v12 _:b1 => nil
;; merge, set v12 _:b1 => _:a1, _:a2

;; map _:b2 :p :o
;; get t2 _:b2 :p :o => _:a1 :p :o, _:a2 :p :o
;; => (_:b2 => _:a1 _:a2)
;; get v12 _:b2 => nil
;; merge, set v12 _:b2 => _:a1, _:a2

;; map :s :p2 _:b1
;; get t2 :s :p2 _:b1 => :s :p2 _:a2, :s :p2 _:a1
;; => (_:b1 => _:a2, _:a1)
;; get v12 _:b1 => _:a1, _:a2
;; merge, set v12 _:b1 => _:a2, _:a1

;; map :s :p2 _:b2
;; get t2 :s :p2 _:b1 => :s :p2 _:a2, :s :p2 _:a1
;; => (_:b1 => _:a2, _:a1)
;; get v12 _:b1 => _:a1, _:a2
;; merge, set v12 _:b1 => _:a2, _:a1

;; _:b1 => _:a1, _:a2
;; _:b2 => _:a1, _:a2

;; (defun rdf-graphs-isomorphic-p-old (graph1 graph2)
;;   (inform "rdf-graphs-isomorphic-p~%graph1 ~{~{~S~^ ~}~^~%       ~}~%~%graph2 ~{~{~S~^ ~}~^~%       ~}~%" graph1 graph2)
;;   (flet ((hash-graph-items (graph)
;; 	   (loop with table = (make-hash-table)
;; 		 for item in graph
;; 		 for items = (gethash (quad-hash-key item) table)
;; 		 do (inform "adding [~{~S~^ ~}]~%to     {~{[~{~S~^ ~}]~^~%        ~}}" item items)
;; 		 when items do (push-to-end-new item (gethash (quad-hash-key item) table) :test #'rdf-quads-equal-p)
;; 		 else do (setf (gethash (quad-hash-key item) table) (list item))
;; 		 do (inform "now    {~{[~{~S~^ ~}]~^~%        ~}}~%" (gethash (quad-hash-key item) table))
;; 		 finally (return table))))
;;     (cond ((not (= (length graph1) (length graph2)))
;; 	   nil)
;; 	  (t
;; 	   (let ((table1 (hash-graph-items graph1))
;; 		 (table2 (hash-graph-items graph2))
;; 		 (var-mappings12 nil)
;; 		 (var-mappings21 nil))
;; 	     (flet ((unify (graph1 table2 var-mappings12)
;; 		      (inform "enter unify ~{[~{~S~^ ~}]~^~%            ~}" graph1)
;;  		      (loop with changedp = t
;;  			    while changedp
;;  			    do (progn
;;  				 (setf changedp nil)
;; 				 (loop for item1 in graph1
;; 				       for item1-vars = (filter #'rdf-blank-node-p item1)
;; 				       for items-from-table2 = (gethash (quad-hash-key item1) table2)
;; 				       for items-from-table2-vars = (mapcar #'(lambda (item) (filter #'rdf-blank-node-p item)) items-from-table2)
;; 				       for new-var-mappings = (apply #'mapcar #'list item1-vars items-from-table2-vars)
;; 				       for old-var-mappings = (mapcar #'(lambda (v1) (assoc v1 var-mappings12 :test #'uniquely-named-object-equal)) item1-vars)
;; 				       do (inform "item1 = [~{~S~^ ~}]~%item1-vars = ~S,~%items-from-table2 = {~{[~{~S~^ ~}]~^~%                    ~}},~%items-from-table2-vars = ~S"
;; 						  item1 item1-vars items-from-table2 items-from-table2-vars)
;; 				       do (inform "new-var-mappings = ~S" new-var-mappings)
;; 				       do (inform "old-var-mappings = ~S" old-var-mappings)
;; 				       do (cond ((null item1-vars)
;; 						 (inform "Comparing non-blank items")
;; 						 (if (not (and (= (length items-from-table2) 1) (equal-quads item1 (first items-from-table2))))
;; 						     (return-from rdf-graphs-isomorphic-p-old nil)))
;; 						(t
;; 						 (loop for old-mapping in old-var-mappings
;; 						       for new-mapping in new-var-mappings
;; 						       do (inform "  old-mapping ~S~%  new-mapping ~S" old-mapping new-mapping)
;; 						       do (cond ((null old-mapping)
;; 								 (setf changedp t)
;; 								 (push new-mapping var-mappings12)
;; 								 (inform "  var-mappings12 = ~S" var-mappings12)
;; 								 )
;; 								(t
;; 								 (let ((merge (intersection (rest old-mapping) (rest new-mapping) :test #'uniquely-named-object-equal)))
;; 								   (inform "  merge ~S" merge)
;; 								   (cond ((null merge)
;; 									  (return-from rdf-graphs-isomorphic-p-old nil))
;; 									 ((< (length merge) (length (rest old-mapping)))
;; 									  (setf changedp t)
;; 									  (setf (rest old-mapping) merge))))))))))))
;; 		      (inform "exit unify, mappings ~S~%" var-mappings12)
;; 		      (if (null var-mappings12) t
;; 			  (and (every #'(lambda (mapping) (= 2 (length mapping))) var-mappings12) var-mappings12))))
;; 	       (and (unify graph1 table2 var-mappings12)
;; 		    (unify graph2 table1 var-mappings21))))))))

(defun sparql-value-to-string (x &key always-use-typed-literal-p prefixes)
  (labels ((as-typed-literal (str type-iri)
	     (format nil "\"~A\"^^<~A>" str type-iri))
	   (maybe-as-typed-literal (str type-iri) (if (not always-use-typed-literal-p) str (as-typed-literal str type-iri))))
    (cond ((typep x 'xsd-string-value) (format nil "\"~A\"" x))
	  ((typep x 'xsd-boolean-value) (maybe-as-typed-literal (if x "true" "false") *xsd-boolean-iri-string*))
	  ((typep x 'xsd-integer-value) (maybe-as-typed-literal (format nil "~D" x) *xsd-integer-iri-string*))
	  ((typep x 'xsd-decimal-value) (maybe-as-typed-literal (format nil "~F" x) *xsd-decimal-iri-string*))
;	  ((typep x 'xsd-float-value) (maybe-as-typed-literal (format nil "~E" x) *xsd-float-iri-string*))
	  ((typep x 'xsd-double-value) (maybe-as-typed-literal (format nil "~E" x) *xsd-double-iri-string*))
	  ((typep x 'xsd-datetime-value) (as-typed-literal (datetime-canonic-string x) *xsd-decimal-iri-string*))
	  ((rdf-literal-p x)
	   (cond ((rdf-literal-type x)
		  (as-typed-literal (rdf-literal-string x) (rdf-literal-type x)))
		 ((rdf-literal-lang x)
		  (format nil "\"~A\"@~A" (rdf-literal-string x) (rdf-literal-lang x)))
		 (t
		  (format nil "\"~A\"" (rdf-literal-string x)))))
	  ((rdf-iri-p x)
	   (iri-to-string x prefixes))
	  ((rdf-blank-node-p x) (uniquely-named-object-name x))
	  ((sparql-unbound-p x) "UNBOUND")
	  (t (format nil "~A" x)))))
	   
