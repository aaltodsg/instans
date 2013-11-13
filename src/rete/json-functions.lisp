;;; -*- Mode: Lisp -*-

(in-package #:instans)

(defun sparql-var-json-name (node var)
  (let ((name (string-downcase (string (uniquely-named-object-name (reverse-resolve-binding (node-instans node) var))))))
    (if (or (char= (char name 0) #\?) (char= (char name 0) #\$)) (subseq name 1) name)))

(defun select-to-json (node tokens &key (stream *standard-output*) &allow-other-keys)
  (declare (ignorable tokens))
  (yason:with-output (stream)
    (yason:with-object ()
	(yason:with-object-element ("head")
	  (yason:with-object-element ("vars")
	    (yason:with-array ()
	      (loop for var in (node-vars-out node)
		    do (yason:encode-array-element (sparql-var-json-name node var)))))))))

  ;; (loop for var in (node-use node)
  ;; 	for print-name = 
  ;; 	for value = (token-value prev token var)
  ;; 	unless (sparql-unbound-p value)
  ;; 	collect (cons print-name value) into bindings
  ;; 	finally (progn
  ;; 		  (setf (gethash "vars" *json-convert-head-table*) print-names)
  ;; 		  (clrhash *json-convert-bindings-table*)
  ;; 		  (loop for item in bindings
  ;; 			for name = (car item)
  ;; 			for item-table = (cdr (assoc name *json-convert-bindings-alist* :test #'equal))
  ;; 			do (progn
  ;; 			     (when (null item-table)
  ;; 			       (setf item-table (make-hash-table :test #'equal))
  ;; 			       (push (cons name item-table) *json-convert-bindings-alist*))
  ;; 			     (setf (gethash name *json-convert-bindings-table*) item-table)
  ;; 			     (loop for (key value) in (cdr item)
  ;; 				   do (setf (gethash key item-table) value))))
