(defgeneric execute-rules (instans)
  (:method ((this instans))
    (let* ((policy (instans-execution-policy this))
	   (queue (instans-rule-instance-queue this)))
      (case policy
	(:first
	 (rule-instance-queue-execute-first queue))
	(:snapshot
	 (rule-instance-queue-execute-snapshot queue))
	(:repeat-first
	 (loop while (rule-instance-queue-execute-first queue)))
	(:repeat-snapshot
	 (loop while (rule-instance-queue-execute-snapshot queue)))
	(t (error* "Unknown execution policy ~A" policy))))))

(defgeneric execute-system (instans)
  (:method ((this instans))
    (initialize-execution this)
    (let ((input-function (instans-input-function this))
	  (input-function-arguments (instans-input-function-arguments this)))
      (loop for input = (apply input-function input-function-arguments)
	    while input
	    do (incf (instans-input-count this))
	    do (let ((op (if (member (car input) '(:add :remove :add-exec-remove)) (pop input) :add)))
		 (case op
		   (:add
		    (loop for (s p o g) in input do (rete-add this s p o g))
		    (execute-rules this))
		   (:remove
		    (loop for (s p o g) in input do (rete-remove this s p o g))
		    (execute-rules this))
		   (:execute
		    (loop for (s p o g) in input do (rete-add this s p o g))
		    (execute-rules this)
		    (loop for (s p o g) in input do (rete-remove this s p o g)))
		   (t
		    (error* "Illegal op ~S" op))))))))

