;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10; Package: INSTANS -*-
;;;
;;; Author: Esko Nuutila (esko.nuutila@aalto.fi)
;;;
;;; Last updated: Tue Dec  9 14:09:16 EET 2014

(in-package #:instans)

;; (require :sb-sprof)
     
;(declaim (optimize speed))

(defun doit (n e)
  (sb-profile:reset)
  (sb-profile:unprofile)
  (sb-profile:profile main
		      gethash
		      ;; instans-run
		      ;; run-input-processor
		      ;; process-query-input
		      ;; rete-add rete-remove execute-rules
  		      ;; parse
		      ;; ll-parse
		      ;; next-input-token
		      ;; peekch get-char unget-char
  		      ;; eat-number eat-fraction eat-exponent eat-blank-node-label eat-identifier eat-pn-local eat-at eat-iri eat-var-name eat-string-literal
  		      ;; slurp-hex get-char-if-looking-at pn-chars-u-digit-p chbuf-put-char char=*
  		      ;; canonize-string
  		      ;; rete-add add-token add-alpha-token add-beta-token
  		      ;; rete-remove remove-token remove-alpha-token remove-beta-token
		      ;; index-get-tokens
		      ;; index-tokens
		      ;; index-put-token
		      ;; index-remove-token
		      ;; index-count
		      ;; index-clear
		      store-count
		      inner-token
		      store-get-token
		      store-put-token
		      store-remove-token
		      store-tokens
		      store-clear
		      
  		      )
;  (main (format nil "--report=memory100 --time=- --rdf-operations=add:execute:remove:execute:flush --rdf-input-unit=block --construct-output-lisp-block=/Users/enu/aalto-dsg/streamprocessingforsupplychains/data/EventRuns/Lisp/eventRun_~D_exp~D-compare.lisp --rules=/Users/enu/aalto-dsg/streamprocessingforsupplychains/data/EventRuns/passthrough.rq --input-lisp-blocks=/Users/enu/aalto-dsg/streamprocessingforsupplychains/data/EventRuns/Lisp/eventRun_~D_exp~D.lisp" n e n e))
;  (main (format nil "--report=memory100 --time=- --rdf-operations=add:execute:remove:execute:flush --rdf-input-unit=block --construct-output-lisp-block=/Users/enu/aalto-dsg/streamprocessingforsupplychains/data/EventRuns/Lisp/eventRun_~D_exp~D.lisp --rules=/Users/enu/aalto-dsg/streamprocessingforsupplychains/data/EventRuns/passthrough.rq --input=/Users/enu/aalto-dsg/streamprocessingforsupplychains/data/EventRuns/eventRun_~D_exp~D.trig" n e n e))
;  (main (format nil "-d /Users/enu/aalto-dsg/streamprocessingforsupplychains/instans --select-output=~D-~D.csv --prefix-encoding=true --print-prefix-encodings=false -r time_engine.rq -r queries_comm_pack.rq -i parameters_exp~D.ttl --allow-rule-instance-removal=true --rdf-operations=add:execute:remove:flush --time=- --input-blocks=../data/EventRuns/eventRun_~D_exp~D.trig -i endMarker.ttl" n e e n e))
  (main (format nil "-d /Users/enu/aalto-dsg/streamprocessingforsupplychains/instans --select-output=e-~D-~D.csv --prefix-encoding=true --print-prefix-encodings=false -r time_engine.rq -r queries_comm_pack.rq -i parameters_exp~D.ttl --allow-rule-instance-removal=true --rdf-operations=add:execute:remove:flush --time=- --input-blocks=../data/EventRuns/eventRun_~D_exp~D.trig -i endMarker.ttl" n e e n e))
  (sb-profile:report))

;; (sb-sprof:with-profiling (:max-samples 1000
;; 				       :report :flat
;; 				       :loop nil)
;;   (doit))
