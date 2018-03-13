(in-package #:cl-user)

(defpackage #:yoshino.classes
  (:use #:cl #:alexandria)
  (:export "NEIGHBOUR"
	   "WORD"
	   "YOSHINO"
	   
	   "LBL" 
	   "FRQ"
	   "NGH"
	   "XPH"
	   "RUL"
	   "VEC" 
	   "PRE"
	   "CEN"
	   "CLU"))

(defpackage #:yoshino.train
  (:use #:cl #:alexandria
	#:asclepius.tools
	#:yoshino.classes)
  (:export "*BURGLED?*"
	   "GENERATE-CLUSTERS"
	   "CALCULATE-VECTOR"
	   "LOAD-FILE"
	   "BURGLE-BATTERIES"
	   "TAG-WORDS"
	   "TAG-TEXT"
	   "CHUNK-TEXT"
	   "GENERATE-WORDS"
	   "CONNECT-WORDS"
	   "GENERATE-VECTORS"
	   "SEGMENT-FORMULAS"
	   "GENERATE-FORMULAS"
	   "PROCESS-TEXT" 
	   "SIMILAR-WORDS"
	   "GET-VECTORS"
	   "FIND-NAMED-WORD"
	   "FIND-NAMED-POSITION"))

(defpackage #:yoshino.sample
  (:use #:cl #:alexandria
	#:asclepius.tools
	#:yoshino.classes
	#:yoshino.train)
  (:export "SAMPLE"
	   "SHIFT-CENTROID"
	   "SAMPLE-WORDS" 
	   "WORD-TYPE"
	   "FILTER-WORDS"))

(defpackage #:yoshino.dev
  (:nicknames #:yoshino/dev)
  (:use #:cl #:alexandria
	#:asclepius.tools
	#:yoshino.classes
	#:yoshino.train
	#:yoshino.sample))
