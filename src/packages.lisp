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
  (:export "CALCULATE-VECTOR"
	   "LOAD-FILE" 
	   "TAG-TEXT"
	   "CHUNK-TEXT"
	   "GENERATE-WORDS"
	   "CONNECT-WORDS"
	   "SEGMENT-FORMULAS"
	   "GENERATE-FORMULAS"
	   "PROCESS-TEXT"
	   "FIND-NAMED-WORD"
	   "WINDOW"))

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
