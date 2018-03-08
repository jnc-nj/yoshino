(in-package #:cl-user)

(defpackage #:yoshino.classes
  (:use #:cl #:alexandria)
  (:export "MODEL"
	   "W2V"
	   "HMM"
	   "DIC"
	   "VEC"
	   "CLU"
	   "CEN"
	   "RUL"
	   "Q-A"
	   "WORD"
	   "STR"
	   "XPH"
	   "FRQ"
	   "NGH"
	   "NEIGHBOUR"
	   "WRD"))

(defpackage #:yoshino.preprocess
  (:use #:cl #:alexandria
	#:asclepius.tools
	#:yoshino.classes)
  (:export "LOAD-FILE" 
	   "TAG-TEXT"
	   "CHUNK-TEXT"
	   "PROCESS-TEXT"
	   "FIND-NAMED-WORD"
	   "WINDOW"))

(defpackage #:yoshino.hmm
  (:use #:cl #:alexandria))
