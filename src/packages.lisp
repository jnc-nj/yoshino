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
	   "CEN"))

(defpackage #:yoshino.process
  (:use #:cl #:alexandria
	#:asclepius.tools
	#:yoshino.classes)
  (:export "LOAD-FILE" 
	   "TAG-TEXT"
	   "CHUNK-TEXT"
	   "GENERATE-WORDS"
	   "CONNECT-WORDS"
	   "SEGMENT-FORMULAS"
	   "GENERATE-FORMULAS"
	   "PROCESS-TEXT"
	   "FIND-NAMED-WORD"
	   "WINDOW"))
