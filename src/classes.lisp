(in-package #:yoshino.classes)

(defclass model ()
  ((w2v :initarg :w2v :initform nil)
   (hmm :initarg :hmm :initform nil)
   (dic :initarg :dic :initform nil)))

(defclass w2v ()
  ((vec :initarg :vec :initform nil)
   (clu :initarg :clu :initform nil)
   (cen :initarg :cen :initform nil)))

(defclass hmm ()
  ((rul :initarg :rul :initform nil)
   (q-a :initarg :q-a :initform nil)))

(defclass word ()
  ((str :initarg :str :initform nil)
   (xph :initarg :xph :initform nil)
   (frq :initarg :frq :initform nil)
   (ngh :initarg :ngh :initform nil)))

(defclass neighbour ()
  ((wrd :initarg :wrd :initform nil)
   (frq :initarg :frq :initform nil)))
