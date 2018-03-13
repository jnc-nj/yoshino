(in-package #:yoshino.classes)

(defclass neighbour ()
  ((lbl :initarg :lbl :initform nil)
   (frq :initarg :frq :initform nil)
   (ngh :initarg :ngh :initform nil)))

(defclass word (neighbour)
  ((xph :initarg :xph :initform nil)
   (rul :initarg :rul :initform nil)
   (vec :initarg :vec :initform nil)
   (pre :initarg :pre :initform nil)))

(defclass yoshino (word)
  ((cen :initarg :cen :initform nil)
   (clu :initarg :clu :initform nil)))

;;yoshino -> lbl, xph, rul, cen, clu
;;word -> lbl, frq, ngh, xph, vec, pre
;;xphrase -> lbl, frq, ngh, rul, vec
;;rule -> lbl, frq, ngh
;;neighbour -> lbl, frq, ngh
