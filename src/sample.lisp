(in-package #:yoshino.sample)

(defmethod sample (sentence (model yoshino) &key (creativity 0.9) (proximity 0.9))
  (let* ((tagged (print (tag-words sentence)))
	 (xps (mapcar #'cadr tagged))
	 (bow (mapcar #'car tagged)))
    (shift-centroid bow model)
    (let* ((xps-samples (print (sample-formula xps model :creativity creativity)))
	   (bow-samples (sample-words xps-samples model :proximity proximity)))
      (string-downcase
       (concatstring
	(get-labels
	 (loop for item in xps-samples collect
	      (weighted-random (gethash item bow-samples))))
	:newline 1)))))

(defmethod shift-centroid (bow (model yoshino))
  (with-slots (lbl cen) model
    (let ((vectors (list cen)))
      (dolist (item bow)
	(let ((found (find-named-word item lbl)))
	  (when found
	    (with-slots (vec) found
	      (push vec vectors)))))
      (setf cen (print (average-vectors vectors)))
      (dolist (item lbl)
        (with-slots (pre) item
	  (setf pre nil))))))

(defmethod sample-words (tags (model yoshino) &key proximity)
  (let ((out (make-hash-table :test 'equal)))
    (dolist (tag (remove-duplicates tags :test 'string=))
      (setf (gethash tag out)
	    (filter-words (count tag tags :test 'string=)
			  tag model
			  :proximity proximity))) 
    out))

(defmethod sample-formula (tags (model yoshino) &key creativity)
  (with-slots (ngh) (closest-formula tags model)
    (with-slots ((_lbl lbl)) (weighted-random ngh)
      (with-slots (lbl) _lbl
        (modify-formula lbl model :creativity creativity)))))

(defmethod modify-formula (formula (model yoshino) &key creativity)
  (with-slots (xph rul) model
    (let (collect)
      (dolist (item formula)
	(let ((roll (random-float 0 1))) 
	  (if (< roll creativity)
	      (with-slots ((_rul rul)) (find-named-word item xph)
		(if _rul
		    (with-slots (lbl) (weighted-random _rul)
		      (push (subseq lbl 0 (ceiling (* creativity (length lbl))))
			    collect))
		    (push (list item) collect)))
	      (push (list item) collect)))) 
      (reduce #'append (reverse collect)))))

(defmethod closest-formula (tags (model yoshino))
  (with-slots (rul) model
    (car (sort rul #'> :key #'(lambda (item) (formula-similarity tags item))))))

(defmethod formula-similarity (tags (rule neighbour))
  (with-slots (lbl) rule
    (let ((union (length (union tags lbl :test 'string=))))
      (if (> union 0)
	  (/ (length (intersection tags lbl :test 'string=))
	     union)
	  0))))

(defmethod word-type ((model word))
  (with-slots (xph) model
    (with-slots (lbl) xph
      lbl)))

(defmethod filter-words (count tag (model yoshino) &key proximity)
  (with-slots (lbl cen) model
    (let ((sequence (sort (remove-if-not #'(lambda (arg) (string= tag (word-type arg))) lbl)
			  #'> :key #'(lambda (arg) (with-slots (pre vec) arg
						 (if pre pre
						     (setf pre (cos-similarity vec cen))))))))
      (subseq sequence 0 (ceiling (* proximity (length sequence)))))))

(defun weighted-random (objects)
  (let ((target (random (reduce #'+ (get-frequencies objects))))
	(current 0))
    (dolist (object objects)
      (with-slots (frq) object 
	(incf current frq)
	(when (> current target)
	  (return-from weighted-random
	    object))))))
