(in-package #:yoshino.sample)

(defmethod sample (sentence (model yoshino))
  (let* ((bow (cl-ppcre:split "[\\s]" sentence))
	 (xps (loop for item in split collect (tag-word item model))))
    (shift-centroid bow model)
    (let ((bow-samples (loop for item in xps collect (sample-words item model)))
	  (xps-samples )))))

(defmethod shift-centroid (bow (model yoshino))
  (with-slots (lbl cen) model
    (let ((vectors (list cen)))
      (dolist (item bow)
	(let ((found (find-named-word item lbl)))
	  (when found
	    (with-slots (vec) found
	      (push vec vectors)))))
      (setf cen (average-vectors vectors))
      (dolist (item lbl)
        (with-slots (pre vec) item
	  (setf pre (cos-similarity vec cen)))))))

(defmethod sample-words (tags (model yoshino))
  (with-slots (lbl cen) model
    (loop for tag in (remove-duplicates tags :test 'string=) 
       collect (filter-words tag cen lbl (count tag tags :test 'string=)))))

(defmethod sample-formula (tags))

(defmethod word-type ((model word))
  (with-slots (xph) model
    (with-slots (lbl) xph
      lbl)))

(defun filter-words (tag centroid words count)
  (let ((sequence (sort (remove-if-not #'(lambda (arg) (string= tag (word-type arg))) words)
			#'> :key #'(lambda (arg) (with-slots (pre) arg pre)))))
    (subseq sequence 0 (when (< count (length sequence)) count))))

