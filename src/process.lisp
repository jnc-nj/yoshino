(in-package #:yoshino.process)

(defmethod sample (bow (model yoshino))
  (shift-centroid bow model) 
  )

(defmethod shift-centroid (bow (model yoshino))
  (with-slots (lbl cen) model
    (let ((divisor (+ 1 (length bow)))
	  (vectors (list cen)))
      (loop for item in bow
	 for found = (find-named-word item lbl)
	 do (when found (with-slots (vec) found
			  (push vec vectors))))
      (setf cen (map 'vector #'(lambda (n) (/ n divisor))
		     (apply #'map 'vector #'+ vectors))))))

(defmethod sample-words (tags (model yoshino))
  (with-slots (lbl cen) model
    (loop for tag in (remove-duplicates tags :test 'string=) 
       for sorted = (filter-words tag cen words (count tag tags :test 'string=)))))

(defmethod calculate-vector (n lbl (model word))
  (let ((vector (make-array n :initial-element 0)))
    (with-slots (ngh) model
      (dolist (neighbour ngh)
	(with-slots ((_lbl lbl) frq) neighbour
	  (with-slots ((local-lbl lbl)) _lbl
	    (incf (aref vector (find-named-position local-lbl lbl))
		  frq)))))
    vector))

(defmethod word-type ((model word))
  (with-slots (xph) model
    (with-slots (lbl) xph
      lbl)))

(defun filter-words (tag centroid words count)
  (let ((sequence (sort (remove-if-not #'(lambda (arg) (string= tag (word-type arg))) words)
			#'> #'(lambda (arg) (with-slots (vec) arg (cos-similarity vec centroid))))))
    (subseq sequence 0 (when (< count (length sequence)) count))))

(defun load-file (path)
  "Load text file from path and return string."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun tag-text (raw-path &key (language "English") (window 5))
  (let* ((path "~/.roswell/local-projects/yoshino/data")
	 (text-path (format nil "~d/~d" path raw-path)))
    (with-info "Tagging text"
      (inferior-shell:run/nil
       (format nil "cd ~d && python RDRPOSTAGGER.py tag ~d ~d ~d" 
	       (pathname (format nil "~d/tagger/pSCRDRtagger/" path))
	       (pathname (format nil "../Models/POS/~d.RDR" language))
	       (pathname (format nil "../Models/POS/~d.DICT" language))
	       (pathname text-path))))
    (process-text (load-file (format nil "~d.TAGGED" text-path))
		  :window window)))

(defun chunk-text (text)
  (let (chunks)
    (with-info "Chunking text"
      (dolist (item (cl-ppcre:split "[\\s]" text))
	(push (cl-ppcre:split "[/]" item) chunks)))
    chunks))

(defun generate-words (chunks words &key (test 'string=) xps window descriptor)
  (let (word-objects)
    (with-info (format nil "Generating ~d" descriptor)
      (dolist (word (remove-duplicates words :test test))
	(let ((xph (car (agethash word chunks))))
	  (push (make-instance 'word
			       :lbl word
			       :xph (when xph (find-named-word xph xps :test test))
			       :frq (count word words :test test))
		word-objects))))
    (connect-words word-objects words :test test :window window :descriptor descriptor)
    (generate-vectors word-objects :descriptor descriptor)
    word-objects))

(defun connect-words (word-objects words &key (test 'string=) window descriptor)
  (with-info (format nil "Connecting ~d" descriptor)
    (dolist (word word-objects)
      (with-slots (lbl ngh) word
	(let ((found (window window lbl words :test test)))
	  (dolist (item (remove-duplicates found :test test))
	    (push (make-instance 'neighbour
				 :lbl (find-named-word item word-objects :test test)
				 :frq (count item found :test test))
		  ngh)))))))

(defun generate-vectors (word-objects &key descriptor)
  (let ((len (length word-objects)))
    (with-info (format nil "Calculating ~d" descriptor)
      (dolist (word-object word-objects)
	(with-slots (vec) word-object
	  (setf vec (calculate-vector len word-objects word-object)))))))

(defun segment-formulas (xps)
  (let (temp-xp collect)
    (with-info "Segmenting formulas"
      (dolist (xp xps)
	(push xp temp-xp)
	(when (and temp-xp (string= xp "."))
	  (push (reverse temp-xp) collect)
	  (setf temp-xp nil))))
    collect))

(defun generate-formulas (xp-objects xps &key window)
  (let ((segmented-formulas (segment-formulas xps))
	unique-formulas)
    (with-info "Generating formulas" 
      (dolist (xp-object xp-objects)
        (with-slots (lbl rul) xp-object
	  (let ((found (remove-if-not #'(lambda (arg) (string= lbl (car arg)))
				      (remove-duplicates segmented-formulas :test 'equal))))
	    (dolist (item found)
	      (let ((new (make-instance 'neighbour
					:lbl item
					:frq (count item segmented-formulas :test 'equal))))
		(pushnew new unique-formulas)
		(push new rul)))))))
    (connect-words unique-formulas segmented-formulas
		   :test 'equal
		   :window window
		   :descriptor "formulas")
    unique-formulas))

(defun process-text (text &key window)
  (let* ((chunks (chunk-text text))
	 (xps (remove nil (mapcar #'cadr chunks)))
	 (words (remove nil (mapcar #'car chunks))) 
	 (xp-objects (generate-words chunks xps :window window :descriptor "xps"))
	 (word-objects (generate-words chunks words :xps xp-objects :window window :descriptor "words"))
	 (formula-objects (generate-formulas xp-objects xps :window window))) 
    (make-instance 'yoshino
		   :xph xp-objects
		   :rul formula-objects
		   :lbl word-objects
		   :cen (make-array (length word-objects)
				    :initial-element 0))))

(defun find-named-word (name words &key (test 'string=))
  (dolist (word words)
    (with-slots (lbl) word
      (when (funcall test lbl name)
	(return-from find-named-word word)))))

(defun find-named-position (name words &key (test 'string=))
  (position (find-named-word name words :test test)
	    words))
