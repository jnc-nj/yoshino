(in-package #:yoshino.train)

(defvar *burgled?* nil)
(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defmethod generate-clusters ((model yoshino) &key threshold)
  (with-info "Generating clusters"
    (with-slots (lbl clu) model
      (let (seen)
	(labels ((cluster (source)
		   (let ((similar-words (similar-words source lbl threshold)))
		     (unless (member similar-words seen :test 'equal)
		       (when (> (length similar-words) 1)
			 (push (make-instance 'word
					      :lbl similar-words
					      :vec (average-vectors
						    (get-vectors similar-words)))
			       clu))
		       (push similar-words seen)))))
	  (lparallel:pmap nil #'cluster lbl))))))

(defmethod calculate-vector (n lbl (model word))
  (let ((vector (make-array n :initial-element 0)))
    (with-slots (ngh) model
      (dolist (neighbour ngh)
	(with-slots ((_lbl lbl) frq) neighbour
	  (with-slots ((local-lbl lbl)) _lbl
	    (incf (aref vector (find-named-position local-lbl lbl))
		  frq)))))
    vector))

(defun load-file (path)
  "Load text file from path and return string."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun burgle-batteries ()
  (unless *burgled?*
    (with-info "Starting NLTK"
      (burgled-batteries:startup-python)
      (burgled-batteries:import "sys")
      (burgled-batteries:run "sys.path.append('/usr/local/lib/python3.6/site-packages/')")
      (burgled-batteries:import "nltk")
      (burgled-batteries:run "nltk.download('punkt')")
      (burgled-batteries:run "nltk.download('averaged_perceptron_tagger')")
      (setf *burgled?* t))))

(defun tag-words (str)
  "Return parts of speech of tokenized str."  
  (burgle-batteries)
  (coerce (burgled-batteries:run
	   (format nil "nltk.pos_tag(nltk.word_tokenize('~d'.decode('utf-8')))"
		   (cl-ppcre:regex-replace-all "[\\W]" str " ")))
          'list))

(defun tag-text (raw-path &key (window 3) (threshold 0.8))
  (let* ((path "~/.roswell/local-projects/yoshino/data")
	 (text-path (format nil "~d/~d" path raw-path)))
    (with-info "Tagging text"
      (inferior-shell:run/nil
       (format nil "cd ~d && python2 RDRPOSTAGGER.py tag ~d ~d ~d" 
	       (pathname (format nil "~d/tagger/pSCRDRtagger/" path))
	       (pathname (format nil "../Models/POS/English.RDR"))
	       (pathname (format nil "../Models/POS/English.DICT"))
	       (pathname text-path))))
    (let* ((text (load-file (format nil "~d.TAGGED" text-path)))
	   (processed (process-text text :window window)))
      ;;(generate-clusters processed :threshold threshold)
      processed)))

(defun chunk-text (text)
  (let (chunks)
    (with-info "Chunking text"
      (dolist (item (cl-ppcre:split "[\\s]" text))
	(push (cl-ppcre:split "[/]" item) chunks)))
    (reverse chunks)))

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
    (labels ((connect (word)
	       (with-slots (lbl ngh) word
		 (let ((found (window window lbl words :test test)))
		   (dolist (item (remove-duplicates found :test test))
		     (push (make-instance 'neighbour
					  :lbl (find-named-word item word-objects :test test) 
					  :frq (count item found :test test))
			   ngh))))))
      (lparallel:pmap nil #'connect word-objects))))

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
	  (unless (member nil temp-xp)
	    (push (reverse temp-xp) collect))
	  (setf temp-xp nil))))
    (reverse collect)))

(defun generate-formulas (xp-objects xps &key window)
  (let ((segmented-formulas (segment-formulas xps))
	unique-formulas)
    (with-info "Generating formulas"
      (labels ((generate (xp-object)
		 (with-slots (lbl rul) xp-object
		   (let ((found (remove-if-not #'(lambda (arg) (string= lbl (car arg)))
					       (remove-duplicates segmented-formulas :test 'equal))))
		     (dolist (item found)
		       (let ((new (make-instance 'neighbour
						 :lbl item
						 :frq (count item segmented-formulas :test 'equal))))
			 (pushnew new unique-formulas)
			 (push new rul)))))))
	(lparallel:pmap nil #'generate xp-objects)))
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

(defun similar-words (source words threshold)
  (remove-if-not
   #'(lambda (target)
       (with-slots ((source-vec vec)) source
	 (with-slots ((target-vec vec)) target
	   (> (cos-similarity source-vec target-vec)
	      threshold))))
   words))

(defun get-vectors (objects)
  (loop for object in objects
     collect (with-slots (vec) object vec)))

(defun get-frequencies (objects)
  (loop for object in objects
     collect (with-slots (frq) object frq)))

(defun get-labels (objects)
  (loop for object in objects
     collect (with-slots (lbl) object lbl)))

(defun find-named-word (name words &key (test 'string=))
  (dolist (word words)
    (with-slots (lbl) word
      (when (funcall test lbl name)
	(return-from find-named-word word)))))

(defun find-named-position (name words &key (test 'string=))
  (position (find-named-word name words :test test)
	    words))
