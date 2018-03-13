(in-package #:yoshino.train)

(defvar *burgled?* nil)

(defmethod generate-clusters ((model yoshino) &key threshold)
  (with-info "Generating clusters"
    (with-slots (lbl clu) model
      (let ((i 0) seen)
	(dolist (source lbl)
	  (let ((similar-words (similar-words source lbl threshold)))
	    (unless (member similar-words seen :test 'equal)
	      (push (make-instance 'word
				   :lbl similar-words
				   :vec (average-vectors
					 (get-vectors similar-words)))
		    clu)
	      (push similar-words seen)))
	  (format t "done: ~d/~d~%" (incf i) (length lbl)))))))

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

(defun tag-text (raw-path &key (window 5) (file-root "../Models/UniPOS/UD_English/en-upos"))
  (let* ((path "~/.roswell/local-projects/yoshino/data")
	 (text-path (format nil "~d/~d" path raw-path)))
    (with-info "Tagging text"
      (inferior-shell:run/nil
       (format nil "cd ~d && python2 RDRPOSTAGGER.py tag ~d ~d ~d" 
	       (pathname (format nil "~d/tagger/pSCRDRtagger/" path))
	       (pathname (format nil "~d.RDR" file-root))
	       (pathname (format nil "~d.DICT" file-root))
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

(defun similar-words (source words threshold)
  (remove-if-not
   #'(lambda (target)
       (with-slots ((source-vec vec)) source
	 (with-slots ((target-vec vec)) target
	   (> (cos-similarity source-vec target-vec)
	      threshold))))
   words))

(defun get-vectors (words)
  (loop for word in words
     collect (with-slots (vec) word vec)))

(defun find-named-word (name words &key (test 'string=))
  (dolist (word words)
    (with-slots (lbl) word
      (when (funcall test lbl name)
	(return-from find-named-word word)))))

(defun find-named-position (name words &key (test 'string=))
  (position (find-named-word name words :test test)
	    words))
