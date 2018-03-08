(in-package #:yoshino.preprocess)

(defun load-file (path)
  "Load text file from path and return string."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun tag-text (raw-path &key (language "English"))
  (let* ((path "~/.roswell/local-projects/yoshino/data")
	 (text-path (format nil "~d/~d" path raw-path)))
    (with-info "Tagging"
      (inferior-shell:run/nil
       (format nil "cd ~d && python RDRPOSTAGGER.py tag ~d ~d ~d" 
	       (pathname (format nil "~d/tagger/pSCRDRtagger/" path))
	       (pathname (format nil "../Models/POS/~d.RDR" language))
	       (pathname (format nil "../Models/POS/~d.DICT" language))
	       (pathname text-path))))
    (preprocess-text (load-file (format nil "~d.TAGGED" text-path)))))

(defun chunk-text (text)
  (loop for item in (cl-ppcre:split "[\\s]" text)
     collect (cl-ppcre:split "[/]" item)))

(defun process-text (text &key (window 5))
  (let* ((chunks (chunk-text text))
	 (words (mapcar #'car chunks))	 
	 collect)
    (with-info "Chunking"
      (dolist (word (remove nil (remove-duplicates words :test 'string=)))
	(push (make-instance 'word
			     :str word
			     :xph (car (agethash word chunks))
			     :frq (count word words :test 'string=))
	      collect)))
    (with-info "Connecting"
      (dolist (word collect)
	(with-slots (str ngh) word
	  (let ((found (window window str words :test 'string=)))
	    (dolist (item found)
	      (push (make-instance 'neighbour
				   :wrd (find-named-word item collect)
				   :frq (count item found :test 'string=))
		    ngh))))))
    collect))

(defun find-named-word (name words)
  (dolist (word words)
    (with-slots (str) word
      (when (string= str name)
	(return-from find-named-word word)))))

(defun window (n object lst &key (test 'equal))
  "Get all elements within window N of OBJECT in LST."
  (let ((lst-length (length lst))
	positions)
    (loop for item in lst
       for index from 0
       do (when (funcall test object item)
	    (push index positions)))
    (reduce #'append
	    (loop for position in positions
	       for lower-bound = (- position n)
	       for upper-bound = (+ position n 1)
	       collect (delete object
			       (subseq lst
				       (if (> lower-bound 0) lower-bound 0)
				       (when (< upper-bound lst-length) upper-bound))
			       :test test)))))
