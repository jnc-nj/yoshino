(in-package #:cl-user)
(asdf:defsystem yoshino
  :author "Jack Nai-Chieh Chou <jacknchou@icloud.com>"
  :maintainer "Jack Nai-Chieh Chou <jacknchou@icloud.com>"
  :serial t
  :components ((:file "src/packages")
	       (:file "src/classes")
	       (:file "src/preprocess") 
	       (:file "src/hmm"))
  :depends-on (:alexandria
	       :asclepius-tools
	       :inferior-shell 
	       :cl-ppcre
	       :ltk))
