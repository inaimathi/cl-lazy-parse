;;;; cl-lazy-parse.asd

(asdf:defsystem #:cl-lazy-parse
  :description "Describe cl-lazy-parse here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:alexandria #:flexi-streams #:usocket)
  :components ((:file "package")
	       (:file "queue")
	       (:file "lazy")
	       (:file "rapid")
               (:file "cl-lazy-parse")))

