;;;; parse-arithmetic.asd

(asdf:defsystem #:parse-arithmetic
  :name "parse-math"
  :description "Parse arithmetic expressions from a string."
  :license "Public Domain"
  :author "Sumant Oemrawsingh"
  :depends-on (#:alexandria #:parse-float)
  :components ((:file "package")
               (:file "parse-arithmetic"
		      :depends-on ("package"))))
