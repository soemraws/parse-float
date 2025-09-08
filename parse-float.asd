;;;; parse-float.asd

(asdf:defsystem #:parse-float
  :name "parse-float"
  :description "Parse floating point values in strings."
  :license "Public Domain"
  :author "Sumant Oemrawsingh"
  :maintainer "Public Domain"
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "parse-float"
		      :depends-on ("package"))))

(asdf:defsystem #:parse-float-tests
  :name "parse-float-tests"
  :description "Tests for parse-float."
  :license "Public Domain"
  :author "Sumant Oemrawsingh"
  :maintainer "Public Domain"
  :depends-on (#:parse-float #:lisp-unit)
  :components ((:file "parse-float-tests")))
