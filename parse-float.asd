;;;; parse-float.asd

(asdf:defsystem #:parse-float
  :name "parse-float"
  :description "Parse floating point values in strings."
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "parse-float"
		      :depends-on ("package"))))
