(defsystem "simple-parse-number"
  :description "A simple version of parse-number."
  :version "0.1"
  :license "BSD-3"
  :components ((:file "package")
               (:file "parse-number" :depends-on ("package"))))
