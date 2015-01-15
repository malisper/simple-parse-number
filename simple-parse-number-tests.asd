(defsystem "simple-parse-number-tests"
  :description "Tests for simple-parse-number."
  :depends-on ("simple-parse-number" "clunit")
  :components ((:module "tests"
                        :components ((:file "package")
                                     (:file "tests" :depends-on ("package"))))))
