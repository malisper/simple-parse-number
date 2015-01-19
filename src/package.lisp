(defpackage :simple-parse-number
  (:nicknames :spn)
  (:use :cl)
  (:export :invalid-number
           :invalid-number-value
	   :invalid-number-reason
	   :parse-number
	   :parse-real-number))
