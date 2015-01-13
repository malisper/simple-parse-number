(defpackage :simple-parse-number
  (:nicknames :spn)
  (:use :cl)
  (:export :invalid-number-value
	   :invalid-number-reason
	   :parse-positive-real-number
	   :parse-real-number
	   :parse-number))
