(in-package :simple-parse-number-tests)

(defun check-all-strings (strings)
  "Check that every element of STRINGS are strings."
  (assert (every #'stringp strings) ()
          "All of the arguments to assert-parse-same should be strings. Given: ~{~A~^, ~}"
          (remove-if #'stringp strings)))

(defmacro assert-parse-same (&rest args)
  "For each argument, assert that parsing it with read-from-string
   will given the same result as parsing it with parse-number."
  (check-all-strings args)
  `(progn ,@(loop for a in args collect
              `(assert-eql (read-from-string ,a) (parse-number ,a)))))

(defmacro assert-invalid (&rest args)
  "For each argument, assert that parsing it with parse-number will
   singal an invalid-number error."
  (check-all-strings args)
  `(progn ,@(loop for a in args collect
              `(assert-condition invalid-number (parse-number ,a)))))

(defsuite parse-number ())
(defsuite valid (parse-number))
(defsuite invalid (parse-number))

(defmacro defvalid-test (name &rest args)
  "Define a test that asserts all of the arguments parse the same."
  `(deftest ,name (valid) (assert-parse-same ,@args)))

(defmacro definvalid-test (name &rest args)
  "Define a test that asserts all of the arguments signal errors
   when being parsed."
  `(deftest ,name (invalid) (assert-invalid ,@args)))

(defvalid-test integers "1" "-1" "1034" "3." "-3." "-364")

(defvalid-test rationals "80/335" "1/2" "10/2")
(defvalid-test floats "1.3214" "3.5333" "2.4E4" "6.8d3" "13.09s3" "35.66l5" "21.4f2")
(defvalid-test radix "#xFF" "#b-1000" "#o-101/75" "#16rFF" "#9r10")
(defvalid-test complex "#C(1 2)" "#c ( #xF #o-1 ) " "#c(1d1 2s1)"  "#C(#9r44/61 4f4)")
(defvalid-test whitespace "2.56 " " 15 " " #c(1 2)" " #x10 ")

(definvalid-test invalid-character "5 . 5" "5abc10")
(definvalid-test multiple-minus "--10")
(definvalid-test assert-invalid "/20" "d10")
(definvalid-test multiple-single-use-chars "5/5/5" "1.2.3" "1d0s0")
(definvalid-test invalid-last-char "10/" "10d")
(definvalid-test conflicting-chars "10.5/20" "15/20.5" "5/10d0" "5d05/10")
(definvalid-test misc "5d0.1" "#x5.0d0" "#x5l0" "10/-5" "#x5.0" "." "#x10/-5")
