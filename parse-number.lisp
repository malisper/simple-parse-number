(defstruct (parsed-integer (:conc-name nil)
                           (:constructor make-parsed-integer
                                         (value num-digits)))
  "A parsed integer contains information on both the value and the
   number of digits. This is useful for reprsenting decimal values."
  num-digits value)

(define-condition invalid-number (parse-error)
  ((value :reader invalid-number-value
	  :initarg :value
	  :initform nil)
   (reason :reader invalid-number-reason
	   :initarg :reason
	   :initform "Not specified"))
  (:report (lambda (c s)
	     (format s "Invalid number: ~S [Reason: ~A]"
		     (invalid-number-value c)
                     (invalid-number-reason c)))))

(defparameter *whitespace-characters*
  '(#\space #\tab #\return #\newline)
  "A list of whitespace characters.")

(defparameter *exponent-markers*
  '(#\D #\E #\L #\F #\S)
  "A list of all of the exponent markers in uppercase.")

(defparameter *zero* (make-parsed-integer 0 0)
  "The zero parsed integer.")

(defun digit-char-value (char)
  "Returns the value of an alphabetical
   character if it were to be used as a digit."
  (+ 10
     (- (char-code (char-upcase char))
        (char-code #\A))))

(defun exponent-markers (radix)
  "Return a list of all of the valid exponent markers in the given
   radix."
  (remove-if-not (lambda (c)
                   (>= (digit-char-value c)
                       radix))
                 *exponent-markers*))

(defun exponent-marker-p (char &optional (radix 10))
  "Is the given character an exponent marker in the given radix?"
  (find (char-upcase char) (exponent-markers radix)))

(defun whitespace-p (x)
  "Is the given character a white space character?"
  (find x *whitespace-characters*))

(defun valid-p (char radix)
  "Is the character valid in the given radix?"
  (or (digit-char-p char radix)
      (exponent-marker-p char radix)
      (find char '(#\. #\/))))

(defun invalid-number (string reason)
  "Given a string for a value and a reason, signal an 'invalid-number'
   error with the given arguments."
  (error 'invalid-number :value string :reason reason))

(defun parse-trimmed-positive-real-number (string &key (radix 10))
  "Parse a positive real number from a string that has been trimmed."
  (macrolet ((err (reason) `(invalid-number string ,reason)))
    (let ((first-char (char string 0))
          (last-char  (char string (- (length string) 1))))
      (cond ((eql (length string) 0)
             (err "Cannot parse empty string"))
            ((string= string ".")
             (err "Only the dot is present"))
            ((some (lambda (x) (not (valid-p x radix))) string)
             (err (format nil "Invalid characters: ~{~:C~^ ~}"
                          (remove-duplicates
                           (remove-if (lambda (c)
                                        (valid-p c radix))
                                      (coerce string 'list))))))
            ((invalid-positive-real-number-first-char first-char radix)
             (err (format nil "Invalid use of ~A at the start ~
                               of a number" first-char)))
            ((invalid-last-char last-char radix)
             (err (format nil "Invalid use of ~A at the end ~
                               of a number" last-char)))
            ((>= (count #\/ string) 2)
             (err "Multiple /'s in number"))
            ((>= (count #\. string) 2)
             (err "Multiple .'s in number"))
            ((>= (count-if #'exponent-marker-p string)
                 2)
             (err "Multiple exponent markers in number"))
            (:else
             (let ((.-pos (position #\. string))
                   (/-pos (position #\/ string))
                   (exp-pos (position-if #'exponent-marker-p string)))
               (cond ((and (/= radix 10) (or exp-pos .-pos))
                      (err (format nil
                                   "Only decimal numbers can contain ~:[~
                                    an exponent-marker~; a decimal point~]"
                                   .-pos)))
                     ((and /-pos .-pos)
                      (err "Both . and / cannot be used at the same time"))
                     ((and /-pos exp-pos)
                      (err "Both an exponent-marker and / cannot be used at the same time."))
                     ((and .-pos exp-pos)
                      (if (< exp-pos .-pos)
                          (err "Exponent-markers must come after a dot.")
                          (apply #'make-exp-float
                                 radix
                                 (char string exp-pos)
                                 (split-and-parse string
                                                  radix
                                                  .-pos
                                                  exp-pos))))
                     (exp-pos
                      (apply #'make-exp-whole-float
                             radix
                             (char string exp-pos)
                             (split-and-parse string radix exp-pos)))
                     (/-pos
                      (apply #'pi-/ (split-and-parse string radix /-pos)))
                     (.-pos
                      (apply #'make-float (split-and-parse string radix .-pos)))
                     (:else (values (parse-integer string :radix radix))))))))))

(defun invalid-positive-real-number-first-char (char radix)
  "Is this character valid except at the start of a number?"
  (or (exponent-marker-p char radix) (find char '(#\. #\/))))

(defun invalid-last-char (char radix)
  "Is this character valid except at the end of a number?"
  (or (exponent-marker-p char radix) (eql char #\/)))

(defun split-and-parse (string radix &rest points)
  "Given a string of integers and a list of locations in beteween
   those integers, parse and return a list of each of those integers.
   The result will be a list of parsed integers."
  (loop with len = (length string)
        for (prev maybe-next) on (cons -1 points)
        for next = (or maybe-next len)
        collect (if (eql (+ prev 1) next) ; If the string would be
                    *zero*                ; empty, return zero instead.
                    (let ((str (subseq string (+ prev 1) next)))
                      (make-parsed-integer (parse-integer str :radix radix)
                                           (length str))))))

(defun pi-/ (x y)
  "Divide two parsed integers and return the resulting value."
  (/ (value x) (value y)))

(defun make-float (whole frac)
  "Make a float out of two parsed integers where X is the WHOLE is
   the whole part and FRACT is the fractional part."
  (coerce (+ (value whole)
            (/ (value frac) (expt 10 (num-digits frac))))
         *read-default-float-format*))

(defun base-for-exponent-marker (char)
  "Return the base for an exponent-marker."
  (case (char-upcase char)
    (#\D 10.0d0)
    (#\E (coerce 10 *read-default-float-format*))
    (#\F 10.0f0)
    (#\S 10.0s0)
    (#\L 10.0l0)))

(defun make-exp-whole-float (radix char whole exponent)
  "Make an float where CHAR is the exponent-marker, WHOLE is the whole
   part and EXPONENT is the exponent."
  (make-exp-float radix char whole *zero* exponent))

(defun make-exp-float (radix char whole frac exponent)
  "Build a float whose whole part is WHOLE, fractional part is FRAC,
   and exponent is EXPONENT. CHAR is the exponent-marker of the float."
  (* (expt (base-for-exponent-marker char) (value exponent))
     (+ (value whole)
        (/ (value frac) (expt radix (num-digits frac))))))
