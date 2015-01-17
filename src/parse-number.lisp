(in-package :simple-parse-number)

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

(defun invalid (string reason)
  "Given a string for a value and a reason, signal an 'invalid-number'
   error with the given arguments."
  (error 'invalid-number :value string :reason reason))

(defparameter *whitespace-characters*
  '(#\space #\tab #\return #\newline)
  "A list of whitespace characters.")

(defun whitespace-p (x)
  "Is the given character a white space character?"
  (find x *whitespace-characters*))

(defmacro deftrimmer (regular trimmer args &body body)
  "Define a function REGULAR which is a normal function with ARGS and
   BODY. Also define a function, TRIMMER, which will take in a string
   and will also accept several keyword arguments such as start, end,
   and radix. It will then call REGULAR with a trimmed version of the
   string and the radix."
  `(progn (defun ,regular ,args ,@body)
	  (defun ,trimmer (string &key (start 0) end (radix 10)
				    ((:float-format *read-default-float-format*)
				     *read-default-float-format*))
	    ,(format nil "Call ~A with the string after trimming it." regular)
	    (,regular (string-trim *whitespace-characters* (subseq string start end))
		      radix))))

(deftrimmer parse-trimmed-number parse-number (string radix)
  "Parse any number out of a string that has been trimmed."
  ;; This mostly handles complex numbers.
  (or (and (eql (char string 0) #\#)
	   (eql (char-upcase (char string 1)) #\C)
	   (let ((pos-left  (position #\( string))
		 (pos-right (position #\) string)))
	     (if (not (and pos-left pos-right (< pos-left pos-right)
			   (eql (count #\( string) 1)
			   (eql (count #\) string) 1)))
		 (invalid string "Mismatched parenthesis")
		 (let* ((starting-left (position-if-not #'whitespace-p string :start (+ pos-left 1)))
			(delimiting-left (position-if #'whitespace-p string :start starting-left))
			(starting-right (position-if-not #'whitespace-p string :end pos-right :from-end t))
			(delimiting-right (position-if #'whitespace-p string :end starting-right :from-end t)))
		   (complex
		    (parse-real-number string :start starting-left :end delimiting-left :radix radix)
		    (parse-real-number string :start delimiting-right :end (+ starting-right 1) :radix radix))))))
      (parse-real-number string :radix radix)))

(defparameter *reader-macro-vals*
  '((#\B 2) (#\O 8) (#\X 16))
  "A list of all of the reader macro characters and their
   corresponding values.")

(deftrimmer parse-trimmed-real-number parse-real-number (string radix)
  "Parse a real number from a trimmed string. Handle all of the
   possible reader macros as well as the minus sign."
  (case (char string 0)
    (#\- (- (parse-trimmed-positive-real-number (subseq string 1) radix)))
    (#\# (let ((pair (assoc (char-upcase (char string 1)) *reader-macro-vals*)))
	   (cond (pair (parse-real-number (subseq string 2) :radix (cadr pair)))
		 ((digit-char-p (char string 1))
		  (let ((r-pos (position #\R string :key #'char-upcase)))
		    (if (not r-pos)
			(invalid string "Missing R in #radixR")
			(parse-real-number (subseq string (+ r-pos 1))
			  :radix (parse-trimmed-positive-real-number
				   (subseq string 1 r-pos) 10)))))
		 (:else (invalid string
			  (format nil "Invalid reader macro #~:C" (char string 1)))))))
    (otherwise (parse-trimmed-positive-real-number string radix))))

(deftrimmer parse-trimmed-positive-real-number parse-positive-real-number (string radix)
  "Parse a positive real number from a string that has been trimmed."
  (macrolet ((err (reason) `(invalid string ,reason)))
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
            ((>= (count-if (lambda (c)
			     (exponent-marker-p c radix))
			   string)
                 2)
             (err "Multiple exponent markers in number"))
            (:else
             (let ((.-pos (position #\. string))
                   (/-pos (position #\/ string))
                   (exp-pos (position-if (lambda (c)
					   (exponent-marker-p c radix))
					 string)))
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

(defparameter *exponent-markers*
  '(#\D #\E #\L #\F #\S)
  "A list of all of the exponent markers in uppercase.")

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

(defun valid-p (char radix)
  "Is the character valid in the given radix?"
  (or (digit-char-p char radix)
      (exponent-marker-p char radix)
      (find char '(#\. #\/))))

(defun digit-char-value (char)
  "Returns the value of an alphabetical
   character if it were to be used as a digit."
  (+ 10
     (- (char-code (char-upcase char))
        (char-code #\A))))

(defun invalid-positive-real-number-first-char (char radix)
  "Is this character valid except at the start of a number?"
  (or (exponent-marker-p char radix) (find char '(#\. #\/))))

(defun invalid-last-char (char radix)
  "Is this character valid except at the end of a number?"
  (or (exponent-marker-p char radix) (eql char #\/)))

(defstruct (parsed-integer (:conc-name nil)
                           (:constructor make-parsed-integer
                                         (value num-digits)))
  "A parsed integer contains information on both the value and the
   number of digits. This is useful for reprsenting decimal values."
  num-digits value)

(defparameter *empty* (make-parsed-integer 0 0)
  "The empty parsed integer.")

(defun split-and-parse (string radix &rest points)
  "Given a string of integers and a list of locations in beteween
   those integers, parse and return a list of each of those integers.
   The result will be a list of parsed integers."
  (loop with len = (length string)
        for (prev maybe-next) on (cons -1 points)
        for next = (or maybe-next len)
        collect (if (eql (+ prev 1) next) ; If the string would be
                    *empty*               ; empty, use the empty pi.
                    (let ((str (subseq string (+ prev 1) next)))
                      (make-parsed-integer (parse-integer str :radix radix)
                                           (length str))))))

(defun pi-/ (x y)
  "Divide two parsed integers and return the resulting value."
  (/ (value x) (value y)))

(defun make-float (whole frac)
  "Make a float out of two parsed integers where X is the WHOLE is
   the whole part and FRACT is the fractional part."
  (if (eql (num-digits frac) 0)
      (value whole)
      (coerce (+ (value whole)
                 (/ (value frac) (expt 10 (num-digits frac))))
              *read-default-float-format*)))

(defun base-for-exponent-marker (marker)
  "Return the base for an exponent-marker."
  (case (char-upcase marker)
    (#\D 10.0d0)
    (#\E (coerce 10 *read-default-float-format*))
    (#\F 10.0f0)
    (#\S 10.0s0)
    (#\L 10.0l0)))

(defun make-exp-whole-float (radix marker whole exponent)
  "Make an float where MARKER is the exponent-marker, WHOLE is the whole
   part and EXPONENT is the exponent."
  (declare (ignore radix))
  (* (value whole)
     (expt (base-for-exponent-marker marker)
	   (value exponent))))

(defun make-exp-float (radix marker whole frac exponent)
  "Build a float whose whole part is WHOLE, fractional part is FRAC,
   and exponent is EXPONENT. MARKER is the exponent-marker of the float."
  (let* ((base (base-for-exponent-marker marker))
	 (exp (expt base (value exponent))))
    ;; It is impossible to factor out the exp because that would
    ;; lead to rounding errors with floats.
    (+ (* exp (value whole))
       (/ (* exp (value frac))
	  (expt (float radix base)
		(num-digits frac))))))
