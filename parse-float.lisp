;;;; parse-float.lisp

(in-package #:parse-float)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1))))

(defvar *whitespace-characters* '(#\Space #\Tab #\Return #\Newline #\Linefeed #\Page)
  "List of whitespace characters")

(declaim (inline sign-char-p))
(defun sign-char-p (character)
  "Predicate for testing if CHARACTER is a sign character (i.e. #\+ or #\-)."
  (declare (type character character))
  (or (char= #\+ character)
      (char= #\- character)))

(declaim (inline whitespace-char-p))
(defun whitespace-char-p (character)
  "Predicate for testing if CHARACTER is a whitespace character."
  (declare (type character character))
  (member character *whitespace-characters*))

(declaim (inline skip-whitespaces))
(defun skip-whitespaces (string &key from-end (start 0) end)
  "For the substring in STRING delimited by START and END, skip all
  the whitespace at the beginning and return the index of the first
  non-whitespace character, or END if no non-whitespace characters
  were found. If FROM-END is T, then the search is reversed, skipping
  all the whitespace at the end and returning the index of the last
  whitespace character from END, or START if no non-whitespace
  characters were found."
  (declare (type string string)
	   (type integer start)
	   (type (or null integer) end))
  (unless end
    (setf end (length string)))
  (if from-end
      (loop for index from (- end 1) downto start
	 while (whitespace-char-p (char string index))
	 finally (return
		   (if (whitespace-char-p (char string index))
		       index
		       (+ index 1))))
      (loop for index from start upto end
	 while (and (< index end)
		    (whitespace-char-p (char string index)))
	 finally
	 (return index))))

(defun parse-integer-only (string &key (start 0) (end nil)
			   (radix 10) (allow-sign t))
  "Parse an integer from a string, without skipping whitespaces.
Returns three values: the integer, the position in the string that
ended the parsing, and a boolean which is T if the parsing ended due
to a whitespace or end of the string, and NIL otherwise.  If
allow-sign is NIL (T by default), also signs are not allowed in the
string (i.e. cannot start with #\+ or #\-)."
  (declare (type string string)
	   (type integer start radix)
	   (type (or null integer) end))
  (let ((end (or end (length string)))
	(index start))
    (if (>= index end)
	(values nil index t)
	(let ((char (char string index)))
	  (if (or (and (not allow-sign) (sign-char-p char))
		  (whitespace-char-p char))
	      (values nil index t)
	      (multiple-value-bind (value position)
		  (parse-integer string
				 :start index
				 :end end
				 :junk-allowed t
				 :radix radix)
		(if (or (= position end)
			(whitespace-char-p (char string position)))
		    (values value position t)
		    (values value position nil))))))))

(defun parse-float (string &key (start 0) (end nil)
		    (radix 10) (junk-allowed nil)
		    (decimal-character #\.) (exponent-character #\e)
		    (type *READ-DEFAULT-FLOAT-FORMAT*))
  "Similar to PARSE-INTEGER, but parses a floating point value and
  returns the value as the specified TYPE (by default
  *READ-DEFAULT-FLOAT-FORMAT*). The DECIMAL-CHARACTER (by default #\.)
  specifies the separator between the integer and decimal parts, and
  the EXPONENT-CHARACTER (by default #\e, case insensitive) specifies
  the character before the exponent. Note that the exponent is only
  parsed if RADIX is 10."
  (declare (type string string)
	   (type integer start radix)
	   (type character decimal-character exponent-character)
	   (type (or null integer) end))
  (let* ((sign 1)			; sign of the float
	 (digits 0)			; number of decimal digits
	 (end (or end (length string)))	; end index of the string
	 (index (skip-whitespaces string ; index walking through string
				  :start start
				  :end end))
	 (integer-part nil)		; parts of the value
	 (decimal-part 0)
	 (exponent-part 0)
	 (result nil))                   ; final result
    (declare (type integer sign index end exponent-part)
	     (type (or null integer) integer-part decimal-part))
    (labels ((parse-sign ()
	       (if (= index end)
		   #'parse-finish
		   (let ((char (char string index)))
		     (cond
		       ((char= #\- char)
			(if (>= (incf index) end)
			    #'parse-finish
			    (progn
			      (setf sign -1)
			      #'parse-integer-part)))
		       ((char= #\+ char)
			(if (>= (incf index) end)
			    #'parse-finish
			    #'parse-integer-part))
		       (t #'parse-integer-part)))))

	     (parse-integer-part ()
	       (multiple-value-bind (value position finished)
		   (parse-integer-only string
				       :start index
				       :end end
				       :radix radix
				       :allow-sign nil)
		 (setf integer-part value
		       index position)
		 (if finished
		     #'parse-finish
		     (let ((char (char string index)))
		       (cond
			 ((char= char decimal-character)
			  (incf index)
			  #'parse-decimal-part)
			 ((null integer-part)
			  #'parse-finish)
			 ((and (char-equal char exponent-character)
			       (= radix 10))
			  (setf index (+ 1 index)
				decimal-part 0)
			  #'parse-exponent-part)
			 (t #'parse-finish))))))

	     (parse-decimal-part ()
	       (multiple-value-bind (value position finished)
		   (parse-integer-only string
				       :start index
				       :end end
				       :radix radix
				       :allow-sign nil)
		 (setf decimal-part (or value 0)
		       digits (- position index)
		       index position)
		 (when (and decimal-part
			    (null integer-part))
		   (setf integer-part 0))
		 (if finished
		     #'parse-finish
		     (progn
		       (unless decimal-part
			 (setf decimal-part 0))
		       (if (and (= radix 10)
				(char-equal (char string index) exponent-character))
			   (progn
			     (incf index)
			     #'parse-exponent-part)
			   #'parse-finish)))))
				 
	     (parse-exponent-part ()
	       (multiple-value-bind (value position)
		   (parse-integer string
				  :start index
				  :end end
				  :junk-allowed t)
		 (setf exponent-part (or value 0)
		       index position)
		 #'parse-finish))

	     (parse-finish ()
	       (unless junk-allowed
		 (setf index (skip-whitespaces string :start index :end end)))
	       (if integer-part
		   (if (or (= index end)
			   junk-allowed)
		       (setf result (coerce (* sign (+ integer-part
						       (* decimal-part
							  (expt radix (- digits))))
					       (expt 10 exponent-part))
					    type))
		       (simple-parse-error "junk in string ~S." string))
		   (unless junk-allowed
		       (simple-parse-error "junk in string ~S." string)))
	       nil))

      (loop with parser = #'parse-sign
	 while parser
	 do (setf parser (funcall parser))

	 finally (return (values result index))))))
