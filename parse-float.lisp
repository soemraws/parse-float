;;;; parse-float.lisp

(in-package #:parse-float)

(eval-when (compile)
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

(declaim (inline skip-whitespace))
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
  (let ((sign 1)			; sign of the float
	(digits 0)			; number of decimal digits
	(index start)			; walking index
	(end (or end (length string)))	; end index of the string
	integer-part			; parts of the value
	(decimal-part 0)
	(exponent 0))
    (declare (type integer sign index end decimal-part exponent)
	     (type (or null integer) integer-part))
    (if (= start end)
	(if junk-allowed
	    (values nil start)
	    (simple-parse-error "No non-whitespace characters in string ~S."
				string))
	(labels ((process-integer (value position)
		   (setf index position)
		   (when value
		     (setf integer-part value
			   sign (if (< integer-part 0) -1 1)))
		   (unless (= index end)
		     (cond
		       ((char= (char string index)
			       decimal-character)
			(incf index)
			(unless (or (= index end)
				    (sign-char-p (char string index))
				    (whitespace-char-p (char string index)))
			  #'process-decimal))
		       ((char-equal (char string index)
				    exponent-character)
			(when (and value (= radix 10))
			  (incf index)
			  (if (or (= index end)
				  (whitespace-char-p (char string index)))
			      (progn (decf index) nil)
			      #'process-exponent))))))
		 (process-decimal (value position)
		   (setf digits (- position index)
			 index position)
		   (when value
		     (setf decimal-part (* sign value)))
		   (unless (= index end)
		     (when (and (= radix 10)
				(char-equal (char string index)
					    exponent-character))
		       (incf index)
		       (if (or (= index end)
			       (whitespace-char-p (char string index)))
			   (progn (decf index) nil)
			   #'process-exponent))))
		 (process-exponent (value position)
		   (setf index position)
		   (if value
		       (setf exponent value)
		       (decf index))
		   nil)
		 (make-float-value ()
		   (coerce (* (+ integer-part
				 (* (or decimal-part 0) (expt radix (- digits))))
			      (expt 10 exponent))
			   type)))
	  (setf index (skip-whitespaces string :start start :end end))
	  (loop with processor = #'process-integer
	     while processor
	     do (multiple-value-bind (value position)
		    (parse-integer string :start index :end end
				   :junk-allowed t :radix radix)
		  (setf processor
			(funcall processor value position)))
	     finally
	     (unless junk-allowed
	       (setf index (skip-whitespaces string :start index :end end))))
	  (cond ((or junk-allowed
		     (and (= index end)
			  integer-part))
		 (values (when integer-part (make-float-value)) index))
		((< index end)
		 (simple-parse-error "junk in string ~S." string))
		(t
		 (simple-parse-error "No non-whitespace characters in string ~S.")))))))
