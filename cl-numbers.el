(defstruct ratio
  numerator
  denumerator)

(defun cl:/ (number &rest numbers)
  (cond
    ((null numbers)
     (/ 1.0 number))
    ((and (= (length numbers) 1)
	  (integerp number)
	  (integerp (first numbers)))
     (if (zerop (% number (first numbers)))
	 (/ number (first numbers))
	 (make-ratio :numerator number :denumerator (first numbers))))
    (t
     (setq number (float number))
     (dolist (n numbers)
       (setq number (/ number n)))
     number)))

(defun cl:<= (number &rest numbers)
  (if (null numbers)
      t
      (and (<= number (first numbers))
	   (apply #'cl:<= numbers))))

(defun* parse-integer (string &key (start 0) (end (length string))
			      (radix 10) junk-allowed)
  (let ((sign 1)
	(integer 0)
	(i start)
	char digit)
    (while (whitespacep (char string i))
      (incf i)
      (when (= i end)
	(if junk-allowed
	    (return-from parse-integer (values nil i))
	    (error))))
    (setq char (char string i))
    (when (find char "+-")
      (when (char= char (code-char 45))
	(setq sign -1))
      (incf i)
      (when (= i end)
	(if junk-allowed
	    (return-from parse-integer (values nil i))
	    (error)))
      (setq char (char string i)))
    (while (setq digit (digit-char-p char radix))
      (setq integer (+ (* integer radix) digit))
      (incf i)
      (when (= i end)
	(return-from parse-integer (values (* sign integer) i)))
      (setq char (char string i)))
    (cond
      (junk-allowed
       (values (* sign integer) i))
      (t
       (do ((j i (1+ j)))
	   ((= j end)
	    (values (* sign integer) i))
	 (unless (whitespacep (char string j))
	   (error)))))))
