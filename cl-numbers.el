;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;;
;;; This file implements operators in chapter 12, Numbers.

(in-package "CL")

;;; Various test cases for bignum addition.
(defun bignum-test ()
  (loop for (x y z) in
	'((67108864		67108864		[bignum -134217728 0])
	  (134217727		1			[bignum -134217728 0])
	  (-134217728		-1			[bignum 134217727 -1])
	  (-134217728		-134217728		[bignum 0 -1])
	  ([bignum 0 2]		[bignum 0 -1]		[bignum 0 1])
	  ([bignum 0 -1]	[bignum 0 2]		[bignum 0 1])
	  ([bignum 0 1]		[bignum 0 -2]		[bignum 0 -1])
	  ([bignum 0 -2]	[bignum 0 1]		[bignum 0 -1])
	  ([bignum 2 2]		[bignum -1 -3]		1)
	  ([bignum 2 2]		[bignum -3 -3]		-1))
	do (unless (equal (cl:+ x y) z)
	     (princ (format "%s + %s /= %s" x y z)))))

(defun cl:= (number &rest numbers)
  (every (lambda (n) (binary= number n)) numbers))

(defun binary= (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (= num1 num2))
    ((or (complexp num1) (complexp num2))
     (and (binary= (realpart num1) (realpart num2))
	  (binary= (imagpart num1) (imagpart num2))))
    ((or (cl::ratiop num1) (cl::ratiop num2))
     (and (binary= (numerator num1) (numerator num2))
	  (binary= (denominator num1) (denominator num2))))
    ((and (cl::bignump num1) (cl::bignump num2))
     (and (= (length num1) (length num2))
	  (every #'eql num1 num2)))
    ((and (cl:numberp num1) (cl:numberp num2))
     nil)
    (t
     (error "type error: = %s %s" num1 num2))))

(defun cl:/= (number &rest numbers)
  (if (null numbers)
      t
      (and (not (some (lambda (num) (binary= number num)) numbers))
	   (apply #'cl:/= (first numbers) (rest numbers)))))

(defun cl:< (number &rest numbers)
  (if (null numbers)
      t
      (and (binary< number (first numbers))
	   (apply #'cl:< (first numbers) (rest numbers)))))

(defun binary< (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (< num1 num2))
    ((or (cl::ratiop num1) (cl::ratiop num2))
     ;; TODO
     (< (/ (float (numerator num1)) (denominator num1))
	(/ (float (numerator num2)) (denominator num2))))
    ((or (cl::bignump num1) (cl::bignump num2))
     (cl:minusp (cl:- num1 num2)))
    (t
     (error "type error: = %s %s" num1 num2))))

(defun cl:> (number &rest numbers)
  (if (null numbers)
      t
      (and (binary< (first numbers) number)
	   (apply #'cl:> (first numbers) (rest numbers)))))

(defun cl:<= (number &rest numbers)
  (if (null numbers)
      t
      (and (binary<= number (first numbers))
	   (apply #'cl:<= (first numbers) (rest numbers)))))

(defun binary<= (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (<= num1 num2))
    ((or (cl::ratiop num1) (cl::ratiop num2))
     ;; TODO
     (<= (/ (float (numerator num1)) (denominator num1))
	 (/ (float (numerator num2)) (denominator num2))))
    ((or (cl::bignump num1) (cl::bignump num2))
     (let ((diff (cl:- num1 num2)))
       (or (minusp diff) (zerop diff))))
    (t
     (error "type error: = %s %s" num1 num2))))

(defun cl:>= (number &rest numbers)
  (if (null numbers)
      t
      (and (binary>= (first numbers) number)
	   (apply #'cl:>= (first numbers) (rest numbers)))))

;;; TODO: max

;;; TODO: min

(defun cl:minusp (num)
  (cond
    ((or (integerp num) (floatp num))
     (minusp num))
    ((cl::bignump num)
     (minusp (aref num (1- (length num)))))
    ((cl::ratiop num)
     (minusp (numerator num)))
    (t
     (error "type error"))))

(defun cl:plusp (num)
  (cond
    ((or (integerp num) (floatp num))
     (plusp num))
    ((cl::bignump num)
     (plusp (aref num (1- (length num)))))
    ((cl::ratiop num)
     (plusp (numerator num)))
    (t
     (error "type error"))))

(defun cl:zerop (num)
  (cond
    ((or (integerp num) (floatp num))
     (zerop num))
    ((cl::ratiop num)
     (zerop (numerator num)))
    ((complexp num)
     (and (cl:zerop (realpart num)) (cl:zerop (imagpart num))))
    (t
     (error "type error"))))

;;; TODO: FLOOR, FFLOOR, CEILING, FCEILING, TRUNCATE, FTRUNCATE, ROUND, FROUND

;;; TODO: SIN, COS, TAN

;;; TODO: ASIN, ACOS, ATAN

;;; TODO: (defconstast pi ...)

;;; TODO: SINH, COSH, TANH, ASINH, ACOSH, ATANH

(defun cl:* (&rest numbers)
  (reduce #'binary* numbers :initial-value 1))

(defconst multiplication-limit (ceiling (sqrt most-positive-fixnum)))

(defun binary* (x y)
  (cond
    ((and (integerp x)
	  (< x multiplication-limit)
	  (> x (- multiplication-limit))
	  (integerp y)
	  (< y multiplication-limit)
	  (> y (- multiplication-limit)))
     (* x y))
    ((or (complexp x) (complexp y))
     (complex (binary- (binary* (realpart x) (realpart y))
		       (binary* (imagpart x) (imagpart y)))
	      (binary+ (binary* (realpart x) (imagpart y))
		       (binary* (imagpart x) (realpart y)))))
    ((floatp x)
     (* x (cl:float y)))
    ((floatp y)
     (* (cl:float x) y))
    ((or (cl::ratiop x) (cl::ratiop y))
     (if (cl:zerop y)
	 (error)
	 (cl::ratio (binary* (numerator x) (denominator y))
		    (binary* (denominator x) (numerator y)))))
    ((or (cl::bignump x) (cl::bignump y))
     (when (integerp x)
       (setq x (vector 'bignum x (if (minusp x) -1 0))))
     (when (integerp y)
       (setq y (vector 'bignum y (if (minusp y) -1 0))))
     (bignum* x y))
    (t
     (error "TODO"))))

(defun bignum* (x y)
  0)

(defun cl:+ (&rest numbers)
  (reduce #'binary+ numbers :initial-value 0))

(defun binary+ (x y)
  (cond
    ((and (integerp x) (integerp y))
     (let ((sum (+ x y)))
       (cond
	 ((and (>= x 0) (>= y 0) (minusp sum))
	  (vector 'bignum sum 0))
	 ((and (minusp x) (minusp y) (>= sum 0))
	  (vector 'bignum sum -1))
	 (t
	  sum))))
    ((or (complexp x) (complexp y))
     (complex (binary+ (realpart x) (realpart y))
	      (binary+ (imagpart x) (imagpart y))))
    ((floatp x)
     (+ x (cl:float y)))
    ((floatp y)
     (+ (cl:float x) y))
    ((or (cl::ratiop x) (cl::ratiop y))
     (cl::ratio (binary+ (binary* (numerator x) (denominator y))
			 (binary* (denominator x) (numerator y)))
		(binary* (denominator x) (denominator y))))
    ((or (cl::bignump x) (cl::bignump y))
     (cond
       ((integerp x)	(bignum+fixnum y x))
       ((integerp y)	(bignum+fixnum x y))
       (t		(bignum+bignum x y))))
    (t
     (error))))

(defun bignum+fixnum (x y)
  (let* ((x0 (aref x 1))
	 (sum (+ x0 y))
	 (new (copy-sequence x)))
    (aset new 1 sum)
;   (print x0)
;   (print y)
;   (print sum)
    (cond
      ((or (and (>= x0 0) (>= y 0) (minusp sum))
	   (and (minusp x0) (>= y 0) (>= sum 0)))
       (bignum+bignum new [bignum 0 1]))
      ((or (and (minusp x0) (minusp y) (>= sum 0))
	   (and (>= x0 0) (minusp y) (minusp sum)))
       (bignum+bignum new [bignum 0 -1]))
      (t
       (canonical-bignum new)))))

(defun bignum+bignum (x y)
  (canonical-bignum (bignum+ (bignum-list x) (bignum-list y))))

(defun* bignum-list (num &optional (index 1))
  (if (= index (length num))
      nil
      (cons (aref num index) (bignum-list num (1+ index)))))

(defun canonical-bignum (object)
  (cond
    ((cl::bignump object)
     (canonical-bignum (bignum-list object)))
    ((listp object)
     (setq object (truncate-sign-extension object))
     (if (eql (length object) 1)
	 (first object)
	 (let ((bignum (make-vector (1+ (length object)) 'bignum))
	       (i 0))
	   (dolist (n object)
	     (aset bignum (incf i) n))
	   bignum)))
    (t
     (error))))

(defun truncate-sign-extension (list &optional prev)
  (if (null list)
      nil
      (let ((rest (truncate-sign-extension (rest list) (first list))))
	(setf (cdr list) rest)
	(if (null rest)
	    (let ((this (first list)))
	      (cond
		((and (zerop this) prev (>= prev 0))
		 nil)
		((and (eql this -1) prev (minusp prev))
		 nil)
		(t
		 list)))
	    list))))

(defun* bignum+ (x y &optional (carry 0))
; (print (format "(bignum+ %s %s %s)" x y carry))
  (cond
    ((null x)
     (if (zerop carry)
	 y
	 (bignum+ y (list carry))))
    ((null y)
     (if (zerop carry)
	 x 
	 (bignum+ x (list carry))))
    (t
     (let* ((x0 (car x))
	    (y0 (car y))
	    (sum (+ x0 y0 carry)))
;      (print (format "x0=%s y0=%s sum=%s" x0 y0 sum))
       (cons sum
	     (bignum+
	      (rest x)
	      (rest y)
	      (if (or (and (minusp x0) (>= y0 0) (>= sum 0) (rest x))
		      (and (>= x0 0) (minusp y0) (>= sum 0) (rest y))
		      (and (minusp x0) (minusp y0) (>= sum 0)))
		  1 0)))))))

(defun cl:- (number &rest numbers)
  (if (null numbers)
      (cond
	((or (integerp number) (floatp number))
	 (if (eql number most-negative-fixnum)
	     (vector 'bignum number 0)
	     (- number)))
	((cl::ratiop number)
	 (vector 'ratio (cl:- (numerator number)) (denominator number)))
	((complexp number)
	 (vector 'complex (cl:- (realpart number)) (cl:- (imagpart number))))
	((cl::bignump number)
	 (bignum+fixnum (cl:lognot number) 1))
	(t
	 (error)))
      (dolist (num numbers number)
	(setq number (binary- number num)))))

(defun binary- (x y)
  (binary+ x (cl:- y)))

(defun cl:/ (number &rest numbers)
  (if (null numbers)
      (cond
	((integerp number)
	 (vector 'ratio 1 number))
	((floatp number)
	 (/ 1.0 number))
	((cl::ratiop number)
	 (cl::ratio (denominator number) (numerator number)))
	((complexp number)
	 (let* ((r (realpart number))
		(i (imagpart number))
		(x (cl:- (cl:* r r) (cl:* i i))))
	   (complex (cl:/ r x) (cl:+ (cl:/ i) x))))
	((cl::bignump number)
	 0)
	(t
	 (error)))
      (dolist (num numbers number)
	(setq number (binary/ number num)))))

(defun binary/ (x y)
  (cond
    ((and (integerp x) (integerp y))
     (if (or (and (eql x most-negative-fixnum) (eql y -1))
	     (and (eql y most-negative-fixnum) (eql x -1)))
	 (vector 'bignum 0)
	 (/ x y)))
    ((or (complexp x) (complexp y))
     (let* ((rx (realpart x))
	    (ry (realpart y))
	    (ix (imagpart x))
	    (iy (imagpart y))
	    (div (cl:+ (cl:* ry ry) (cl:* iy iy))))
       (complex (cl:/ (cl:+ (cl:* rx ry) (cl:* ix iy)) div)
		(cl:/ (cl:- (cl:* ix ry) (cl:* rx iy)) div))))
    ((floatp x)
     (/ x (cl:float y)))
    ((floatp y)
     (/ (cl:float x) y))
    ((or (cl::ratiop x) (cl::ratiop y))
     (cl::ratio (cl:* (numerator x) (denominator y))
	    (cl:* (denominator x) (numerator y))))
    ;; bignum
    (t 0)))
  
(defun cl:1+ (number)
  (cl:+ number 1))

(defun cl:1- (number)
  (cl:- number 1))

(defun cl:abs (number)
  (cond
    ((integerp number)
     (if (eql number most-negative-fixnum)
	 (vector 'bignum number 0)
	 (abs number)))
    ((floatp number)
     (abs number))
    ((cl::ratiop number)
     (vector 'ratio (cl:abs (numerator number)) (denominator number)))
    ((complexp number)
     (sqrt (+ (expt (realpart number) 2) (expt (imagpart number) 2))))
    ((cl::bignump number)
     0)
    (t
     (error))))

;;; TODO: EVENP, ODDP

;;; TODO: EXP, EXPT

(defun gcd (&rest numbers)
  (reduce #'binary-gcd numbers :initial-value 0))

(defun binary-gcd (x y)
  (if (and (integerp x) (integerp y))
      (progn
	(when (> y x)
	  (psetq x y y x))
	(while (not (zerop y))
	  (psetq y (% x y) x y))
	(abs x))
      0))

;;; TODO: INCF, DECF

;;; TODO: LCM

;;; TODO: LOG

;;; TODO: MOD, REM

;;; TODO: SIGNUM

;;; TODO: SQRT, ISQRT

;;; TODO: MAKE-RANDOM-STATE

;;; TODO: RANDOM

;;; TODO: RANDOM-STATE-P

;;; TODO: *RANDOM-STATE*

(defun cl:numberp (object)
  (or (numberp object)
      (and (vectorp object)
	   (let ((type (aref object 0)))
	     (or (eq type 'bignum)
		 (eq type 'ratio)
		 (eq type 'complex))))))

;;; TODO: CIS

(defun complex (realpart &optional imagpart)
  (check-type realpart 'real)
  (if (or (null imagpart) (zerop imagpart))
      realpart
      (progn
	(check-type realpart 'real)
	(when (floatp realpart)
	  (setq imagpart (float realpart)))
	(when (floatp imagpart)
	  (setq realpart (float realpart)))
	(vector 'complex realpart imagpart))))

(defun complexp (object)
  (and (vectorp object) (eq (aref object 0) 'complex)))

(defun conjugage (num)
  (complex (realpart num) (- (imagpart num))))

(defun phase (num)
  (atan (imagpart num) (realpart num)))

(defun realpart (num)
  (if (complexp num)
      (aref num 1)
      num))

(defun imagpart (num)
  (if (complexp num)
      (aref num 2)
      0))

(defun upgraded-complex-part-type (typespec &optional env)
  'real)

(defun realp (num)
  (or (rationalp num) (floatp num)))

(defun cl::ratio (num den)
  (unless (and (cl:integerp num) (cl:integerp den))
    (error))
  (let* ((gcd (gcd num den))
	 (num (cl:/ num gcd))
	 (den (cl:/ den gcd)))
    (cond
      ((eql den 1)
	num)
      ((minusp den)
       (vector 'ratio (cl:- num) den))
      (t
       (vector 'ratio num den)))))

(defun cl::ratiop (num)
  (vector-and-typep num 'ratio))

(defun numerator (num)
  (if (cl::ratiop num)
      (aref num 1)
      num))

(defun denominator (num)
  (if (cl::ratiop num)
      (aref num 2)
      1))

;;; TODO: rational

;;; TODO: rationalize

(defun rationalp (num)
  (or (cl:integerp num) (cl::ratiop num)))

(defun cl:ash (num shift)
  (cond
    ((cl:zerop shift)
     num)
    ((cl:minusp shift)
     (cond
       ((integerp num)
	(ash num shift))
       ((cl::bignump num)
	(let ((new (copy-sequence num)))
	  (while (cl:minusp shift)
	    (shift-right new)
	    (incf shift))
	  (canonical-bignum new)))
       (t
	(error))))
    (t
     (while (> shift 0)
       (setq num (binary+ num num)
	     shift (1- shift)))
     num)))

(defun shift-right (num)
  (let ((i (1- (length num)))
	(first t)
	(carry 0))
    (while (plusp i)
      (let ((n (aref num i)))
	(aset num i (if first
			(ash n -1)
			(logior (lsh n -1) (ash carry 27))))
	(setq carry (logand n 1)
	      first nil))
      (decf i))))

;;; TODO: integer-length

(defun cl::bignump (num)
  (vector-and-typep num 'bignum))

(defun cl:integerp (num)
  (or (integerp num) (cl::bignump num)))

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

(defun cl:lognot (num)
  (cond
    ((integerp num)
     (lognot num))
    ((cl::bignump num)
     ;; TODO: may need one more element in result.
     (let ((new (make-vector (length num) 'bignum)))
       (dotimes (i (1- (length num)))
	 (aset new (1+ i) (lognot (aref num (1+ i)))))
       new))
    (t
     (error "type error"))))

(defun cl:logand (&rest numbers)
  (reduce #'binary-logand numbers :initial-value -1))

(defun binary-logand (x y)
  (cond
    ((and (integerp x) (integerp y))
     (logand x y))
    ((and (cl::bignump x) (integerp y))
     (if (minusp y)
	 (let ((new (copy-sequence x)))
	   (aset new 1 (logand (aref x 1) y))
	   new)
	 (logand (aref x 1) y)))
    ((and (cl::bignump y) (integerp x))
     (if (minusp x)
	 (let ((new (copy-sequence y)))
	   (aset new 1 (logand (aref y 1) x))
	   new)
	 (logand (aref y 1) x)))
    ((and (cl::bignump x) (cl::bignump y))
     0)))

(defun cl:logior (&rest numbers)
  (reduce #'binary-logior numbers :initial-value -1))

(defun binary-logior (x y)
  (cond
    ((and (integerp x) (integerp y))
     (logior x y))
    ((and (cl::bignump x) (integerp y))
     (if (minusp y)
	 (logior (aref x 1) y)
	 (let ((new (copy-sequence x)))
	   (aset new 1 (logior (aref x 1) y))
	   new)))
    ((and (cl::bignump y) (integerp x))
     (if (minusp x)
	 (logior (aref y 1) x)
	 (let ((new (copy-sequence y)))
	   (aset new 1 (logior (aref y 1) x))
	   new)))
    ((and (cl::bignump x) (cl::bignump y))
     0)))

(defun cl:lognand (x y)
  (cl:lognot (cl:logand x y)))

(defun cl:logandc1 (x y)
  (cl:logand (cl:lognot x) y))

(defun cl:logandc2 (x y)
  (cl:logand x (cl:lognot y)))

(defun cl:lognor (x y)
  (cl:lognot (cl:logior x y)))

(defun cl:logorc1 (x y)
  (cl:logior (cl:lognot x) y))

(defun cl:logorc2 (x y)
  (cl:logior x (cl:lognot y)))

(defun cl:logeqv (&rest numbers)
  (cl:lognot (apply #'cl:logxor numbers)))

(defun cl:logxor (&rest numbers)
  (reduce #'binary-logxor numbers :initial-value 0))

(defun binary-logxor (x y)
  (cond
    ((and (integerp x) (integerp y))
     (logxor x y))
    ((and (cl::bignump x) (integerp y))
     (let ((new (copy-sequence x)))
       (aset new 1 (logxor (aref x 1) y))
       (when (minusp y)
	 (dotimes (i (- (length x) 2))
	   (aset new (+ i 2) (lognot (aref new (+ i 2))))))
       new))
    ((and (cl::bignump y) (integerp x))
     (let ((new (copy-sequence y)))
       (aset new 1 (logior (aref y 1) x))
       (when (minusp x)
	 (dotimes (i (- (length y) 2))
	   (aset new (+ i 2) (lognot (aref new (+ i 2))))))
       new))
    ((and (cl::bignump x) (cl::bignump y))
     0)))


;;; TODO: logbitp

;;; TODO: logcount

;;; TODO: logtest

;;; TODO: byte, byte-size, byte-position

;;; TODO: deposit-field

;;; TODO: dpb

;;; TODO: ldb

;;; TODO: ldb-test

;;; TODO: mask-field

;;; TODO: decode-float, scale-float, float-radix, float-sign, float-digits,
;;; float-precision, integer-decode-float

(defun cl:float (num &optional prototype)
  (cond
    ((integerp num)
     (float num))
    ((floatp num)
     num)
    ((cl::ratiop num)
     (/ (cl:float (numerator num)) (cl:float (denominator num))))
    ((cl::bignump num)
     1.0)
    (t
     (error "type error"))))

;;; floatp ok as is

;;; TODO: ARITHMETIC-ERROR-OPERANDS, ARITHMETIC-ERROR-OPERATION
