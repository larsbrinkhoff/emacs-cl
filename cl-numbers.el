;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;;
;;; This file implements operators in chapter 12, Numbers.

(IN-PACKAGE "EMACS-CL")

;;; Various test cases for bignum addition.
; (defun bignum-test ()
;   (loop for (x y z) in
; 	'((67108864		67108864		[bignum -134217728 0])
; 	  (134217727		1			[bignum -134217728 0])
; 	  (-134217728		-1			[bignum 134217727 -1])
; 	  (-134217728		-134217728		[bignum 0 -1])
; 	  ([bignum -1 0]	[bignum -1 0]		[bignum -2 1])
; 	  ([bignum 0 -1]	[bignum 0 -1]		[bignum 0 -2])
; 	  ([bignum 0 2]		[bignum 0 -1]		[bignum 0 1])
; 	  ([bignum 0 -1]	[bignum 0 2]		[bignum 0 1])
; 	  ([bignum 0 1]		[bignum 0 -2]		[bignum 0 -1])
; 	  ([bignum 0 -2]	[bignum 0 1]		[bignum 0 -1])
; 	  ([bignum 2 2]		[bignum -1 -3]		1)
; 	  ([bignum 2 2]		[bignum -3 -3]		-1)
; 	  ([bignum -54323701 6]	[bignum 16292363 17]	[bignum -38031338 23])
; 	  ([bignum 119720045 12408]
; 				[bignum 38283770 30621]
; 						    [bignum -110431641 43029])
; 	  ([bignum -134217728 2] -1			[bignum 134217727 2])
; 	  ([bignum 0 100000000]	[bignum 0 100000000]	[bignum 0 -68435456 0])
; 	  ([bignum -24181363 103035877]
; 				[bignum -24181363 103035877]
; 					       [bignum -48362726 -62363701 0]))
; 	do (unless (equal (cl:+ x y) z)
; 	     (princ (format "%s + %s /= %s\n" x y z)))))

(defun cl:= (number &rest numbers)
  (every (lambda (n) (binary= number n)) numbers))

(defun binary= (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (= num1 num2))
    ((or (COMPLEXP num1) (COMPLEXP num2))
     (and (binary= (REALPART num1) (REALPART num2))
	  (binary= (IMAGPART num1) (IMAGPART num2))))
    ((or (cl::ratiop num1) (cl::ratiop num2))
     (and (binary= (NUMERATOR num1) (NUMERATOR num2))
	  (binary= (DENOMINATOR num1) (DENOMINATOR num2))))
    ((and (cl::bignump num1) (cl::bignump num2))
     (and (= (length num1) (length num2))
	  (every #'eql num1 num2)))
    ((and (NUMBERP num1) (NUMBERP num2))
     nil)
    (t
     (error "type error: = %s %s" num1 num2))))

(defun cl:/= (number &rest numbers)
  (if (null numbers)
      T
      (and (not (some (lambda (num) (binary= number num)) numbers))
	   (apply #'cl:/= (first numbers) (rest numbers)))))

(defun cl:< (number &rest numbers)
  (if (null numbers)
      T
      (and (binary< number (first numbers))
	   (apply #'cl:< (first numbers) (rest numbers)))))

(defun binary< (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (< num1 num2))
    ((or (cl::ratiop num1) (cl::ratiop num2))
     ;; TODO
     (< (/ (float (NUMERATOR num1)) (DENOMINATOR num1))
	(/ (float (NUMERATOR num2)) (DENOMINATOR num2))))
    ((or (cl::bignump num1) (cl::bignump num2))
     (MINUSP (binary- num1 num2)))
    (t
     (error "type error: = %s %s" num1 num2))))

(defun cl:> (number &rest numbers)
  (if (null numbers)
      T
      (and (binary< (first numbers) number)
	   (apply #'cl:> (first numbers) (rest numbers)))))

(defun cl:<= (number &rest numbers)
  (if (null numbers)
      T
      (and (binary<= number (first numbers))
	   (apply #'cl:<= (first numbers) (rest numbers)))))

(defun binary<= (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (<= num1 num2))
    ((or (cl::ratiop num1) (cl::ratiop num2))
     ;; TODO
     (<= (/ (float (NUMERATOR num1)) (DENOMINATOR num1))
	 (/ (float (NUMERATOR num2)) (DENOMINATOR num2))))
    ((or (cl::bignump num1) (cl::bignump num2))
     (let ((diff (binary- num1 num2)))
       (or (MINUSP diff) (ZEROP diff))))
    (t
     (error "type error: = %s %s" num1 num2))))

(defun cl:>= (number &rest numbers)
  (if (null numbers)
      t
      (and (binary<= (first numbers) number)
	   (apply #'cl:>= (first numbers) (rest numbers)))))

(defun MAX (&rest numbers)
  (if (null numbers)
      (error "")
      (reduce (lambda (num1 num2) (if (cl:>= num1 num2) num1 num2)) numbers)))

(defun MIN (&rest numbers)
  (if (null numbers)
      (error "")
      (reduce (lambda (num1 num2) (if (cl:<= num1 num2) num1 num2)) numbers)))

(defun MINUSP (num)
  (cond
    ((or (integerp num) (floatp num))
     (minusp num))
    ((cl::bignump num)
     (minusp (aref num (1- (length num)))))
    ((cl::ratiop num)
     (minusp (NUMERATOR num)))
    (t
     (error "type error"))))

(defun PLUSP (num)
  (cond
    ((or (integerp num) (floatp num))
     (plusp num))
    ((cl::bignump num)
     (>= (aref num (1- (length num))) 0))
    ((cl::ratiop num)
     (plusp (NUMERATOR num)))
    (t
     (error "type error"))))

(defun ZEROP (num)
  (cond
    ((or (integerp num) (floatp num))
     (zerop num))
    ((cl::ratiop num)
     (zerop (NUMERATOR num)))
    ((COMPLEXP num)
     (and (ZEROP (REALPART num)) (ZEROP (IMAGPART num))))
    ((cl::bignump num)
     nil)
    (t
     (error "type error"))))

(defun integer-truncate (x y)
  (cond
    ((and (integerp x) (integerp y))
     (if (and (eql x MOST-NEGATIVE-FIXNUM) (eql y -1))
	 (VALUES (vector 'bignum MOST-NEGATIVE-FIXNUM 0) nil)
	 (VALUES (/ x y) (not (zerop (% x y))))))
    ((and (INTEGERP x) (INTEGERP y))
     (let ((sign 1)
	   (q 0)
	   (r 0)
	   (i (1- (if (integerp x) 28 (* 28 (1- (length x)))))))
       (when (MINUSP x)
	 (setq sign -1))
       (when (MINUSP y)
	 (setq sign (- sign)))
       (while (>= i 0)
;	 (print (format "x=%s y=%s q=%s r=%s" x y q r))
	 (setq r (ASH r 1))
	 (when (LOGBITP i x)
	   (setq r (LOGIOR r 1)))
	 (setq q (ASH q 1))
	 (when (cl:>= r y)
	   (setq q (LOGIOR q 1))
	   (setq r (binary- r y)))
	 (decf i))
       (VALUES (binary* sign q) (not (ZEROP r)))))
    (t
     (error "type error"))))

(defun* FLOOR (number &optional (divisor 1))
  (let (quotient remainder)
    (cond
      ((or (floatp number) (floatp divisor))
       (setq quotient (ceiling (FLOAT number) (FLOAT divisor))))
      ((or (cl::ratiop number) (cl::ratiop divisor))
       (MULTIPLE-VALUE-SETQ (quotient remainder)
	 (integer-truncate
	  (binary* (NUMERATOR number) (DENOMINATOR divisor))
	  (binary* (DENOMINATOR number) (NUMERATOR divisor)))))
      ((and (INTEGERP number) (INTEGERP divisor))
       (MULTIPLE-VALUE-SETQ (quotient remainder)
	 (integer-truncate number divisor)))
      (t
       (error "type error")))
    (when (and remainder (MINUSP quotient))
      (setq quotient (binary- quotient 1)))
    (VALUES quotient (binary- number (binary* quotient divisor)))))

;;; TODO: ffloor

(defun* CEILING (number &optional (divisor 1))
  (let (quotient remainder)
    (cond
      ((or (floatp number) (floatp divisor))
       (setq quotient (ceiling (FLOAT number) (FLOAT divisor))))
      ((or (cl::ratiop number) (cl::ratiop divisor))
       (MULTIPLE-VALUE-SETQ (quotient remainder)
	 (integer-truncate
	  (binary* (NUMERATOR number) (DENOMINATOR divisor))
	  (binary* (DENOMINATOR number) (NUMERATOR divisor)))))
      ((and (INTEGERP number) (INTEGERP divisor))
       (MULTIPLE-VALUE-SETQ (quotient remainder)
	 (integer-truncate number divisor)))
      (t
       (error "type error")))
    (when (and remainder (PLUSP quotient))
      (setq quotient (binary+ quotient 1)))
    (VALUES quotient (binary- number (binary* quotient divisor)))))

;;; TODO: fceiling

(defun* TRUNCATE (number &optional (divisor 1))
  (let (quotient)
    (cond
      ((or (floatp number) (floatp divisor))
       (setq quotient (truncate (FLOAT number) (FLOAT divisor))))
      ((or (cl::ratiop number) (cl::ratiop divisor))
       (setq quotient (integer-truncate
		       (binary* (NUMERATOR number) (DENOMINATOR divisor))
		       (binary* (DENOMINATOR number) (NUMERATOR divisor)))))
      ((and (INTEGERP number) (INTEGERP divisor))
       (setq quotient (integer-truncate number divisor)))
      (t
       (error "type error")))
    (VALUES quotient (binary- number (binary* quotient divisor)))))

;;; TODO: ftruncate, round, fround

;;; TODO: sin, cos, tan

;;; TODO: asin, acos, atan

(DEFCONSTANT PI 3.141592653589793)

;;; TODO: sinh, cosh, tanh, asinh, acosh, atanh

(defun cl:* (&rest numbers)
  (reduce #'binary* numbers :initial-value 1))

(defconst multiplication-limit (ceiling (sqrt most-positive-fixnum)))

(defun binary* (x y)
  (cond
    ((and (integerp x) (integerp y))
     (if (and (< x multiplication-limit)
	      (> x (- multiplication-limit))
	      (< y multiplication-limit)
	      (> y (- multiplication-limit)))
	 (* x y)
	 (bignum* (vector 'bignum x (if (minusp x) -1 0))
		  (vector 'bignum y (if (minusp y) -1 0)))))
    ((or (COMPLEXP x) (COMPLEXP y))
     (COMPLEX (binary- (binary* (REALPART x) (REALPART y))
		       (binary* (IMAGPART x) (IMAGPART y)))
	      (binary+ (binary* (REALPART x) (IMAGPART y))
		       (binary* (IMAGPART x) (REALPART y)))))
    ((floatp x)
     (* x (FLOAT y)))
    ((floatp y)
     (* (FLOAT x) y))
    ((or (cl::ratiop x) (cl::ratiop y))
     (if (ZEROP y)
	 (error)
	 (cl::ratio (binary* (NUMERATOR x) (DENOMINATOR y))
		    (binary* (DENOMINATOR x) (NUMERATOR y)))))
    ((or (INTEGERP x) (INTEGERP y))
     (when (integerp x)
       (setq x (vector 'bignum x (if (minusp x) -1 0))))
     (when (integerp y)
       (setq y (vector 'bignum y (if (minusp y) -1 0))))
     (bignum* x y))
    (t
     (error "TODO"))))

(defun bignum* (x y)
  (cond
    ((equal x [bignum 1 0])
     (canonical-bignum y))
    ((equal x [bignum -1 -1])
     (cl:- (canonical-bignum y)))
    ((equal y [bignum 10 0])
     (setq x (canonical-bignum x))
;    (print (format "(bignum* %s %s)" x y))
     (let* ((2x (binary+ x x))
	    (4x (binary+ 2x 2x))
	    (5x (binary+ 4x x)))
;      (print (format "%s %s %s" 2x 4x 5x))
       (binary+ 5x 5x)))
    (t
     (setq x (canonical-bignum x))
     (setq y (canonical-bignum y))
     (let ((sign 1)
	   (z 0))
       (when (MINUSP x)
	 (setq x (cl:- x) sign -1))
       (when (MINUSP y)
	 (setq y (cl:- y) sign (- sign)))
       (while (PLUSP x)
	 (when (LOGBITP 0 x)
	   (setq z (binary+ z y)))
	 (setq y (ASH y 1))
	 (setq x (ASH x -1)))
       (binary* sign z)))))

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
    ((or (COMPLEXP x) (COMPLEXP y))
     (COMPLEX (binary+ (REALPART x) (REALPART y))
	      (binary+ (IMAGPART x) (IMAGPART y))))
    ((floatp x)
     (+ x (FLOAT y)))
    ((floatp y)
     (+ (FLOAT x) y))
    ((or (cl::ratiop x) (cl::ratiop y))
     (cl::ratio (binary+ (binary* (NUMERATOR x) (DENOMINATOR y))
			 (binary* (DENOMINATOR x) (NUMERATOR y)))
		(binary* (DENOMINATOR x) (DENOMINATOR y))))
    ((or (cl::bignump x) (cl::bignump y))
;    (print (format "%s %s" x y))
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
      ;; negative + positive -> positive: carry
      ((and (minusp x0) (>= y 0) (>= sum 0))
       (bignum+bignum new [bignum 0 1]))
      ;; positive + negative -> negative: borrow
      ((and (>= x0 0) (minusp y) (minusp sum))
       (bignum+bignum new [bignum 0 -1]))
      ;; positive + positive -> negative: no overflow
      ;; negative + negative -> positive: no overflow
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
       (if (and (null (rest x)) (null (rest y))
		(>= x0 0) (>= y0 0) (minusp sum))
	   ;; Last number wrapped from positive to negative.
	   ;; Need a final zero.
	   (cons sum '(0))
	   (cons sum
		 (bignum+
		  (rest x)
		  (rest y)
		  (if (or (and (minusp x0) (>= y0 0) (>= sum 0) (rest x))
			  (and (>= x0 0) (minusp y0) (>= sum 0) (rest y))
			  (and (minusp x0) (minusp y0) (rest x) (rest y)))
		      1 0))))))))

(defun cl:- (number &rest numbers)
  (if (null numbers)
      (cond
	((or (integerp number) (floatp number))
	 (if (eql number MOST-NEGATIVE-FIXNUM)
	     (vector 'bignum number 0)
	     (- number)))
	((cl::ratiop number)
	 (vector 'ratio (cl:- (NUMERATOR number)) (DENOMINATOR number)))
	((COMPLEXP number)
	 (vector 'complex (cl:- (REALPART number)) (cl:- (IMAGPART number))))
	((cl::bignump number)
	 (bignum+fixnum (LOGNOT number) 1))
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
	((cl::bignump number)
	 (vector 'ratio 1 number))
	((cl::ratiop number)
	 (cl::ratio (DENOMINATOR number) (NUMERATOR number)))
	((COMPLEXP number)
	 (let* ((r (REALPART number))
		(i (IMAGPART number))
		(x (binary- (binary* r r) (binary* i i))))
	   (COMPLEX (binary/ r x) (binary+ (binary/ i x)))))
	(t
	 (error)))
      (dolist (num numbers number)
	(setq number (binary/ number num)))))

(defun binary/ (x y)
  (cond
    ((and (INTEGERP x) (INTEGERP y))
     (cl::ratio x y))
    ((or (COMPLEXP x) (COMPLEXP y))
     (let* ((rx (REALPART x))
	    (ry (REALPART y))
	    (ix (IMAGPART x))
	    (iy (IMAGPART y))
	    (div (binary+ (binary* ry ry) (binary* iy iy))))
       (COMPLEX (binary/ (binary+ (binary* rx ry) (binary* ix iy)) div)
		(binary/ (binary- (binary* ix ry) (binary* rx iy)) div))))
    ((floatp x)
     (/ x (FLOAT y)))
    ((floatp y)
     (/ (FLOAT x) y))
    ((or (RATIONALP x) (RATIONALP y))
     (cl::ratio (binary* (NUMERATOR x) (DENOMINATOR y))
		(binary* (DENOMINATOR x) (NUMERATOR y))))
    (t
     (error "type error"))))
  
(defun cl:1+ (number)
  (binary+ number 1))

(defun cl:1- (number)
  (binary- number 1))

(defun ABS (number)
  (cond
    ((integerp number)
     (if (eql number MOST-NEGATIVE-FIXNUM)
	 (vector 'bignum number 0)
	 (abs number)))
    ((floatp number)
     (abs number))
    ((cl::ratiop number)
     (vector 'ratio (ABS (NUMERATOR number)) (DENOMINATOR number)))
    ((COMPLEXP number)
     (sqrt (+ (expt (REALPART number) 2) (expt (IMAGPART number) 2))))
    ((cl::bignump number)
     (if (MINUSP number)
	 (cl:- number)
	 number))
    (t
     (error "type error"))))

;;; TODO: EVENP, ODDP

;;; TODO: EXP, EXPT

(defun GCD (&rest numbers)
  (reduce #'binary-gcd numbers :initial-value 0))

(defun binary-gcd (x y)
  (if (and (integerp x) (integerp y))
      (progn
	(when (> y x)
	  (psetq x y y x))
	(while (not (zerop y))
	  (psetq y (% x y) x y))
	(abs x))
      (progn
	(when (cl:> y x)
	  (psetq x y y x))
	(while (not (ZEROP y))
	  (psetq y (REM x y) x y))
	(ABS x))))

(cl:defmacro INCF (place &optional delta)
  (unless delta
    (setq delta 1))
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    `(LET* ,(MAPCAR #'list temps values)
       (LET ((,(first variables)
	      ,(if (eq delta 1)
		   `(,(INTERN "1+" *cl-package*) ,getter)
		   `(,(INTERN "+" *cl-package*) ,getter ,delta))))
	 ,setter))))

(cl:defmacro DECF (place &optional delta)
  (unless delta
    (setq delta 1))
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    `(LET* ,(MAPCAR #'list temps values)
       (LET ((,(first variables)
	      ,(if (eq delta 1)
		   `(,(INTERN "1-" *cl-package*) ,getter)
		   `(,(INTERN "-" *cl-package*) ,getter ,delta))))
	 ,setter))))

;;; TODO: LCM

;;; TODO: LOG

(defun MOD (number divisor)
  (NTH-VALUE 1 (FLOOR number divisor)))

(defun REM (number divisor)
  (NTH-VALUE 1 (TRUNCATE number divisor)))

;;; TODO: SIGNUM

;;; TODO: SQRT, ISQRT

;;; TODO: MAKE-RANDOM-STATE

(defun RANDOM (limit &optional random-state)
  (cond
    ((integerp limit)
     (random limit))
    ((floatp limit)
     (/ (* limit (random MOST-POSITIVE-FIXNUM)) MOST-POSITIVE-FIXNUM))
    ((cl::bignump limit)
     ;; TODO
     0)))

;;; TODO: RANDOM-STATE-P

;;; TODO: *RANDOM-STATE*

(defun NUMBERP (object)
  (or (numberp object)
      (and (vectorp object)
	   (let ((type (aref object 0)))
	     (or (eq type 'bignum)
		 (eq type 'ratio)
		 (eq type 'complex))))))

;;; TODO: CIS

(defun COMPLEX (realpart &optional imagpart)
  (CHECK-TYPE realpart 'REAL)
  (if (or (null imagpart) (ZEROP imagpart))
      realpart
      (progn
	(CHECK-TYPE realpart 'REAL)
	(when (floatp realpart)
	  (setq imagpart (float realpart)))
	(when (floatp imagpart)
	  (setq realpart (float realpart)))
	(vector 'complex realpart imagpart))))

(defun COMPLEXP (object)
  (vector-and-typep object 'complex))

(defun CONJUGAGE (num)
  (COMPLEX (REALPART num) (- (IMAGPART num))))

(defun PHASE (num)
  (ATAN (IMAGPART num) (REALPART num)))

(defun REALPART (num)
  (if (COMPLEXP num)
      (aref num 1)
      num))

(defun IMAGPART (num)
  (if (COMPLEXP num)
      (aref num 2)
      0))

(defun UPGRADED-COMPLEX-PART-TYPE (typespec &optional env)
  'REAL)

(defun REALP (num)
  (or (RATIONALP num) (FLOATP num)))

(defun cl::ratio (num den)
  (unless (and (INTEGERP num) (INTEGERP den))
    (error "type error"))
  (if (and (eq x MOST-NEGATIVE-FIXNUM) (eq y -1))
      (vector 'bignum MOST-NEGATIVE-FIXNUM 0)
      (let* ((gcd (GCD num den))
	     (num (binary/ num gcd))
	     (den (binary/ den gcd)))
	(cond
	  ((eq den 1)
	   num)
	  ((MINUSP den)
	   (vector 'ratio (cl:- num) (cl:- den)))
	  (t
	   (vector 'ratio num den))))))

(defun cl::ratiop (num)
  (vector-and-typep num 'ratio))

(defun NUMERATOR (num)
  (if (cl::ratiop num)
      (aref num 1)
      num))

(defun DENOMINATOR (num)
  (if (cl::ratiop num)
      (aref num 2)
      1))

;;; TODO: rational

;;; TODO: rationalize

(defun RATIONALP (num)
  (or (INTEGERP num) (cl::ratiop num)))

(defun ASH (num shift)
  (cond
    ((ZEROP shift)
     num)
    ((MINUSP shift)
     (cond
       ((integerp num)
	(ash num shift))
       ((cl::bignump num)
	(let ((new (copy-sequence num)))
	  (while (MINUSP shift)
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

(defun INTEGER-LENGTH (num)
  (when (MINUSP num)
    (setq num (cl:- num)))
  0)

(defun cl::bignump (num)
  (vector-and-typep num 'bignum))

(defun INTEGERP (num)
  (or (integerp num) (cl::bignump num)))

(defun* PARSE-INTEGER (string &key (start 0) (end (length string))
			      (radix 10) junk-allowed)
  (let ((sign 1)
	(integer 0)
	(i start)
	char digit)
    (while (whitespacep (CHAR string i))
      (incf i)
      (when (= i end)
	(if junk-allowed
	    (return-from PARSE-INTEGER (VALUES nil i))
	    (error))))
    (setq char (CHAR string i))
    (when (find (CHAR-CODE char) "+-")
      (when (CHAR= char (CODE-CHAR 45))
	(setq sign -1))
      (incf i)
      (when (= i end)
	(if junk-allowed
	    (return-from PARSE-INTEGER (VALUES nil i))
	    (error)))
      (setq char (CHAR string i)))
    (while (setq digit (DIGIT-CHAR-P char radix))
;     (print (format "before: %s %s" (cl:* integer 10) digit))
      (setq integer (cl:+ (cl:* integer radix) digit))
;     (PRINT integer)
;     (print (format "after: %s" integer))
      (incf i)
      (when (= i end)
;	(print (format "int: %s" integer))
	(return-from PARSE-INTEGER (VALUES (cl:* sign integer) i)))
      (setq char (CHAR string i)))
    (cond
      (junk-allowed
       (VALUES (cl:* sign integer) i))
      (t
       (do ((j i (1+ j)))
	   ((= j end)
	    (VALUES (cl:* sign integer) i))
	 (unless (whitespacep (CHAR string j))
	   (error)))))))

(defun LOGNOT (num)
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

(defun LOGAND (&rest numbers)
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

(defun LOGIOR (&rest numbers)
  (reduce #'binary-logior numbers :initial-value 0))

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

(defun LOGNAND (x y)
  (LOGNOT (LOGAND x y)))

(defun LOGANDC1 (x y)
  (LOGAND (LOGNOT x) y))

(defun LOGANDC2 (x y)
  (LOGAND x (LOGNOT y)))

(defun LOGNOR (x y)
  (LOGNOT (LOGIOR x y)))

(defun LOGORC1 (x y)
  (LOGIOR (LOGNOT x) y))

(defun LOGORC2 (x y)
  (LOGIOR x (LOGNOT y)))

(defun LOGEQV (&rest numbers)
  (LOGNOT (apply #'LOGXOR numbers)))

(defun LOGXOR (&rest numbers)
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

(defun LOGBITP (index integer)
  (unless (integerp index)
    (error "TODO"))
  (when (minusp index)
    (error "type error"))
  (cond
    ((integerp integer)
     (if (>= index 28)
	 (minusp integer)
	 (not (zerop (logand integer (ash 1 index))))))
    ((cl::bignump integer)
     (if (>= index (* 28 (1- (length integer))))
	 (MINUSP integer)
	 (let ((i (1+ (/ index 28)))
	       (j (% index 28)))
	   (not (zerop (logand (aref integer i) (ash 1 j)))))))
    (t
     (error "type error"))))

(defun LOGCOUNT (num)
  (when (MINUSP num)
    (setq num (cl:- num)))
  (let ((len 0))
    (cond
      ((integerp num)
       (dotimes (i 28)
	 (when (LOGBITP i num)
	   (incf len))))
      (t
       (dotimes (i (1- (length num)))
	 (dotimes (j 28)
	   (when (LOGBITP i num)
	     (incf len))))))
    len))

(defun LOGTEST (num1 num2)
  (NOT (ZEROP (LOGAND num1 num2))))

(defun BYTE (size pos)
  (list size pos))

(defun BYTE-SIZE (bytespec)
  (first bytespec))

(defun BYTE-POSITION (bytespec)
  (second bytespec))

(defun DEPOSIT-FIELD (newbyte bytespec integer)
  (LOGIOR (LOGAND integer (LOGNOT (DPB -1 bytespec 0)))
	  (MASK-FIELD bytespec newbyte)))

(defun DPB (newbyte bytespec integer)
  (let ((mask (cl:1- (ASH 1 (BYTE-SIZE bytespec)))))
    (LOGIOR (LOGANDC2 integer (ASH mask (BYTE-POSITION bytespec)))
	    (ASH (LOGAND newbyte mask) (BYTE-POSITION bytespec)))))

(defun LDB (bytespec integer)
  (LOGAND (ASH integer (cl:- (BYTE-POSITION bytespec)))
	  (cl:1- (ASH 1 (BYTE-SIZE bytespec)))))

(DEFINE-SETF-EXPANDER LDB (bytespec integer &environment env)
  (multiple-value-bind (temps values variables setter getter)
      (get-setf-method integer env)
    (let ((byte (gensym))
	  (value (gensym)))
      (values (cons byte temps)
	      (cons bytespec values)
	      (list value)
	      `(let ((,(first variables) (DPB ,value ,byte ,getter)))
		,setter
		,value)
	      `(LDB ,byte ,getter)))))

(defun LDB-TEST (bytespec integer)
  (NOT (ZEROP (LDB bytespec integer))))

(defun MASK-FIELD (bytespec integer)
  (LOGAND integer (DPB -1 bytespec 0)))

(DEFCONSTANT MOST-POSITIVE-FIXNUM 134217727)

(DEFCONSTANT MOST-NEGATIVE-FIXNUM -134217728)

(define-setf-method MASK-FIELD (bytespec integer &environment env)
  (multiple-value-bind (temps values variables setter getter)
      (get-setf-method integer env)
    (let ((byte (gensym))
	  (value (gensym)))
    (values (cons byte temps)
	    (cons bytespec values)
	    (list value)
	    `(let ((,(first variables) (DEPOSIT-FIELD ,value ,byte ,getter)))
	      ,setter
	      ,value)
	    `(MASK-FIELD ,byte ,getter)))))

;;; TODO: decode-float, scale-float, float-radix, float-sign, float-digits,
;;; float-precision, integer-decode-float

(defun FLOAT (num &optional prototype)
  (cond
    ((integerp num)
     (float num))
    ((floatp num)
     num)
    ((cl::ratiop num)
     (/ (FLOAT (NUMERATOR num)) (FLOAT (DENOMINATOR num))))
    ((cl::bignump num)
     1.0)
    (t
     (error "type error"))))

(fset 'FLOATP (symbol-function 'floatp))

;;; TODO: ARITHMETIC-ERROR-OPERANDS, ARITHMETIC-ERROR-OPERATION
