;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 12, Numbers.

(IN-PACKAGE "EMACS-CL")

;;; Various test cases for bignum addition.
; (defun bignum-test ()
;   (loop for (x y z) in
; 	'((67108864		67108864		[BIGNUM -134217728 0])
; 	  (134217727		1			[BIGNUM -134217728 0])
; 	  (-134217728		-1			[BIGNUM 134217727 -1])
; 	  (-134217728		-134217728		[BIGNUM 0 -1])
; 	  ([BIGNUM -1 0]	[BIGNUM -1 0]		[BIGNUM -2 1])
; 	  ([BIGNUM 0 -1]	[BIGNUM 0 -1]		[BIGNUM 0 -2])
; 	  ([BIGNUM 0 2]		[BIGNUM 0 -1]		[BIGNUM 0 1])
; 	  ([BIGNUM 0 -1]	[BIGNUM 0 2]		[BIGNUM 0 1])
; 	  ([BIGNUM 0 1]		[BIGNUM 0 -2]		[BIGNUM 0 -1])
; 	  ([BIGNUM 0 -2]	[BIGNUM 0 1]		[BIGNUM 0 -1])
; 	  ([BIGNUM 2 2]		[BIGNUM -1 -3]		1)
; 	  ([BIGNUM 2 2]		[BIGNUM -3 -3]		-1)
; 	  ([BIGNUM -54323701 6]	[BIGNUM 16292363 17]	[BIGNUM -38031338 23])
; 	  ([BIGNUM 119720045 12408]
; 				[BIGNUM 38283770 30621]
; 						    [BIGNUM -110431641 43029])
; 	  ([BIGNUM -134217728 2] -1			[BIGNUM 134217727 2])
; 	  ([BIGNUM 0 100000000]	[BIGNUM 0 100000000]	[BIGNUM 0 -68435456 0])
; 	  ([BIGNUM -24181363 103035877]
; 				[BIGNUM -24181363 103035877]
; 					       [BIGNUM -48362726 -62363701 0]))
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
    ((or (ratiop num1) (ratiop num2))
     (and (binary= (NUMERATOR num1) (NUMERATOR num2))
	  (binary= (DENOMINATOR num1) (DENOMINATOR num2))))
    ((and (bignump num1) (bignump num2))
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
    ((or (ratiop num1) (ratiop num2))
     ;; TODO
     (< (/ (FLOAT (NUMERATOR num1)) (FLOAT (DENOMINATOR num1)))
	(/ (FLOAT (NUMERATOR num2)) (FLOAT (DENOMINATOR num2)))))
    ((or (bignump num1) (bignump num2))
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
    ((or (ratiop num1) (ratiop num2))
     ;; TODO
     (<= (/ (FLOAT (NUMERATOR num1)) (FLOAT (DENOMINATOR num1)))
	 (/ (FLOAT (NUMERATOR num2)) (FLOAT (DENOMINATOR num2)))))
    ((or (bignump num1) (bignump num2))
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
    ((bignump num)
     (minusp (aref num (1- (length num)))))
    ((ratiop num)
     (minusp (NUMERATOR num)))
    (t
     (error "type error"))))

(defun PLUSP (num)
  (cond
    ((or (integerp num) (floatp num))
     (plusp num))
    ((bignump num)
     (>= (aref num (1- (length num))) 0))
    ((ratiop num)
     (plusp (NUMERATOR num)))
    (t
     (error "type error"))))

(defun ZEROP (num)
  (cond
    ((or (integerp num) (floatp num))
     (zerop num))
    ((ratiop num)
     (zerop (NUMERATOR num)))
    ((COMPLEXP num)
     (and (ZEROP (REALPART num)) (ZEROP (IMAGPART num))))
    ((bignump num)
     nil)
    (t
     (error "type error"))))

(defun integer-truncate (x y)
  (cond
    ((and (integerp x) (integerp y))
     (if (and (eql x MOST-NEGATIVE-FIXNUM) (eql y -1))
	 (VALUES (vector 'BIGNUM MOST-NEGATIVE-FIXNUM 0) nil)
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

(cl:defun FLOOR (number &optional (divisor 1))
  (let (quotient remainder)
    (cond
      ((or (floatp number) (floatp divisor))
       ;; TODO: floor can only output an Emacs Lisp integer.
       (setq quotient (floor (FLOAT number) (FLOAT divisor))))
      ((or (ratiop number) (ratiop divisor))
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

(cl:defun FFLOOR (number &optional (divisor 1))
  (MULTIPLE-VALUE-BIND (quotient remainder) (FLOOR number divisor)
    (VALUES (FLOAT quotient remainder))))

(cl:defun CEILING (number &optional (divisor 1))
  (let (quotient remainder)
    (cond
      ((or (floatp number) (floatp divisor))
       ;; TODO: ceiling can only output an Emacs Lisp integer.
       (setq quotient (ceiling (FLOAT number) (FLOAT divisor))))
      ((or (ratiop number) (ratiop divisor))
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

(cl:defun FCEILING (number &optional (divisor 1))
  (MULTIPLE-VALUE-BIND (quotient remainder) (CEILING number divisor)
    (VALUES (FLOAT quotient remainder))))

(cl:defun TRUNCATE (number &optional (divisor 1))
  (let (quotient)
    (cond
      ((or (floatp number) (floatp divisor))
       ;; TODO: truncate can only output an Emacs Lisp integer.
       (setq quotient (truncate (FLOAT number) (FLOAT divisor))))
      ((or (ratiop number) (ratiop divisor))
       (setq quotient (integer-truncate
		       (binary* (NUMERATOR number) (DENOMINATOR divisor))
		       (binary* (DENOMINATOR number) (NUMERATOR divisor)))))
      ((and (INTEGERP number) (INTEGERP divisor))
       (setq quotient (integer-truncate number divisor)))
      (t
       (error "type error")))
    (VALUES quotient (binary- number (binary* quotient divisor)))))

(cl:defun FTRUNCATE (number &optional (divisor 1))
  (MULTIPLE-VALUE-BIND (quotient remainder) (TRUNCATE number divisor)
    (VALUES (FLOAT quotient remainder))))

;;; TODO: round, fround

(defun SIN (x)
  (cond
    ((REALP x)		(sin (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(error "type error"))))

(defun COS (x)
  (cond
    ((REALP x)		(cos (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(error "type error"))))

(defun TAN (x)
  (cond
    ((REALP x)		(tan (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(error "type error"))))

(defun ASIN (x)
  (cond
    ((REALP x)		(asin (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(error "type error"))))

(defun ACOS (x)
  (cond
    ((REALP x)		(acos (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(error "type error"))))

(defun ATAN (x &optional y)
  (when y (error "TODO"))
  (cond
    ((REALP x)		(atan (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(error "type error"))))

(DEFCONSTANT PI 3.141592653589793)

;;; TODO: sinh, cosh, tanh, asinh, acosh, atanh

(defun cl:* (&rest numbers)
  (reduce #'binary* numbers :initial-value 1))

(defconst multiplication-limit (floor (sqrt most-positive-fixnum)))

(defun binary* (x y)
  (cond
    ((and (integerp x) (integerp y))
     (if (and (< x multiplication-limit)
	      (> x (- multiplication-limit))
	      (< y multiplication-limit)
	      (> y (- multiplication-limit)))
	 (* x y)
	 (bignum* (vector 'BIGNUM x (if (minusp x) -1 0))
		  (vector 'BIGNUM y (if (minusp y) -1 0)))))
    ((or (COMPLEXP x) (COMPLEXP y))
     (COMPLEX (binary- (binary* (REALPART x) (REALPART y))
		       (binary* (IMAGPART x) (IMAGPART y)))
	      (binary+ (binary* (REALPART x) (IMAGPART y))
		       (binary* (IMAGPART x) (REALPART y)))))
    ((floatp x)
     (* x (FLOAT y)))
    ((floatp y)
     (* (FLOAT x) y))
    ((or (ratiop x) (ratiop y))
     (if (ZEROP y)
	 (error)
	 (make-ratio (binary* (NUMERATOR x) (NUMERATOR y))
		     (binary* (DENOMINATOR x) (DENOMINATOR y)))))
    ((or (INTEGERP x) (INTEGERP y))
     (when (integerp x)
       (setq x (vector 'BIGNUM x (if (minusp x) -1 0))))
     (when (integerp y)
       (setq y (vector 'BIGNUM y (if (minusp y) -1 0))))
     (bignum* x y))
    (t
     (error "type error"))))

(defun bignum* (x y)
  (cond
    ((equal x [BIGNUM 1 0])
     (canonical-bignum y))
    ((equal x [BIGNUM -1 -1])
     (cl:- (canonical-bignum y)))
    ((equal y [BIGNUM 10 0])
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
	  (vector 'BIGNUM sum 0))
	 ((and (minusp x) (minusp y) (>= sum 0))
	  (vector 'BIGNUM sum -1))
	 (t
	  sum))))
    ((or (COMPLEXP x) (COMPLEXP y))
     (COMPLEX (binary+ (REALPART x) (REALPART y))
	      (binary+ (IMAGPART x) (IMAGPART y))))
    ((floatp x)
     (+ x (FLOAT y)))
    ((floatp y)
     (+ (FLOAT x) y))
    ((or (ratiop x) (ratiop y))
     (make-ratio (binary+ (binary* (NUMERATOR x) (DENOMINATOR y))
			  (binary* (DENOMINATOR x) (NUMERATOR y)))
		 (binary* (DENOMINATOR x) (DENOMINATOR y))))
    ((or (bignump x) (bignump y))
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
       (bignum+bignum new [BIGNUM 0 1]))
      ;; positive + negative -> negative: borrow
      ((and (>= x0 0) (minusp y) (minusp sum))
       (bignum+bignum new [BIGNUM 0 -1]))
      ;; positive + positive -> negative: no overflow
      ;; negative + negative -> positive: no overflow
      (t
       (canonical-bignum new)))))

(defun bignum+bignum (x y)
  (canonical-bignum (bignum+ (bignum-list x) (bignum-list y))))

(cl:defun bignum-list (num &optional (index 1))
  (if (= index (length num))
      nil
      (cons (aref num index) (bignum-list num (1+ index)))))

(defun canonical-bignum (object)
  (cond
    ((bignump object)
     (canonical-bignum (bignum-list object)))
    ((listp object)
     (setq object (truncate-sign-extension object))
     (if (eql (length object) 1)
	 (first object)
	 (let ((bignum (make-vector (1+ (length object)) 'BIGNUM))
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

(cl:defun bignum+ (x y &optional (carry 0))
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
	     (vector 'BIGNUM number 0)
	     (- number)))
	((ratiop number)
	 (vector 'RATIO (cl:- (NUMERATOR number)) (DENOMINATOR number)))
	((COMPLEXP number)
	 (vector 'COMPLEX (cl:- (REALPART number)) (cl:- (IMAGPART number))))
	((bignump number)
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
	 (vector 'RATIO 1 number))
	((floatp number)
	 (/ 1.0 number))
	((bignump number)
	 (vector 'RATIO 1 number))
	((ratiop number)
	 (make-ratio (DENOMINATOR number) (NUMERATOR number)))
	((COMPLEXP number)
	 (let* ((r (REALPART number))
		(i (IMAGPART number))
		(x (binary- (binary* r r) (binary* i i))))
	   (COMPLEX (binary/ r x) (cl:- (binary/ i x)))))
	(t
	 (error)))
      (dolist (num numbers number)
	(setq number (binary/ number num)))))

(defun binary/ (x y)
  (cond
    ((and (INTEGERP x) (INTEGERP y))
     (make-ratio x y))
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
     (make-ratio (binary* (NUMERATOR x) (DENOMINATOR y))
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
	 (vector 'BIGNUM number 0)
	 (abs number)))
    ((floatp number)
     (abs number))
    ((ratiop number)
     (vector 'RATIO (ABS (NUMERATOR number)) (DENOMINATOR number)))
    ((COMPLEXP number)
     (let ((r (FLOAT (REALPART number)))
	   (i (FLOAT (IMAGPART number))))
       (sqrt (+ (* r r) (* i i)))))
    ((bignump number)
     (if (MINUSP number)
	 (cl:- number)
	 number))
    (t
     (error "type error"))))

(defun EVENP (num)
  (if (INTEGERP num)
      (LOGBITP 0 num)
      (error "type error")))

(defun ODDP (num)
  (if (INTEGERP num)
      (LOGBITP 0 num)
      (error "type error")))

(defun EXP (num)
  (cond
    ((REALP num)	(exp (FLOAT num)))
    ((COMPLEXP num)	(error "TODO"))
    (t			(error "type error"))))

(defun EXPT (base power)
  (cond
    ((and (RATIONALP base) (INTEGERP power))
     (exact-expt base power))
    ((and (REALP base) (REALP power))
     (expt (FLOAT base) (FLOAT power)))
    ((and (NUMBERP base) (NUMBERP power))
     (error "TODO"))
    (t
     (error "type error"))))

(defun exact-expt (base power)
  (cond
    ((ZEROP power)
     1)
    ((MINUSP power)
     (make-ratio 1 (exact-expt base (cl:- power))))
    (t
     (let ((result 1))
       (while (PLUSP power)
	 (when (LOGBITP 0 power)
	   (setq result (binary* result base)))
	 (setq base (binary* base base))
	 (setq power (ASH power -1)))
       result))))

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

;;; TODO: &environment
(cl:defmacro INCF (place &optional delta)
  (unless delta
    (setq delta 1))
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place nil)
    `(LET* (,@(MAPCAR #'list temps values)
	    (,(first variables)
	     ,(if (eq delta 1)
		  `(,(INTERN "1+" *cl-package*) ,getter)
		  `(,(INTERN "+" *cl-package*) ,getter ,delta))))
       ,setter)))

;;; TODO: &environment
(cl:defmacro DECF (place &optional delta)
  (unless delta
    (setq delta 1))
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place nil)
    `(LET* (,@(MAPCAR #'list temps values)
	    (,(first variables)
	     ,(if (eq delta 1)
		  `(,(INTERN "1-" *cl-package*) ,getter)
		  `(,(INTERN "-" *cl-package*) ,getter ,delta))))
       ,setter)))

(defun LCM (&rest numbers)
  (if (null numbers)
      1
      (reduce #'binary-lcm numbers)))

(defun binary-lcm (x y)
  (if (or (ZEROP x) (ZEROP y))
      0
      (integer-truncate (ABS (binary* x y)) (GCD x y))))

(cl:defun LOG (number &optional (base (exp 1)))
  (cond
    ((and (REALP number) (REALP base))
     (log (FLOAT number) (FLOAT base)))
    ((and (NUMBERP number) (NUMBERP base))
     (error "TODO"))
    (t
     (error "type error"))))
  
(defun MOD (number divisor)
  (NTH-VALUE 1 (FLOOR number divisor)))

(defun REM (number divisor)
  (NTH-VALUE 1 (TRUNCATE number divisor)))

(defun SIGNUM (number)
  (cond
    ((RATIONALP number) (cond ((PLUSP number)	1)
			      ((ZEROP number)	0)
			      ((MINUSP number)	-1)))
    ((floatp number)	(cond ((plusp number)	1.0)
			      ((zerop number)	0.0)
			      ((minusp number)	-1.0)))
    ((COMPLEXP number)	(if (ZEROP number)	number
						(binary/ number (ABS number))))
    (t			(error "type error"))))

(defun SQRT (number)
  (cond
    ((REALP number)	(sqrt (FLOAT number)))
    ((COMPLEXP number)	(error "TODO"))
    (t			(error "type error"))))

(defun ISQRT (number)
  (VALUES (FLOOR (sqrt (FLOAT number)))))

;;; TODO: MAKE-RANDOM-STATE

(defun RANDOM (limit &optional random-state)
  (cond
    ((integerp limit)
     (random limit))
    ((floatp limit)
     (/ (* limit (random MOST-POSITIVE-FIXNUM)) MOST-POSITIVE-FIXNUM))
    ((bignump limit)
     ;; TODO
     0)))

;;; TODO: RANDOM-STATE-P

;;; TODO: *RANDOM-STATE*

(defun NUMBERP (object)
  (or (numberp object)
      (and (vectorp object)
	   (let ((type (aref object 0)))
	     (or (eq type 'BIGNUM)
		 (eq type 'RATIO)
		 (eq type 'COMPLEX))))))

;;; TODO: CIS

(cl:defun COMPLEX (realpart &optional (imagpart 0))
  (cond
    ((floatp realpart)
     (setq imagpart (float imagpart)))
    ((floatp imagpart)
     (setq realpart (float realpart))))
  (if (eq imagpart 0)
      realpart
      (vector 'COMPLEX realpart imagpart)))

(defun COMPLEXP (object)
  (vector-and-typep object 'COMPLEX))

(defun CONJUGAGE (num)
  (vector 'COMPLEX (REALPART num) (cl:- (IMAGPART num))))

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

(defun make-ratio (num den)
  (unless (and (INTEGERP num) (INTEGERP den))
    (error "type error"))
  (if (and (eq num MOST-NEGATIVE-FIXNUM) (eq den -1))
      (vector 'BIGNUM MOST-NEGATIVE-FIXNUM 0)
      (let* ((gcd (GCD num den))
	     (num (integer-truncate num gcd))
	     (den (integer-truncate den gcd)))
	(VALUES
	 (cond
	   ((eq den 1)
	    num)
	   ((MINUSP den)
	    (vector 'RATIO (cl:- num) (cl:- den)))
	   (t
	    (vector 'RATIO num den)))))))

(defun ratiop (num)
  (vector-and-typep num 'RATIO))

(defun NUMERATOR (num)
  (if (ratiop num)
      (aref num 1)
      num))

(defun DENOMINATOR (num)
  (if (ratiop num)
      (aref num 2)
      1))

;;; TODO: rational

;;; TODO: rationalize

(defun RATIONALP (num)
  (or (INTEGERP num) (ratiop num)))

(defun ASH (num shift)
  (cond
    ((ZEROP shift)
     num)
    ((MINUSP shift)
     (cond
       ((integerp num)
	(ash num shift))
       ((bignump num)
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
  (cond
    ((eq num 0)		0)
    ((integerp num)	(1+ (logb num)))
    ((bignump num)	(let* ((len (length num))
			       (last (aref num (1- len))))
			  (+ (* 28 (- len 2))
			     (if (zerop last)
				 0
				 (1+ (logb last))))))
    (t			(error "type error"))))

(defun bignump (num)
  (vector-and-typep num 'BIGNUM))

(defun INTEGERP (num)
  (or (integerp num) (bignump num)))

(cl:defun PARSE-INTEGER (string &key (start 0) (end (LENGTH string))
			             (radix 10) junk-allowed)
  (let ((sign 1)
	(integer 0)
	(i start)
	char digit)
    (catch 'PARSE-INTEGER
      (while (whitespacep (CHAR string i))
	(incf i)
	(when (= i end)
	  (if junk-allowed
	      (throw 'PARSE-INTEGER (VALUES nil i))
	      (error))))
      (setq char (CHAR string i))
      (when (find (CHAR-CODE char) "+-")
	(when (CHAR= char (CODE-CHAR 45))
	  (setq sign -1))
	(incf i)
	(when (= i end)
	  (if junk-allowed
	      (throw 'PARSE-INTEGER (VALUES nil i))
	      (error)))
	(setq char (CHAR string i)))
      (while (setq digit (DIGIT-CHAR-P char radix))
;       (print (format "before: %s %s" (cl:* integer 10) digit))
	(setq integer (cl:+ (cl:* integer radix) digit))
;     	(PRINT integer)
;     	(print (format "after: %s" integer))
	(incf i)
	(when (= i end)
;	  (print (format "int: %s" integer))
	  (throw 'PARSE-INTEGER (VALUES (cl:* sign integer) i)))
	(setq char (CHAR string i)))
      (cond
	(junk-allowed
	 (VALUES (cl:* sign integer) i))
	(t
	 (do ((j i (1+ j)))
	     ((= j end)
	      (VALUES (cl:* sign integer) i))
	   (unless (whitespacep (CHAR string j))
	     (error))))))))

(DEFCONSTANT BOOLE-1		 1)
(DEFCONSTANT BOOLE-2		 2)
(DEFCONSTANT BOOLE-AND		 3)
(DEFCONSTANT BOOLE-ANDC1	 4)
(DEFCONSTANT BOOLE-ANDC2	 5)
(DEFCONSTANT BOOLE-C1		 6)
(DEFCONSTANT BOOLE-C2		 7)
(DEFCONSTANT BOOLE-CLR		 8)
(DEFCONSTANT BOOLE-EQV		 9)
(DEFCONSTANT BOOLE-IOR		10)
(DEFCONSTANT BOOLE-NAND		11)
(DEFCONSTANT BOOLE-NOR		12)
(DEFCONSTANT BOOLE-ORC1		13)
(DEFCONSTANT BOOLE-ORC2		14)
(DEFCONSTANT BOOLE-SET		15)
(DEFCONSTANT BOOLE-XOR		16)

(defun BOOLE (op integer1 integer2)
  (ecase op
    (1	integer1)
    (2	integer2)
    (3	(LOGAND integer1 integer2))
    (4	(LOGANDC1 integer1 integer2))
    (5	(LOGANDC2 integer1 integer2))
    (6	(LOGNOT integer1))
    (7	(LOGNOT integer2))
    (8	0)
    (9	(LOGEQV integer1 integer2))
    (10	(LOGIOR integer1 integer2))
    (11	(LOGNAND integer1 integer2))
    (12	(LOGNOR integer1 integer2))
    (13	(LOGORC1 integer1 integer2))
    (14	(LOGORC2 integer1 integer2))
    (15	-1)
    (16	(LOGXOR integer1 integer2))))
    
(defun LOGNOT (num)
  (cond
    ((integerp num)
     (lognot num))
    ((bignump num)
     ;; TODO: may need one more element in result.
     (let ((new (make-vector (length num) 'BIGNUM)))
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
    ((and (bignump x) (integerp y))
     (if (minusp y)
	 (let ((new (copy-sequence x)))
	   (aset new 1 (logand (aref x 1) y))
	   new)
	 (logand (aref x 1) y)))
    ((and (bignump y) (integerp x))
     (if (minusp x)
	 (let ((new (copy-sequence y)))
	   (aset new 1 (logand (aref y 1) x))
	   new)
	 (logand (aref y 1) x)))
    ((and (bignump x) (bignump y))
     0)))

(defun LOGIOR (&rest numbers)
  (reduce #'binary-logior numbers :initial-value 0))

(defun binary-logior (x y)
  (cond
    ((and (integerp x) (integerp y))
     (logior x y))
    ((and (bignump x) (integerp y))
     (if (minusp y)
	 (logior (aref x 1) y)
	 (let ((new (copy-sequence x)))
	   (aset new 1 (logior (aref x 1) y))
	   new)))
    ((and (bignump y) (integerp x))
     (if (minusp x)
	 (logior (aref y 1) x)
	 (let ((new (copy-sequence y)))
	   (aset new 1 (logior (aref y 1) x))
	   new)))
    ((and (bignump x) (bignump y))
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
    ((and (bignump x) (integerp y))
     (let ((new (copy-sequence x)))
       (aset new 1 (logxor (aref x 1) y))
       (when (minusp y)
	 (dotimes (i (- (length x) 2))
	   (aset new (+ i 2) (lognot (aref new (+ i 2))))))
       new))
    ((and (bignump y) (integerp x))
     (let ((new (copy-sequence y)))
       (aset new 1 (logior (aref y 1) x))
       (when (minusp x)
	 (dotimes (i (- (length y) 2))
	   (aset new (+ i 2) (lognot (aref new (+ i 2))))))
       new))
    ((and (bignump x) (bignump y))
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
    ((bignump integer)
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

;;; TODO: &environment
(DEFINE-SETF-EXPANDER LDB (bytespec integer)
  (multiple-value-bind (temps values variables setter getter)
      (get-setf-method integer nil)
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

;;; TODO: &environment
(define-setf-method MASK-FIELD (bytespec integer)
  (multiple-value-bind (temps values variables setter getter)
      (get-setf-method integer nil)
    (let ((byte (gensym))
	  (value (gensym)))
    (values (cons byte temps)
	    (cons bytespec values)
	    (list value)
	    `(let ((,(first variables) (DEPOSIT-FIELD ,value ,byte ,getter)))
	      ,setter
	      ,value)
	    `(MASK-FIELD ,byte ,getter)))))

(defun DECODE-FLOAT (float)
  (unless (floatp float)
    (error "type error"))
  (if (zerop float)
      (VALUES 0.0 0 1.0)
      (let ((exponent (1+ (logb float))))
	(VALUES (* (abs float) (expt 2 (- (float exponent))))
		exponent
		(if (minusp float) -1.0 1.0)))))

(defun SCALE-FLOAT (float integer)
  (unless (and (floatp float) (INTEGERP integer))
    (error "type error"))
  (* float (expt 2.0 (FLOAT integer))))

(defun FLOAT-RADIX (float)
  (unless (floatp float)
    (error "type error"))
  2)

(defun* FLOAT-SIGN (float1 &optional (float2 1.0))
  (if (minusp float1)
      (- float2)
      float2))

(defun FLOAT-DIGITS (float)
  (unless (floatp float)
    (error "type error"))
  53)

(defun FLOAT-PRECISION (float)
  (unless (floatp float)
    (error "type error"))
  (if (zerop float)
      0
      ;; TODO: return number of significant digits in denormals.
      53))

(defun INTEGER-DECODE-FLOAT (float)
  (unless (floatp float)
    (error "type error"))
  (if (zerop float)
      (VALUES 0.0 0 1)
      (let ((exponent (1+ (logb float))))
	(VALUES (* (abs float) (expt 2 (- (float exponent))))
		exponent
		(if (minusp float) 1 1)))))

(defun bignum-float (num)
  (do ((i 1 (1+ i))
       (w 1.0 (* w 268435456.0))
       (x 0.0)
       (len (1- (length num))))
      ((eq i len)
       (+ x (* w (aref num i))))
    (let ((y (aref num i)))
      (incf x (* w (if (minusp y) (+ 268435456.0 y) y))))))

(defun FLOAT (num &optional prototype)
  (cond
    ((integerp num)
     (float num))
    ((floatp num)
     num)
    ((ratiop num)
     (/ (FLOAT (NUMERATOR num)) (FLOAT (DENOMINATOR num))))
    ((bignump num)
     (bignum-float num))
    (t
     (error "type error"))))

(fset 'FLOATP (symbol-function 'floatp))

(DEFCONSTANT MOST-POSITIVE-SHORT-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-SHORT-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT 0.0)
(DEFCONSTANT MOST-POSITIVE-DOUBLE-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-DOUBLE-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT 0.0)
(DEFCONSTANT MOST-POSITIVE-LONG-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-LONG-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-NORMALIZED-LONG-FLOAT 0.0)
(DEFCONSTANT MOST-POSITIVE-SINGLE-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-SINGLE-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT 0.0)
(DEFCONSTANT MOST-NEGATIVE-SHORT-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-SHORT-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT 0.0)
(DEFCONSTANT MOST-NEGATIVE-SINGLE-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-SINGLE-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT 0.0)
(DEFCONSTANT MOST-NEGATIVE-DOUBLE-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-DOUBLE-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT 0.0)
(DEFCONSTANT MOST-NEGATIVE-LONG-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-LONG-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT 0.0)

(DEFCONSTANT SHORT-FLOAT-EPSILON 0.0)
(DEFCONSTANT SHORT-FLOAT-NEGATIVE-EPSILON 0.0)
(DEFCONSTANT SINGLE-FLOAT-EPSILON 0.0)
(DEFCONSTANT SINGLE-FLOAT-NEGATIVE-EPSILON 0.0)
(DEFCONSTANT DOUBLE-FLOAT-EPSILON 0.0)
(DEFCONSTANT DOUBLE-FLOAT-NEGATIVE-EPSILON 0.0)
(DEFCONSTANT LONG-FLOAT-EPSILON 0.0)
(DEFCONSTANT LONG-FLOAT-NEGATIVE-EPSILON 0.0)

;;; TODO: ARITHMETIC-ERROR-OPERANDS, ARITHMETIC-ERROR-OPERATION
