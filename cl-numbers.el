;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;;
;;; This file implements operators in chapter 12, Numbers.

(in-package "CL")

(defun cl:= (number &rest numbers)
  (every (lambda (n) (two-arg-= number n)) numbers))

(defun two-arg-= (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (= num1 num2))
    ((or (complexp num1) (complexp num2))
     (and (two-arg-= (realpart num1) (realpart num2))
	  (two-arg-= (imagpart num1) (imagpart num2))))
    ((or (cl::ratiop num1) (cl::ratiop num2))
     (and (two-arg-= (numerator num1) (numerator num2))
	  (two-arg-= (denominator num1) (denominator num2))))
    ((or (cl::bignump num1) (cl::bignump num2))
     ;; TODO
     t)
    (t
     (error "type error: = %s %s" num1 num2))))

;;; /=

(defun cl:< (number &rest numbers)
  (if (null numbers)
      t
      (two-arg-< number (first numbers))))

(defun two-arg-< (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (< num1 num2))
    ((or (cl::ratiop num1) (cl::ratiop num2))
     ;; TODO
     (< (/ (numerator num1) (denominator num1))
	(/ (numerator num2) (denominator num2))))
    ((or (cl::bignump num1) (cl::bignump num2))
     ;; TODO
     nil)
    (t
     (error "type error: = %s %s" num1 num2))))

;;; >

(defun cl:<= (number &rest numbers)
  (if (null numbers)
      t
      (two-arg-<= number (first numbers))))

(defun two-arg-<= (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (<= num1 num2))
    ((or (cl::ratiop num1) (cl::ratiop num2))
     ;; TODO
     (<= (/ (numerator num1) (denominator num1))
	 (/ (numerator num2) (denominator num2))))
    ((or (cl::bignump num1) (cl::bignump num2))
     ;; TODO
     nil)
    (t
     (error "type error: = %s %s" num1 num2))))

;;; >=

;;; max

;;; min

;;; minusp

;;; plusp

;;; zerop

;;; FLOOR, FFLOOR, CEILING, FCEILING, TRUNCATE, FTRUNCATE, ROUND, FROUND

;;; SIN, COS, TAN

;;; ASIN, ACOS, ATAN

;;; (defconstast pi ...)

;;; SINH, COSH, TANH, ASINH, ACOSH, ATANH

(defun cl:* (&rest numbers)
  (reduce #'two-arg-+ numbers :initial-value 1))

(defun cl:+ (&rest numbers)
  (reduce #'two-arg-+ numbers :initial-value 0))

(defun two-arg-+ (x y)
  (cond
    ((and (integerp x) (integerp y))
     (let ((sum (+ x y)))
       (cond
	 ((and (plusp x) (plusp y) (minusp sum))
	  (vector 'bignum sum 0))
	 ((and (minusp x) (minusp y) (plusp sum))
	  (vector 'bignum sum -1))
	 (t
	  sum))))
    ((or (complexp x) (complexp y))
     (complex (two-arg-+ (realpart x) (realpart y))
	      (two-arg-+ (imagpart x) (imagpart y))))
    ((floatp x)
     (+ x (cl:float y)))
    ((floatp y)
     (+ (cl:float x) y))
    ((or (cl::ratiop x) (cl::ratiop y))
     (cl::ratio (two-arg-+ (cl:* (numerator x) (denominator y))
		       (cl:* (denominator y) (numerator x)))
	    (cl:* (denominator x) (denominator y))))
    ;; bignum
    (t 0)))

(defun cl:- (number &rest numbers)
  (if (null numbers)
      (cond
	((integerp number)
	 (if (eql number most-negative-fixnum)
	     (vector 'bignum 0)
	     (- number)))
	((floatp number)
	 (- number))
	((cl::ratiop number)
	 (vector 'ratio (- (numerator number)) (denominator number)))
	((complexp number)
	 (vector 'complex (- (realpart number)) (- (imagpart number))))
	((cl::bignump number)
	 0)
	(t
	 (error)))
      (dolist (num numbers number)
	(setq number (two-arg-- number num)))))

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
	(setq number (two-arg-/ number num)))))

(defun two-arg-/ (x y)
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
	 (vector 'bignum 0)
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

;;; EVENP, ODDP

;;; EXP, EXPT

(defun gcd (&rest numbers)
  (reduce #'two-arg-gcd numbers :initial-value 0))

(defun two-arg-gcd (x y)
  (if (and (integerp x) (integerp y))
      (progn
	(when (> y x)
	  (psetq x y y x))
	(while (not (zerop y))
	  (psetq y (% x y) x y))
	(abs x))
      0))

;;; INCF, DECF

;;; LCM

;;; LOG

;;; MOD, REM

;;; SIGNUM

;;; SQRT, ISQRT

;;; MAKE-RANDOM-STATE

;;; RANDOM

;;; RANDOM-STATE-P

;;; *RANDOM-STATE*

(defun cl:numberp (object)
  (or (numberp object)
      (and (vectorp object)
	   (let ((type (aref object 0)))
	     (or (eq type 'bignum)
		 (eq type 'ratio)
		 (eq type 'complex))))))

;;; CIS

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
  (and (vectorp num) (eq (aref num 0) 'ratio)))

(defun numerator (num)
  (if (cl::ratiop num)
      (aref num 1)
      num))

(defun denominator (num)
  (if (cl::ratiop num)
      (aref num 2)
      1))

;;; rational

;;; rationalize

(defun rationalp (num)
  (or (cl:integerp num) (cl::ratiop num)))

;;; ash

;;; integer-length

(defun cl::bignump (num)
  (and (vectorp num) (eq (aref num 0) 'bignum)))

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

;;; LOGAND, LOGANDC1, LOGANDC2, LOGEQV, LOGIOR, LOGNAND, LOGNOR,
;;; LOGNOT, LOGORC1, LOGORC2, LOGXOR

;;; LOGBITP

;;; LOGCOUNT

;;; LOGTEST

;;; BYTE, BYTE-SIZE, BYTE-POSITION

;;; DEPOSIT-FIELD

;;; DPB

;;; LDB

;;; LDB-TEST

;;; MASK-FIELD

;;; DECODE-FLOAT, SCALE-FLOAT, FLOAT-RADIX, FLOAT-SIGN, FLOAT-DIGITS,
;;; FLOAT-PRECISION, INTEGER-DECODE-FLOAT

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

;;; ARITHMETIC-ERROR-OPERANDS, ARITHMETIC-ERROR-OPERATION
