;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements the TYPEP function from chapter 4, Types and Classes.

(IN-PACKAGE "EMACS-CL")

(defvar *atomic-typespecs* (make-hash-table))
(defvar *compound-typespecs* (make-hash-table))

;;; Implements TYPEP for "typespec".
(defmacro* define-typep ((var typespec env &optional compound-only) &body body)
  (if (consp typespec)
      `(setf (gethash ',(first typespec) *compound-typespecs*)
	     (function* (lambda (,var ,env ,@(rest typespec)) ,@body))
	     ,@(unless compound-only
	         `((gethash ',(first typespec) *atomic-typespecs*)
		            (function* (lambda (,var ,env ,@(rest typespec))
			                 ,@body)))))
      `(setf (gethash ',typespec *atomic-typespecs*)
	     (function* (lambda (,var ,env) ,@body)))))

(defun in-range (num low high)
  "Check that NUM is in the range specified by the interval designators
   LOW and HIGH."
  (let* ((low-exclusive (consp low))
	 (low (if low-exclusive (car low) low))
	 (high-exclusive (consp high))
	 (high (if high-exclusive (car high) high)))
    (and (cond
	   ((eq low star) t)
	   (low-exclusive (cl:< low num))
	   (t (cl:<= low num)))
	 (cond
	   ((eq high star) t)
	   (high-exclusive (cl:< num high))
	   (t (cl:<= num high))))))

(defvar star (INTERN "*" "EMACS-CL"))

(defmacro star-or (type &rest forms)
  `(or (eq ,type star) ,@forms))


;;; Definitions for all type specifiers recognized by TYPEP follows.

(define-typep (object (AND &rest types) env :compound-only)
  (every (lambda (type) (TYPEP object type env)) types))

(define-typep (object ARITHMETIC-ERROR env)
  nil)

(define-typep (object (ARRAY &optional type dim) env)
  nil)

(define-typep (object ATOM env)
  (not (consp object)))

(define-typep (object BASE-CHAR env)
  (TYPEP object 'CHARACTER env))

(define-typep (object (BASE-STRING) env)
  (TYPEP object 'STRING env))

(define-typep (object BIGNUM env)
  (bignump object))

(define-typep (object BIT env)
  (or (eq object 0) (eq object 1)))

(define-typep (object (BIT-VECTOR &optional (size star)) env)
  (and (BIT-VECTOR-P object)
       (star-or size (eql size (length object)))))

(define-typep (object BOOLEAN env)
  (or (null object) (eq object T)))

;;; broadcast-stream (atomic only)
;;; built-in-class (atomic only)
;;; cell-error (atomic only)

(define-typep (object CHARACTER env)
  (CHARACTERP object))

;;; class (atomic only)

(define-typep (object COMPILED-FUNCTION env)
  (COMPILED-FUNCTION-P object))

(define-typep (object (COMPLEX &optional (type star)) env)
  (and (COMPLEXP object)
       (star-or type
		(unless (SUBTYPEP type 'real)
		  (error "invalid complex part type: %s" type)))))

;;; concatenated-stream (atomic only)
;;; condition (atomic only)

(define-typep (object (CONS &optional (car-type star) (cdr-type star)) env)
  (and (consp object)
       (star-or car-type (TYPEP (car object) car-type env))
       (star-or cdr-type (TYPEP (cdr object) cdr-type env))))

;;; control-error (atomic only)
;;; division-by-zero (atomic only)

(define-typep (object (DOUBLE-FLOAT &optional (low star) (high star)) env)
  (TYPEP object `(SINGLE-FLOAT ,low ,high)))

;;; echo-stream (atomic only)
;;; end-of-file (atomic only)

(define-typep (obj1 (EQL obj2) env :compound-only)
  (EQL obj1 obj2))

;;; error (atomic only)

(define-typep (object EXTENDED-CHAR env)
  (TYPEP object '(AND CHARACTER (NOT BASE-CHAR))))

;;; file-error (atomic only)
;;; file-stream (atomic only)

(define-typep (object FIXNUM env)
  (integerp object))

(define-typep (object (FLOAT &optional (low star) (high star)) env)
  (TYPEP object `(SINGLE-FLOAT ,low ,high)))

;;; floating-point-inexact (atomic only)
;;; floating-point-invalid-operation (atomic only)
;;; floating-point-overflow (atomic only)
;;; floating-point-underflow (atomic only)

(define-typep (object FUNCTION env)
  (FUNCTIONP object))

(define-typep (object (FUNCTION &rest args) env :compound-only)
  (error "(function ...) not allowed"))

;;; generic-function (atomic only)

(define-typep (object HASH-TABLE env)
  (HASH-TABLE-P object))

(define-typep (object (INTEGER &optional (low star) (high star)) env)
  (and (INTEGERP object) (in-range object low high)))

(define-typep (object INTERPRETED-FUNCTION env)
  (INTERPRETED-FUNCTION-P object))

(define-typep (object KEYWORD env)
  (KEYWORDP object))

(define-typep (object LIST env)
  (listp object))

(define-typep (object LOGICAL-PATHNAME env)
  (vector-and-typep object 'LOGICAL-PATHNAME))

(define-typep (object (LONG-FLOAT &optional (low star) (high star)) env)
  (TYPEP object `(SINGLE-FLOAT ,low ,high)))

(define-typep (object (MEMBER &rest objects) env :compound-only)
  (member object objects))

;;; method (atomic only)
;;; method-combination (atomic only)

(define-typep (object (MOD n) env :compound-only)
  (TYPEP object `(INTEGER 0 ,(1- n)) env))

(define-typep (object nil env)
  nil)

(define-typep (object (NOT type) env)
  (not (TYPEP object type env)))

(define-typep (object NULL env)
  (null object))

(define-typep (object NUMBER env)
  (NUMBERP object))

(define-typep (object (OR &rest types) env :compound-only)
  (some (lambda (type) (TYPEP object type env)) types))

(define-typep (object PACKAGE env)
  (PACKAGEP object))

;;; package-error (atomic only)
;;; parse-error (atomic only)

(define-typep (object PATHNAME env)
  (or (PATHNAMEP object)
      (TYPEP object 'LOGICAL-PATHNAME)))

;;; print-not-readable (atomic only)
;;; program-error (atomic only)

(define-typep (object RANDOM-STATE env)
  (RANDOM-STATE-P object))

(define-typep (object RATIO env)
  (ratiop object))

(define-typep (object (RATIONAL &optional (low star) (high star)) env)
  (and (RATIONALP object) (in-range object low high)))

;;; reader-error (atomic only)

;;; DEFSTRUCT arranges for this.
; (define-typep (object READTABLE env)
;  (READTABLEP object))

(define-typep (object (REAL &optional (low star) (high star)) env)
  (and (REALP object) (in-range object low high)))

;;; restart (atomic only)

(define-typep (object (SATISFIES fn) env :compound-only)
  (funcall fn object))

(define-typep (object SEQUENCE env)
  (or (listp object) (VECTORP object)))

;;; serious-condition (atomic only)

(define-typep (object (SHORT-FLOAT &optional (low star) (high star)) env)
  (TYPEP object `(SINGLE-FLOAT ,low ,high)))

(define-typep (object (SIGNED-BYTE &optional n) env)
  (if n
      (TYPEP object
		`(INTEGER ,(- (expt 2 (1- n))) ,(1- (expt 2 (1- n))))
		env)
      (INTEGERP object)))

;;; simple-array

(define-typep (object (SIMPLE-BASE-STRING &optional (size star)) env)
  (TYPEP object `(SIMPLE-STRING ,size)))

(define-typep (object (SIMPLE-BIT-VECTOR &optional (size star)) env)
  (and (bool-vector-p object)
       (star-or size (eq size (LENGTH object)))))

(define-typep (object (SIMPLE-STRING &optional (size star)) env)
  (and (stringp object)
       (star-or size (eq size (LENGTH object)))))

(define-typep (object (SIMPLE-VECTOR &optional (size star)) env)
  (and (SIMPLE-VECTOR-P object)
       (star-or size (eq size (LENGTH object)))))

(define-typep (object (SINGLE-FLOAT &optional (low star) (high star)) env)
  (and (floatp object) (in-range object low high)))

(define-typep (object STANDARD-CHAR env)
  (STANDARD-CHAR-P object))

;;; standard-class (atomic only)
;;; standard-generic-function (atomic only)
;;; standard-method (atomic only)
;;; standard-object (atomic only)
;;; storage-condition (atomic only)
;;; stream (atomic only)
;;; stream-error (atomic only)

(define-typep (object (STRING &optional (size star)) env)
  (and (STRINGP object)
       (star-or size (eq size (LENGTH object)))))

;;; string-stream (atomic only)
;;; structure-class (atomic only)
;;; structure-object (atomic only)
;;; style-warning (atomic only)

(define-typep (object SYMBOL env)
  (SYMBOLP object))

;;; synonym-stream (atomic only)

(define-typep (object T env)
  T)

;;; two-way-stream (atomic only)
;;; type-error (atomic only)
;;; unbound-slot (atomic only)
;;; unbound-variable (atomic only)
;;; undefined-function (atomic only)

(define-typep (object (UNSIGNED-BYTE &optional n) env)
  (TYPEP object `(INTEGER 0 ,(if n (1- (expt 2 n)) star) env)))

(define-typep (object (VALUES &rest args) env :compound-only)
  (error "values not allowed"))

(define-typep (object (VECTOR &optional type size) env)
  (and (VECTORP object)
       ;; TODO: SUBTYPEP upgraded type ...
       (star-or T)
       ;; TODO: vector size, not length
       (star-or (eql size (LENGTH object)))))



(defun TYPEP (object type &optional env)
  (setq type (expand-type type))
  (if (consp type)
      (let ((fn (gethash (first type) *compound-typespecs*)))
	(if fn
	    (apply fn object env (rest type))
	    (error "invalid typespec: %s" type)))
      (let ((fn (gethash type *atomic-typespecs*)))
	(if fn
	    (funcall fn object env)
	    (ERROR "Invalid typespec: ~A" type)))))
