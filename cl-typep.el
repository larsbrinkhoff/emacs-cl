;;;; -*- emacs-lisp -*-

(defvar *atomic-typespecs* (make-hash-table))
(defvar *compound-typespecs* (make-hash-table))

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
  (let* ((low-exclusive (consp low))
	 (low (if low-exclusive (car low) low))
	 (high-exclusive (consp high))
	 (high (if high-exclusive (car high) high)))
    (and (cond
	   ((eq low '*) t)
	   (low-exclusive (< low num))
	   (t (<= low num)))
	 (cond
	   ((eq high '*) t)
	   (high-exclusive (> high num))
	   (t (>= high num))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typep (object (and &rest types) env :compound-only)
  (every (lambda (type) (cl:typep object type env)) types))

(define-typep (object arithmetic-error env) nil)

(define-typep (object (array &optional type dim) env) nil)

(define-typep (object atom env)
  (not (consp object)))

(define-typep (object base-char env) nil)

(define-typep (object (base-string) env) nil)

(define-typep (object bignum env) nil)

(define-typep (object bit env)
  (and (integerp object)
       (or (eql object 0) (eql object 1))))

;;; bit-vector
;;; broadcast-stream (atomic only)
;;; built-in-class (atomic only)
;;; cell-error (atomic only)
;;; character (atomic only)
;;; class (atomic only)

(define-typep (object compiled-function env)
  (compiled-function-p object))

(define-typep (object (complex &optional type) env) nil)

;;; concatenated-stream (atomic only)
;;; condition (atomic only)

(define-typep (object (cons &optional (car-type *) (cdr-type *)) env)
  (and (consp object)
       (or (eq car-type '*) (cl:typep (car object) car-type env))
       (or (eq cdr-type '*) (cl:typep (cdr object) cdr-type env))))

;;; control-error (atomic only)
;;; division-by-zero (atomic only)

(define-typep (object (double-float &optional (low '*) (high '*)) env)
  (cl:typep object `(single-float ,low ,high)))

;;; echo-stream (atomic only)
;;; end-of-file (atomic only)

(define-typep (obj1 (eql obj2) env :compound-only)
  (eql obj1 obj2))

;;; error (atomic only)
;;; extended-char (atomic only)
;;; file-error (atomic only)
;;; file-stream (atomic only)

(define-typep (object fixnum env)
  (cl:typep object `(integer ,most-negative-fixnum ,most-positive-fixnum) env))

(define-typep (object (float &optional (low '*) (high '*)) env)
  (cl:typep object `(single-float ,low ,high)))

;;; floating-point-inexact (atomic only)
;;; floating-point-invalid-operation (atomic only)
;;; floating-point-overflow (atomic only)
;;; floating-point-underflow (atomic only)

(define-typep (object function env)
  (functionp object))

(define-typep (object (function &rest args) env :compound-only)
  (error "(function ...) not allowed"))

;;; generic-function (atomic only)
;;; hash-table (atomic only)

(define-typep (object (integer &optional (low '*) (high '*)) env)
  (and (integerp object) (in-range object low high)))

(define-typep (object keyword env)
  (and (symbolp object)
       (let ((package (symbol-package object)))
	 (and package (equal (package-name package) "KEYWORD")))))

(define-typep (object list env)
  (listp object))

;;; logical-pathname (atomic only)

(define-typep (object (long-float &optional (low '*) (high '*)) env)
  (cl:typep object `(single-float ,low ,high)))

(define-typep (object (member &rest objects) env :compound-only)
  (member object objects))

;;; method (atomic only)
;;; method-combination (atomic only)

(define-typep (object (mod n) env :compound-only)
  (cl:typep object `(integer 0 ,(1- (second type))) env))

(define-typep (object nil env)
  nil)

(define-typep (object (not type) env)
  (not (cl:typep object type env)))

(define-typep (object null env)
  (null object))

(define-typep (object number env)
  (cl:typep object '(or real complex) env))

(define-typep (object (or &rest types) env :compound-only)
  (some (lambda (type) (cl:typep object type env)) types))

;;; package (atomic only)
;;; package-error (atomic only)
;;; parse-error (atomic only)
;;; pathname (atomic only)
;;; print-not-readable (atomic only)
;;; program-error (atomic only)
;;; random-state (atomic only)

(define-typep (object ratio env)
    nil)

(define-typep (object (rational &optional (low '*) (high '*)) env)
  (cl:typep object `(or ratio (integer ,low ,high)) env))

;;; reader-error (atomic only)
;;; readtable (atomic only)

(define-typep (object (real &optional (low '*) (high '*)) env)
  (cl:typep object
	    `(or ratio (integer ,low ,high) (single-float ,low ,high))
	    env))

;;; restart (atomic only)

(define-typep (object (satisfies fn) env :compound-only)
  (funcall fn object))

(define-typep (object sequence env)
  (or (listp object) (cl:typep object 'vector)))

;;; serious-condition (atomic only)

(define-typep (object (short-float &optional (low '*) (high '*)) env)
  (cl:typep object `(single-float ,low ,high)))

(define-typep (object (signed-byte &optional n) env)
  (if n
      (cl:typep object
		`(integer ,(- (expt 2 (1- n))) ,(1- (expt 2 (1- n))))
		env)
      (integerp object)))

;;; simple-array
;;; simple-base-string
;;; simple-bit-vector
;;; simple-condition (atomic only)
;;; simple-error (atomic only)
;;; simple-string
;;; simple-type-error (atomic only)
;;; simple-vector
;;; simple-warning (atomic only)

(define-typep (object (single-float &optional (low '*) (high '*)) env)
  (and (floatp object) (in-range object low high)))

;;; standard-char (atomic only)
;;; standard-class (atomic only)
;;; standard-generic-function (atomic only)
;;; standard-method (atomic only)
;;; standard-object (atomic only)
;;; storage-condition (atomic only)
;;; stream (atomic only)
;;; stream-error (atomic only)
;;; string
;;; string-stream (atomic only)
;;; structure-class (atomic only)
;;; structure-object (atomic only)
;;; style-warning (atomic only)

(define-typep (object symbol env)
  (symbolp object))

;;; synonym-stream (atomic only)

(define-typep (object t env)
  t)

;;; two-way-stream (atomic only)
;;; type-error (atomic only)
;;; unbound-slot (atomic only)
;;; unbound-variable (atomic only)
;;; undefined-function (atomic only)

(define-typep (object (unsigned-byte &optional n) env)
  (cl:typep object `(integer 0 ,(if n (1- (expt 2 n)) '*) env)))

(define-typep (object (values &rest args) env :compound-only)
  (error "values not allowed"))

;;; vector
;;; warning (atomic only)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cl:typep (object type &optional env)
  (if (consp type)
      (let ((fn (gethash (first type) *compound-typespecs*)))
	(if fn
	    (apply fn object env (rest type))
	    (error "invalid typespec: %s" type)))
      (let ((fn (gethash type *atomic-typespecs*)))
	(if fn
	    (funcall fn object env)
	    (error "invalid typespec: %s" type)))))
