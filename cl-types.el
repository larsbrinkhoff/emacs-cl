;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 4, Types and Classes.

(IN-PACKAGE "EMACS-CL")

;;; TODO: GENERIC-FUNCTION
;;; TODO: STANDARD-GENERIC-FUNCTION
;;; TODO: CLASS
;;; TODO: BUILT-IN-CLASS
;;; TODO: STRUCTURE-CLASS
;;; TODO: STANDARD-CLASS
;;; TODO: METHOD
;;; TODO: STANDARD-METHOD
;;; TODO: STRUCTURE-OBJECT
;;; TODO: STANDARD-OBJECT
;;; TODO: METHOD-COMBINATION

(defun COERCE (object type)
  (cond
    ((TYPEP object type)
     object)
    ((SUBTYPEP type 'SEQUENCE)
     (MAP 'LIST #'IDENTITY object))
    ((eq type 'CHARACTER)
     (CHARACTER object))
    ((eq type 'COMPLEX)
     (COMPLEX object (COERCE 0 (TYPE-OF object))))
    ((SUBTYPEP type 'FLOAT)
     (float object))
    ((eq type 'FUNCTION)
     (FDEFINITION object))
    ((eq type 'T)
     object)
    (t
     (error "type error"))))

(defvar *deftype-expanders* (make-hash-table))

(defmacro* DEFTYPE (name lambda-list &body body)
  `(progn
    (setf (gethash ',name *deftype-expanders*)
          (function (lambda ,lambda-list ,@body)))
    ',name))

(defun expand-type (orig-type)
  (let* ((type (ensure-list orig-type))
	 (fn (gethash (first type) *deftype-expanders*)))
    (if fn
	(expand-type (apply fn (rest type)))
	orig-type)))

; (DEFTYPE LIST () `(OR CONS NULL))

(defun TYPE-OF (object)
  (case object
    ((nil)		'NULL)
    ((T)		'BOOLEAN)
    (t
     (ecase (type-of object)
       ;; This is supposed to be an exhaustive enumeration of all
       ;; possible return values for Emacs Lisp type-of.
       ((bit-vector bool-vector)
			`(SIMPLE-BIT-VECTOR ,(length object)))
       ((compiled-function subr)
			'COMPILED-FUNCTION)
       (cons		'CONS)
       (float		'SINGLE-FLOAT)
       (hash-table	'HASH-TABLE)
       (integer		'FIXNUM)
       (string		`(SIMPLE-STRING ,(length object)))
       (symbol		'SYMBOL)
       (vector
	(case (aref object 0)
	  (ARRAY	`(ARRAY T ,(array-dims object)))
	  (BIGNUM	'BIGNUM)
	  (bit-array	`(ARRAY BIT ,(array-dims object)))
	  (BIT-VECTOR	`(BIT-VECTOR ,(vector-size object)))
	  (char-array	`(ARRAY CHARACTER ,(array-dims object)))
	  (CHARACTER	'CHARACTER)
	  (COMPLEX	'COMPLEX)
	  (INTERPRETED-FUNCTION
			'INTERPRETED-FUNCTION)
	  (RATIO	'RATIO)
	  (SIMPLE-VECTOR
			`(SIMPLE-VECTOR ,(1- (length object))))
	  (STRING	`(STRING ,(vector-size object)))
	  (VECTOR	`(VECTOR ,(vector-size object)))
	  (t		(aref object 0))))
       ;; For now, throw an error on these.
       ((buffer char-table frame marker overlay process
	 subr window window-configuration)
			(error "Unknown type: %s" (type-of object)))))))

;;; TYPEP defined in cl-typep.el.

;;; TYPE-ERROR, TYPE-ERROR-DATUM, TYPE-ERROR-EXPECTED-TYPE, and
;;; SIMPLE-TYPE-ERROR defined in cl-conditions.el.
