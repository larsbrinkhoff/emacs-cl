;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 4, Types and Classes.

(in-package "CL")

(defmacro* CHECK-TYPE (place type &optional string &environment env)
  `(unless (TYPEP ,place ,type ,env)
    (error ,string)))

(defun TYPE-OF (object)
  (case object
    ((NIL)		'NULL)
    ((T)		'BOOLEAN)
    (t
     (let ((type (type-of object)))
       (ecase type
	 ;; This is supposed to be an exhaustive enumeration of all
	 ;; possible return values for Emacs Lisp type-of.
	 (bool-vector	`(SIMPLE-BIT-VECTOR ,(length object)))
	 (compiled-function
			'COMPILED-FUNCTION)
	 (cons		'CONS)
	 (float		'SINGLE-FLOAT)
	 (hash-table	'HASH-TABLE)
	 (integer	'FIXNUM)
	 (string	`(SIMPLE-STRING ,(length object)))
	 (symbol	'SYMBOL)
	 (vector
	  (case (aref object 0)
	    (bignum	'BIGNUM)
	    (ratio	'RATIO)
	    (complex	'COMPLEX)
	    (character	'CHARACTER)
	    (bit-vector	`(BIT-VECTOR ,(length (aref object 2))))
	    (bit-array	`(ARRAY BIT ,(aref object 1)))
	    (string	`(STRING ,(length (aref object 2))))
	    (char-array	`(ARRAY CHARACTER ,(aref object 1)))
	    (simple-vector
			`(SIMPLE-VECTOR (1- (length object))))
	    (vector	`(VECTOR ,(length (aref object 2))))
	    (array	`(ARRAY T ,(aref object 1)))
	    (t		(aref object 0))))
	 ;; For now, throw an error on these.
	 ((buffer char-table frame marker overlay process
	   subr window window-configuration)
			(error "Unknown type: %s" type)))))))
