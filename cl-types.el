;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 4, Types and Classes.

(in-package "CL")

(defun subtypep (type1 type2 &optional env)
  t)

(defmacro* check-type (place type &optional string &environment env)
  `(unless (cl:typep ,place ,type ,env)
    (error ,string)))

(defun cl:type-of (object)
  (case object
    ((nil) 'null)
    ((t) 'boolean)
    (t
     (let ((type (type-of object)))
       (ecase type
	 ;; This is supposed to be an exhaustive enumeration of all
	 ;; possible return values for Emacs Lisp type-of.
	 (bool-vector	`(simple-bit-vector ,(length object)))
	 (float		'single-float)
	 (integer	'fixnum)
	 (string	`(simple-string ,(length object)))
	 (vector
	  (ecase (aref object 0)
	    (bignum	'bignum)
	    (ratio	'ratio)
	    (complex	'complex)
	    (character	'character)
	    (bit-vector	`(bit-vector ,(length (aref object 2))))
	    (bit-array	`(array bit ,(aref object 1)))
	    (string	`(string ,(length (aref object 2))))
	    (char-array	`(array char ,(aref object 1)))
	    (simple-vector
			`(simple-vector (1- (length object))))
	    (vector	`(vector ,(length (aref object 2))))
	    (array	`(array t ,(aref object 1)))))
	 ((compiled-function cons hash-table symbol)
			type)
	 ((buffer char-table frame marker overlay process
	   subr window window-configuration)
			(error)))))))

(defun vector-and-typep (object type)
  (and (vectorp object)
       (eq (aref object 0) type)))
