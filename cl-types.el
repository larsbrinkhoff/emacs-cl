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
    (error)))

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
	 (string	`(simple-array (unsigned-byte 8) (,(length object))))
	 (vector	(aref object 0))
	 ((compiled-function cons hash-table symbol)
			type)
	 ((buffer char-table frame marker overlay process
	   subr window window-configuration)
			(error)))))))
