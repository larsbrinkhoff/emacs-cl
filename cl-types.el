;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 4, Types and Classes.

(IN-PACKAGE "CL")

(defun COERCE (object type)
  (cond
    ((TYPEP object type)
     object)
    ((SUBTYPEP type 'SEQUENCE)
     (MAP 'LIST #'IDENTITY object))
    ((eq type 'CHARACTER)
     (CHARACTER object))
    ((eq type 'COMPLEX)
     (complex object (COERCE 0 (TYPE-OF object))))
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

(DEFTYPE LIST () `(OR CONS NULL))

(defmacro* CHECK-TYPE (place type &optional string &environment env)
  `(unless (TYPEP ,place ,type ,env)
    (error ,string)))

(defun TYPE-OF (object)
  (case object
    ((nil)		'NULL)
    ((T)		'BOOLEAN)
    (t
     (ecase (type-of object)
       ;; This is supposed to be an exhaustive enumeration of all
       ;; possible return values for Emacs Lisp type-of.
       (bool-vector	`(SIMPLE-BIT-VECTOR ,(length object)))
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
	  (array	`(ARRAY T ,(aref object 1)))
	  (bignum	'BIGNUM)
	  (bit-array	`(ARRAY BIT ,(aref object 1)))
	  (bit-vector	`(BIT-VECTOR ,(length (aref object 2))))
	  (char-array	`(ARRAY CHARACTER ,(aref object 1)))
	  (character	'CHARACTER)
	  (complex	'COMPLEX)
	  (interpreted-function
			'INTERPRETED-FUNCTION)
	  (ratio	'RATIO)
	  (simple-vector
			`(SIMPLE-VECTOR ,(1- (length object))))
	  (string	`(STRING ,(length (aref object 2))))
	  (vector	`(VECTOR ,(length (aref object 2))))
	  (t		(aref object 0))))
       ;; For now, throw an error on these.
       ((buffer char-table frame marker overlay process
	 subr window window-configuration)
			(error "Unknown type: %s" (type-of object)))))))
