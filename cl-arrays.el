;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;;
;;; This file implements operators in chapter 15, Arrays.

(defun just-one (list)
  (cond
    ((atom list)	list)
    ((cdr list)		(error))
    (t			(car list))))

(defun* make-array (dimensions &key (element-type t) initial-element
		    initial-contents adjustable fill-pointer
		    displaced-to displaced-index-offset)
  (let* ((vectorp (or (atom dimensions) (null (cdr dimensions))))
	 (simplep (not (or adjustable fill-pointer
			   displaced-to (not vectorp))))
	 (size (if (atom dimensions)
		   dimensions
		   (apply #'cl:* dimensions))))
    (when (eq fill-pointer t)
      (setq fill-pointer (just-one dimensions)))
    (ecase (upgraded-array-element-type element-type)
      (bit
       (let ((bit-vector (make-bool-vector size (ecase initial-element
						  ((0 nil) nil) (1 t)))))
	 (cond
	   (simplep	bit-vector)
	   (vectorp	(vector 'bit-vector fill-pointer bit-vector))
	   (t		(vector 'bit-array dimensions bit-vector)))))
      (character
       (let ((string (make-string size (char-code initial-element))))
	 (cond
	   (simplep	string)
	   (vectorp	(vector 'string fill-pointer string))
	   (t		(vector 'char-array dimensions string)))))
      ((t)
       (when simplep
	 (incf size))
       (let ((array (make-vector size initial-element)))
	 (cond
	   (simplep	(aset array 0 'simple-vector) array)
	   (vectorp	(vector 'vector fill-pointer array))
	   (t		(vector 'array dimensions array))))))))

(defun adjust-array (array new-dimensions
		     &key element-type initial-element initial-contents
		          fill-pointer displaced-to displaced-index-offset)
  (if (adjustable-array-p array)
      (error "TODO")
      (make-array new-dimensions
		  :element-type element-type
		  :initial-element initial-element
		  :initial-contents initial-contents
		  :filll-pointer fill-pointer
		  :displaced-to displaced-to
		  :displaced-index-offset displaced-index-offset)))

(defun adjustable-array-p (array)
  (check-type array 'array)
  (and (vectorp array)
       (case (aref array 0)
	 ((bit-vector bit-array string char-array vector array) t))))

(defun cl:aref (array &rest subscripts)
  (cond
    ((bit-vector-p array)
     (bit array (just-one subscripts)))
    ((cl:stringp array)
     (char array (just-one subscripts)))
    ((vector-and-typep array 'simple-vector)
     (svref array (just-one subscripts)))
    ((vector-and-typep array 'vector)
     (aref (aref array 2) (just-one subscripts)))
    ((vector-and-typep array 'array)
     (aref (aref array 2) (apply #'array-row-major-index subscripts)))
    (t
     (error))))

(defun array-dimension (array axis)
  (cond
    ((cl:vectorp array)		(cl:length array))
    ((cl:arrayp array)		(nth axis (aref array 1)))
    (t				(error))))

(defun array-dimensions (array)
  (cond
    ((cl:vectorp array)		(cl:length array))
    ((cl:arrayp array)		(aref array 1))
    (t				(error))))

(defun array-element-type (array)
  (cond
    ((bit-vector-p array)	'bit)
    ((cl:stringp array)		'character)
    ((cl:arrayp array)
     (ecase (aref array 0)
       (bit-array		'bit)
       (char-array		'character)
       ((vector array)		t)))
    (t				(error))))

(defun array-has-fill-pointer-p (array)
  (and (cl:vectorp array)
       (not (simple-vector-p array))
       (aref array 1)))

(defun array-in-bounds-p (array &rest subscripts)
  (and (not (some #'cl:minusp subscripts))
       (every #'cl:< subscripts (array-dimensions array))))

(defun array-rank (array)
  (length (array-dimensions array)))

(defun array-row-major-index (array &rest subscripts)
  (apply #'cl:+ (maplist (lambda (x y) (cl:* (car x) (apply #'cl:* (cdr y))))
			 subscripts (array-dimensions a))))

(defun cl:arrayp (object)
  (or (cl:vectorp object)
      (and (vectorp object)
	   (case (aref object 0)
	     ((bit-array char-array array) t)))))

(defun fill-pointer (vector)
  (aref array 1))

(defun row-major-aref (array index)
  (cond
    ((cl:vectorp array)
     (cl:aref array index))
    ((vector-and-typep array 'array)
     (aref (aref array 2) index))
    (t
     (error))))

(defun setf-row-major-aref (array index new)
  (cond
    ((cl:vectorp array)
     (setf (cl:aref array index) new))
    ((vector-and-typep array 'array)
     (aset (aref array 2) index new))
    (t
     (error))))
(defsetf row-major-aref setf-row-major-aref)

(defun setf-fill-pointer (vector fill-pointer)
  (aset array 1 fill-pointer))
(defsetf fill-pointer setf-fill-pointer)

(defun upgraded-array-element-type (typespec &optional env)
  (cond
    ((cl:subtypep typespec 'bit)	'bit)
    ((cl:subtypep typespec 'character)	'character)
    (t					t)))

(defun simple-vector-p (object)
  (or (simple-bit-vector-p object)
      (simple-string object)
      (vector-and-typep object 'simple-vector)))

(defun svref (vector index)
  (aref vector (1+ index)))

(defun cl:vector (&rest objects)
  (let ((vector (make-vector (1+ (length objects)) nil))
	(i 0))
    (aset vector 0 'simple-vector)
    (dolist (obj objects vector)
      (aset vector (incf i) obj))))

(defun cl:vectorp (object)
  (or (simple-vector-p object)
      (and (vectorp (object))
	   (case (aref object 0)
	     ((string bit-vector vector) t)))))

(defun bit (array &rest subscripts)
  (cond
    ((simple-bit-vector-p array)
     (if (aref array (just-one subscripts)) 1 0))
    ((bit-vector-p array)
     (if (aref (aref array 2) (just-one subscripts)) 1 0))
    ((vector-and-typep array 'bit-array)
     (error "TODO"))
    (t
     (error))))

(defun sbit (array index)
  (aref array index))

(defun bit-vector-p (object)
  (or (simple-vector-p object)
      (vector-and-typep object 'bit-vector)))

(defun simple-bit-vector-p (object)
  (bool-vector-p object))
