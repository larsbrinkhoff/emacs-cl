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

(defun* MAKE-ARRAY (dimensions &key (element-type t) initial-element
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
    (ecase (UPGRADED-ARRAY-ELEMENT-TYPE element-type)
      (bit
       (let ((bit-vector (or displaced-to
			     (make-bool-vector size (ecase initial-element
						      ((0 nil) nil) (1 t))))))
	 (cond
	   (simplep	bit-vector)
	   (vectorp	(vector 'bit-vector fill-pointer bit-vector
				displaced-index-offset))
	   (t		(vector 'bit-array dimensions bit-vector
				displaced-index-offset)))))
      (character
       (let ((string (or displaced-to
			 (make-string size
				      (if initial-element
					  (char-code initial-element)
					  0)))))
	 (cond
	   (simplep	string)
	   (vectorp	(vector 'string fill-pointer string
				displaced-index-offset))
	   (t		(vector 'char-array dimensions string
				displaced-index-offset)))))
      ((t)
       (when simplep
	 (incf size))
       (let ((array (or displaced-to (make-vector size initial-element))))
	 (cond
	   (simplep	(aset array 0 'simple-vector) array)
	   (vectorp	(vector 'vector fill-pointer array
				displaced-index-offset))
	   (t		(vector 'array dimensions array
				displaced-index-offset))))))))

(defun ADJUST-ARRAY (array new-dimensions
		     &key element-type initial-element initial-contents
		          fill-pointer displaced-to displaced-index-offset)
  (if (ADJUSTABLE-ARRAY-P array)
      (error "TODO")
      (MAKE-ARRAY new-dimensions
		  :element-type element-type
		  :initial-element initial-element
		  :initial-contents initial-contents
		  :filll-pointer fill-pointer
		  :displaced-to displaced-to
		  :displaced-index-offset displaced-index-offset)))

(defun ADJUSTABLE-ARRAY-P (array)
  (check-type array 'array)
  (and (vectorp array)
       (case (aref array 0)
	 ((bit-vector bit-array string char-array vector array) t))))

(defun AREF (array &rest subscripts)
  (cond
    ((BIT-VECTOR-P array)
     (bit array (just-one subscripts)))
    ((STRINGP array)
     (char array (just-one subscripts)))
    ((vector-and-typep array 'simple-vector)
     (svref array (just-one subscripts)))
    ((vector-and-typep array 'vector)
     (aref (aref array 2) (just-one subscripts)))
    ((vector-and-typep array 'array)
     (aref (aref array 2) (apply #'array-row-major-index subscripts)))
    (t
     (error))))

(defun ARRAY-DIMENSION (array axis)
  (cond
    ((VECTORP array)		(LENGTH array))
    ((ARRAYP array)		(nth axis (ARRAY-DIMENSIONS array)))
    (t				(error))))

(defun ARRAY-DIMENSIONS (array)
  (cond
    ((VECTORP array)		(list (LENGTH array)))
    ((ARRAYP array)		(aref array 1))
    (t				(error))))

(defun ARRAY-ELEMENT-TYPE (array)
  (cond
    ((BIT-VECTOR-P array)		'bit)
    ((STRINGP array)			'character)
    ((ARRAYP array)
     (ecase (aref array 0)
       (bit-array			'bit)
       (char-array			'character)
       ((simple-vector vector array)	t)))
    (t					(error))))

(defun ARRAY-HAS-FILL-POINTER-P (array)
  (and (VECTORP array)
       (not (SIMPLE-VECTOR-P array))
       (aref array 1)))

(defun array-in-bounds-p (array &rest subscripts)
  (and (not (some #'MINUSP subscripts))
       (every #'cl:< subscripts (ARRAY-DIMENSIONS array))))

(defun array-rank (array)
  (length (ARRAY-DIMENSIONS array)))

(defun array-row-major-index (array &rest subscripts)
  (apply #'cl:+ (maplist (lambda (x y) (cl:* (car x) (apply #'cl:* (cdr y))))
			 subscripts (ARRAY-DIMENSIONS a))))

(defun ARRAYP (object)
  (or (VECTORP object)
      (and (vectorp object)
	   (case (aref object 0)
	     ((bit-array char-array array) t)))))

(defun fill-pointer (vector)
  (aref vector 1))

(defsetf fill-pointer (vector) (fill-pointer)
  `(aset ,vector 1 ,fill-pointer))

(defun row-major-aref (array index)
  (cond
    ((VECTORP array)
     (AREF array index))
    ((vector-and-typep array 'array)
     (aref (aref array 2) index))
    (t
     (error))))

(defsetf row-major-aref (array index) (new)
  `(cond
    ((VECTORP ,array)
     (setf (AREF ,array ,index) ,new))
    ((and (vectorp ,array)
          (case (aref ,array 0)
	    ((array bit-array char-array))))
     (aset (aref ,array 2) ,index ,new))
    (t
     (error "type error"))))

(defun UPGRADED-ARRAY-ELEMENT-TYPE (typespec &optional env)
  (cond
    ((SUBTYPEP typespec 'bit)		'bit)
    ((SUBTYPEP typespec 'character)	'character)
    (t					t)))

(defun SIMPLE-VECTOR-P (object)
  (or (SIMPLE-BIT-VECTOR-P object)
      (SIMPLE-STRING-P object)
      (vector-and-typep object 'simple-vector)))

(defun svref (vector index)
  (aref vector (1+ index)))

(defun cl:vector (&rest objects)
  (let ((vector (make-vector (1+ (length objects)) nil))
	(i 0))
    (aset vector 0 'simple-vector)
    (dolist (obj objects vector)
      (aset vector (incf i) obj))))

(defun vector-pop (vector)
  (unless (and (VECTORP vector)
	       (ARRAY-HAS-FILL-POINTER-P vector)
	       (plusp (fill-pointer vector)))
    (error))
  (aref vector (aref vector 2) (aset vector 1 (1- (aref vector 1)))))

(defun vector-push (new-element vector)
  (unless (and (VECTORP vector) (ARRAY-HAS-FILL-POINTER-P vector))
    (error))
  (let ((ptr (fill-pointer vector))
	(storage (aref vector 2)))
    (unless (eql ptr (length storage))
      (aset storage ptr new-element)
      (aset vector 1 (1+ ptr)))))

(defun vector-push-extend (new-element vector &optional extension)
  (unless (and (VECTORP vector) (ARRAY-HAS-FILL-POINTER-P vector))
    (error))
  (let* ((storage (aref vector 2))
	 (len (length storage))
	 (ptr (fill-pointer vector)))
    (when (eql ptr len)
      (let ((new-storage (make-vector (+ len (or extension (length storage)))
				      nil)))
	(dotimes (i len)
	  (aset new-storage i (aref (aref vector 2) i)))
	(aset vector 2 (setq storage new-storage))))
    (aset storage ptr new-element)
    (aset vector 1 (1+ ptr))))

(defun VECTORP (object)
  (or (SIMPLE-VECTOR-P object)
      (and (vectorp object)
	   (case (aref object 0)
	     ((string bit-vector vector) t)))))

(defun bit (array &rest subscripts)
  (cond
    ((SIMPLE-BIT-VECTOR-P array)
     (if (aref array (just-one subscripts)) 1 0))
    ((BIT-VECTOR-P array)
     (if (aref (aref array 2) (just-one subscripts)) 1 0))
    ((vector-and-typep array 'bit-array)
     (error "TODO"))
    (t
     (error))))

(defun sbit (array index)
  (aref array index))

(defun BIT-VECTOR-P (object)
  (or (SIMPLE-BIT-VECTOR-P object)
      (vector-and-typep object 'bit-vector)))

(defun SIMPLE-BIT-VECTOR-P (object)
  (bool-vector-p object))
