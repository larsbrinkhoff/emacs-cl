;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 15, Arrays.

(IN-PACKAGE "EMACS-CL")

(cl:defun MAKE-ARRAY (dimensions &key (element-type T) initial-element
		      initial-contents adjustable fill-pointer
		      displaced-to displaced-index-offset)
  (let* ((vectorp (or (atom dimensions) (null (cdr dimensions))))
	 (simplep (not (or adjustable fill-pointer
			   displaced-to (not vectorp))))
	 (size (if (atom dimensions)
		   dimensions
		   (apply #'cl:* dimensions))))
    (when (eq fill-pointer T)
      (setq fill-pointer (just-one dimensions)))
    (ecase (UPGRADED-ARRAY-ELEMENT-TYPE element-type)
      (BIT
       (let ((bit-vector (or displaced-to
			     (make-bool-vector size (ecase initial-element
						      ((0 nil) nil) (1 t))))))
	 (cond
	   (simplep	bit-vector)
	   (vectorp	(vector 'BIT-VECTOR fill-pointer bit-vector
				displaced-index-offset))
	   (t		(vector 'bit-array dimensions bit-vector
				displaced-index-offset)))))
      (CHARACTER
       (let ((string (or displaced-to
			 (make-string size
				      (if initial-element
					  (CHAR-CODE initial-element)
					  0)))))
	 (cond
	   (simplep	string)
	   (vectorp	(vector 'STRING fill-pointer string
				displaced-index-offset))
	   (t		(vector 'char-array dimensions string
				displaced-index-offset)))))
      ((T)
       (when simplep
	 (incf size))
       (let ((array (or displaced-to (make-vector size initial-element))))
	 (cond
	   (simplep	(aset array 0 'SIMPLE-VECTOR) array)
	   (vectorp	(vector 'VECTOR fill-pointer array
				displaced-index-offset))
	   (t		(vector 'ARRAY dimensions array
				displaced-index-offset))))))))

(cl:defun ADJUST-ARRAY (array new-dimensions
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
  (and (vectorp array)
       (case (aref array 0)
	 ((BIT-VECTOR bit-array STRING char-array VECTOR ARRAY) T))))

(defun AREF (array &rest subscripts)
  (cond
    ((BIT-VECTOR-P array)
     (BIT array (just-one subscripts)))
    ((STRINGP array)
     (CHAR array (just-one subscripts)))
    ((vector-and-typep array 'SIMPLE-VECTOR)
     (SVREF array (just-one subscripts)))
    ((vector-and-typep array 'VECTOR)
     (aref (aref array 2) (just-one subscripts)))
    ((vector-and-typep array 'ARRAY)
     (aref (aref array 2) (apply #'ARRAY-ROW-MAJOR-INDEX subscripts)))
    (t
     (error))))

(defsetf AREF (array &rest subscripts) (obj)
  `(cond
     ((BIT-VECTOR-P ,array)
      (setf (BIT ,array (just-one ',subscripts))) ,obj)
     ((STRINGP ,array)
      (setf (CHAR ,array (just-one ',subscripts))) ,obj)
     ((vector-and-typep ,array 'SIMPLE-VECTOR)
      (setf (SVREF ,array ,(first subscripts)) ,obj))
     ((vector-and-typep ,array 'VECTOR)
      (aset (aref ,array 2) (just-one ',subscripts) ,obj))
     ((vector-and-typep ,array 'ARRAY)
      (aset (aref ,array 2) (ARRAY-ROW-MAJOR-INDEX ,@subscripts) ,obj))
     (t
      (error))))

(DEFSETF AREF (array &rest subscripts) (obj)
  `(COND
     ((BIT-VECTOR-P ,array)
      (SETF (BIT ,array (first ',subscripts))) ,obj)
     ((STRINGP ,array)
      (SETF (CHAR ,array (first ',subscripts))) ,obj)
     ((vector-and-typep ,array 'SIMPLE-VECTOR)
      (SETF (SVREF ,array ,(first subscripts)) ,obj))
     ((vector-and-typep ,array 'VECTOR)
      (ASET (aref ,array 2) (first ',subscripts) ,obj))
     ((vector-and-typep ,array 'ARRAY)
      (aset (aref ,array 2) (ARRAY-ROW-MAJOR-INDEX ,@subscripts) ,obj))
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
    ((BIT-VECTOR-P array)		'BIT)
    ((STRINGP array)			'CHARACTER)
    ((ARRAYP array)
     (ecase (aref array 0)
       (bit-array			'BIT)
       (char-array			'CHARACTER)
       ((simple-vector vector array)	T)))
    (t					(error))))

(defun ARRAY-HAS-FILL-POINTER-P (array)
  (unless (ARRAYP array)
    (ERROR 'TYPE-ERROR))
  (and (VECTORP array)
       (not (SIMPLE-VECTOR-P array))))

(defun ARRAY-IN-BOUNDS-P (array &rest subscripts)
  (and (not (some #'MINUSP subscripts))
       (every #'cl:< subscripts (ARRAY-DIMENSIONS array))))

(defun ARRAY-RANK (array)
  (length (ARRAY-DIMENSIONS array)))

(defun ARRAY-ROW-MAJOR-INDEX (array &rest subscripts)
  (apply #'cl:+ (maplist (lambda (x y) (cl:* (car x) (apply #'cl:* (cdr y))))
			 subscripts (ARRAY-DIMENSIONS array))))

(defun ARRAYP (object)
  (or (VECTORP object)
      (and (vectorp object)
	   (case (aref object 0)
	     ((bit-array char-array array) T)))))

(defun FILL-POINTER (vector)
  (unless (ARRAY-HAS-FILL-POINTER-P vector)
    (ERROR 'TYPE-ERROR))
  (aref vector 1))

(defsetf FILL-POINTER (vector) (fill-pointer)
  `(aset ,vector 1 ,fill-pointer))

(DEFSETF FILL-POINTER (vector) (fill-pointer)
  `(aset ,vector 1 ,fill-pointer))

(defun ROW-MAJOR-AREF (array index)
  (cond
    ((VECTORP array)
     (AREF array index))
    ((vector-and-typep array 'ARRAY)
     (aref (aref array 2) index))
    (t
     (error))))

(defsetf ROW-MAJOR-AREF (array index) (new)
  `(cond
    ((VECTORP ,array)
     (setf (AREF ,array ,index) ,new))
    ((and (vectorp ,array)
          (case (aref ,array 0)
	    ((ARRAY bit-array char-array))))
     (aset (aref ,array 2) ,index ,new))
    (t
     (error "type error"))))

(defun UPGRADED-ARRAY-ELEMENT-TYPE (typespec &optional env)
  (cond
    ((SUBTYPEP typespec 'BIT)		'BIT)
    ((SUBTYPEP typespec 'CHARACTER)	'CHARACTER)
    (t					T)))

(defun SIMPLE-VECTOR-P (object)
  (or (stringp object)
      (bool-vector-p object)
      (vector-and-typep object 'SIMPLE-VECTOR)))

(defun SVREF (vector index)
  (aref vector (1+ index)))

(defsetf SVREF (vector index) (obj)
  `(setf (aref ,vector (1+ ,index)) ,obj))

(defun VECTOR (&rest objects)
  (let ((vector (make-vector (1+ (length objects)) 'SIMPLE-VECTOR))
	(i 0))
    (dolist (obj objects vector)
      (aset vector (incf i) obj))))

(defun VECTOR-POP (vector)
  (unless (and (VECTORP vector)
	       (ARRAY-HAS-FILL-POINTER-P vector)
	       (plusp (FILL-POINTER vector)))
    (error))
  (aref (aref vector 2) (aset vector 1 (1- (aref vector 1)))))

(defun VECTOR-PUSH (new-element vector)
  (unless (and (VECTORP vector) (ARRAY-HAS-FILL-POINTER-P vector))
    (error))
  (let ((ptr (FILL-POINTER vector))
	(storage (aref vector 2)))
    (unless (eql ptr (length storage))
      (aset storage ptr new-element)
      (aset vector 1 (1+ ptr)))))

(defun VECTOR-PUSH-EXTEND (new-element vector &optional extension)
  (unless (and (VECTORP vector) (ARRAY-HAS-FILL-POINTER-P vector))
    (error))
  (let* ((storage (aref vector 2))
	 (len (length storage))
	 (ptr (FILL-POINTER vector)))
    (when (eq ptr len)
      (let ((new-storage (make-vector (+ len (or extension len)) nil)))
	(dotimes (i len)
	  (aset new-storage i (aref storage i)))
	(aset vector 2 (setq storage new-storage))))
    (aset storage ptr (ecase (aref vector 0)
			(BIT-VECTOR		(if new-element 1 0))
			(STRING		(CHAR-CODE new-element))
			(VECTOR		new-element)))
    (aset vector 1 (1+ ptr))))

(defun VECTORP (object)
  (or (stringp object)
      (bool-vector-p object)
      (and (vectorp object)
	   (case (aref object 0)
	     ((STRING BIT-VECTOR VECTOR SIMPLE-VECTOR) T)))))

(defun BIT (array &rest subscripts)
  (if (cond
	((SIMPLE-BIT-VECTOR-P array)
	 (aref array (just-one subscripts)))
	((BIT-VECTOR-P array)
	 (aref (aref array 2) (just-one subscripts)))
	((vector-and-typep array 'bit-array)
	 (aref (aref array 2) (apply #'ARRAY-ROW-MAJOR-INDEX subscripts)))
	(t
	 (error)))
      1 0))

(defsetf BIT (array &rest subscripts) (bit)
  `(let ((bool (not (zerop ,bit))))
     (cond
       ((SIMPLE-BIT-VECTOR-P ,array)
	(aset ,array (just-one ',subscripts) bool))
       ((BIT-VECTOR-P ,array)
	(aset (aref ,array 2) (just-one ',subscripts) bool))
       ((vector-and-typep ,array 'bit-array)
	(aset (aref ,array 2) (ARRAY-ROW-MAJOR-INDEX ,@subscripts) bool))
       (t
	(error)))))

(defun SBIT (array index)
  (if (aref array index) 1 0))

(defsetf SBIT (array index) (bit)
  `(aset ,array ,index (not (zerop ,bit))))

(defun BIT-VECTOR-P (object)
  (or (bool-vector-p object)
      (vector-and-typep object 'BIT-VECTOR)))

(defun SIMPLE-BIT-VECTOR-P (object)
  (bool-vector-p object))
