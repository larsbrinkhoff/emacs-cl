;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 15, Arrays.

(IN-PACKAGE "EMACS-CL")

;;; System Class	ARRAY
;;; Type		SIMPLE-ARRAY
;;; System Class	VECTOR
;;; Type		SIMPLE-VECTOR
;;; System Class	BIT-VECTOR
;;; Type		SIMPLE-BIT-VECTOR

(cl:defun MAKE-ARRAY (dimensions &key (ELEMENT-TYPE T) INITIAL-ELEMENT
		      INITIAL-CONTENTS ADJUSTABLE FILL-POINTER
		      DISPLACED-TO DISPLACED-INDEX-OFFSET)
  (let* ((vectorp (or (atom dimensions) (null (cdr dimensions))))
	 (simplep (not (or ADJUSTABLE FILL-POINTER
			   DISPLACED-TO (not vectorp))))
	 (size (if (atom dimensions)
		   dimensions
		   (apply #'cl:* dimensions))))
    (when (eq FILL-POINTER T)
      (setq FILL-POINTER (just-one dimensions)))
    (ecase (UPGRADED-ARRAY-ELEMENT-TYPE ELEMENT-TYPE)
      (BIT
       (let ((bit-vector (or DISPLACED-TO
			     (make-bool-vector size (ecase INITIAL-ELEMENT
						      ((0 nil) nil) (1 t))))))
	 (cond
	   (simplep	bit-vector)
	   (vectorp	(vector 'BIT-VECTOR FILL-POINTER bit-vector
				DISPLACED-INDEX-OFFSET))
	   (t		(vector 'bit-array dimensions bit-vector
				DISPLACED-INDEX-OFFSET)))))
      (CHARACTER
       (let ((string (or DISPLACED-TO
			 (make-string size
				      (if INITIAL-ELEMENT
					  (CHAR-CODE INITIAL-ELEMENT)
					  0)))))
	 (cond
	   (simplep	string)
	   (vectorp	(vector 'STRING FILL-POINTER string
				DISPLACED-INDEX-OFFSET))
	   (t		(vector 'char-array dimensions string
				DISPLACED-INDEX-OFFSET)))))
      ((T)
       (when simplep
	 (incf size))
       (let ((array (or DISPLACED-TO (make-vector size INITIAL-ELEMENT))))
	 (cond
	   (simplep	(aset array 0 'SIMPLE-VECTOR) array)
	   (vectorp	(vector 'VECTOR FILL-POINTER array
				DISPLACED-INDEX-OFFSET))
	   (t		(vector 'ARRAY dimensions array
				DISPLACED-INDEX-OFFSET))))))))

(cl:defun ADJUST-ARRAY (array new-dimensions
			&key ELEMENT-TYPE INITIAL-ELEMENT INITIAL-CONTENTS
			     FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET)
  (if (ADJUSTABLE-ARRAY-P array)
      (error "TODO")
      (MAKE-ARRAY new-dimensions
		  (kw ELEMENT-TYPE) ELEMENT-TYPE
		  (kw INITIAL-ELEMENT) INITIAL-ELEMENT
		  (kw INITIAL-CONTENTS) INITIAL-CONTENTS
		  (kw FILL-POINTER) FILL-POINTER
		  (kw DISPLACED-TO) DISPLACED-TO
		  (kw DISPLACED-INDEX-OFFSET) DISPLACED-INDEX-OFFSET)))

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
     (aref (aref array 2) (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)))
    (t
     (type-error array 'ARRAY))))

(defsetf AREF (array &rest subscripts) (obj)
  `(ASET ,obj ,array ,@subscripts))

(DEFINE-SETF-EXPANDER AREF (array &rest subscripts)
  (let ((obj (gensym))
	(atemp (gensym))
	(stemps (map-to-gensyms subscripts)))
    (VALUES (cons atemp stemps)
	    (cons array subscripts)
	    (list obj)
	    `(ASET ,obj ,atemp ,@stemps)
	    `(AREF ,atemp ,@stemps))))

(defun ASET (object array &rest subscripts)
  (cond
    ((BIT-VECTOR-P array)
     (setf (BIT array (just-one subscripts)) object))
    ((STRINGP array)
     (setf (CHAR array (just-one subscripts)) object))
    ((vector-and-typep array 'SIMPLE-VECTOR)
     (setf (SVREF array (just-one subscripts)) object))
    ((vector-and-typep array 'VECTOR)
     (aset (aref array 2) (just-one subscripts) object))
    ((vector-and-typep array 'ARRAY)
     (aset (aref array 2)
	   (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)
	   object))
    (t
     (type-error array 'ARRAY))))

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
    (type-error array 'ARRAY))
  (and (VECTORP array)
       (not (SIMPLE-VECTOR-P array))))

(defun ARRAY-DISPLACEMENT (array)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (if (or (bool-vector-p array)
	  (stringp array)
	  (eq (aref array 0) 'SIMPLE-VECTOR))
      (VALUES nil 0)
      (VALUES (aref array 2) (aref array 3))))

(defun ARRAY-IN-BOUNDS-P (array &rest subscripts)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (and (not (some #'MINUSP subscripts))
       (every #'cl:< subscripts (ARRAY-DIMENSIONS array))))

(defun ARRAY-RANK (array)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (length (ARRAY-DIMENSIONS array)))

(defun ARRAY-ROW-MAJOR-INDEX (array &rest subscripts)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (apply #'cl:+ (maplist (lambda (x y) (cl:* (car x) (apply #'cl:* (cdr y))))
			 subscripts (ARRAY-DIMENSIONS array))))

(defun ARRAY-TOTAL-SIZE (array)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (reduce #'* (ARRAY-DIMENSIONS array)))

(defun ARRAYP (object)
  (or (stringp object)
      (bool-vector-p object)
      (and (vectorp object)
	   (case (aref object 0)
	     ((BIT-VECTOR bit-array STRING char-array
	       SIMPLE-VECTOR VECTOR ARRAY) T)))))

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

(DEFCONSTANT ARRAY-DIMENSION-LIMIT MOST-POSITIVE-FIXNUM)
(DEFCONSTANT ARRAY-RANK-LIMIT MOST-POSITIVE-FIXNUM)
(DEFCONSTANT ARRAY-TOTAL-SIZE-LIMIT MOST-POSITIVE-FIXNUM)

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

(defun bit-array-p (object)
  (or (bool-vector-p object)
      (vector-and-typep object 'bit-array)))

(defun default-result (array result)
  (cond
    ((bit-array-p result)	result)
    ((eq result T)		array)
    (t				(if (BIT-VECTOR-P array)
				    (make-bool-vector (LENGTH array) nil)
				    (vector 'bit-array
					    (ARRAY-DIMENSIONS array)
					    (make-bool-vector
					     (ARRAY-TOTAL-SIZE array nil))
					    0)))))

(defun BIT-AND (array1 array2 &optional result)
  (let ((result (default-result array1 result))
	(storage1 (if (bool-vector-p array1) array1 (aref array1 2)))
	(storage2 (if (bool-vector-p array2) array2 (aref array2 2))))
    (dotimes (i (ARRAY-TOTAL-SIZE result))
      (aset result i (and (aref storage1 i) (aref storage2 i))))))

(defun BIT-ANDC1 (array1 array2 &optional result)
  (BIT-AND (BIT-NOT array1) array2 result))

(defun BIT-ANDC2 (array1 array2 &optional result)
  (BIT-AND (BIT-NOT array1) array2 result))

(defun BIT-EQV (array1 array2 &optional result)
  (BIT-NOT (BIT-XOR array1 array2 result)))

(defun BIT-IOR (array1 array2 &optional result)
  (let ((result (default-result array1 result))
	(storage1 (if (bool-vector-p array1) array1 (aref array1 2)))
	(storage2 (if (bool-vector-p array2) array2 (aref array2 2))))
    (dotimes (i (ARRAY-TOTAL-SIZE result))
      (aset result i (or (aref storage1 i) (aref storage2 i))))))

(defun BIT-NAND (array1 array2 &optional result)
  (BIT-NOT (BIT-AND array1 array2 result)))

(defun BIT-NOR (array1 array2 &optional result)
  (BIT-NOT (BIT-IOR array1 array2 result)))

(defun BIT-ORC1 (array1 array2 &optional result)
  (BIT-IOR (BIT-NOT array1) array2 result))

(defun BIT-ORC2 (array1 array2 &optional result)
  (BIT-IOR array1 (BIT-NOT array2) result))

(defun binary-xor (x y)
  (and (or x y)
       (not (and x y))))

(defun BIT-XOR (array1 array2 &optional result)
  (let ((result (default-result array1 result))
	(storage1 (if (bool-vector-p array1) array1 (aref array1 2)))
	(storage2 (if (bool-vector-p array2) array2 (aref array2 2))))
    (dotimes (i (ARRAY-TOTAL-SIZE result))
      (aset result i (binary-xor (aref storage1 i) (aref storage2 i))))))

(defun BIT-NOT (array &optional result)
  (let ((result (default-result array result))
	(storage (if (bool-vector-p array) array (aref array 2))))
    (dotimes (i (ARRAY-TOTAL-SIZE array))
      (aset result i (not (aref storage i))))))

(defun BIT-VECTOR-P (object)
  (or (bool-vector-p object)
      (vector-and-typep object 'BIT-VECTOR)))

(defun SIMPLE-BIT-VECTOR-P (object)
  (bool-vector-p object))
