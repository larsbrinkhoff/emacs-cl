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

(define-storage-layout array (dims storage offset))

(define-storage-layout vector (size storage offset fp))

(defun set-initial-contents (n i storage contents fn)
  (cond
    ((zerop n)	(aset storage 0 (funcall fn contents)))
    ((eq n 1)	(dosequence (x contents i)
		  (aset storage i (funcall fn x))
		  (incf i)))
    (t		(dosequence (x contents i)
		  (setq i (set-initial-contents (1- n) i storage x fn))))))

(defun bit-bool (bit)
  (ecase bit
    (0 nil)
    (1 t)))

(defun make-simple-vector (size initial-element)
  (let ((vector (make-vector (1+ size) initial-element)))
    (aset vector 0 'SIMPLE-VECTOR)
    vector))

(cl:defun MAKE-ARRAY (dimensions &key (ELEMENT-TYPE T) INITIAL-ELEMENT
		      INITIAL-CONTENTS ADJUSTABLE FILL-POINTER
		      DISPLACED-TO DISPLACED-INDEX-OFFSET)
  (setq dimensions (ensure-list dimensions))
  (when (eq FILL-POINTER T)
    (setq FILL-POINTER (just-one dimensions)))
  (let* ((size (apply #'cl:* dimensions))
	 (start-index 0)
	 (ndims (length dimensions))
	 (vectorp (eq ndims 1))
	 (simplep (not (or ADJUSTABLE
			   FILL-POINTER
			   DISPLACED-TO
			   (not vectorp)))))
    (multiple-value-bind (initial-element make-fn convert-fn
			  vector-type array-type)
	(ecase (UPGRADED-ARRAY-ELEMENT-TYPE ELEMENT-TYPE)
	  (BIT
	   (values (or INITIAL-ELEMENT 0)
		   #'make-bool-vector
		   #'bit-bool
		   'BIT-VECTOR
		   'bit-array))
	  (CHARACTER
	   (values (or INITIAL-ELEMENT (ch 0))
		   #'make-string
		   #'CHAR-CODE
		   'STRING
		   'char-array))
	  ((T)
	   (when simplep
	     (setq start-index 1))
	   (values INITIAL-ELEMENT
		   (if simplep #'make-simple-vector #'make-vector)
		   #'IDENTITY
		   'VECTOR
		   'ARRAY)))
      (let ((storage (or DISPLACED-TO
			 (funcall make-fn size
				  (funcall convert-fn initial-element)))))
	(when INITIAL-CONTENTS
	  (set-initial-contents
	   ndims start-index storage INITIAL-CONTENTS convert-fn))
	(cond
	  (simplep	storage)
	  (vectorp	(vector vector-type FILL-POINTER storage
				DISPLACED-INDEX-OFFSET))
	  (t		(vector array-type dimensions storage
				DISPLACED-INDEX-OFFSET)))))))

;     (ecase (UPGRADED-ARRAY-ELEMENT-TYPE ELEMENT-TYPE)
;       (BIT
;        (let* ((initial-element (bit-bool (or INITIAL-ELEMENT 0)))
; 	      (storage (or DISPLACED-TO
; 			   (make-bool-vector size initial-element)))
; 	      (array (cond
; 		       (simplep	storage)
; 		       (vectorp	(vector 'BIT-VECTOR FILL-POINTER storage
; 					DISPLACED-INDEX-OFFSET))
; 		       (t	(vector 'bit-array dimensions storage
; 					DISPLACED-INDEX-OFFSET)))))
; 	 (when INITIAL-CONTENTS
; 	   (set-initial-contents (length dimensions) 0 storage
; 				 INITIAL-CONTENTS #'bit-bool))
; 	 array))
;       (CHARACTER
;        (let* ((initial-element (CHAR-CODE (or INITIAL-ELEMENT (ch 0))))
; 	      (storage (or DISPLACED-TO (make-string size initial-element)))
; 	      (array (cond
; 		       (simplep	storage)
; 		       (vectorp	(vector 'STRING FILL-POINTER storage
; 					DISPLACED-INDEX-OFFSET))
; 		       (t	(vector 'char-array dimensions storage
; 					DISPLACED-INDEX-OFFSET)))))
; 	 (when INITIAL-CONTENTS
; 	   (set-initial-contents (length dimensions) 0 storage
; 				 INITIAL-CONTENTS #'CHAR-CODE)
; 	 array))
;       ((T)
;        (when simplep
; 	 (incf size))
;        (let* ((storage (or DISPLACED-TO (make-vector size INITIAL-ELEMENT)))
; 	      (array (cond
; 		       (simplep	(aset storage 0 'SIMPLE-VECTOR) storage)
; 		       (vectorp	(vector 'VECTOR FILL-POINTER storage
; 					DISPLACED-INDEX-OFFSET))
; 		       (t	(vector 'ARRAY dimensions storage
; 					DISPLACED-INDEX-OFFSET)))))
; 	 (when INITIAL-CONTENTS
; 	   (set-initial-contents (length dimensions) (if simplep 1 0)
; 				 storage INITIAL-CONTENTS #'IDENTITY))
; 	 array)))))

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
     (aref (array-storage array) (just-one subscripts)))
    ((vector-and-typep array 'ARRAY)
     (aref (array-storage array) (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)))
    ((vector-and-typep array 'bit-array)
     (BIT (array-storage array) (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)))
    ((vector-and-typep array 'char-array)
     (CHAR (array-storage array) (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)))
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
     (aset (array-storage array) (just-one subscripts) object))
    ((vector-and-typep array 'ARRAY)
     (aset (array-storage array)
	   (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)
	   object))
    ((vector-and-typep array 'bit-array)
     (aset (array-storage array)
	   (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)
	   (bit-bool object)))
    ((vector-and-typep array 'char-array)
     (aset (array-storage array)
	   (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)
	   (CHAR-CODE object)))
    (t
     (type-error array 'ARRAY))))

(defun ARRAY-DIMENSION (array axis)
  (cond
    ((VECTORP array)		(LENGTH array))
    ((ARRAYP array)		(nth axis (ARRAY-DIMENSIONS array)))
    (t				(error))))

(defun ARRAY-DIMENSIONS (array)
  (cond
    ((VECTORP array)		(vector-size array))
    ((ARRAYP array)		(array-dims array))
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
      (VALUES (array-storage array) (array-offset array))))

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
  (vector-fp vector))

(defsetf FILL-POINTER (vector) (fill-pointer)
  `(setf (vector-fp ,vector) ,fill-pointer))

(DEFSETF FILL-POINTER (vector) (fill-pointer)
  `(aset ,vector 4 ,fill-pointer))

(defun ROW-MAJOR-AREF (array index)
  (cond
    ((VECTORP array)
     (AREF array index))
    ((vector-and-typep array 'ARRAY)
     (aref (array-storage array) index))
    (t
     (error))))

(defsetf ROW-MAJOR-AREF (array index) (new)
  `(cond
    ((VECTORP ,array)
     (setf (AREF ,array ,index) ,new))
    ((and (vectorp ,array)
          (case (aref ,array 0)
	    ((ARRAY bit-array char-array))))
     (aset (array-storage ,array) ,index ,new))
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
  (let ((vector (make-simple-vector (length objects) nil))
	(i 0))
    (dolist (obj objects vector)
      (aset vector (incf i) obj))))

(defun VECTOR-POP (vector)
  (unless (and (VECTORP vector)
	       (ARRAY-HAS-FILL-POINTER-P vector)
	       (plusp (FILL-POINTER vector)))
    (error))
  (aref (vector-storage vector) (decf (vector-fp vector))))

(defun VECTOR-PUSH (object vector)
  (unless (and (VECTORP vector) (ARRAY-HAS-FILL-POINTER-P vector))
    (error))
  (let ((ptr (FILL-POINTER vector))
	(storage (vector-storage vector)))
    (unless (eq ptr (vector-size vector))
      (aset storage ptr (ecase (aref vector 0)
			  (BIT-VECTOR	(if object 1 0))
			  (STRING	(CHAR-CODE object))
			  (VECTOR	object)))
      (setf (vector-fp vector) (1+ ptr)))))

(defun VECTOR-PUSH-EXTEND (object vector &optional extension)
  (unless (and (VECTORP vector) (ARRAY-HAS-FILL-POINTER-P vector))
    (type-error vector '(AND VECTOR (NOT SIMPLE-VECTOR))))
  (let ((storage (vector-storage vector))
	(len (vector-size vector))
	(ptr (FILL-POINTER vector)))
    (when (eq ptr len)
      (let ((new-storage (make-vector (+ len (or extension len)) nil)))
	(dotimes (i len)
	  (aset new-storage i (aref storage i)))
	(setf (vector-storage vector) (setq storage new-storage))))
    (aset storage ptr (ecase (aref vector 0)
			(BIT-VECTOR	(if object 1 0))
			(STRING		(CHAR-CODE object))
			(VECTOR		object)))
    (setf (vector-fp vector) (1+ ptr))))

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
	 (aref (array-storage array) (just-one subscripts)))
	((vector-and-typep array 'bit-array)
	 (aref (array-storage array) (apply #'ARRAY-ROW-MAJOR-INDEX subscripts)))
	(t
	 (error)))
      1 0))

(defsetf BIT (array &rest subscripts) (bit)
  `(let ((bool (not (zerop ,bit))))
     (cond
       ((SIMPLE-BIT-VECTOR-P ,array)
	(aset ,array (just-one ',subscripts) bool))
       ((BIT-VECTOR-P ,array)
	(aset (array-storage ,array) (just-one ',subscripts) bool))
       ((vector-and-typep ,array 'bit-array)
	(aset (array-storage ,array) (ARRAY-ROW-MAJOR-INDEX ,@subscripts) bool))
       (t
	(error)))))

(defun SBIT (array index)
  (if (aref array index) 1 0))

(defsetf SBIT (array index) (bit)
  `(aset ,array ,index (not (zerop ,bit))))

(DEFSETF SBIT (array index) (bit)
  `(aset ,array ,index (NOT (ZEROP ,bit))))

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
	(storage1 (if (bool-vector-p array1) array1 (array-storage array1)))
	(storage2 (if (bool-vector-p array2) array2 (array-storage array2))))
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
	(storage1 (if (bool-vector-p array1) array1 (array-storage array1)))
	(storage2 (if (bool-vector-p array2) array2 (array-storage array2))))
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
	(storage1 (if (bool-vector-p array1) array1 (array-storage array1)))
	(storage2 (if (bool-vector-p array2) array2 (array-storage array2))))
    (dotimes (i (ARRAY-TOTAL-SIZE result))
      (aset result i (binary-xor (aref storage1 i) (aref storage2 i))))))

(defun BIT-NOT (array &optional result)
  (let ((result (default-result array result))
	(storage (if (bool-vector-p array) array (array-storage array))))
    (dotimes (i (ARRAY-TOTAL-SIZE array))
      (aset result i (not (aref storage i))))))

(defun BIT-VECTOR-P (object)
  (or (bool-vector-p object)
      (vector-and-typep object 'BIT-VECTOR)))

(defun SIMPLE-BIT-VECTOR-P (object)
  (bool-vector-p object))
