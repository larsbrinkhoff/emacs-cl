;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 17, Sequences.

(defun COPY-SEQ (sequence)
  (cond
    ((listp sequence)
     (copy-list sequence))
    ((SIMPLE-VECTOR-P sequence)
     (copy-sequence sequence))
    ((vector-and-typep sequence 'vector)
     (let ((storage (aref sequence 2))
	   (vector
	    (make-vector
	     (1+ (or (fill-pointer sequence) (LENGTH sequence))) nil)))
       (aset vector 0 'simple-vector)
       (do ((i 1 (1+ i)))
	   ((= i (length vector)))
	 (aset vector i (aref storage (1- i))))
       vector))
    ((VECTORP sequence)
     (subseq (aref sequence 2) 0 (fill-pointer sequence)))
    (t
     (error))))

(defun ELT (sequence index)
  (cond
    ((listp sequence)
     (nth index sequence))
    ((VECTORP sequence)
     (if (and (ARRAY-HAS-FILL-POINTER-P sequence)
	      (cl:< index (fill-pointer sequence)))
	 (aref sequence index)
	 (error)))
    (t
     (error))))

;;; TODO: fill

(defun* make-sequence (type size &key initial-element)
  (cond
    ((SUBTYPEP type 'list)
     (make-list size initial-element))
    ((SUBTYPEP type 'bit-vector)
     (make-bool-vector size (ecase initial-element ((0 nil) nil) (1 t))))
    ((SUBTYPEP type 'string)
     (make-string size (if initial-element (CHAR-CODE initial-element) 0)))
    ((SUBTYPEP type 'vector)
     (let ((vector (make-vector (1+ size) initial-element)))
       (aset vector 0 'simple-vector)
       vector))
    (t
     (error))))

;;; TODO: subseq

(defun* MAP (type fn &rest sequences)
  (unless (eq type 'LIST)
    (error "TODO"))
  (let ((result nil))
    (dolist (seq sequences)
      (dosequence (x seq)
        (push (FUNCALL fn x) result)))
    (nreverse result)))

;;; TODO: map-into

;;; TODO: reduce

;;; TODO: count, count-if, count-if-NOT

(defun LENGTH (sequence)
  (cond
    ((or (listp sequence)
	 (SIMPLE-BIT-VECTOR-P sequence)
	 (SIMPLE-STRING-P sequence))
     (length sequence))
    ((SIMPLE-VECTOR-P sequence)
     (1- (length sequence)))
    ((VECTORP sequence)
     (if (ARRAY-HAS-FILL-POINTER-P sequence)
	 (fill-pointer sequence)
	 (length (aref sequence 2))))
    (t
     (error))))

(defmacro* dovector ((var vector &optional result) &body body)
  (let ((i (gensym))
	(len (gensym))
	(vec (gensym)))
    `(let* (,var (,i 0) (,vec ,vector) (,len (LENGTH ,vec)))
      (while (< ,i ,len)
	(setq ,var (AREF ,vec ,i))
	,@body
	(incf ,i))
      ,result)))

(defmacro* dosequence ((var sequence &optional result) &body body)
  (let ((seq (gensym)))
    `(let ((,seq ,sequence))
      (if (listp ,seq)
	  (dolist (,var ,seq ,result) ,@body)
	  (dovector (,var ,seq ,result) ,@body)))))

(defun CONCATENATE (type &rest sequences)
  (ecase type
    (VECTOR
     (let ((vector (make-vector (1+ (reduce #'+ (mapcar #'LENGTH sequences)))
				'simple-vector))
	   (i 0))
       (dolist (seq sequences)
	 (dosequence (x seq)
	   (aset vector (incf i) x)))
       vector))
    (STRING
     (let ((string
	    (make-string (reduce #'+ (mapcar #'LENGTH sequences)) 0))
	   (i -1))
       (dolist (seq sequences)
	 (dosequence (x seq)
	   (aset string (incf i) (CHAR-CODE x))))
       string))))

;;; ...

;;; TODO: merge

;;; TODO: remove, remove-if, remove-if-not, delete, delete-if, delete-if-not

;;; TODO: remove-duplicates, delete-duplicates
