;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 17, Sequences.

(defun cl:copy-seq (sequence)
  (cond
    ((listp sequence)
     (copy-list sequence))
    ((simple-vector-p sequence)
     (copy-sequence sequence))
    ((vector-and-typep sequence 'vector)
     (let ((storage (aref sequence 2))
	   (vector
	    (make-vector
	     (1+ (or (fill-pointer sequence) (cl:length sequence))) nil)))
       (aset vector 0 'simple-vector)
       (do ((i 1 (1+ i)))
	   ((= i (length vector)))
	 (aset vector i (aref storage (1- i))))
       vector))
    ((cl:vectorp sequence)
     (subseq (aref sequence 2) 0 (fill-pointer sequence)))
    (t
     (error))))

(defun cl:elt (sequence index)
  (cond
    ((listp sequence)
     (nth index sequence))
    ((cl:vectorp sequence)
     (if (and (array-has-fill-pointer-p sequence)
	      (cl:< index (fill-pointer sequence)))
	 (aref sequence index)
	 (error)))
    (t
     (error))))

;;; TODO: fill

(defun* make-sequence (type size &key initial-element)
  (cond
    ((cl:subtypep type 'list)
     (make-list size initial-element))
    ((cl:subtypep type 'bit-vector)
     (make-bool-vector size (ecase initial-element ((0 nil) nil) (1 t))))
    ((cl:subtypep type 'string)
     (make-string size (if initial-element (char-code initial-element) 0)))
    ((cl:subtypep type 'vector)
     (let ((vector (make-vector (1+ size) initial-element)))
       (aset vector 0 'simple-vector)
       vector))
    (t
     (error))))

;;; TODO: subseq

;;; TODO: map

;;; TODO: map-into

;;; TODO: reduce

;;; TODO: count, count-if, count-if-NOT

(defun cl:length (sequence)
  (cond
    ((or (listp sequence)
	 (simple-bit-vector-p sequence)
	 (simple-string-p sequence))
     (length sequence))
    ((simple-vector-p sequence)
     (1- (length sequence)))
    ((cl:vectorp sequence)
     (if (array-has-fill-pointer-p sequence)
	 (fill-pointer sequence)
	 (length (aref sequence 2))))
    (t
     (error))))

(defun cl:mapcar (fn &rest seqs)
  (if (null (cdr seqs))
      (mapcar fn (car seqs))
      (cl-mapcar-many fn seqs)))

(defun cl:mapcan (fn &rest seqs)
  (apply #'nconc
   (if (null (cdr seqs))
       (mapcar fn (car seqs))
       (cl-mapcar-many fn seqs))))
