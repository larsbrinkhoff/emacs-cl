;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements the SUBTYPEP operator.  The implementation
;;;; is based on Henry Baker's paper A Decision Procedure for Common
;;;; Lisp's SUBTYPEP Predicate.

(in-package "CL")

(defvar *types*
  '(complex number null boolean keyword symbol cons list))

(defvar *objects*
  (list (complex 0 1) 0 nil t (cl:intern "reallyunlikelysymbolname" "KEYWORD")
	(make-symbol "") (cons nil nil)))

(defvar *type-val* (make-hash-table :test 'equal))

(defun object-val (object)
  (let ((pos (position object *objects*)))
    (if pos
	(ash 1 pos)
	(error))))

(defun register (object)
  (dolist (type *types*)
    (when (cl:typep object type)
      (setf (gethash type *type-val*)
	    (logior (gethash type *type-val*) (object-val object))))))

(dolist (type *types*)
  (setf (gethash type *type-val*) 0))

(dolist (object *objects*)
  (register object))

(integer * 0)

(integer -10 0)

(defun negate-range (range)
  (cond
    ((null range)
     '(* *))
    (t
     (setq range (if (eq (first range) '*)
		     (rest range)
		     (cons '* range)))
     (setq range (if (eq (first (last range)) '*)
		     (butlast range)
		     (append range '(*))))
     (mapcar (lambda (x)
	       (cond
		 ((eq x '*) '*)
		 ((consp x) (first x))
		 (t (list x))))
	     range))))

(defun ll<= (x y)
  (cond
    ((eq x '*))
    ((eq y '*) nil)
    ((consp x)
     (if (consp y)
	 (<= (first x) (first y))
	 (< (first x) y)))
    ((consp y)
     (<= x (first y)))
    ((<= x y))))

(defun lh> (x y)
  (cond
    ((eq x '*) nil)
    ((eq y '*) nil)
    ((consp x)
     (if (consp y)
	 (if (and (cl::ratiop x) (cl::ratiop y))
	     (> (first x) (first y))
	     (>= (first x) (first y)))
	 (> (first x) y)))
    ((consp y)
     (> x (first y)))
    ((> x y))))

(defun hh<= (x y)
  (cond
    ((eq x '*) nil)
    ((eq y '*))
    ((consp x)
     (if (consp y)
	 (<= (first x) (first y))
	 (<= (first x) y)))
    ((consp y)
     (< x (first y)))
    ((<= x y))))

(defun union-ranges (ranges1 ranges2)
  (when (and ranges1 ranges2 (ll<= (first ranges2) (first ranges1)))
    (psetq ranges1 ranges2
	   ranges2 ranges1))
;  (print (format "union %s %s" ranges1 ranges2))
  (let ((low1 (first ranges1))
	(low2 (first ranges2))
	(high1 (second ranges1))
	(high2 (second ranges2)))
    (cond
      ((null ranges1)
       ranges2)
      ((null ranges2)
       ranges1)
      ((lh> low2 high1)
;        (print
; 	(let ((standard-output (lambda (ch) nil)))
; 	 (format "A: (%s %s ...)   %s + %s -> %s\n=> %s"
; 		 low1 high1 (cddr ranges1) ranges2
; 		 (union-ranges (cddr ranges1) ranges2)
; 		 (list* low1 high1 (union-ranges (cddr ranges1) ranges2)))))
       (list* low1 high1 (union-ranges (cddr ranges1) ranges2)))
      ((hh<= high1 high2)
;        (print
; 	(let ((standard-output (lambda (ch) nil)))
; 	 (format "B: (%s %s ...)   %s + %s -> %s\n=> %s"
; 		 low1 high2 (cddr ranges1) ranges2
; 		 (union-ranges (cddr ranges1) ranges2)
; 		 (let ((u (union-ranges (cddr ranges1) ranges2)))
; 		   (when (and u (hh<= high2 (second u)))
; 		     (setq high2 (second u)))
; 		   (list* low1 high2 (cddr u))))))
       (let ((u (union-ranges (cddr ranges1) ranges2)))
	 (when (and u (hh<= high2 (second u)))
	   (setq high2 (second u)))
	 (list* low1 high2 (cddr u))))
      (t
;        (print
; 	(let ((standard-output (lambda (ch) nil)))
; 	 (format "C: (%s %s ...)   %s + %s -> %s"
; 		 low1 high1 ranges1 (cddr ranges2)
; 		 (union-ranges ranges1 (cddr ranges2)))))
       (list* low1 high1 (cddr (union-ranges ranges1 (cddr ranges2))))))))

(defun union-types (v1 v2)
  (let* ((r1 (second v1))
	 (r2 (second v2)))
    (print (format "union: %s" (union-ranges (first r1) (first r2))))
    `(,(logior (first v1) (first v2))
      (,(union-ranges (first r1) (first r2))
       ,(union-ranges (second r1) (second r2))
       ,(union-ranges (third r1) (third r2))))))

(defun type-val (type)
  (cond
    ((member type *types*)
     `(,(gethash type *type-val*) (() () ())))
    ((atom type)
     (let ((num 0)
	   (ranges
	    (ecase type
	      (bit
	       `((0 1) () () ()))
	      (fixnum
	       `((,most-negative-fixnum ,most-positive-fixnum) () ()))
	      (bignum
	       `((* (,most-negative-fixnum) (,most-positive-fixnum) *) () ()))
	      (integer
	       `((* *) () ()))
	      (ratio
	       `(() (* *) ()))
	      (rational
	       `((* *) (* *) ()))
	      ((float short-float single-float double-float long-float)
	       `(() () (* *)))
	      (real
	       `((* *) (* *) (* *))))))
       (dolist (object *objects* `(,num ,ranges))
 	 (when (cl:typep object type)
 	   (setq num (logior num (object-val object)))))))
    (t
     (print (format "type: %s" type))
     (ecase (first type)
       (integer	`(0 (,type () ())))
       (rational
		`(0 ((integer ,(second type) ,(third type))
		     (ratio ,(second type) ,(third type))
		     ())))
       ((float short-float single-float double-float long-float)
		`(0 (() () ,type)))
       (real	`(0 ((,(second type) ,(third type))
		     (,(second type) ,(third type))
		     (,(second type) ,(third type)))))
       (eql	`(,(object-val (second type))
		  (,(second type) ,(second type))
		  (,(second type) ,(second type))
		  (,(second type) ,(second type))))
       (member	(type-val `(or ,@(mapcar (lambda (obj)) `(eql ,obj))
			       (rest type))))
       (and	;(reduce #'logand (mapcar #'type-val (rest type))))
		;(reduce #'intersect-types (rest type)))
		(type-val `(not (or ,@(mapcar (lambda (type) `(not ,type))
				              (rest type))))))
       (or	;(reduce #'logior (mapcar #'type-val (rest type))))
		(print (format "or %s => %s" (rest type)
			       (reduce #'union-types (rest type) :key #'type-val)))
	        (reduce #'union-types (rest type) :key #'type-val))
       (not	(let* ((val (type-val (second type)))
		       (ranges (second val)))
		  (print (format "not %s => %s" type val))
		  `(,(lognot (first val))
		    (,(negate-range (first ranges))
		     ,(negate-range (second ranges))
		     ,(negate-range (third ranges))))))))))

(defun find-new-objects (type)
  (when (consp type)
    (case (first type)
      ((and or not)
       (mapc #'find-new-objects (rest type)))
      ((member eql)
       (push type *types*)
       (dolist (object (rest type))
	 (pushnew object *objects*)))
      (t
       (error)))))

(defun cl:subtypep (type1 type2 &optional env)
  (let ((*types* *types*)
	(*type-val* *type-val*)
	(*objects* *objects*))
    (find-new-objects type1)
    (find-new-objects type2)
    (dolist (type *types*)
      (setf (gethash type *type-val*) 0))
    (dolist (object *objects*)
      (register object))
    (print *objects*)
    (dolist (type *types*)
      (print (format "%s => %s" type (type-val type))))
    (let* ((val (type-val `(and ,type1 (not ,type2))))
	   (ranges (second val)))
      (print (format "type val %s" val))
      (and (zerop (first val))
	   (null (first ranges))
	   (null (second ranges))
	   (null (third ranges))))))
