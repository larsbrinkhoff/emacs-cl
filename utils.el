;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file provides various small utilities.

(defun strcat (&rest string-designators)
  (apply #'CONCATENATE 'STRING (mapcar #'STRING string-designators)))

(defun symcat (&rest string-designators)
  (let ((sym (intern (apply #'strcat string-designators))))
    (setf (SYMBOL-PACKAGE sym) *PACKAGE*)
    sym))

(defun just-one (list)
  (cond
    ((atom list)	list)
    ((cdr list)		(error))
    (t			(car list))))

(defun mappend (fn &rest lists)
  (apply #'append
   (if (null (cdr lists))
       (mapcar fn (car lists))
       (cl-mapcar-many fn lists))))

(defun vector-and-typep (object type)
  (and (vectorp object)
       (eq (aref object 0) type)))

(defun curry (fn &rest args1)
  `(lambda (&rest args2) (apply ',fn ,@args1 args2)))

(defun rcurry (fn &rest args2)
  `(lambda (&rest args1) (apply ',fn (append args1 ',args2))))

(defmacro compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	`(lambda (&rest args)
	  ,(reduce (lambda (f1 f2) `(,f1 ,f2)) fns
		   :from-end t :initial-value `(apply ',fn1 args))))
      #'identity))

(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))

(defmacro* do-list-designator ((var list &optional result) &body body)
  `(dolist (,var (ensure-list ,list) ,result)
     ,@body))
