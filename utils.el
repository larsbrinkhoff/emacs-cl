;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file provides various small utilities.

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

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun mappend (fn &rest lists)
  (apply #'append
   (if (null (cdr lists))
       (mapcar fn (car lists))
       (cl-mapcar-many fn lists))))

(defun vector-and-typep (object type)
  (and (vectorp object)
       (eq (aref object 0) type)))
