;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file provides various small utilities.

(defun strcat (&rest string-designators)
  (apply #'CONCATENATE 'STRING (mapcar #'STRING string-designators)))

(defun symcat (&rest string-designators)
  (nth-value 0 (INTERN (apply #'strcat string-designators))))

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
