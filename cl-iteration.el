;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 6, Iteration.

(IN-PACKAGE "EMACS-CL")

;;; TODO: (cl:defmacro DO ...)

;;; TODO: (cl:defmacro DO* ...)

(cl:defmacro DOTIMES ((var count &optional result) &body body)
  `(DO ((,var 0 (1+ ,var))
	(,end ,count))
       ((EQL ,var ,end)
	,result)
     ,@body))

(cl:defmacro DOLIST ((var list &optional result) &body body)
  `(PROGN
    (MAPC (LAMBDA (,var) ,@body) ,list)
    ,result))
