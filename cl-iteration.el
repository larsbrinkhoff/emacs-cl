;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 6, Iteration.

(IN-PACKAGE "EMACS-CL")

(cl:defmacro DOLIST ((var list &optional result) &body body)
  `(MAPC (LAMBDA (,var) ,@body) ,list))
