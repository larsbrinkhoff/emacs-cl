;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 25, Environment.

(IN-PACKAGE "EMACS-CL")

(defvar cl:* nil)
(defvar ** nil)
(defvar ***)
(defvar cl:+ nil)
(defvar ++ nil)
(defvar +++)
(defvar cl:/ nil)
(defvar // nil)
(defvar ///)
(defvar cl:- nil)

(defun LISP-IMPLEMENTATION-TYPE ()
  "Emacs CL")

(defun LISP-IMPLEMENTATION-VERSION ()
  "0.1")

(defun top-level-loop ()
  (loop
   (princ (format "%s> " (package-name *PACKAGE*)))
   (setq +++ ++
	 ++ +
	 + -
	 - (read)
	 /// //
	 *** **
	 // /
	 ** *
	 / (list (eval -))
	 * (first /))
   (dolist (val /)
     (princ val))
   (princ "\n")))
