;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;;
;;; This file implements operators in chapter 25, Environment.

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
