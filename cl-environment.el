;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;;
;;; This file implements operators in chapter 25, Environment.

(defvar * nil)
(defvar ** nil)
(defvar ***)
(defvar + nil)
(defvar ++ nil)
(defvar +++)
(defvar / nil)
(defvar // nil)
(defvar ///)
(defvar - nil)

(defun top-level-loop ()
  (loop
   (princ (format "%s> " (package-name *package*)))
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
