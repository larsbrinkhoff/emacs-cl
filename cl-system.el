;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 24, System Construction.

(defvar *FEATURES* (list (nth-value 0 (INTERN "ANSI-CL" "KEYWORD"))
		         (nth-value 0 (INTERN "COMMON-LISP" "KEYWORD"))
			 (nth-value 0 (INTERN "EMACS-CL" "KEYWORD"))))
