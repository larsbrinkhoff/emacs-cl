;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 25, Environment.

(IN-PACKAGE "EMACS-CL")

(defun SLEEP (seconds)
  (sit-for (if (or (integerp seconds) (floatp seconds))
	       seconds
	       (FLOAT seconds))
	   0 t)
  ;; TODO: if sit-for didn't return t, sleep some more.
  nil)

(cl:defmacro TIME (form)
  (with-gensyms (start val end)
    `(LET* ((,start (GET-INTERNAL-REAL-TIME))
	    (,val (MULTIPLE-VALUE-LIST ,form))
	    (,end (GET-INTERNAL-REAL-TIME)))
       (PRINC "\nElapsed real time: ")
       (PRIN1 (,(INTERN "*" "CL")
	       (,(INTERN "-" "CL") ,end ,start)
	       ,(/ 1.0 INTERNAL-TIME-UNITS-PER-SECOND)))
       (PRINC " seconds")
       (VALUES-LIST ,val))))

(DEFCONSTANT INTERNAL-TIME-UNITS-PER-SECOND 1000000)

(defun GET-INTERNAL-REAL-TIME ()
  (let* ((time (current-time))
	 (high (first time))
	 (low (second time))
	 (microsec (third time)))
    (binary+ (binary* (binary+ (binary* high 65536) low) 1000000) microsec)))

;;; (defun GET-INTERNAL-RUN-TIME ())

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
