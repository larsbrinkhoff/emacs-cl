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

(defun DISASSEMBLE (fn)
  (when (or (symbolp fn) (setf-name-p fn))
    (setq fn (FDEFINITION fn)))
  (when (INTERPRETED-FUNCTION-P fn)
    nil)
  (disassemble fn)
  nil)

(defun* ROOM (&optional (x :default))
  (let* ((info (garbage-collect))
         (foo `("conses" "symbols" "misc" "string chars"
                "vector slots" "floats" "intervals" "strings"))
         (cons-info (first info))
         (sym-info (second info))
         (misc-info (third info))
         (used-string-chars (fourth info))
         (used-vector-slots (fifth info))
         (float-info (sixth info))
         (interval-info (seventh info))
         (string-info (eighth info)))
    (case x
      ((nil) nil)
      ((:default)
       (do ((i info (cdr i))
            (j foo (cdr j)))
           ((null i) nil)
         (PRINC (format "Used %s: " (car j)))
         (cond
           ((null (car i)))
           ((atom (car i))
            (PRINC (format "%d.\n" (car i))))
           (t
            (PRINC (format "%d, free %s: %d\n"
                           (caar i) (car j) (cdar i)))))))
      ((t)
       (ROOM)
       (PRINC "Consed so far:\n")
       (PRINC (format "%d conses,\n" cons-cells-consed))
       (PRINC (format "%d floats,\n" floats-consed))
       (PRINC (format "%d vector cells,\n" vector-cells-consed))
       (PRINC (format "%d symbols,\n" symbols-consed))
       (PRINC (format "%d string chars,\n" string-chars-consed))
       (PRINC (format "%d misc objects,\n" misc-objects-consed))
       (PRINC (format "%d intervals,\n" intervals-consed))
       (PRINC (format "%d strings\n" strings-consed))))))

(defun SHORT-SITE-NAME ()
  nil)

(defun LONG-SITE-NAME ()
  nil)

(defun MACHINE-INSTANCE ()
  (system-name))

(defun MACHINE-TYPE ()
  (subseq system-configuration 0 (position ?- system-configuration)))

(defun MACHINE-VERSION ()
  nil)

(defun SOFTWARE-TYPE ()
  (STRING system-type))

(defun SOFTWARE-VERSION ()
  nil)

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
