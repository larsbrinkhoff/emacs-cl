;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 25, Environment.

(IN-PACKAGE "EMACS-CL")

;;; TODO: Function DECODE-UNIVERSAL-TIME

;;; TODO: function ENCODE-UNIVERSAL-TIME

;;; TODO: Function GET-UNIVERSAL-TIME, GET-DECODED-TIME

(defun SLEEP (seconds)
  (sleep-for (if (or (integerp seconds) (floatp seconds))
		 seconds
		 (FLOAT seconds)))
  nil)

;;; TODO: Function APROPOS, APROPOS-LIST

;;; TODO: Function DESCRIBE

;;; TODO: Standard Generic Function DESCRIBE-OBJECT

;;; TODO: Macro TRACE, UNTRACE

;;; TODO: Macro STEP

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

;;; TODO: Function GET-INTERNAL-RUN-TIME

(defun DISASSEMBLE (fn)
  (when (or (symbolp fn) (setf-name-p fn))
    (setq fn (FDEFINITION fn)))
  (when (INTERPRETED-FUNCTION-P fn)
    nil)
  (disassemble fn)
  nil)

;;; TODO: Standard Generic Function DOCUMENTATION, (SETF DOCUMENTATION)

(cl:defun ROOM (&optional (x (kw DEFAULT)))
  (let* ((info (garbage-collect))
         (foo '("conses" "symbols" "misc" "string chars"
                "vector slots" "floats" "intervals" "strings"))
         (cons-info (first info))
         (sym-info (second info))
         (misc-info (third info))
         (used-string-chars (fourth info))
         (used-vector-slots (fifth info))
         (float-info (sixth info))
         (interval-info (seventh info))
         (string-info (eighth info)))
    (cond
      ((eq x nil))
      ((eq x (kw DEFAULT))
       (do ((i info (cdr i))
            (j foo (cdr j)))
           ((null i) nil)
         (PRINC (format "\nUsed %-13s:  " (car j)))
         (cond
           ((null (car i)))
           ((atom (car i))
            (PRINC (format "%7d." (car i))))
           (t
            (PRINC (format "%7d, free %-10s: %7d"
                           (caar i) (car j) (cdar i)))))))
      ((eq x 'T)
       (ROOM)
       (PRINC "\nConsed so far:")
       (PRINC (format "\n%d conses," cons-cells-consed))
       (PRINC (format "\n%d floats," floats-consed))
       (PRINC (format "\n%d vector cells," vector-cells-consed))
       (PRINC (format "\n%d symbols," symbols-consed))
       (PRINC (format "\n%d string chars," string-chars-consed))
       (PRINC (format "\n%d misc objects," misc-objects-consed))
       (PRINC (format "\n%d intervals" intervals-consed))
       (if (boundp 'strings-consed)
	   ;; Use symbol-value to shut up compiler warnings.
	   (PRINC (format "\n%d strings." (symbol-value 'strings-consed)))
	   (PRINC ".")))
      (t
       (type-error x `(OR BOOLEAN (EQL ,(kw DEFAULT))))))))

(defun ED (&optional x)
  (cond
    ((null x)
     (switch-to-buffer (generate-new-buffer "*ED*")))
    ((or (PATHNAMEP x) (STRINGP x))
     (load-file (NAMESTRING (PATHNAME x))))
    ((or (SYMBOLP x) (setf-name-p x))
     (find-tag (prin1-to-string x)))
    (t
     (type-error x '(OR NULL PATHNAME STRING SYMBOL
		        (CONS (EQ SETF) (CONS SYMBOL NULL)))))))

;;; TODO: Function INSPECT

;;; TODO: Function DRIBBLE

(defvar cl:- nil)
(defvar cl:+ nil)
(defvar ++ nil)
(defvar +++ nil)
(defvar cl:* nil)
(defvar ** nil)
(defvar *** nil)
(defvar cl:/ nil)
(defvar // nil)
(defvar /// nil)

(defun LISP-IMPLEMENTATION-TYPE ()
  "Emacs CL")

(defun LISP-IMPLEMENTATION-VERSION ()
  "0.1")

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

(defun USER-HOMEDIR-PATHNAME (&optional host)
  ;; TODO: look at host
  (PATHNAME "~/"))
