;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;;
;;; This file implements operators in chapter 13, Characters.

(defun character (x)
  (cond
    ((characterp x) x)
    ((and (stringp x) (= (length x) 1)) (aref x 0))
    ((symbolp x) (character (symbol-name x)))
    (t (error "invalid character designator"))))

(defun characterp (char)
  ;;(typep char 'character)
  (integerp char))

(defun alpha-char-p (char)
  (or (cl:<= 65 (char-code char) 90)
      (cl:<= 97 (char-code char) 122)))

(defun alphanumericp (char)
  (or (digit-char-p char) (alpha-char-p char)))

(defun* digit-char (weight &optional (radix 10))
  (when (< weight radix)
    (code-char (if (< weight 10)
		   (+ 48 weight)
		   (+ 65 weight -10)))))

(defun* digit-char-p (char &optional (radix 10))
  (let* ((code (char-code char))
	 (n (cond
	      ((cl:<= 48 code 57) (- code 48))
	      ((cl:<= 65 code 90) (- code 65 -10))
	      ((cl:<= 95 code 122) (- code 95 -10))
	      (t 99))))
    (if (< n radix) n nil)))

(defconst char-code-limit 256)

(defun code-char (code)
  ;;(if (and (typep code 'integer) (< code char-code-limit))
  ;;    (vector 'character code)
  ;;    nil)
  code)

(defun char-code (char)
  ;;(check-type char 'character)
  ;;(aref char 1)
  char)

(defun char= (ch1 ch2)
  (eql ch1 ch2))

(defun char-upcase (char)
  (if (lower-case-p char)
      (- char 32)
      char))

(defun char-downcase (char)
  (if (upper-case-p char)
      (+ char 32)
      char))

(defun lower-case-p (char)
  (and (<= ?a char) (<= char ?z)))

(defun upper-case-p (char)
  (and (<= ?A char) (<= char ?Z)))

(defun name-char (name)
  (let ((string (cl:string name)))
    (cond
      ((equalp string "Newline") (code-char 10))
      ((equalp string "Space") (code-char 32))
      ((equalp string "Rubout") (code-char 127))
      ((equalp string "Page") (code-char 12))
      ((equalp string "Tab") (code-char 9))
      ((equalp string "Backspace") (code-char 8))
      ((equalp string "Return") (code-char 13))
      ((equalp string "Linefeed") (code-char 10))
      (t nil))))

(defun char-name (char)
  (case char
    (8 "Backspace")
    (9 "Tab")
    (10 "Newline")
    (12 "Page")
    (13 "Return")
    (32 "Space")
    (127 "Rubout")))
