;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements the FORMAT function from chapter 22, Printer.

(IN-PACKAGE "EMACS-CL")

(defun FORMAT (stream-designator format &rest args)
  (let ((stream (or (and (eq stream-designator t) *STANDARD-OUTPUT*)
		    stream-designator
		    (MAKE-STRING-OUTPUT-STREAM)))
	(i 0))
    (while (< i (LENGTH format))
      (let ((char (CHAR format i)))
	(if (eq (CHAR-CODE char) 126)
	    (case (CHAR-CODE (CHAR format (incf i)))
	      (37	(TERPRI))
	      (65	(PRINC (pop args) stream))
	      (68	(PRIN1 (pop args) stream)))
	    (WRITE-CHAR char stream)))
      (incf i))
    (if stream-designator
	nil
	(GET-OUTPUT-STREAM-STRING stream))))
