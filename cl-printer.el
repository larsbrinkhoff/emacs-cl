;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 14, Conses.

(defun terpri ()
  (princ "\n"))

;;; Ad-hoc unexensible.
(defun cl:print (object &optional stream-designator)
  (let ((stream (resolve-output-stream-designator stream-designator)))
    (cond
      ((or (integerp object)
	   (floatp object)
	   (symbolp object)
	   (stringp object))
       (princ object))
      ((characterp object)
       (princ "#\\")
       (princ (or (char-name object)
		  (string (char-code object)))))
      ((cl::bignump object)
       (when (cl:minusp object)
	 (princ "-")
	 (setq object (cl:- object)))
       (princ "#x")
       (let ((start t))
	 (dotimes (i (1- (length object)))
	   (let ((num (aref object (- (length object) i 1))))
	     (dotimes (j 7)
	       (let ((n (logand (ash num (* -4 (- 6 j))) 15)))
		 (unless (and (zerop n) start)
		   (setq start nil)
		   (princ (string (aref "0123456789ABCDEF" n))))))))))
      ((bit-vector-p object)
       (princ "#*")
       (dotimes (i (cl:length object))
	 (princ (AREF object i))))
      ((cl:stringp object)
       (print (copy-seq object)))
      ((cl:vectorp object)
       (princ "#(")
       (dotimes (i (cl:length object))
	 (cl:print (AREF object i))
	 (when (< (1+ i) (cl:length object))
	   (princ " ")))
       (princ ")"))
      (t
       (error))))
  object)
