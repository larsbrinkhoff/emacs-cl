;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;;
;;; This file implements operators in chapter 18, Hash Tables.

(IN-PACKAGE "EMACS-CL")

(defun* MAKE-HASH-TABLE (&key test size rehash-size rehash-threshold)
  (make-hash-table :test test :size size))

(defun* GETHASH (key hash &optional default)
  (gethash key hash default))

(unless (fboundp 'puthash)
  (defun puthash (key value table)
    (setf (gethash key table) value)))

(DEFINE-SETF-EXPANDER GETHASH (key hash &optional default)
  (with-gensyms (keytemp hashtemp val)
    (VALUES (list keytemp hashtemp)
	    (list key hash)
	    (list val)
	    `(puthash ,keytemp ,val ,hashtemp)
	    `(GETHASH ,keytemp ,hashtemp))))
