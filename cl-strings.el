;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 16, Strings.

(defun simple-string-p (object)
  (stringp object))

(defun char (string index)
  (cond
    ((simple-string-p string)
     (schar string index))
    ((cl:stringp string)
     (schar (aref string 2) index))
    (t
     (error "type error"))))

(defsetf char (string index) (char)
  `(cond
    ((simple-string-p string)
     (setf (schar ,string ,index) ,char))
    ((cl:stringp string)
     (setf (schar (aref ,string 2) ,index) ,char))
    (t
     (error "type error"))))

(defun schar (string index)
  (code-char (aref string index)))

(defsetf schar (string index) (char)
  `(aset ,string ,index (char-code ,char)))

(defun cl:string (x)
  (cond
    ((cl:stringp x)	x)
    ((symbolp x)	(symbol-name x))
    ((characterp x)	(cl:make-string 1 :initial-element x))
    (t			(error))))

;;; TODO: string-upcase, string-downcase, string-capitalize,
;;; nstring-upcase, nstring-downcase, nstring-capitalize

;;; TODO: string-trim, string-left-trim, string-right-trim

;;; TODO: string=, string/=, string<, string>, string<=, string>=,
;;; string-equal, string-not-equal, string-lessp, string-greaterp,
;;; string-not-greaterp, string-not-lessp

(defun cl:stringp (object)
  (or (simple-string-p object)
      (vector-and-typep object 'string)))

(defun* cl:make-string (size &key initial-element element-type)
  (make-string size (if initial-element (char-code initial-element) 0)))
