;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 16, Strings.

(defun SIMPLE-STRING-P (object)
  (stringp object))

(defun CHAR (string index)
  (cond
    ((SIMPLE-STRING-P string)
     (SCHAR string index))
    ((STRINGP string)
     (SCHAR (aref string 2) index))
    (t
     (error "type error"))))

(defsetf CHAR (string index) (char)
  `(cond
    ((SIMPLE-STRING-P string)
     (setf (SCHAR ,string ,index) ,char))
    ((STRINGP string)
     (setf (SCHAR (aref ,string 2) ,index) ,char))
    (t
     (error "type error"))))

(defun SCHAR (string index)
  (CODE-CHAR (aref string index)))

(defsetf SCHAR (string index) (char)
  `(aset ,string ,index (CHAR-CODE ,char)))

(defun STRING (x)
  (cond
    ((STRINGP x)	x)
    ((SYMBOLP x)	(SYMBOL-NAME x))
    ((CHARACTERP x)	(MAKE-STRING 1 :initial-element x))
    (t			(error))))

;;; TODO: string-upcase, string-downcase, string-capitalize,
;;; nstring-upcase, nstring-downcase, nstring-capitalize

;;; TODO: string-trim, string-left-trim, string-right-trim

;;; TODO: string=, string/=, string<, string>, string<=, string>=,
;;; string-equal, string-not-equal, string-lessp, string-greaterp,
;;; string-not-greaterp, string-not-lessp

(defun STRINGP (object)
  (or (SIMPLE-STRING-P object)
      (vector-and-typep object 'string)))

(defun* MAKE-STRING (size &key initial-element element-type)
  (make-string size (if initial-element (CHAR-CODE initial-element) 0)))
