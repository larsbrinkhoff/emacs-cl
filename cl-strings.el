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

(defun schar (string index)
  (code-char (aref string index)))

(defun setf-char (string index new)
  (check-type new 'character)
  (cond
    ((simple-string-p string)
     (setf (schar string index) new))
    ((cl:stringp string)
     (setf (schar (aref string 2) index) new))
    (t
     (error "type error"))))

(defun setf-schar (string index new)
  (check-type new 'character)
  (aset string index (char-code new)))

(defsetf schar setf-schar)
(defsetf char setf-char)

(defun cl:string (x)
  (cond
    ((cl:stringp x)	x)
    ((symbolp x)	(symbol-name x))
    ((characterp x)	(cl:make-string 1 :initial-element x))
    (t			(error))))

(defun cl:stringp (object)
  (or (simple-string-p object)
      (and (vectorp object) (eq (aref object 0) 'string))))

(defun* cl:make-string (size &key initial-element element-type)
  (make-string size (if initial-element (char-code initial-element) 0)))
