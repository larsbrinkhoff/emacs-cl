;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 24, System Construction.

(IN-PACKAGE "EMACS-CL")

;;; TODO:
; (cl:defun COMPILE-FILE (input-file &key output-file
;				     (verbose *COMPILE-VERBOSE*)
; 				     (print *COMPILE-PRINT*)
; 				     external-format)
;   nil)

;;; TODO: Function COMPILE-FILE-PATHNAME

(cl:defun LOAD (filespec &key (verbose *LOAD-VERBOSE*) (print *LOAD-PRINT*)
		              if-does-not-exist external-format)
  (let ((*LOAD-PATHNAME* filespec)
	(*LOAD-TRUENAME* (TRUENAME filespec)))
    (load filespec)))

;;; TODO: Macro WITH-COMPILATION-UNIT

(defvar *FEATURES* (list (kw COMMON-LISP)
; not yet	         (kw ANSI-CL)
			 (kw EMACS-CL)))

(defvar *COMPILE-FILE-PATHNAME* nil)
(defvar *COMPILE-FILE-TRUENAME* nil)

(defvar *LOAD-PATHNAME* nil)
(defvar *LOAD-TRUENAME* nil)

(defvar *COMPILE-PRINT* nil)
(defvar *COMPILE-VERBOSE* nil)

(defvar *LOAD-PRINT* nil)
(defvar *LOAD-VERBOSE* nil)

(defvar *MODULES* nil)

(defun PROVIDE (name)
  (let ((string (STRING name)))
    (pushnew string *MODULES* :test #'STRING=)
    string))

(defun REQUIRE (name &optional pathnames)
  (let ((string (STRING name)))
    (unless (find string *MODULES* :test #'STRING=)
      (do-list-designator (file pathnames)
	(LOAD file)))))
