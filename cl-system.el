;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 24, System Construction.

(IN-PACKAGE "EMACS-CL")

;;; COMPILE-FILE is defined in cl-compile.el.

(defun elc-file (filename)
  (MERGE-PATHNAMES (MAKE-PATHNAME (kw TYPE) "elc") filename))

(cl:defun COMPILE-FILE-PATHNAME (input-file
				 &KEY (OUTPUT-FILE (elc-file input-file))
				 &ALLOW-OTHER-KEYS)
  (let* ((input (MERGE-PATHNAMES input-file)))
    (MERGE-PATHNAMES OUTPUT-FILE input)))

(cl:defun LOAD (file &KEY (VERBOSE *LOAD-VERBOSE*)
		     	  (PRINT *LOAD-PRINT*)
		     	  (IF-DOES-NOT-EXIST T)
		     	  (EXTERNAL-FORMAT (kw DEFAULT)))
  (let* ((*PACKAGE* *PACKAGE*)
	 (*READTABLE* *READTABLE*)
	 (*LOAD-PATHNAME* (MERGE-PATHNAMES file))
	 (*LOAD-TRUENAME* (TRUENAME *LOAD-PATHNAME*)))
    (cond
      ((STREAMP file)
       (let ((eof (gensym)))
;	 (do ((form #1=(READ file nil eof) #1#))
	 (do ((form (READ file nil eof) (READ file nil eof)))
	     ((eq form eof)
	      T)
	   (let ((val (EVAL form)))
	     (when PRINT
	       (PRINT val))))))
      ((or (STRINGP file) (PATHNAMEP file))
       (when VERBOSE
	 (PRINT "; Loading file ~S" (NAMESTRING file)))
       (if (STRING= (PATHNAME-TYPE file) "elc")
	   (load (NAMESTRING *LOAD-PATHNAME*))
	   (WITH-OPEN-FILE (stream file)
	     (LOAD stream (kw PRINT) PRINT)))
       T)
      (t
       (type-error file '(OR PATHNAME STRING STREAM))))))

(defvar *compilation-unit* nil)
(defvar *deferred-compilation-actions* nil)

(cl:defmacro WITH-COMPILATION-UNIT ((&key OVERRIDE) &body body)
  `(PROGN
     (LET ((*compilation-unit* T))
       ,@body)
     (WHEN (OR ,OVERRIDE (NOT *compilation-unit*))
       (DOLIST (fn (NREVERSE *deferred-compilation-actions*))
	 (FUNCALL fn))
       (SETQ *deferred-compilation-actions* nil))))

(defmacro* WITH-COMPILATION-UNIT ((&key OVERRIDE) &body body)
  `(progn
     (let ((*compilation-unit* T))
       ,@body)
     (when (or ,OVERRIDE (not *compilation-unit*))
       (dolist (fn (nreverse *deferred-compilation-actions*))
	 (FUNCALL fn))
       (setq *deferred-compilation-actions* nil))))

(defvar *FEATURES* (list ;; TODO: (kw ANSI-CL)
			 (kw EMACS-CL)
		         (kw COMMON-LISP)))

(let ((cons (ASSOC (emacs-version)
		   '(("GNU Emacs" .	(kw GNU-EMACS))
		     ("XEmacs" .	(kw XEMACS))
		     ("Hemlock" .	(kw HEMLOCK)))
		   (kw TEST) (lambda (version string)
			       (STRING= version string
					(kw END1) (LENGTH string))))))
  (push (if cons (cdr cons) (kw UNKNOWN-EMACS)) *FEATURES*))

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
