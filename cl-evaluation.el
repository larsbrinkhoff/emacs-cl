;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 3, Evaluation and Compilation.

(IN-PACKAGE "EMACS-CL")

;;; Assigned later in populate-packages.
(defvar *global-environment* nil)

(defvar *compiler-macro-functions* (make-hash-table))

(defvar *macro-functions* (make-hash-table))

(defvar *symbol-macro-functions* (make-hash-table))

(defun COMPILER-MACRO-FUNCTION (name &optional env)
  (gethash name *compiler-macro-functions*))

(defsetf COMPILER-MACRO-FUNCTION (name &optional env) (fn)
  `(setf (gethash ,name *compiler-macro-functions*) ,fn))

(defmacro* DEFINE-COMPILER-MACRO (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (COMPILER-MACRO-FUNCTION ',name)
          (cl:lambda (form env)
	    (destructuring-bind ,lambda-list (cdr form)
	      ,@body)))
    ',name))

;;; Redefined later in cl-eval.el.
(defun lexical-function (name env)
  nil)

(defun MACRO-FUNCTION (name &optional env)
  (when (null env)
    (setq env *global-environment*))
  (let ((fn (lexical-function name env)))
    (or fn (gethash name *macro-functions*))))

(defsetf MACRO-FUNCTION (name &optional env) (fn)
  `(if (null ,env)
       (setf (gethash ,name *macro-functions*) ,fn)
       (set-local-macro ,name ,fn ,env)))

(defmacro* cl:defmacro (name lambda-list &body body)
  `(progn
     (setf (MACRO-FUNCTION ',name)
           (cl:lambda (form env)
	     (destructuring-bind ,lambda-list (cdr form)
	       ,@body)))
    ',name))

(cl:defmacro DEFMACRO (name lambda-list &body body)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
      (SETF (MACRO-FUNCTION (QUOTE ,name))
	    (LAMBDA (form env)
	      (DESTRUCTURING-BIND ,lambda-list (CDR form)
		,@body)))
      (QUOTE ,name)))

(cl:defmacro LAMBDA (lambda-list &body body)
  `(FUNCTION (LAMBDA ,lambda-list ,@body)))

;;; TODO: COMPILE

(defun MACROEXPAND-1 (form &optional env)
  (cond
    ((consp form)
     (let ((fn (MACRO-FUNCTION (car form) env)))
       (if fn
	   (let ((new (FUNCALL *MACROEXPAND-HOOK* fn form env)))
	     (VALUES new (not (eq form new))))
	   (VALUES form nil))))
    ((symbolp form)
     (let ((fn (gethash form *symbol-macro-functions*)))
       (if fn
	   (VALUES (funcall *MACROEXPAND-HOOK* fn form env) T)
	   (VALUES form nil))))
    (t
     (VALUES form nil))))

(defun* MACROEXPAND (form &optional env)
  (let ((form form) (expanded-p nil) exp)
    (loop
     (MULTIPLE-VALUE-SETQ (form exp) (MACROEXPAND-1 form env))
     (if exp
	 (setq expanded-p T)
	 (return-from MACROEXPAND (VALUES form expanded-p))))))

(defmacro* DEFINE-SYMBOL-MACRO (symbol expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',symbol *symbol-macro-functions*)
           (cl:lambda (form env) ',expansion))
     ',symbol))

(cl:defmacro DEFINE-SYMBOL-MACRO (symbol expansion)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (puthash (QUOTE ,symbol) (LAMBDA (form env) (QUOTE ,expansion))
              *symbol-macro-functions*)
     (QUOTE ,symbol)))

;;; TODO: SYMBOL-MACROLET

(defvar *MACROEXPAND-HOOK* 'FUNCALL)

(defvar *declarations* '(IGNORE IGNORABLE DYNAMIC-EXTENT TYPE INLINE
			 NOTINLINE FTYPE DECLARATION OPTIMIZE SPECIAL))

(defun PROCLAIM (declaration)
  (unless (and (consp declaration)
	       (memq (car declaration) *declarations*))
    (type-error declaration `(CONS (MEMBER ,@*declarations*) LIST)))
  (case (car declaration)
    (SPECIAL)
    (DECLARATION
     (dolist (d (rest declaration))
       (pushnew d *declarations*))))
  nil)

(cl:defmacro DECLAIM (&rest declarations)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     ,@(mapcar (lambda (decl) `(PROCLAIM (QUOTE ,decl)))
	       declarations)))

(defun SPECIAL-OPERATOR-P (symbol)
  (member symbol
	  '(BLOCK CATCH EVAL-WHEN FLET FUNCTION GO IF LABELS LET LET*
	    LOAD-TIME-VALUE LOCALLY MACROLET MULTIPLE-VALUE-CALL
	    MULTIPLE-VALUE-PROG1 PROGN PROGV QUOTE RETURN-FROM SETQ
	    SYMBOL-MACROLET TAGBODY THE THROW UNWIND-PROTECT)))

(defun CONSTANTP (form &optional env)
  (unless env
    (setq env *global-environment*))
  (cond
    ((atom form)
     (cond
       ((KEYWORDP form)		T)
       ((symbolp form)		(member form *constants*))
       (t			T)))
    (t				(eq (first form) 'QUOTE))))
