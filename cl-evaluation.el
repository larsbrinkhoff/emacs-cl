;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 3, Evaluation and Compilation.

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
          (function* (lambda (form env)
	               (destructuring-bind ,lambda-list (cdr form)
			 ,@body))))
    ',name))

(defmacro* DEFMACRO (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (MACRO-FUNCTION ',name)
          (function* (lambda (form env)
	               (destructuring-bind ,lambda-list (cdr form)
			 ,@body))))
    ',name))

(defun MACRO-FUNCTION (name &optional env)
  (gethash name *macro-functions*))

(defsetf MACRO-FUNCTION (name &optional env) (fn)
  `(setf (gethash ,name *macro-functions*) ,fn))

(defun MACROEXPAND-1 (form &optional env)
  (cond
    ((consp form)
     (let ((fn (MACRO-FUNCTION (car form))))
       (if fn
	   (let ((new (funcall *MACROEXPAND-HOOK* fn form env)))
	     (values new (not (eq form new))))
	   (values form nil))))
    ((symbolp form)
     (let ((fn (gethash form *symbol-macro-functions*)))
       (if fn
	   (values (funcall *MACROEXPAND-HOOK* fn form env) T)
	   (values form nil))))
    (t
     (values form nil))))

(defun* MACROEXPAND (form &optional env)
  (let ((form form) (expanded-p nil) exp)
    (loop
     (multiple-value-setq (form exp) (MACROEXPAND-1 form env))
     (if exp
	 (setq expanded-p T)
	 (return-from MACROEXPAND (values form expanded-p))))))

(defmacro* DEFINE-SYMBOL-MACRO (symbol expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (gethash ',symbol *symbol-macro-functions*)
          (function* (lambda (form env) ',expansion)))
    ',symbol))

;;; TODO: symbol-macrolet

(defvar *MACROEXPAND-HOOK* #'funcall)

;;;

(defun SPECIAL-OPERATOR-P (symbol)
  (member symbol
	  '(BLOCK CATCH EVAL-WHEN FLET FUNCTION GO IF LABELS LET LET*
	    LOAD-TIME-VALUE LOCALLY MACROLET MULTIPLE-VALUE-CALL
	    MULTIPLE-VALUE-PROG1 PROGN PROGV QUOTE RETURN-FROM SETQ
	    SYMBOL-MACROLET TAGBODY THE THROW UNWIND-PROTECT)))

(defun CONSTANTP (form)
  (cond
    ((atom form)
     (cond
       ((KEYWORDP form)		T)
       ((symbolp form)		(member form *constants*))
       (t			T)))
    (t				(eq (first form) 'QUOTE))))

(defun EVAL (form)
  ;(setq form (nth-value 0 (MACROEXPAND form)))
  (cond
    ((SYMBOLP form)
     (SYMBOL-VALUE form))
    ((ATOM form)
     form)
    (t
     (case (first form)
       (IF
	(if (EVAL (second form))
	    (third form)
	    (fourth form)))
       (QUOTE
	(second form))
       (SETQ
	(set (second form) (EVAL (third form))))
       (t
	(apply (symbol-function (first form))
	       (mapcar #'EVAL (rest form))))))))
