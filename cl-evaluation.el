;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 3, Evaluation and Compilation.

(defvar *MACROEXPAND-HOOK* #'funcall)

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
	               (destructuring-bind ,lambda-list form
			 ,@body))))))

(defmacro* DEFMACRO (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (MACRO-FUNCTION ',name)
          (function* (lambda (form env)
	               (destructuring-bind ,lambda-list form
			 ,@body))))))

(defun MACRO-FUNCTION (name &optional env)
  (gethash name *macro-functions*))

(defsetf MACRO-FUNCTION (name &optional env) (fn)
  `(setf (gethash ,name *macro-functions*) ,fn))

(defun MACROEXPAND-1 (form &optional env)
  (cond
    ((CONSP form)
     (let ((fn (MACRO-FUNCTION (CAR form))))
       (if fn
	   (let ((new (funcall *macroexpand-hook* fn form env)))
	     (values form (not (eq form new))))
	   (values form nil))))
    ((symbolp form)
     (let ((fn (gethash form *symbol-macro-functions*)))
       (if fn
	   (values (funcall *macroexpand-hook* fn form env) t)
	   (values form nil))))
    (t
     (values form nil))))

(defun* MACROEXPAND (form &optional env)
  (let ((form form) expandedp)
    (loop
     (multiple-value-setq (form expandedp) (MACROEXPAND-1 form env))
     (unless expandedp
       (return-from MACROEXPAND form)))))
