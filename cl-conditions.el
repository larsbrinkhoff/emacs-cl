;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 9, Conditions.

(defvar *condition-constructors* (make-hash-table))

(defmacro* DEFINE-CONDITION (name parents slots &rest options)
  (with-gensyms (constructor)
    `(progn
       (DEFSTRUCT (,name
		   (:copier nil)
		   (:constructor ,constructor)
		   ,@(when parents
		       `((:include ,(first parents)))))
	 ,@slots)
       (puthash ',name ',constructor *condition-constructors*)
       ',name)))

(DEFINE-CONDITION CONDITION () ())

(DEFINE-CONDITION SIMPLE-CONDITION (CONDITION)
  (FORMAT-CONTROL FORMAT-ARGUMENTS))

(DEFINE-CONDITION SERIOUS-CONDITION (CONDITION) ())

(DEFINE-CONDITION ERROR (SERIOUS-CONDITION) ())

(DEFINE-CONDITION SIMPLE-ERROR (ERROR)
  (FORMAT-CONTROL FORMAT-ARGUMENTS))

(defvar *condition-handler-alist* nil)

(defun resolve-condition-designator (datum args default-type)
  (cond
    ((TYPEP datum 'CONDITION)
     datum)
    ((symbolp datum)
     (apply #'MAKE-CONDITION datum args))
    ((STRINGP datum)
     (MAKE-CONDITION default-type :FORMAT-CONTROL datum
		                  :FORMAT-ARGUMENTS args))
    (t
     (error "invalid condition designator"))))

(defun ERROR (datum &rest args)
  (let ((condition (resolve-condition-designator datum args 'SIMPLE-ERROR)))
    (SIGNAL condition)
    (INVOKE-DEBUGGER condition)))

(defun SIGNAL (datum &rest args)
  (let ((condition (resolve-condition-designator
		    datum args 'SIMPLE-CONDITION)))
    (when (TYPEP condition *BREAK-ON-SIGNALS*)
      (INVOKE-DEBUGGER condition))
    (let ((handler (ASSOC condition *condition-handler-alist* :test #'TYPEP)))
      (when handler
	(FUNCALL (cdr handler) condition)))
    nil))

(defun INVOKE-DEBUGGER (condition)
  (let* ((hook *DEBUGGER-HOOK*)
	 (*DEBUGGER-HOOK* nil))
    (when hook
      (FUNCALL hook condition hook))
    (debug)))

(defun BREAK (&optional format &rest args)
  (debug))

(defvar *DEBUGGER-HOOK* nil)

(defvar *BREAK-ON-SIGNALS* nil)

(cl:defmacro HANDLER-BIND (bindings &body forms)
  `(LET ((*condition-handler-alist*
	  (APPEND (LIST ,@(mapcar (lambda (binding)
				    `(CONS (QUOTE ,(first binding))
				           ,(second binding)))
				  bindings))
		  *condition-handler-alist*)))
     ,@forms))

(cl:defmacro IGNORE-ERRORS (&rest forms)
  (with-gensyms (block)
    `(BLOCK ,block
       (HANDLER-BIND ((ERROR (LAMBDA (c)
			       (RETURN-FROM ,block (VALUES nil c)))))
	 ,@forms))))

(defun MAKE-CONDITION (type &rest args)
  (let ((fn (gethash type *condition-constructors*)))
    (if fn
	(APPLY fn args)
	(error "no such condition type"))))
