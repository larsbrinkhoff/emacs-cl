;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 9, Conditions.

(defvar *condition-handler-alist* nil)

(defun ERROR (datum &rest args)
  (SIGNAL datum args)
  (INVOKE-DEBUGGER))

(defun SIGNAL (datum &rest args)
  (when (TYPEP datum *BREAK-ON-SIGNALS*)
    (INVOKE-DEBUGGER))
  (let ((handler (assoc datum *condition-handler-alist*)))
    (when handler
      (FUNCALL (cdr handler) nil)))
  nil)

(defvar *BREAK-ON-SIGNALS* nil)

(cl:defmacro HANDLER-BIND (bindings &body forms)
  `(LET ((*condition-handler-alist*
	  (APPEND (LIST ,@(mapcar (lambda (binding)
				    `(CONS (QUOTE ,(first binding))
				           ,(second binding)))
				  bindings))
		  *condition-handler-alist*)))
     ,@forms))
