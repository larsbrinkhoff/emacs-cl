;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 8, Structures.

;;; TODO: defstruct

(defmacro* DEFSTRUCT (name &rest slots)
  (let ((options nil))
    (when (consp name)
      (setq options (rest name))
      (setq name (first name)))
    (let ((conc-name (CONCATENATE 'STRING (STRING name) "-"))
	  (constructors nil)
	  (copier nil)
	  (include nil)
	  (initial-offset nil)
	  (named nil)
	  (predicate (INTERN (CONCATENATE 'STRING (STRING name) "-P")))
	  (print-object nil)
	  (print-function nil)
	  (type nil))
      (dolist (option options)
	(multiple-value-bind (opt-name opt-args)
	    (if (atom option)
		(values option nil)
		(values (first option) (rest option)))
	  (ecase opt-name
	    (:conc-name		(setq conc-name (first opt-args)))
	    (:constructor	(push opt-args constructors))
	    (:copier		(setq copier (first opt-args)))
	    (:include		(setq include (rest opt-args))
	    (:initial-offset	(setq initial-offset (first opt-args))
	    (:named		(setq named t))
	    (:predicate		(setq predicate (first opt-args)))
	    (:print-object)
	    (:print-function)
	    (:type))))))
      (when (null constructors)
	(setq constructors
	      (INTERN (CONCATENATE 'STRING "MAKE-" (STRING name)))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	(defun ,(first constructors) ()
	  (let ((object (make-vector ,(1+ (length slots)) nil)))
	    (aset object 0 ',name)
	    object))
	(define-typep (object ,name env)
	  (or (and (vectorp object) (eq (aref object 0) ',name))
	      ,@(when include `((TYPEP object ',include)))))
	,@(let ((index 0))
	    (mapcan (lambda (slot)
		      (let ((name (nth-value 0 (INTERN (CONCATENATE
							'STRING conc-name
							(STRING slot))))))
			`((defun ,name (struct)
			    (aref struct ,(incf index)))
			  (defsetf ,name (struct) (new)
			    (list 'aset struct ,index new)))))
		    slots))
	',name))))

;;; TODO: copy-structure
