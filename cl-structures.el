;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 8, Structures.

(IN-PACKAGE "CL")

;;; A hash table keyed on a structure type (symbol).  The data is a
;;; list of types that are subtypes of the key type.
(defvar *structure-subtypes* (make-hash-table))

(defun strcat (&rest string-designators)
  (apply #'CONCATENATE 'STRING (mapcar #'STRING string-designators)))

(defun symcat (&rest string-designators)
  (nth-value 0 (INTERN (apply #'strcat string-designators))))

(defmacro* DEFSTRUCT (name &rest slots)
  (multiple-value-bind (name options) (if (consp name)
					  (values (first name) (rest name))
					  (values name nil))
    (let ((conc-name		(strcat name "-"))
	  (constructors		nil)
	  (no-constructor	nil)
	  (copier		(symcat "COPY-" name))
	  (include		nil)
	  (initial-offset	nil)
	  (named		nil)
	  (predicate		nil)
	  (print-object		nil)
	  (print-function	nil)
	  (type			nil)
	  (struct-size		nil))

      ;; Process structure options.
      (dolist (option options)
	(multiple-value-bind (name args) (if (atom option)
					     (values option nil)
					     (values (first option)
						     (rest option)))
	  (ecase name
	    (:conc-name		(setq conc-name (or (first args) "")))
	    (:constructor	(ecase (length args)
				  (0)
				  (1	(if (null (first args))
					    (setq no-constructor t)
					    (push (first args)
						  constructors)))
				  (2	(push args constructors))))
	    (:copier		(setq copier (first args)))
	    (:include		(setq include (rest args)))
	    (:initial-offset	(setq initial-offset (first args)))
	    (:named		(setq named t))
	    (:predicate		(setq predicate (first args)))
	    (:print-object	(setq print-object (first args)))
	    (:print-function	(setq print-function (first args)))
	    (:type		(setq type (first args))))))

      ;; Provide a default constructor if appropriate.
      (when (and (null constructors) (not no-constructor))
	(setq constructors (list (symcat "MAKE-" name))))

      ;; Calculate initial-offset and structure size.
      (when (and initial-offset (not type))
	(error ":initial-offset used without :type"))
      (unless initial-offset
	(setq initial-offset 0))
      (setq struct-size (+ initial-offset (length slots)))
      (unless (and type (not named))
	(incf struct-size))

      ;; Register the structure as a subtype of an included structure,
      ;; and provide a default predicate if appropriate.
      (when include
	(pushnew name (gethash (first include) *structure-subtypes*)))
      (when (and type (not named) predicate)
	(error))
      (unless predicate
	(setq predicate (symcat name "-P")))

      ;; Macro expansion.
      `(eval-when (:compile-toplevel :load-toplevel :execute)

	;; Constructors.
	,@(mapcar
	   (lambda (constructor)
	     `(defun* ,@(if (consp constructor)
			    `(,(first constructor) ,(second constructor))
			    `(,constructor (&key ,@slots)))
	       ,(ecase type
		  ((nil)
		   `(let ((object (make-vector ,struct-size ',name)))
		     ,@(let ((index initial-offset))
		         (mapcar (lambda (slot)
				   `(aset object ,(incf index) ,slot))
				 slots))
		      object))
		  (vector
		   `(let ((object (MAKE-ARRAY ,struct-size)))
		     ,@(let ((index (1- initial-offset)))
		         `(,@(when named
			       `((setf (AREF object ,(incf index) ',name))))
			   ,@(mapcar (lambda (slot)
				       `(setf (AREF object ,(incf index))
					      ,slot))
				     slots)))
		     object))
		  (list
		   `(list ,@(make-list initial-offset nil)
		          ,@(when named (list (list 'quote name)))
		          ,@slots)))))
	   constructors)

	;; Copier.
	,@(when copier
	   `((defun ,copier (object)
	       (copy-sequence object))))

	;; Predicate.
	,@(when predicate
	   `((defun ,predicate (object)
	       (TYPEP object ',name))))

	;; TYPEP.
	,@(unless (and type (not named))
	   `((define-typep (object ,name env)
	       ,(ecase type
		  ((nil)
		   `(and (vectorp object)
		     (let ((type (aref object 0)))
		       (or (eq type ',name)
			   (member type (gethash ',name
						 *structure-subtypes*))))))
		  (vector
		   `(and (VECTORP object)
		     (let ((type (AREF object ,initial-offset)))
		       (or (eq type ',name)
			   (member type (gethash ',name
						 *structure-subtypes*))))))
		  (list
		   `(and (listp object)
		     (let ((type (nth ,initial-offset object)))
		       (or (eq type ',name)
			   (member type (gethash ',name
						 *structure-subtypes*))))))))))

	;; Accessors.
	,@(let ((index initial-offset))
	    (when (or named (not type))
	      (incf index))
	    (mapcan (lambda (slot)
		      (let ((name (symcat conc-name slot)))
			(prog1
			  (ecase type
			    ((nil)
			     `((defun ,name (object)
				 (aref object ,index))
			       (defsetf ,name (object) (new)
				 (list 'aset object ,index new))))
			    (vector
			     `((defun ,name (object)
				 (AREF object ,index))
			       (defsetf ,name (object) (new)
				 (list 'setf (list 'AREF object ,index)
				       new))))
			    (list
			     `((defun ,name (object)
				 (nth ,index object))
			       (defsetf ,name (object) (new)
				 (list 'setf (list 'nth ,index object)
				       new)))))
			(incf index))))
		    slots))
	',name))))

(defun COPY-STRUCTURE (object)
  (copy-sequence object))
