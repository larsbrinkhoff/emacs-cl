;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 8, Structures.

(IN-PACKAGE "CL")

;;; A hash table keyed on a structure type (symbol).  The data is a
;;; list of types that are subtypes of the key type.
(defvar *structure-subtypes* (make-hash-table))

(defun struct-subtypep (type1 type2)
  (or (eq type1 type2)
      (member type1 (gethash type2 *structure-subtypes*))))

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
	    (multiple-value-bind (type-predicate get-type)
		(ecase type
		  ((nil)	(values 'vectorp
					'(aref object 0)))
		  (vector	(values 'VECTORP
					`(AREF object ,initial-offset)))
		  (list		(values 'listp
					`(nth ,initial-offset object))))
	      `((defun ,predicate (object)
		  (and (,type-predicate object)
		       (struct-subtypep ,get-type ',name))))))

	;; TYPEP.
	,@(unless type
	   `((define-typep (object ,name env)
	       (and (vectorp object)
		    (struct-subtypep (aref object 0) ',name)))))

	;; Accessors.
	,@(let ((index initial-offset))
	    (when (or named (not type))
	      (incf index))
	    (mappend
	     (lambda (slot)
	       (multiple-value-bind (getter setter)
		   (ecase type
		     ((nil)
		      (values `(aref object ,index)
			      `(list 'aset object ,index new)))
		     (vector
		      (values `(AREF object ,index)
			      `(list 'setf (list 'AREF object ,index) new)))
		     (list
		      (values `(nth ,index object)
			      `(list 'setf (list 'nth ,index object) new))))
		 (incf index)
		 (let ((name (symcat conc-name slot)))
		   `((defun ,name (object) ,getter)
		     (defsetf ,name (object) (new) ,setter)))))
	       slots))

	;; Finally, return structure name.
	',name))))

(defun COPY-STRUCTURE (object)
  (copy-sequence object))
