;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 8, Structures.

(IN-PACKAGE "EMACS-CL")

;;; A hash table keyed on a structure name.  The data is a cons which
;;; car is a list of structure names that are subtypes of the key.
;;; The cdr is a list of slot descriptions in the form `(,name
;;; ,initval ,type ,read-only).
(defvar *structure-info* (make-hash-table))

(defun struct-subtypep (type1 type2)
  (or (eq type1 type2)
      (member type1 (car (gethash type2 *structure-info*)))))

(defun add-struct-subtype (struct sub)
  (maphash (lambda (key val)
	     (setf (car val) (delete sub val)))
	   *structure-info*)
  (push sub (car (gethash struct *structure-info*))))

(defun struct-slots (struct)
  (cdr (gethash struct *structure-info*)))

(defsetf struct-slots (struct) (slots)
  `(setf (gethash ,struct *structure-info*) (cons nil ,slots)))

(defun slot-name (slot)
  (first slot))

(defun slot-initval (slot)
  (second slot))

(defun slot-type (slot)
  (third slot))

(defun slot-read-only-p (slot)
  (fourth slot))

(defun param-with-default (param slots)
  (cond
    ((consp param)
     (if (> (length param) 1)
	 param
	 (param-with-default (first param) slots)))
    ((let ((slot (find param slots :key #'slot-name)))
       (when slot
	 (list param (slot-initval slot)))))
    (t
     param)))

(defun lambda-list-with-defaults (lambda-list slots)
  (let ((required t))
    (mapcar
     (lambda (param)
       (cond
	 ;; A lambda list keyword is passed
	 ;; through unchanged.
	 ((member param LAMBDA-LIST-KEYWORDS)
	  (setq required nil)
	  param)
	 ;; A required argument is passed
	 ;; through unchanged.
	 (required
	  param)
	 ;; If a non-required argument
	 ;; doesn't have a default value,
	 ;; supply the default value of the
	 ;; slot.
	 (t
	  (param-with-default param slots))))
     lambda-list)))


;;; The defstruct macro proper.
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
	  (struct-size		nil)
	  (unbound		nil))

      ;; Process structure options.
      (dolist (option options)
	(multiple-value-bind (name args) (if (atom option)
					     (values option nil)
					     (values (first option)
						     (rest option)))
	  (ecase name
	    (:conc-name		(setq conc-name (STRING (or (first args) ""))))
	    (:constructor	(ecase (length args)
				  (0)
				  (1	(if (null (first args))
					    (setq no-constructor t)
					    (push (first args)
						  constructors)))
				  (2	(push args constructors))))
	    (:copier		(setq copier (first args)))
	    (:include		(setq include args))
	    (:initial-offset	(setq initial-offset (first args)))
	    (:named		(setq named t))
	    (:predicate		(setq predicate (first args)))
	    (:print-object	(setq print-object (first args)))
	    (:print-function	(setq print-function (first args)))
	    (:type		(setq type (first args))))))

      ;; Provide a default constructor if appropriate.
      (when (and (null constructors) (not no-constructor))
	(setq constructors (list (symcat "MAKE-" name))))

      ;; Calculate the effective slot list.
      (setq slots
	    (mapcar (lambda (slot)
		      (cond
			((atom slot)
			 (list slot unbound t nil))
			((= (length slot) 1)
			 (list (first slot) unbound t nil))
			(t
			 (list (first slot) (second slot)
			       (getf (cddr slot) :type T)
			       (getf (cddr slot) :read-only)))))
		    slots))
      (when include
	(setq slots (append (struct-slots (first include)) slots)))
      (setf (struct-slots name) slots)

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
	(add-struct-subtype (first include) name))
      (when (and type (not named) predicate)
	(error))
      (unless predicate
	(setq predicate (symcat name "-P")))

      ;; Generate or process the lambda lists of the constructors.
      (setq constructors
	    (mapcar (lambda (constructor)
		      (if (atom constructor)
			  `(,constructor
			    ,(lambda-list-with-defaults
			      `(&key ,@(mapcar #'slot-name slots)) slots))
			  `(,(first constructor)
			    ,(lambda-list-with-defaults
			      (second constructor) slots))))
		    constructors))

      ;; Macro expansion.
      `(eval-when (:compile-toplevel :load-toplevel :execute)

	;; Constructors.
	,@(mapcar
	   (lambda (constructor)
	     (let ((slotps (mapcar (lambda (x) (gensym)) slots)))
	       `(defun* ,@constructor
		 ,(ecase type
		    ((nil)
		     `(let ((object (make-vector ,struct-size ',name)))
		       ,@(let ((index initial-offset))
			   (mapcar (lambda (slot)
				     `(aset object ,(incf index)
					    ,(slot-name slot)))
				   slots))
		       object))
		    (vector
		     `(let ((object (MAKE-ARRAY ,struct-size)))
		       ,@(let ((index (1- initial-offset)))
		           `(,@(when named
			         `((setf (AREF object ,(incf index) ',name))))
			     ,@(mapcar (lambda (slot)
					 `(setf (AREF object ,(incf index))
					        ,(slot-name slot)))
				       slots)))
		       object))
		    (list
		     `(list ,@(make-list initial-offset nil)
		            ,@(when named (list (list 'quote name)))
		            ,@(mapcar #'slot-name slots)))))))
	   constructors)

	;; Copier.
	,@(when copier
	   `((defun ,copier (object)
	       (copy-sequence object))))

	;; Predicate.
	,@(when predicate
	    (multiple-value-bind (type-predicate get-type)
		(ecase type
		  ((nil)      (values 'vectorp '(aref object 0)))
		  (vector     (values 'VECTORP `(AREF object ,initial-offset)))
		  (list	      (values 'listp   `(nth ,initial-offset object))))
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
		 (let ((name (symcat conc-name (slot-name slot))))
		   `((defun ,name (object) ,getter)
		     ,@(unless (slot-read-only-p slot)
		         `((defsetf ,name (object) (new) ,setter)))))))
	       slots))

	;; Finally, return structure name.
	',name))))

(defun COPY-STRUCTURE (object)
  (copy-sequence object))
