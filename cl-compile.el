;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements the compiler.

(require 'cl)

(defvar *registers* (list (gensym)))
(defvar *next-register* nil)
(defvar *variables* nil)
(defvar *bound* nil)
(defvar *unbound* nil)
(defvar *closure-slot* nil)

(defmacro* with-fresh-context (&body body)
  `(let ((*next-register* *registers*)
	 (*variables* (make-hash-table)))
    ,@body))

(defun symbol-register (sym)
  (gethash sym *variables*))

(defun setf-symbol-register (sym reg)
  (setf (gethash sym *variables*) reg))
(defsetf symbol-register setf-symbol-register)

(defun* new-symbol (sym &key special)
  (cond
    ((symbol-register sym)
     (error "symbol already defined: %S" sym))
    (special
     (setf (symbol-register sym) sym))
    (t
     (push sym *bound*)
     (setf (symbol-register sym) (new-register)))))

(defun new-register ()
  (prog1
      (car *next-register*)
    (when (null (cdr *next-register*))
      (setf (cdr *next-register*) (list (gensym))))
    (setf *next-register* (cdr *next-register*))))

(defun symbol-special-p (sym)
  (eq (symbol-register sym) sym))

(defun symbol-lexical-p (sym)
  (not (symbol-special-p sym)))

(defun cl:compile (name &optional definition)
  (when (null definition)
    (if (fboundp name)
	(setq definition (fdefinition name))
	(setq definition (symbol-macro name))))
  (with-fresh-context
    (let* ((compiled (compile-form definition))
	   (function (byte-compile compiled)))
      (print compiled)
      (when name
	(setf (fdefinition name) function))
      (values compiled nil nil))))

(defun* compile-form (form &key (values 1))
  ;;(setq form (macroexpand form))
  (cond
    ((symbolp form)
     (unless (zerop values)
       (case form
	 ((t nil) form)
	 (t (compile-variable form)))))
    ((atom form)
     (unless (zerop values)
       (compile-literal form)))
    ((get (first form) 'compiler)
     (funcall (get (first form) 'compiler) form))
    ((consp (first form))
     (unless (eq (caar form) 'lambda)
       (error))
     (compile-form `(funcall ,@form)))
    (t
     (compile-call form))))

(defvar count 0)

(defun compile-variable (sym)
  (if (and (not (find sym *bound*))
	   (not (assoc sym *unbound*))
	   (not (symbol-special-p sym)))
      (let ((reg `(aref closure ,(incf *closure-slot*))))
	(push (cons sym reg) *unbound*)
	reg)
      (let ((reg (symbol-register sym)))
	(or reg (error "undefined variable: %S" sym)))))

(defun compile-literal (literal)
  (cond
    ((or (symbolp literal) (consp literal))
     `(quote ,literal))
    (t
     literal)))

(defun compile-call (forms)
  (let ((fn (first forms))
	(args (cdr forms)))
    (compile-funcall `(funcall ',fn ,@args))))

(defun compile-funcall (forms)
  (let ((fn (second forms))
	(args (cddr forms))
	(output nil))
    (if (and (consp fn) (eq (first fn) 'quote) (symbolp (second fn)))
	(if (eq (second fn) 'funcall)
	    (compile-funcall `(funcall ,@args))
	    `(,(second fn) ,@(mapcar #'compile-form args)))
	`(let ((closure ,(compile-form fn)))
	  (if (vectorp closure)
	      (funcall (aref closure 0) ,@(mapcar #'compile-form args))
	      (funcall closure))))))

(defun compile-declare (forms)
  (dolist (declaration (cdr forms))
    (let ((name (or (and (atom declaration) declaration)
		    (first declaration)))
	  (args (and (consp declaration) (cdr declaration))))
      (case name
	(special
	 (dolist (sym args)
	   (setf (symbol-register sym) sym)))))))

;;; (setq f (mapcar (lambda (x)
;;; 		  (closure-lambda ()
;;; 		    (incf x)))
;;; 		'(0 42 99)))
;;;
;;; (defmacro* closure-lambda (lambda-list &body body)
;;;   `(let* ((loc (gensym))
;;; 	  (urk ',lambda-list)
;;; 	  (var (gensym))
;;; 	  (b ',body))
;;;     (set loc x)
;;;     `(lambda ,urk
;;;       (symbol-macrolet ((x ,loc))
;;; 	,@b))))

(defmacro define-compiler (operator &rest operands)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-block (forms)
  (let ((tag (second forms))
	(body (cddr forms)))
    (compile-forms body)))

(define-compiler (catch tag &rest body)
  `(catch ,(compile-form tag)
     ,(compile-forms body)))

(defun compile-catch (forms)
  (let ((tag (second forms))
	(body (cddr forms)))
    `(catch ,(compile-form tag)
      ,(compile-forms body))))

(defun compile-eval-when (forms)
  (let ((situations (second forms))
	(body (cddr forms)))
    (compile-forms body)))

(defun compile-flet (forms)
  (let ((functions (second forms))
	(body (cddr forms)))
    (compile-forms body)))

(define-compiler (function fn)
  (cond
    ((symbolp fn)
     `(function fn))
    ((atom fn)
     (error))
    ((eq (first fn) 'lambda)
     (compile-lambda fn))
    ((eq (first fn) 'setf)
     nil)
    (t
     (error)))

(defun compile-function (form)
  (let ((fn (second form)))
    (cond
      ((symbolp fn)
       `(function fn))
      ((atom fn)
       (error))
      ((eq (first fn) 'lambda)
       (compile-lambda fn))
      ((eq (first fn) 'setf)
       nil)
      (t
       (error)))))

(defun compile-go (form)
  (let ((tag (second form)))
    nil))

(define-compiler (if condition then &optional else)
  `(if ,(compile-form condition)
       ,(compile-form then)
       ,@(when else
	   (list (compile-form else)))))

(defun compile-if (form)
  `(if ,(compile-form (second form))
       ,(compile-form (third form))
       ,@(when (cdddr form)
	   (list (compile-form (fourth form))))))

(defun compile-labels (forms)
  (let ((functions (second forms))
	(body (cddr forms)))
    (compile-forms body)))

(defvar *compile-lambda* nil)

(defun* compile-lambda (form)
  (let ((vars (second form))
	(body (cddr form))
	(*bound* nil)
	(*closure-slot* 0))
    (dolist (sym vars)
      (new-symbol sym))
    (do* ((forms body (cdr forms))
	  (form #1=(first forms) #1#))
	 ((or (atom form) (not (eq (first form) 'declare)))
	  (let* ((*unbound* nil)
		 (body (compile-forms forms))
		 (lambda-expr
		  `(lambda ,(mapcar #'symbol-register vars) ,@body))
		 (compiled-expr (if *compile-lambda*
				    (byte-compile lambda-expr)
				    lambda-expr)))
	    (if *unbound*
		(let* ((closure (new-register)))
		  `(let ((,closure
			  (make-vector ,(1+ (length *unbound*)) nil)))
		    (aset ,closure 0 ,compiled-expr)
		    ,@(mappend (lambda (x)
				 (unless (symbol-special-p (car x))
				   `((aset ,closure ,(cadddr x)
				      ,(compile-variable (car x))))))
			       *unbound*)
		    ,closure))
		compiled-expr)))
      (compile-declare form))))

(defun mappend (fn &rest lists)
  (apply #'append
   (if (null (cdr lists))
       (mapcar fn (car lists))
       (cl-mapcar-many fn lists))))

(defun compile-let (form)
  (let ((bindings (second form))
	(body (cddr form))
	(vars nil)
	(inits nil)
	(*bound* *bound*))
    (dolist (sym bindings)
      (cond
	((consp sym)
	 (push (second sym) inits)
	 (setq sym (first sym)))
	(t
	 (push nil inits)))
      (new-symbol sym)
      (push sym vars))
    (do* ((forms body (cdr forms))
	  (form #1=(first forms) #1#))
	 ((or (atom form) (not (eq (first form) 'declare)))
	  `(let ,(cl:mapcar (lambda (var init)
			      `(,(symbol-register var)
				,(compile-form init)))
			    (nreverse vars) (nreverse inits))
	    ,@(compile-forms forms)))
      (compile-declare form))))

(defun compile-let* (form)
  (let ((bindings (second form))
	(body (cddr form))
	(vars nil)
	(inits nil)
	(*bound* *bound*))
    (dolist (sym bindings)
      (cond
	((consp sym)
	 (push (second sym) inits)
	 (setq sym (first sym)))
	(t
	 (push nil inits)))
      (new-symbol sym)
      (push sym vars))
    (do* ((forms body (cdr forms))
	  (form #1=(first forms) #1#))
	 ((or (atom form) (not (eq (first form) 'declare)))
	  `(let* ,(cl:mapcar (lambda (var init)
			       `(,(symbol-register var)
				 ,(compile-form init)))
			     (nreverse vars) (nreverse inits))
	    ,@(compile-forms forms)))
      (compile-declare form))))

(defun compile-load-time-value (form)
  nil)

(defun* compile-locally (forms)
  (do* ((forms (cdr forms) (cdr forms))
	(form #1=(first forms) #1#))
       ((or (atom form) (not (eq (first form) 'declare)))
	`(progn ,@(compile-forms forms)))
    (compile-declare form)))

(defun compile-macrolet (forms)
  (let ((macros (second forms))
	(body (cddr forms)))
    (compile-forms body)))

;;; (defun* compile-multiple-value-bind (forms)

;;; (defun* compile-multiple-value-call (forms)

;;; compile-multiple-value-prog1

(defun* compile-forms (forms)
  (if (null forms)
      nil
      (do* ((forms forms (cdr forms))
	    (form #1=(car forms) #1#)
	    (result))
	   ((null (cdr forms))
	    (push (compile-form form :values t) result)
	    (nreverse result))
	(let ((comp (compile-form form :values 0)))
	  (when comp
	    (push comp result))))))

(defun* compile-progn (forms)
  `(progn ,@(compile-forms (cdr forms))))

;;; compile-progv

(defun compile-quote (form)
  (compile-literal (second form)))

(defun compile-return-from (form)
  (let ((tag (second form)))
    (when (cddr form)
      (compile-form (third form)))))

(defun compile-setq (forms)
  (let ((output nil))
    (do ((forms (cdr forms) (cddr forms)))
	((null forms))
      (let* ((var (first forms))
	     (val (second forms))
	     (expanded (macroexpand var)))
	(push (if (symbolp expanded)
		  `(setq ,(symbol-register expanded)
		         ,(compile-form (second forms)))
		  (macroexpand
		   `(setf ,expanded 
		          ,(compile-form (second forms)))))
	      output)))
    `(progn ,@(nreverse output))))

(defun compile-symbol-macrolet (forms)
  (let ((macros (second forms))
	(body (cddr forms)))
    (compile-forms body)))

(defun compile-tagbody (forms)
  (dolist (form (cdr forms))
    (cond
      ((consp form)
       (compile-form form))
      ((or (integerp form) (symbolp form))
       nil)
      (t
       (error)))))

(defun compile-the (form)
  (compile-form (third form)))

(defun compile-throw (form)
  `(throw ,(compile-form (second form))))

(defun compile-unwind-protect (form)
  (let ((protected (second form))
	(cleanups (cddr form)))
    `(unwind-protect
        ,(compile-form protected)
      ,@(compile-forms cleanups))))

(put 'block			'compiler #'compile-block)
(put 'catch			'compiler #'compile-catch)
(put 'eval-when			'compiler #'compile-eval-when)
(put 'flet			'compiler #'compile-flet)
(put 'function			'compiler #'compile-function)
(put 'go			'compiler #'compile-go)
(put 'if			'compiler #'compile-if)
(put 'labels			'compiler #'compile-labels)
(put 'lambda			'compiler #'compile-lambda)
(put 'let			'compiler #'compile-let)
(put 'let*			'compiler #'compile-let*)
(put 'load-time-value		'compiler #'compile-load-time-value)
(put 'locally			'compiler #'compile-locally)
(put 'macrolet			'compiler #'compile-macrolet)
(put 'multiple-value-call	'compiler #'compile-multiple-value-call)
(put 'multiple-value-prog1	'compiler #'compile-multiple-value-prog1)
(put 'progn			'compiler #'compile-progn)
(put 'progv			'compiler #'compile-progv)
(put 'quote			'compiler #'compile-quote)
(put 'return-from		'compiler #'compile-return-from)
(put 'setq			'compiler #'compile-setq)
(put 'symbol-macrolet		'compiler #'compile-symbol-macrolet)
(put 'tagbody			'compiler #'compile-tagbody)
(put 'the			'compiler #'compile-the)
(put 'throw			'compiler #'compile-throw)
(put 'unwind-protect		'compiler #'compile-unwind-protect)
