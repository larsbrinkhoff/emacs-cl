;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements EVAL and environment objects.

(IN-PACKAGE "EMACS-CL")

(defvar *special-operator-evaluators* (make-hash-table))

(defmacro* define-special-operator (name (&rest args) env &body body)
  `(setf (gethash ',name *special-operator-evaluators*)
         (function* (lambda (,env ,@args) ,@body))))

(defvar *global-environment* nil)


;;; Definitions for all special operators follows.

;;; TODO: BLOCK

(define-special-operator CATCH (tag &rest forms) env
  (catch (eval-with-env tag env)
    (let (lastval)
      (dolist (form forms lastval)
	(setq lastval (eval-with-env form env))))))

;;; TODO: EVAL-WHEN

;;; TODO: FLET

(define-special-operator FUNCTION (form) env
  (VALUES
    (cond
      ((SYMBOLP form)
       (SYMBOL-FUNCTION form))
      ((ATOM form)
       (error "syntax error"))
      ((case (first form)
	 (LAMBDA
	     (enclose form env))
	 (SETF
	  (FDEFINITION form))
	 (t
	  (error "syntax error")))))))

;;; TODO: GO

(define-special-operator IF (condition then &optional else) env
  (if (eval-with-env condition env)
      (eval-with-env then env)
      (eval-with-env else env)))

;;; TODO: LABELS

(define-special-operator LET (bindings &rest forms) env
  (let ((new-env
	 (augment-environment env
	  :variable (mapcar (lambda (binding) (if (symbolp binding)
						  binding
						  (car binding)))
			    bindings)))
	(lastval nil))
    (dolist (binding bindings)
      (if (symbolp binding)
	  (setf (lexical-value binding new-env) nil)
	  (setf (lexical-value (first binding) new-env)
		(eval-with-env (second binding) env))))
    (dolist (form forms (VALUES lastval))
      (setq lastval (eval-with-env form new-env)))))

(define-special-operator LET* (bindings &rest forms) env
  (dolist (binding bindings)
    (if (symbolp binding)
	(setf (lexical-value binding env) nil
	      env (augment-environment env :variable (list binding)))
	(setf (lexical-value (first binding) env)
	      (eval-with-env (second binding) env)
	      env (augment-environment env :variable (list (first binding))))))
  (let ((lastval nil))
    (dolist (form forms (VALUES lastval))
      (setq lastval (eval-with-env form env)))))

;;; TODO: LOAD-TIME-VALUE

;;; TODO: LOCALLY

;;; TODO: MACROLET

;;; TODO: MULTIPLE-VALUE-CALL

;;; TODO: MULTIPLE-VALUE-PROG1

(define-special-operator PROGN (&rest forms) env
  (let (lastval)
    (dolist (form forms (VALUES lastval))
      (setq lastval (eval-with-env form env)))))

;;; TODO: PROGV

(define-special-operator QUOTE (form) env
  (VALUES form))

;;; TODO: RETURN-FROM

(define-special-operator SETQ (&rest forms) env
  (when (oddp (length forms))
    (error "syntax error"))
  (do* (lastval
	(forms forms (cddr forms))
	(var (first forms))
	(val (eval-with-env (second forms) env)))
       ((null forms)
	(VALUES lastval))
    (setq lastval
	  (ecase (nth-value 0 (variable-information var env))
	    ((nil)		(error "unbound variable %s" form))
	    (:special		(set var val))
	    (:lexical		(setf (lexical-value var env) val))
	    (:symbol-macro	(error "shouldn't happen"))
	    (:constant		(error "setting constant"))))))

;;; TODO: SYMBOL-MACROLET

;;; TODO: TAGBODY

(define-special-operator THE (type form) env
  (eval-with-env form env))

(define-special-operator THROW (tag form) env
  (throw (eval-with-env tag env) (eval-with-env form env)))

(define-special-operator UNWIND-PROTECT (protected &rest cleanups) env
  (unwind-protect (eval-with-env protected env)
    (dolist (form cleanups)
      (eval-with-env form env))))



(defun variable-information (var &optional env)
  (unless env
    (setq env *global-environment*))
  (let ((info (assoc var (aref env 1))))
    (when (and (null info) (boundp var))
      (setq info (if (CONSTANTP var env)
		     (cons var :constant)
		     (cons var :special))))
    (values (cdr-safe info)
	    (member var (aref env 2))
	    nil)))

(defun lexical-value (var env)
  (cdr (assoc var (aref env 3))))

(defsetf lexical-value (var env) (val)
  `(let ((cons (assoc ,var (aref ,env 3))))
    (if cons
	(setf (cdr cons) ,val)
	(progn (aset ,env 3 (acons ,var ,val (aref ,env 3)))
	       ,val))))

(defun function-information (fn &optional env)
  (unless env
    (setq env *global-environment*))
  (values (assoc fn (aref env 4))
	  (member fn (aref env 5))
	  nil))

(defun* augment-environment (env &key variable symbol-macro function
				      macro declare)
  (unless env
    (setq env *global-environment*))
  (let ((var-info (aref env 1))
	(var-local (aref env 2))
	(fn-info (aref env 4))
	(fn-local (aref env 5)))
    (setq var-info (reduce (lambda (env var) (acons var :lexical env))
			   variable
			   :initial-value var-info))
    (setq var-info (reduce (lambda (env var) (acons var :symbol-macro env))
			   symbol-macro
			   :initial-value var-info))
    (setq var-local (append variable var-local))
    (setq fn-info (reduce (lambda (fn var) (acons var :function env))
			  function
			  :initial-value fn-info))
    (setq fn-info (reduce (lambda (mac var) (acons mac :macro env))
			  macro
			  :initial-value fn-info))
    (setq fn-local (append function fn-local))
  (vector 'environment var-info var-local (aref env 3) fn-info fn-local)))

(defun enclose (lambda-exp &optional env)
  (unless env
    (setq env *global-environment*))
  (vector 'interpreted-function lambda-exp env))

(defun INTERPRETED-FUNCTION-P (object)
  (vector-and-typep object 'interpreted-function))



(defun eval-lambda-form (form env &optional eval-args)
  (let ((new-env
	 (augment-environment env
	  :variable (mappend (lambda (var)
			       (unless (member var LAMBDA-LIST-KEYWORDS)
				 (list var)))
			     (cadar form))))
	(lastval nil)
	(args (rest form))
	(body (cddar form)))
    (dolist (var (cadar form))
      (unless (member var LAMBDA-LIST-KEYWORDS)
	(setf (lexical-value var new-env)
	      (if eval-args
		  (eval-with-env (pop args) env)
		  (pop args)))))
    (dolist (form body lastval)
      (setq lastval (eval-with-env form new-env)))))

(defun eval-with-env (form env)
  (unless env
    (setq env *global-environment*))
  (setq form (NTH-VALUE 0 (MACROEXPAND form)))
  (cond
    ((SYMBOLP form)
     (VALUES
       (ecase (nth-value 0 (variable-information form env))
	 ((nil)		(error "unbound variable %s" form))
	 (:special	(SYMBOL-VALUE form))
	 (:lexical	(lexical-value form env))
	 (:symbol-macro	(error "shouldn't happen"))
	 (:constant	(SYMBOL-VALUE form)))))
    ((ATOM form)
     (VALUES form))
    (t
     (if (consp (car form))
	 (if (eq (caar form) 'LAMBDA)
	     (eval-lambda-form form env t)
	     (error "syntax error"))
	 (let ((fn (gethash (first form) *special-operator-evaluators*)))
	   (if fn
	       (apply fn env (rest form))
	       (let ((fn (symbol-function (first form))))
		 (if (listp fn)
		     ;; Special hack for interpreted Emacs Lisp function.
		     (apply fn (mapcar (lambda (arg) (eval-with-env arg env))
				       (rest form)))
		     (APPLY fn (mapcar (lambda (arg) (eval-with-env arg env))
				       (rest form)))))))))))

(defun EVAL (form)
  (eval-with-env form nil))
