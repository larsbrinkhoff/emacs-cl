;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements EVAL and environment objects.

(IN-PACKAGE "EMACS-CL")

(defvar *special-operator-evaluators* (make-hash-table))

(defmacro* define-special-operator (name (&rest args) env &body body)
  `(setf (gethash ',name *special-operator-evaluators*)
         (function* (lambda (,env ,@args) ,@body))))

;;; Redefined later in populate-packages.
(defvar *global-environment* nil)


;;; Definitions for all special operators follows.

(define-special-operator BLOCK (tag &rest forms) env
  (let* (lastval
	 (catch-tag (gensym))
	 (new-env (augment-environment env :block (cons tag catch-tag))))
    (catch catch-tag
      (dolist (form forms lastval)
	(setq lastval (eval-with-env form new-env))))))

(define-special-operator CATCH (tag &rest forms) env
  (catch (eval-with-env tag env)
    (let (lastval)
      (dolist (form forms lastval)
	(setq lastval (eval-with-env form env))))))

(define-special-operator EVAL-WHEN (situations &body body) env
  ;; TODO: Proper implementation.
  (let (lastval)
    (dolist (form body lastval)
      (setq lastval (eval-with-env form env)))))

(define-special-operator FLET (fns &rest forms) env
  (let ((new-env (augment-environment env :function (mapcar #'first fns)))
	(lastval nil))
    (dolist (fn fns)
      (setf (lexical-function (first fn) new-env)
	    (enclose `(LAMBDA ,@(rest fn)) env)))
    (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
      (dolist (form body lastval)
	(setq lastval (eval-with-env form new-env))))))

(defun lexical-or-global-function (name env)
  (multiple-value-bind (type localp decl) (function-information name env)
    (ecase type
      ((nil)		(error "unbound function %s" name))
      (:function	(if localp
			    (lexical-function name env)
			    (FDEFINITION name)))
      (t		(error "syntax error")))))

(define-special-operator FUNCTION (form) env
  (VALUES
    (cond
      ((SYMBOLP form)		(lexical-or-global-function form env))
      ((ATOM form)		(error "syntax error"))
      ((case (first form)
	 (LAMBDA		(enclose form env))
	 (SETF			(lexical-or-global-function form env))
	 (t			(error "syntax error")))))))

(define-special-operator GO (tag) env
  (let ((info (tagbody-information tag env)))
    (if info
	(throw info tag)
	(error "syntax error"))))

(define-special-operator IF (condition then &optional else) env
  (if (eval-with-env condition env)
      (eval-with-env then env)
      (eval-with-env else env)))

(define-special-operator LABELS (fns &rest forms) env
  (let ((new-env (augment-environment env :function (mapcar #'first fns)))
	(lastval nil))
    (dolist (fn fns)
      (setf (lexical-function (first fn) new-env)
	    (enclose `(LAMBDA ,@(rest fn)) new-env)))
    (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
      (dolist (form body lastval)
	(setq lastval (eval-with-env form new-env))))))

(define-special-operator LET (bindings &rest forms) env
  (let* ((vars (mappend (lambda (binding)
			  (let ((var (if (symbolp binding)
					 binding
					 (car binding))))
			    (unless (eq (nth-value 0 (variable-information var env))
					:special)
			      (list var))))
			bindings))
	 (new-env (if vars (augment-environment env :variable vars) env))
	 (lastval nil)
	 (oldvals nil))
    (dolist (binding bindings)
      (multiple-value-bind (var val) (if (symbolp binding)
					 (values binding nil)
					 (values (first binding)
						 (second binding)))
	(if (member var vars)
	    (setf (lexical-value var new-env) (eval-with-env val env))
	    (progn
	      (push (symbol-value var) oldvals)
	      (setf (symbol-value var) (eval-with-env val env))))))
    (unwind-protect
	 (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
	   (dolist (form body lastval)
	     (setq lastval (eval-with-env form new-env))))
      (dolist (binding bindings)
	(multiple-value-bind (var val) (if (symbolp binding)
					   (values binding nil)
					   (values (first binding)
						   (second binding)))
	  (unless (member var vars)
	    (setf (symbol-value var) (pop oldvals))))))))

(define-special-operator LET* (bindings &rest forms) env
  (let* ((vars (mappend (lambda (binding)
			  (let ((var (if (symbolp binding)
					 binding
					 (car binding))))
			    (unless (eq (nth-value 0 (variable-information var env))
					:special)
			      (list var))))
			bindings))
	 (new-env (if vars (augment-environment env :variable vars) env))
	 (lastval nil)
	 (oldvals nil))
    (dolist (binding bindings)
      (multiple-value-bind (var val) (if (symbolp binding)
					 (values binding nil)
					 (values (first binding)
						 (second binding)))
	(if (member var vars)
	    (setf (lexical-value var new-env) (eval-with-env val new-env))
	    (progn
	      (push (symbol-value var) oldvals)
	      (setf (symbol-value var) (eval-with-env val env))))))
    (unwind-protect
	 (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
	   (dolist (form body lastval)
	     (setq lastval (eval-with-env form new-env))))
      (dolist (binding bindings)
	(multiple-value-bind (var val) (if (symbolp binding)
					   (values binding nil)
					   (values (first binding)
						   (second binding)))
	  (unless (member var vars)
	    (setf (symbol-value var) (pop oldvals))))))))

;;; TODO: LOAD-TIME-VALUE

(define-special-operator LOCALLY (&rest forms) env
  (let (lastval)
    (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
      (dolist (form body lastval)
	(setq lastval (eval-with-env form env))))))

(define-special-operator MACROLET (macros &rest forms) env
  (let ((new-env (augment-environment env :macro (mapcar #'first macros)))
	(lastval nil))
    (dolist (macro macros)
      (setf (MACRO-FUNCTION (first macro) new-env)
	    (enclose `(LAMBDA (form env)
		        ;; TODO: destructuring-bind
		        (APPLY (LAMBDA ,@(rest macro)) (CDR form))))))
    (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
      (dolist (form body lastval)
	(setq lastval (eval-with-env form new-env))))))

(define-special-operator MULTIPLE-VALUE-CALL (fn &rest forms) env
  (let ((values nil))
    (dolist (form forms)
      (setq values (append values
			   (MULTIPLE-VALUE-LIST (eval-with-env form env)))))
    (APPLY (eval-with-env fn env) values)))

(define-special-operator MULTIPLE-VALUE-PROG1 (form &rest forms) env
  (let ((values (MULTIPLE-VALUE-LIST (eval-with-env form env))))
    (dolist (form forms)
      (eval-with-env form env))
    (VALUES-LIST values)))

(define-special-operator PROGN (&rest forms) env
  (let (lastval)
    (dolist (form forms lastval)
      (setq lastval (eval-with-env form env)))))

;;; TODO: PROGV

(define-special-operator QUOTE (form) env
  (VALUES form))

(define-special-operator RETURN-FROM (tag &optional form) env
  (let ((info (block-information tag env)))
    (if info
	(throw (cdr info) (eval-with-env form env))
	(error "syntax error"))))

(define-special-operator SETQ (&rest forms) env
  (when (oddp (length forms))
    (error "syntax error"))
  (do* (lastval
	(forms forms (cddr forms)))
       ((null forms)
	(VALUES lastval))
    (let ((var (first forms))
	  (val (eval-with-env (second forms) env)))
      (setq lastval
	    (ecase (nth-value 0 (variable-information var env))
	      ;;((nil)		(error "unbound variable %s" form))
	      ((:special nil)	(set var val))
	      (:lexical		(setf (lexical-value var env) val))
	      (:symbol-macro	(error "shouldn't happen"))
	      (:constant	(error "setting constant")))))))

;;; TODO: SYMBOL-MACROLET

(defun go-tag-p (object)
  (or (INTEGERP object) (symbolp object)))

(define-special-operator TAGBODY (&rest forms) env
  (let* ((catch-tag (gensym))
	 (new-env (augment-environment
		   env :tagbody
		   (cons catch-tag (remove-if-not #'go-tag-p forms))))
	 (exe forms)
	 (no-tag 0.0))
    (while exe
      (let ((form (first exe)))
	(if (go-tag-p form)
	    (setq exe (rest exe))
	    (let ((tag (catch catch-tag
			 (eval-with-env form new-env)
			 no-tag)))
	      (if (eq tag no-tag)
		  (setq exe (rest exe))
		  (setq exe (member tag forms)))))))
    nil))

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
  (values
    (let ((info (assoc var (aref env 1))))
      (if info
	  (cdr info)
	  (when (boundp var)
	    (if (CONSTANTP var env)
		:constant
		:special))))
    (member var (aref env 2))
    nil))

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
  (values
    (let ((info (assoc fn (aref env 4))))
      (if info
	  (cdr info)
	  (cond
	    ((FBOUNDP fn)		:function)
	    ((MACRO-FUNCTION fn)	:macro)
	    ((SPECIAL-OPERATOR-P fn)	:special-operator))))
    (member fn (aref env 5))
    nil))

(defun lexical-function (name env)
  (cdr (assoc name (aref env 6))))

(defsetf lexical-function (name env) (fn)
  `(let ((cons (assoc ,name (aref ,env 6))))
     (if cons
	 (setf (cdr cons) ,fn)
	 (progn (aset ,env 6 (acons ,name ,fn (aref ,env 6)))
		,fn))))

(defun block-information (tag env)
  (when env
    (assoc tag (aref env 7))))

(defun tagbody-information (tag env)
  (when env
    (let ((tagbody (find-if (lambda (x) (member tag (rest x))) (aref env 8))))
      (first tagbody))))

(defun* augment-environment (env &key variable symbol-macro function
				      macro declare block tagbody)
  (unless env
    (setq env *global-environment*))
  (let ((var-info (aref env 1))
	(var-local (aref env 2))
	(fn-info (aref env 4))
	(fn-local (aref env 5))
	(block-info (aref env 7))
	(tagbody-info (aref env 8)))
    (setq var-info (reduce (lambda (env var) (acons var :lexical env))
			   variable
			   :initial-value var-info))
    (setq var-info (reduce (lambda (env var) (acons var :symbol-macro env))
			   symbol-macro
			   :initial-value var-info))
    (setq var-local (append variable var-local))
    (setq fn-info (reduce (lambda (env fn) (acons fn :function env))
			  function
			  :initial-value fn-info))
    (setq fn-info (reduce (lambda (env mac) (acons mac :macro env))
			  macro
			  :initial-value fn-info))
    (setq fn-local (append function fn-local))
    (setq block-info (cons block block-info))
    (setq tagbody-info (cons tagbody tagbody-info))
  (vector 'environment var-info var-local (aref env 3)
	               fn-info fn-local (aref env 6)
	               block-info tagbody-info)))

(defun enclose (lambda-exp &optional env)
  (unless env
    (setq env *global-environment*))
  (vector 'interpreted-function lambda-exp env))

(defun INTERPRETED-FUNCTION-P (object)
  (vector-and-typep object 'interpreted-function))



(defun* parse-body (body &optional doc-allowed)
  (let ((decl nil)
	(doc nil))
    (flet ((done () (return-from parse-body (VALUES body decl doc))))
      (while body
	(let ((form (first body)))
	  (cond
	    ((STRINGP form)
	     (if (and doc-allowed (not doc))
		 (setq doc form)
		 (done)))
	    ((and (consp form) (eq (first form) 'DECLARE))
	     (push form decl))
	    (t
	     (done))))
	(setq body (rest body))))))

(defun set-local-macro (name fn env)
  (augment-environment env :macro (list name))
  (setf (lexical-function name env) fn))

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
  (setq form (NTH-VALUE 0 (MACROEXPAND form env)))
  (cond
    ((SYMBOLP form)
     (VALUES
       (ecase (nth-value 0 (variable-information form env))
	 ((nil)		(error "unbound variable %s" form))
	 (:special	(SYMBOL-VALUE form))
	 (:lexical	(lexical-value form env))
	 (:symbol-macro	(error "shouldn't happen yet"))
	 (:constant	(SYMBOL-VALUE form)))))
    ((ATOM form)
     (VALUES form))
    (t
     (if (consp (car form))
	 (if (eq (caar form) 'LAMBDA)
	     (eval-lambda-form form env t)
	     (error "syntax error: %s" form))
	 (let ((fn (gethash (first form) *special-operator-evaluators*)))
	   (if fn
	       (apply fn env (rest form))
	       (let ((fn (lexical-or-global-function (first form) env)))
		 (if (listp fn)
		     ;; Special hack for interpreted Emacs Lisp function.
		     (apply fn (mapcar (lambda (arg) (eval-with-env arg env))
				       (rest form)))
		     (APPLY fn (mapcar (lambda (arg) (eval-with-env arg env))
				       (rest form)))))))))))

(defun EVAL (form)
  (eval-with-env form nil))
