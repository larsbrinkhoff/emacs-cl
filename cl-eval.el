;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements EVAL and environment objects.

(IN-PACKAGE "EMACS-CL")

(defvar *special-operator-evaluators* (make-hash-table))

(defmacro* define-special-operator (name (&rest args) env &body body)
  `(setf (gethash ',name *special-operator-evaluators*)
         (function* (lambda (,env ,@args) ,@body))))


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
  (when (or (memq (kw EXECUTE) situations) (memq 'EVAL situations))
    (let (lastval)
      (dolist (form body lastval)
	(setq lastval (eval-with-env form env))))))

(define-special-operator FLET (fns &rest forms) env
  (let ((new-env (augment-environment env :function (mapcar #'first fns)))
	(lastval nil))
    (dolist (fn fns)
      (setf (lexical-function (first fn) new-env)
	    (enclose `(LAMBDA ,@(rest fn)) env (first fn))))
    (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
      (dolist (form body lastval)
	(setq lastval (eval-with-env form new-env))))))

(defun lexical-or-global-function (name env)
  (multiple-value-bind (type localp decl) (function-information name env)
    (case type
      ((nil)		(ERROR 'UNDEFINED-FUNCTION (kw NAME) name))
      (:function	(if localp
			    (lexical-function name env)
			    (FDEFINITION name)))
      (t		(error "syntax error")))))

(define-special-operator FUNCTION (form) env
  (cl:values
    (cond
      ((SYMBOLP form)		(lexical-or-global-function form env))
      ((ATOM form)		(not-function-name-error form))
      ((case (first form)
	 (LAMBDA		(enclose form env form))
	 (SETF			(lexical-or-global-function form env))
	 (t			(not-function-name-error form)))))))

(define-special-operator GO (tag) env
  (let ((info (tagbody-information tag env)))
    (if info
	(throw info tag)
	(ERROR 'PROGRAM-ERROR))))

(define-special-operator IF (condition then &optional else) env
  (if (eval-with-env condition env)
      (eval-with-env then env)
      (eval-with-env else env)))

(define-special-operator LABELS (fns &rest forms) env
  (let ((new-env (augment-environment env :function (mapcar #'first fns)))
	(lastval nil))
    (dolist (fn fns)
      (setf (lexical-function (first fn) new-env)
	    (enclose `(LAMBDA ,@(rest fn)) new-env (first fn))))
    (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
      (dolist (form body lastval)
	(setq lastval (eval-with-env form new-env))))))

(defun lexical-binding-variables (bindings env)
  (mappend (lambda (binding)
	     (let ((var (if (symbolp binding)
			    binding
			    (car binding))))
	       (unless (eq (nth-value 0 (variable-information var env))
			   :special)
		 (list var))))
	   bindings))

;;; TODO: let* bindings shouldn't be evaluated in an environment where
;;; succeeding bindings exist.
(defun eval-let (bindings forms env old-env)
  (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
    (let* ((vars (lexical-binding-variables bindings env))
	   (new-env (if vars (augment-environment env :variable vars) env))
	   (lastval nil)
	   (oldvals nil))
      (dolist (binding bindings)
	(multiple-value-bind (var val) (if (symbolp binding)
					   (values binding nil)
					   (values (first binding)
						   (second binding)))
	  (if (member var vars)
	      (setf (lexical-value var new-env)
		    (eval-with-env val (or old-env new-env)))
	      (progn
		(push (symbol-value var) oldvals)
		(setf (symbol-value var)
		      (eval-with-env val (or old-env new-env)))))))
      (unwind-protect
	   (dolist (form body lastval)
	     (setq lastval (eval-with-env form new-env)))
	(setq oldvals (nreverse oldvals))
	(dolist (binding bindings)
	  (multiple-value-bind (var val) (if (symbolp binding)
					     (values binding nil)
					     (values (first binding)
						     (second binding)))
	    (unless (member var vars)
	      (setf (symbol-value var) (pop oldvals)))))))))

(define-special-operator LET (bindings &rest forms) env
  (eval-let bindings forms env env))

(define-special-operator LET* (bindings &rest forms) env
  (eval-let bindings forms env nil))

;;; TODO: LOAD-TIME-VALUE

(define-special-operator LOCALLY (&rest forms) env
  (let (lastval)
    (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
      (dolist (form body lastval)
	(setq lastval (eval-with-env form env))))))

(define-special-operator MACROLET (macros &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :macro (mapcar #'first macros)))
	  (lastval nil))
      (dolist (macro macros)
	(setf (MACRO-FUNCTION (first macro) new-env)
	      (enclose `(LAMBDA (form env)
			 ;; TODO: destructuring-bind
			 (APPLY (LAMBDA ,@(rest macro)) (CDR form)))
		       env (first macro))))
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

(defmacro defun-do-progv ()
  (with-gensyms (sym syms vals fn temp state)
  `(defun do-progv (,syms ,vals ,fn)
    (let ((,state nil))
      (unwind-protect
	   (progn
	     (dolist (,sym ,syms)
	       (let ((,temp (boundp ,sym)))
		 (when ,temp
		   (push (symbol-value ,sym) ,state))
		 (push ,temp ,state))
	       (if (null ,vals)
		   (makunbound ,sym)
		   (set ,sym (pop ,vals))))
	     (FUNCALL ,fn))
	(dolist (,sym ,syms)
	  (if (pop ,state)
	      (set ,sym (pop ,state))
	      (makunbound ,sym))))))))

(defun-do-progv)

(define-special-operator PROGV (symbols values &rest forms) env
  (do-progv (eval-with-env symbols env)
            (eval-with-env values env)
	    (enclose `(LAMBDA () ,@forms) env)))

(define-special-operator QUOTE (form) env
  (cl:values form))

(define-special-operator RETURN-FROM (tag &optional form) env
  (let ((info (block-information tag env)))
    (if info
	(throw (cdr info) (eval-with-env form env))
	(ERROR 'PROGRAM-ERROR))))

(define-special-operator SETQ (&rest forms) env
  (when (oddp (length forms))
    (ERROR "Odd number of forms in SETQ"))
  (do ((lastval nil)
       (forms forms (cddr forms)))
      ((null forms)
       (cl:values lastval))
    (let ((var (first forms))
	  (val (eval-with-env (second forms) env)))
      (unless (symbolp var)
	(ERROR "Setting non-symbol ~S" var))
      (setq lastval
	    (ecase (nth-value 0 (variable-information var env))
	      (:lexical		(setf (lexical-value var env) val))
	      (:special		(set var val))
	      ((nil)		(WARN "Setting undefined variable ~S" var)
				(set var val))
	      (:symbol-macro	(eval-with-env `(SETF ,var (QUOTE ,val)) env))
	      (:constant	(ERROR "Setting constant ~S" var)))))))

(define-special-operator SYMBOL-MACROLET (macros &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :symbol-macro
					(mapcar #'first macros)))
	  (lastval nil))
      (dolist (macro macros)
	(setf (lexical-value (first macro) new-env)
	      (enclose `(LAMBDA (form env) (QUOTE ,(second macro)))
		       env (first macro))))
      (dolist (form body lastval)
	(setq lastval (eval-with-env form new-env))))))

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
	(cond
	  ((go-tag-p form)
	   (setq exe (rest exe)))
	  ((consp form)
	   (let ((tag (catch catch-tag
			(eval-with-env form new-env)
			no-tag)))
	     (if (eq tag no-tag)
		 (setq exe (rest exe))
		 (setq exe (member tag forms)))))
	  (t
	   (ERROR "Syntax error: ~S in tagbody is neither a go tag ~
		   nor a compound expression" form)))))))

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
  (cdr (assq var (aref env 3))))

(defsetf lexical-value (var env) (val)
  `(setf (cdr (assq ,var (aref ,env 3))) ,val))

(defun function-information (fn &optional env)
  (unless env
    (setq env *global-environment*))
  (values
   (let ((info (assoc fn (aref env 4))))
     (if info
	 (cdr info)
	 (cond
	   ((gethash fn *macro-functions*)	:macro)
	   ((FBOUNDP fn)			:function)
	   ((SPECIAL-OPERATOR-P fn)		:special-operator))))
   (member fn (aref env 5))
   nil))

(defun lexical-function (name env)
  (cdr-safe (assoc name (aref env 6))))

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
	(var-storage (aref env 3))
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
    (setq var-local (append variable symbol-macro var-local))
    (dolist (var variable)
      (push (cons var nil) var-storage))
    (dolist (var symbol-macro)
      (push (cons var nil) var-storage))
    (setq fn-info (reduce (lambda (env fn) (acons fn :function env))
			  function
			  :initial-value fn-info))
    (setq fn-info (reduce (lambda (env mac) (acons mac :macro env))
			  macro
			  :initial-value fn-info))
    (setq fn-local (append function macro fn-local))
    (setq block-info (cons block block-info))
    (setq tagbody-info (cons tagbody tagbody-info))
  (vector 'environment var-info var-local var-storage
	               fn-info fn-local (aref env 6)
	               block-info tagbody-info)))

(cl:defun enclose (lambda-exp &OPTIONAL env (name ""))
  (unless env
    (setq env *global-environment*))
  (vector 'INTERPRETED-FUNCTION lambda-exp env name))

(defun INTERPRETED-FUNCTION-P (object)
  (vector-and-typep object 'INTERPRETED-FUNCTION))

(defun function-name (fn)
  (cond
    ((INTERPRETED-FUNCTION-P fn)
     (interp-fn-name fn))
    ((byte-code-function-p fn)
     "")
    ((subrp fn)
     (let ((string (prin1-to-string fn)))
       (substring string 7 (1- (length string)))))
    ((listp fn)
     "")
    (t
     (type-error fn 'FUNCTION))))

(defsetf function-name set-function-name)

(DEFSETF function-name set-function-name)

(defun set-function-name (fn name)
  (cond
    ((INTERPRETED-FUNCTION-P fn)
     (setf (interp-fn-name fn) name))
    ((byte-code-function-p fn)
     name)
    ((subrp fn)
     name)
    ((listp fn)
     name)
    (t
     (type-error fn 'FUNCTION))))



(defun* parse-body (body &optional doc-allowed)
  (do ((decl nil)
       (doc nil)
       (body body (rest body)))
      ((null body))
    (flet ((done () (return-from parse-body (cl:values body decl doc))))
      (let ((form (first body)))
	(cond
	  ((STRINGP form)
	   (if (and doc-allowed (not doc))
	       (setq doc form)
	       (done)))
	  ((and (consp form) (eq (first form) 'DECLARE))
	   (push (rest form) decl))
	  (t
	   (done)))))))

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
	(args (rest form)))
    (MULTIPLE-VALUE-BIND (body decls doc) (parse-body (cddar form) t)
      (dolist (var (cadar form))
	(unless (member var LAMBDA-LIST-KEYWORDS)
	  (setf (lexical-value var new-env)
		(if eval-args
		    (eval-with-env (pop args) env)
		    (pop args)))))
      (dolist (form body lastval)
	(setq lastval (eval-with-env form new-env))))))

(defun eval-with-env (form env)
  (unless env
    (setq env *global-environment*))
  (setq form (cl:values (MACROEXPAND form env)))
  (cond
    ((SYMBOLP form)
     (ecase (nth-value 0 (variable-information form env))
       ((nil)		(ERROR 'UNBOUND-VARIABLE (kw NAME) form))
       (:special	(SYMBOL-VALUE form))
       (:lexical	(lexical-value form env))
       (:symbol-macro	(error "shouldn't happen"))
       (:constant	(SYMBOL-VALUE form))))
    ((ATOM form)
     form)
    ((consp (car form))
     (if (eq (caar form) 'LAMBDA)
	 (eval-lambda-form form env t)
	 (ERROR 'PROGRAM-ERROR)))
    (t
     (let* ((name (first form))
	    (fn (gethash name *special-operator-evaluators*)))
       (cond
	 (fn
	  (apply fn env (rest form)))
	 ((setq fn (lexical-or-global-function name env))
	  (if (listp fn)
	      ;; Special hack for interpreted Emacs Lisp function.
	      (apply fn (mapcar (lambda (arg) (eval-with-env arg env))
				(rest form)))
	      (APPLY fn (mapcar (lambda (arg) (eval-with-env arg env))
				(rest form))))))))))

(defun EVAL (form)
  (eval-with-env form nil))
