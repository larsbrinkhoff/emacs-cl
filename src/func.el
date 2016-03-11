;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file provides cl:lambda, cl:function, and cl:defun.

(defmacro cl:function (name)
  (let ((fn (FDEFINITION name)))
    (if (subrp fn)
	;; Have to return symbol since #<subr ...> in .elc isn't readable.
	`',name
	fn)))

(defmacro cl:defun (name lambda-list &rest body)
  (when byte-compile-warnings
    (byte-compile-log-1 (format "cl:defun %s" name)))
  `(progn
     (fset ',name (cl:lambda ,lambda-list ,@body))
     ',name))

(defun lambda-keyword-p (x)
  (memq x '(&OPTIONAL &REST &KEY)))

(defvar rest-sym (make-symbol "rest"))

(defvar unbound (make-symbol "unbound"))

(defun* simplify-lambda-list (lambda-list &optional env)
  (let ((result nil)
	(state :required))
    (dolist (x lambda-list)
      (cond
	((memq x '(&KEY &REST))
	 (push '&rest result)
	 (push rest-sym result)
	 (return-from simplify-lambda-list (nreverse result)))
	((lambda-keyword-p x)
	 (setq state x)
	 (push (cdr (assq x '((&OPTIONAL . &optional)))) result))
	((symbolp x)
	 (push (if env (lexical-value x env) x) result))
	((consp x)
	 (when (eq state :required)
	   (error "required parameters must be symbols"))
	 (when (memq (car result) '(&optional &rest))
	   (pop result))
	 (push '&rest result)
	 (push rest-sym result)
	 (return-from simplify-lambda-list (nreverse result)))
	(t
	 (error "syntax error"))))
    (nreverse result)))

(defun* lambda-list-bindings (lambda-list env)
  (let ((bindings nil)
	(state :required)
	x)
    (while lambda-list
      (setq x (pop lambda-list))
      (cond
	((eq x '&KEY)
	 (dolist (y (lambda-list-keyword-vars (cons '&KEY lambda-list) env t))
	   (push `(,y ',unbound) bindings))
	 (return-from lambda-list-bindings (nreverse bindings)))
	((lambda-keyword-p x)
	 (setq state x))
	((symbolp x)
	 (when env (setq x (lexical-value x env)))
	 (case state
	   (:optional-rest
	    (push `(,x (pop ,rest-sym)) bindings))
	   (&REST
	    (push `(,x ,rest-sym) bindings))))
	((consp x)
	 (when (eq state '&OPTIONAL)
	   (setq state :optional-rest))
	 (case state
	   (:optional-rest
	    (let ((var (first x))
		  (default (second x))
		  (supplied (third x)))
	      (when supplied
		(when env (setq supplied (lexical-value supplied env)))
		(push `(,supplied ,rest-sym) bindings))
	      (when env (setq var (lexical-value var env)))
	      (push `(,var (if ,rest-sym (pop ,rest-sym) ,default)) bindings)))
	   (t
	    (error "syntax error"))))
	(t
	 (error "syntax error"))))
    (nreverse bindings)))

(defun lambda-list-keys (lambda-list)
  (let ((key (copy-list (member '&KEY lambda-list))))
    (when key
      (let ((x key))
	(while (rest x)
	  (if (lambda-list-keyword-p (second x))
	      (rplacd x nil)
	      (setq x (rest x)))))
      (mapcar (lambda (var)
		(cond
		  ((symbolp var)
		   (keyword (symbol-name var)))
		  ((and (consp var) (symbolp (first var)))
		   (keyword (symbol-name (first var))))
		  ((and (consp var) (consp (first var)))
		   (caar var))
		  (t
		   (error "syntax error"))))
	      (rest key)))))

(defun lambda-list-keyword-vars (lambda-list env &optional include-supplied)
  (let ((key (copy-list (member '&KEY lambda-list))))
    (when key
      (let ((x key))
	(while (rest x)
	  (if (lambda-list-keyword-p (second x))
	      (rplacd x nil)
	      (setq x (rest x)))))
      (mappend (lambda (var)
		 (cond
		   ((symbolp var)
		    (when env (setq var (lexical-value var env)))
		    (list var))
		   ((and (consp var) (symbolp (first var)))
		    (if (and (cddr var) include-supplied)
			(if env
			    (list (lexical-value (first var) env)
				  (lexical-value (third var) env))
			    (list (first var) (third var)))
			(if env
			    (list (lexical-value (first var) env))
			    (list (first var)))))
		   ((and (consp var) (consp (first var)))
		    (if (and (cddr var) include-supplied)
			(if env
			    (list (lexical-value (cadar var) env)
				  (lexical-value (third var) env))
			    (list (cadar var) (third var)))
			(if env
			    (list (lexical-value (cadar var) env))
			    (list (cadar var)))))))
	       (rest key)))))

(defun lambda-list-keyword-defaults (lambda-list)
  (let ((key (copy-list (member '&KEY lambda-list))))
    (when key
      (let ((x key))
	(while (rest x)
	  (if (lambda-list-keyword-p (second x))
	      (rplacd x nil)
	      (setq x (rest x)))))
      (mapcar (lambda (var)
		(when (and (consp var) (cdr var))
		  (second var)))
	      (rest key)))))

(defun keyword-bindings (lambda-list env)
  (let ((allow-other-keys-p (member '&ALLOW-OTHER-KEYS lambda-list))
	(temp (gensym))
	(keys (lambda-list-keys lambda-list))
	(vars (lambda-list-keyword-vars lambda-list env))
	(defaults (lambda-list-keyword-defaults lambda-list)))
    (when keys
      `((while ,rest-sym
	  (let ((,temp (position (pop ,rest-sym) ; ',keys)))
				 ;; TODO: have to do run-time computation
				 ;; since compiler doesn't preserve object
				 ;; identities.
				 (mapcar #'keyword
					 ',(mapcar #'symbol-name keys)))))
	    ,@(unless allow-other-keys-p
	       `((unless ,temp (ERROR "Unknown keyword"))))
	    (set (nth ,temp ',vars) (pop ,rest-sym))))
	,@(mappend (lambda (var default)
		     `((when (eq ,var ',unbound)
			 (setq ,var ,default))))
		   vars defaults)))))

(defun translate-lambda-list (lambda-list env)
  (mapcar (lambda (x)
	    (let ((cons (assq x '((&OPTIONAL . &optional)
				  (&REST . &rest)))))
	      (cond
		(cons	(cdr cons))
		(env	(lexical-value x env))
		(t	x))))
	  lambda-list))

(defun lambda-list-variables (lambda-list)
  (let ((vars nil))
    (dolist (x lambda-list (nreverse vars))
      (cond
	((lambda-list-keyword-p x))
	((symbolp x)
	 (push x vars))
	((consp x)
	 (push (if (consp (car x)) (cadar x) (car x)) vars)
	 (when (eq (length x) 3)
	   (push (third x) vars)))))))

(defun expand-lambda (lambda-list body &optional env)
  (dolist (k '(&optional &rest &key &aux &allow-other-keys))
    (when (memq k lambda-list)
      (error "Emacs Lisp lambda list keywords not allowed")))
  (if (and (every 'symbolp lambda-list)
	   (notany (lambda (x) (member x '(&KEY))) lambda-list))
      ;; Easy case: no defaults, suppliedp, or keyword arguments.
      `(lambda ,(translate-lambda-list lambda-list env) ,@body)
      ;; Difficult case:
      `(lambda ,(simplify-lambda-list lambda-list env)
	(let* ,(lambda-list-bindings lambda-list env)
	  ,@(keyword-bindings lambda-list env)
	  ,@body))))

(defmacro cl:lambda (lambda-list &rest body)
;  (byte-compile
   (expand-lambda lambda-list body));)
