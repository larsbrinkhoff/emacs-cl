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
  (member x '(&OPTIONAL &REST &KEY)))

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
	 (when (lambda-keyword-p (car result))
	   (pop result))
	 (push '&rest result)
	 (push rest-sym result)
	 (return-from simplify-lambda-list (nreverse result)))
	(t
	 (error "syntax error"))))
    (nreverse result)))

(defun* lambda-list-bindings (lambda-list)
  (let ((bindings nil)
	(state :required)
	x)
    (while lambda-list
      (setq x (pop lambda-list))
      (cond
	((eq x '&KEY)
	 (dolist (y (lambda-list-keyword-vars (cons '&KEY lambda-list) t))
	   (push `(,y ',unbound) bindings))
	 (return-from lambda-list-bindings (nreverse bindings)))
	((lambda-keyword-p x)
	 (setq state x))
	((symbolp x)
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
		(push `(,supplied ,rest-sym) bindings))
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

(defun lambda-list-keyword-vars (lambda-list &optional include-supplied)
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
		    (list var))
		   ((and (consp var) (symbolp (first var)))
		    (if (and (cddr var) include-supplied)
			(list (first var) (third var))
			(list (first var))))
		   ((and (consp var) (consp (first var)))
		    (if (and (cddr var) include-supplied)
			(list (cadar var) (third var))
			(list (cadar var))))))
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

(defun keyword-bindings (lambda-list)
  (let ((allow-other-keys (member '&ALLOW-OTHER-KEYS lambda-list))
	(result nil)
	(temp (gensym))
	(keys (lambda-list-keys lambda-list))
	(vars (lambda-list-keyword-vars lambda-list))
	(defaults (lambda-list-keyword-defaults lambda-list)))
    (when keys
      `((while ,rest-sym
	  (let ((,temp (position (pop ,rest-sym) ; ',keys)))
				 ;; TODO: have to do run-time computation
				 ;; since compiler doesn't preserve object
				 ;; identities.
				 (mapcar #'keyword
					 ',(mapcar #'symbol-name keys)))))
	    ,@(unless allow-other-keys
	       `((unless ,temp (error "unknown keyword"))))
	    (set (nth ,temp ',vars) (pop ,rest-sym))))
	,@(mappend (lambda (var default)
		     `((when (eq ,var ',unbound)
			 (setq ,var ,default))))
		   vars defaults)))))

(defun translate-lambda-list (lambda-list env)
  (mapcar (lambda (x)
	    (let ((cons (assq x '((&OPTIONAL . &optional)
				  (&REST . &rest)))))
	      (if cons
		  (cdr cons)
		  x)))
	  lambda-list))

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
	(let* ,(lambda-list-bindings lambda-list)
	  ,@(keyword-bindings lambda-list)
	  ,@body))))

(defmacro cl:lambda (lambda-list &rest body)
;  (byte-compile
   (expand-lambda lambda-list body));)
