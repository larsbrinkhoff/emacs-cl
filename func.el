;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file provides cl:lambda, cl:function, and cl:defun.

(defmacro cl:function (name)
  (FDEFINITION name))

(defmacro cl:defun (name lambda-list &rest body)
  `(progn
     (fset ',name (cl:lambda ,lambda-list ,@body))
     ',name))

(defun lambda-keyword-p (x)
  (member x '(&optional &rest &key)))

(defvar rest-sym (make-symbol "rest"))

(defvar unbound (make-symbol "unbound"))

(defun* simplify-lambda-list (lambda-list)
  (let ((result nil)
	(state :required))
    (dolist (x lambda-list)
      (cond
	((eq x '&key)
	 (push '&rest result)
	 (push rest-sym result)
	 (return-from simplify-lambda-list (nreverse result)))
	((lambda-keyword-p x)
	 (setq state x)
	 (push x result))
	((symbolp x)
	 (push x result))
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
	((eq x '&key)
	 (dolist (y (lambda-list-keyword-vars (cons '&key lambda-list) t))
	   (push `(,y ',unbound) bindings))
	 (return-from lambda-list-bindings (nreverse bindings)))
	((lambda-keyword-p x)
	 (setq state x))
	((symbolp x)
	 (case state
	   (:optional-rest
	    (push `(,x (pop ,rest-sym)) bindings))
	   (&rest
	    (push `(,x ,rest-sym) bindings))))
	((consp x)
	 (when (eq state '&optional)
	   (setq state :optional-rest))
	 (ecase state
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
  (let ((key (copy-list (member '&key lambda-list))))
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
  (let ((key (copy-list (member '&key lambda-list))))
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
  (let ((key (copy-list (member '&key lambda-list))))
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
  (let ((allow-other-keys (member '&allow-other-keys lambda-list))
	(result nil)
	(temp (gensym))
	(keys (lambda-list-keys lambda-list))
	(vars (lambda-list-keyword-vars lambda-list))
	(defaults (lambda-list-keyword-defaults lambda-list)))
    (when keys
      `((while ,rest-sym
	  (let ((,temp (position (pop ,rest-sym) ',keys)))
	    ,@(unless allow-other-keys
	       `((unless ,temp (error))))
	    (set (nth ,temp ',vars) (pop ,rest-sym))))
	,@(mappend (lambda (var default)
		     `((when (eq ,var ',unbound)
			 (setq ,var ,default))))
		   vars defaults)))))

(defmacro cl:lambda (lambda-list &rest body)
  (byte-compile
    (if (and (every 'symbolp lambda-list)
	     (notany (lambda (x) (member x '(&key))) lambda-list))
	;; Easy case: no defaults, suppliedp, or keyword arguments.
	`(lambda ,lambda-list ,@body)
	;; Difficult case:
	`(lambda ,(simplify-lambda-list lambda-list)
	   (let* ,(lambda-list-bindings lambda-list)
	     ,@(keyword-bindings lambda-list)
	     ,@body)))))
