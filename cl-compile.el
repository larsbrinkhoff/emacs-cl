;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements the compiler.

(IN-PACKAGE "EMACS-CL")

(defmacro* with-fresh-context (&body body)
  `(let ((*next-register* *registers*)
	 (*free* nil))
     ,@body))

(defun COMPILE (name &optional definition)
  (when (null definition)
    (if (fboundp name)
	(setq definition (fdefinition name))
	(setq definition (symbol-macro name))))
  (with-fresh-context
    (let* ((compiled (compile-form definition *global-environment*))
	   (function (byte-compile compiled)))
      (when name
	(setf (fdefinition name) function))
      (VALUES (subst-free compiled) nil nil))))



(defvar *registers* (list (gensym)))
(defvar *next-register* nil)

(defvar *bound* nil
  "A list of variables bound in the function being compiled.")
(defvar *free* nil
  "An alist of free variables in the top-level form being compiled.")

(defvar *unbound* nil)
(defvar *closure-slot* nil)

(defun symbol-register (sym)
  (gethash sym *variables*))

(defsetf symbol-register (sym) (reg)
  `(setf (gethash ,sym *variables*) ,reg))

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

(defun subst-free (fn)
  (cond
    ((symbolp fn)
     (let ((var (assq fn *free*)))
       (if var
	   (cdr var)
	   fn)))
    ((consp fn)
     (cons (subst-free (car fn)) (subst-free (cdr fn))))
    (t
     fn)))

(defun lambda-expr-p (form)
  (and (consp form)
       (eq (car form) 'LAMBDA)))

(defun compile-args (args env)
  (mapcar (lambda (arg) (compile-form arg env)) args))

(defun* compile-form (form &optional env &key (values 1))
  (when (and (consp form)
	     (symbolp (first form)))
    (let* ((name (first form))
	   (fn (gethash name *form-compilers*)))
      (when fn
	(return-from compile-form (apply fn env (rest form))))))
  (setq form (VALUES (MACROEXPAND form env)))
  (cond
    ((SYMBOLP form)
     (unless (eq values 0)
       (let ((val (compile-variable form env)))
	 (if (eq values t)
	     `(progn (setq nvals 1 mvals nil) ,val)
	     val))))
    ((ATOM form)
     (unless (eq values 0)
       (let ((val (compile-literal form)))
	 (if (eq values t)
	     `(progn (setq nvals 1 mvals nil) ,val)
	     val))))
    ((lambda-expr-p (first form))
     `(,(compile-lambda form env) ,@(compile-args (rest form) env)))
    ((symbolp (first form))
     (let* ((name (first form))
	    (fn (gethash name *form-compilers*)))
       (if fn
	   (apply fn env (rest form))
	   (compile-form `(FUNCALL (QUOTE ,name) ,@(rest form)) env))))
    (t
     (ERROR "Syntax error: ~S" form))))

(defun compile-variable (var env)
  (multiple-value-bind (type localp decls) (variable-information var env)
    (ecase type
      ((:special nil)	var)
      (:lexical		(unless (memq var *bound*)
			  (push (cons (lexical-value var env)
				      `(lexical-value ',var ,env))
				*free*))
			(lexical-value var env))
      (:constant	(symbol-value var)))))

;   (if (and (not (find sym *bound*))
; 	   (not (assoc sym *unbound*))
; 	   (not (symbol-special-p sym)))
;       (let ((reg `(aref closure ,(incf *closure-slot*))))
; 	(push (cons sym reg) *unbound*)
; 	reg)
;       (let ((reg (symbol-register sym)))
; 	(or reg (ERROR "undefined variable: ~S" sym)))))

(defun compile-literal (literal)
  (cond
    ((or (symbolp literal) (consp literal))
     `(quote ,literal))
    (t
     literal)))

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

(defvar *form-compilers* (make-hash-table))

(defmacro* define-compiler (operator lambda-list env &body body)
  `(setf (gethash ',operator *form-compilers*)
	 (function* (lambda (,env ,@lambda-list) ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-compiler APPLY (fn &rest args) env
  (let ((fn (compile-form fn env))
	(args (compile-args args env)))
    (cond
      ((subrp fn)
       `(apply #',(intern (function-name fn)) ,@args))
      ((byte-code-function-p fn)
       `(apply ,fn ,@args))
      (t
       `(APPLY ,fn ,@args)))))

(define-compiler BLOCK (tag &rest body) env
  (let* ((block (gensym))
	 (new-env (augment-environment env :block (cons tag block))))
  `(catch ,block ,@(compile-forms body env))))

(define-compiler CATCH (tag &rest body) env
  `(catch ,(compile-form tag env) ,@(compile-forms body env)))

(define-compiler COND (&rest clauses) env
  `(cond ,@(mapcar (lambda (clause)
		     (unless (consp clause)
		       (ERROR 'PROGRAM-ERROR))
		     (if (and (CONSTANTP (first clause))
			      (not (null (first clause))))
			 `(t ,@(compile-forms (rest clause) env))
			 (mapcar (lambda (form) (compile-form form env))
				 clause)))
		   clauses)))

;;; TODO: eval-when
(define-compiler EVAL-WHEN (situations &rest body) env
  `(progn ,@(compile-forms body env)))

(define-compiler FLET (fns &rest body) env
  (let ((new-env (augment-environment env :function (mapcar #'first fns))))
    (dolist (fn fns)
      (setf (lexical-function (first fn) new-env)
	    (compile-form `(LAMBDA ,@(rest fn)) env)))
    `(progn ,@(compile-forms body))))

(define-compiler FUNCALL (fn &rest args) env
  (if (and (consp fn) (eq (first fn) 'QUOTE) (symbolp (second fn)))
      (if (eq (second fn) 'FUNCALL)
	  (compile-form `(FUNCALL ,@args) env)
	  `(,(second fn) ,@(compile-args args env)))
      (let ((fn (compile-form fn env))
	    (args (compile-args args env)))
	(cond
	  ((subrp fn)
	   `(,(intern (function-name fn)) ,@args))
	  ((byte-code-function-p fn)
	   `(funcall ,fn ,@args))
	  (t
	   `(FUNCALL ,fn ,@args))))))

(define-compiler FUNCTION (name) env
  (if (lambda-expr-p name)
      (let ((*bound* nil))
	(compile-lambda name env))
      (multiple-value-bind (type localp decl) (function-information name env)
	(cond
	  (localp		(lexical-function name env))
	  ((symbolp name)	(symbol-function name))
	  ((setf-name-p name)	(FDEFINITION name))
	  (t			(ERROR "Syntax error: (FUNCTION ~S)" name))))))

(define-compiler GO (tag) env
  (let ((info (tagbody-information tag env)))
    (if info
	`(throw ,info ,tag)
	(ERROR "No tagbody for (GO ~S)" tag))))

(define-compiler IF (condition then &optional else) env
  `(if ,(compile-form condition env)
       ,(compile-form then env)
       ,@(when else
	   (list (compile-form else env)))))

(define-compiler LABELS (fns &rest body) env
  (let ((new-env (augment-environment env :function (mapcar #'first fns))))
    (dolist (fn fns)
      (setf (lexical-function (first fn) new-env)
	    (compile-form `(LAMBDA ,@(rest fn)) new-env)))
    `(progn ,@(compile-forms body))))

(defun lambda-list-parameters (lambda-list)
  (let ((result nil))
    (dolist (var lambda-list (nreverse result))
      (unless (lambda-list-keyword-p var)
	(push (cond
		((symbolp var)			    var)
		((consp var)	(let ((var (car var)))
				  ((symbolp var)    var)
				  ((consp var)	    (car var))
				  (t		    (ERROR 'PROGRAM-ERROR))))
		(t				    (ERROR 'PROGRAM-ERROR)))
	      result)))))

(defvar *compile-lambda* nil)

;;; TODO: lambda
(defun* compile-lambda (form env)
  (MULTIPLE-VALUE-BIND (body decls) (cddr form)
    (let* ((vars (second form))
	   (new-env (if vars (augment-environment env :variable vars) env))
	   (compiled-vars nil))
      (dolist (var (lambda-list-parameters vars))
	(setf (lexical-value var new-env) (new-register))
	(push var *bound*))
      (expand-lambda vars (compile-forms body new-env) new-env))))

;   (let ((vars (second form))
; 	(body (cddr form))
; 	(*bound* nil)
; 	(*closure-slot* 0))
;     (dolist (sym vars)
;       (new-symbol sym))
;     (do* ((forms body (cdr forms))
; 	  (form #1=(first forms) #1#))
; 	 ((or (atom form) (not (eq (first form) 'declare)))
; 	  (let* ((*unbound* nil)
; 		 (body (compile-forms forms))
; 		 (lambda-expr
; 		  `(lambda ,(mapcar #'symbol-register vars) ,@body))
; 		 (compiled-expr (if *compile-lambda*
; 				    (byte-compile lambda-expr)
; 				    lambda-expr)))
; 	    (if *unbound*
; 		(let* ((closure (new-register)))
; 		  `(let ((,closure
; 			  (make-vector ,(1+ (length *unbound*)) nil)))
; 		    (aset ,closure 0 ,compiled-expr)
; 		    ,@(mappend (lambda (x)
; 				 (unless (symbol-special-p (car x))
; 				   `((aset ,closure ,(cadddr x)
; 				      ,(compile-variable (car x))))))
; 			       *unbound*)
; 		    ,closure))
; 		compiled-expr)))
;       (compile-declare form))))

(define-compiler LET (bindings &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((vars (lexical-binding-variables bindings))
	   (new-env (if vars (augment-environment env :variable vars) env)))
      (dolist (var vars)
	(setf (lexical-value var new-env) (new-register))
	(push var *bound*))
      `(let ,(mapcar (lambda (binding)
		       (cond
			 ((symbolp binding)
			  `(binding nil))
			 ((consp binding)
			  `(,(compile-variable (first binding) new-env)
			    ,(compile-form (second binding) env)))
			 (t
			  (ERROR 'PROGRAM-ERROR))))
		     bindings)
	 ,@(compile-forms body new-env)))))

(define-compiler LET* (bindings &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((vars (lexical-binding-variables bindings))
	   (new-env (if vars (augment-environment env :variable vars) env)))
      (dolist (var vars)
	(setf (lexical-value var new-env) (new-register))
	(push var *bound*))
      `(let* ,(mapcar (lambda (binding)
			(cond
			  ((symbolp binding)
			   `(binding nil))
			  ((consp binding)
			   `(,(compile-variable (first binding) new-env)
			     ,(compile-form (second binding) new-env)))
			  (t
			   (ERROR 'PROGRAM-ERROR))))
		      bindings)
	 ,@(compile-forms body new-env)))))

;;; TODO: load-time-value

(define-compiler LOCALLY (&rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (mapc #'compile-declare decls)
    `(progn ,@(compile-forms forms env))))

(define-compiler MACROLET (macros &body forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    ;; TODO: bind macros, process decls
    (compile-forms body env)))

(define-compiler MULTIPLE-VALUE-BIND (vars form &body forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (if vars (augment-environment env :variable vars) env)))
      (dolist (var vars)
	(setf (lexical-value var new-env) (new-register))
	(push var *bound*))
      (if (null vars)
	  `(progn
	     ,(compile-form form :values 0)
	     ,@(compile-forms body env))
	  `(let* ((,(first vars) ,(compile-form form env :values t))
		  ,@(mapcar (lambda (var) `(,var (pop mvals))) (rest vars)))
	     ,@(compile-forms body env))))))

(define-compiler MULTIPLE-VALUE-CALL (fn &rest forms) env
  (if (null forms)
      (compile-form `(FUNCALL ,fn) env)
      `(APPLY ,(compile-form fn env)
	      (append ,@(mapcar (lambda (form)
				  `(MULTIPLE-VALUE-LIST
				    ,(compile-form form env :values t)))
				forms)))))

(define-compiler MULTIPLE-VALUE-LIST (form) env
  (let ((val (new-register)))
    `(let* ((,val ,(compile-form form env :values t)))

(define-compiler MULTIPLE-VALUE-PROG1 (form &rest forms) env
  (let ((val (new-register))
	(ntemp (new-register))
	(mtemp (new-register)))
    `(let* ((,val ,(compile-form form env :values t))
	    (,ntemp nvals)
	    (,mtemp mvals))
       ,@(compile-forms forms env :values 0)
       (setq nvals ,ntemp mvals ,mtemp)
       ,val)))

(defun* compile-forms (forms env &key (values t))
  (if (null forms)
      nil
      (do* ((forms forms (cdr forms))
	    (form #1=(car forms) #1#)
	    (result nil))
	   ((null (cdr forms))
	    (push (compile-form form env :values values) result)
	    (nreverse result))
	(let ((comp (compile-form form env :values 0)))
	  (when comp
	    (push comp result))))))

(define-compiler PROGN (&rest body) env
  `(progn ,@(compile-forms body env)))

;;; TODO: progv

(define-compiler QUOTE (form) env
  (compile-literal form))

(define-compiler RETURN-FROM (tag &optional form) env
  (let ((block (block-information tag env)))
    (if block
	`(throw ,(cdr block) ,(compile-form form env))
	(ERROR "No block for (RETURN-FROM ~S)" form))))

(define-compiler SETQ (var val &rest more) env
  (multiple-value-bind (type localp) (variable-information var env)
    (ecase type
      ((:lexical :special nil)
       `(setq ,(compile-variable var env) ,(compile-form val env)
	      ,@(when more
		  (rest (compile-form `(SETQ ,@more)))))))))

(defun compile-symbol-macrolet (forms)
  (let ((macros (second forms))
	(body (cddr forms)))
    (compile-forms body)))

(define-compiler TAGBODY (&rest forms) env
  (let* ((tagbody (gensym))
	 (new-env (augment-environment
		   env :tagbody
		   (cons tagbody (remove-if-not #'go-tag-p forms)))))
    nil))

(define-compiler THE (type form) env
  (compile-form form env))

(define-compiler THROW (tag form) env
  `(throw ,(compile-form tag env) ,(compile-form form env)))

(define-compiler UNWIND-PROTECT (protected &rest cleanups) env
  `(unwind-protect ,(compile-form protected env)
     ,@(compile-forms cleanups env)))

(define-compiler VALUES (&rest forms) env
  (let ((n (length forms)))
    (case n
      (0	`(setq nvals 0 mvals nil))
      (1	`(progn
		   (setq nvals 1 mvals nil)
		   ,(compile-form (car forms) env)))
      (t	`(progn
		   (setq nvals ,n
		         mvals ',(compile-args (cdr forms) env))
		   ,(compile-form (car forms) env))))))
