;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements the compiler.

(IN-PACKAGE "EMACS-CL")

;;; (defun fac (n) (if (< n 2) 1 (* n (fac (1- n)))))
;;; (fac 100)
;;; 0.31 seconds

;;; (defun fib (n) (if (< n 2) 1 (+ (fib (1- n)) (fib (- n 2)))))
;;; (fib 22)
;;; 2.223 seconds

;;; (defun tak (x y z)
;;;   (if (not (< y x))
;;;       z
;;;       (tak (tak (1- x) y z) (tak (1- y) z x) (tak (1- z) x y))))
;;; (tak 18 12 6)
;;; 1.134 seconds

(defun COMPILE (name &optional definition)
  (when (null definition)
    (if (FBOUNDP name)
	(setq definition (FDEFINITION name))
	(setq definition (MACRO-FUNCTION name))))
  (when (INTERPRETED-FUNCTION-P definition)
    (setq definition (FUNCTION-LAMBDA-EXPRESSION definition)))
  (when (consp definition)
    (setq definition (compile1 definition)))
  (if name
      (progn
	(if (FBOUNDP name)
	    (setf (FDEFINITION name) definition)
	    (setf (MACRO-FUNCTION name) definition))
	(cl:values name nil nil))
      (cl:values definition nil nil)))



(defvar *registers* (list (gensym)))
(defvar *next-register* nil)

(defvar *bound* nil
  "A list of variables bound in the function currently being compiled.")

(defvar *free* nil
  "An alist of all variables are ever free in any function in the
   top-level form being compiled.")

(defvar *blocks-mentioned* nil
  "A list of all block names mentioned in the top-level form being compiled.")

(defun new-register ()
  (prog1
      (car *next-register*)
    (when (null (cdr *next-register*))
      (setf (cdr *next-register*) (list (gensym))))
    (setf *next-register* (cdr *next-register*))))

(defun lambda-expr-p (form)
  (and (consp form)
       (eq (car form) 'LAMBDA)))

(defmacro* with-fresh-context (&body body)
  `(let ((*next-register* *registers*)
	 (*free* nil)
	 (*blocks-mentioned* nil))
     ,@body))

(defun compile1 (form)
  (byte-compile (compile2 form)))

(defun compile2 (form)
  (with-fresh-context
    (compile-form form *global-environment*)))

(defun* compile-form (form &optional env &key (values 1))
  (when (and (consp form)
	     (symbolp (first form)))
    (let* ((name (first form))
	   (fn (gethash name *form-compilers*)))
      (when fn
	(return-from compile-form (apply fn env (rest form))))))
  (setq form (cl:values (MACROEXPAND form env)))
  (cond
    ((symbolp form)
     (unless (eq values 0)
       (let ((val (compile-variable form env)))
	 (if (eq values t)
	     `(progn (setq nvals 1 mvals nil) ,val)
	     val))))
    ((atom form)
     (unless (eq values 0)
       (let ((val (compile-literal form)))
	 (if (eq values t)
	     `(progn (setq nvals 1 mvals nil) ,val)
	     val))))
    ((lambda-expr-p (first form))
     `(,(compile-lambda form env) ,@(compile-forms (rest form) env)))
    ((symbolp (first form))
     (let* ((name (first form))
	    (fn (gethash name *form-compilers*)))
       (if fn
	   (apply fn env (rest form))
	   (compile-funcall `(QUOTE ,name)
			    (compile-forms (rest form) env) env))))
    (t
     (ERROR "Syntax error: ~S" form))))

(defun compile-forms (args env)
  (mapcar (lambda (arg) (compile-form arg env)) args))

(defun* compile-body (forms env &key (values t))
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

(defun variable-bound-p (var env)
  (multiple-value-bind (type localp decls) (variable-information var env)
    type))

(defun compile-variable (var env)
  (multiple-value-bind (type localp decls) (variable-information var env)
    (ecase type
      ((:special nil)	var)
      (:lexical		(when (and (not (memq var *bound*))
				   (not (MEMBER var *free* (kw KEY) #'car)))
			  (push (cons var (lexical-value var env)) *free*))
			(lexical-value var env))
      (:constant	(symbol-value var)))))

(defun compile-literal (literal)
  (cond
    ((or (symbolp literal) (consp literal))
     `(quote ,literal))
    (t
     literal)))

(defvar *form-compilers* (make-hash-table))

(defmacro* define-compiler (operator lambda-list env &body body)
  (when (stringp operator)
    (setq operator (INTERN operator *cl-package*)))
  `(setf (gethash ',operator *form-compilers*)
	 (function* (lambda (,env ,@lambda-list) ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-compiler "*" (&rest args) env
  (case (length args)
    (0	1)
    (1	(compile-form (first args) env))
    (2	`(binary* ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "*" *cl-package*)) args env))))

(define-compiler "+" (&rest args) env
  (case (length args)
    (0	0)
    (1	(compile-form (first args) env))
    (2	`(binary* ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "*" *cl-package*)) args env))))

(define-compiler "1+" (arg) env
  `(binary+ ,(compile-form arg env) 1))

(define-compiler "1-" (arg) env
  `(binary+ ,(compile-form arg env) -1))

(define-compiler APPLY (fn &rest args) env
  (let ((fn (compile-form fn env))
	(args (compile-forms args env)))
    (cond
      ((subrp fn)
       `(apply #',(intern (function-name fn)) ,@args))
      ((byte-code-function-p fn)
       `(apply ,fn ,@args))
      (t
       `(APPLY ,fn ,@args)))))

(define-compiler BLOCK (tag &rest body) env
  (let* ((block (gensym))
	 (new-env (augment-environment env :block (cons tag block)))
	 (compiled-body (compile-body body new-env)))
    (if (memq block *blocks-mentioned*)
	`(catch ',block ,@compiled-body)
	`(progn ,@compiled-body))))

(define-compiler CATCH (tag &rest body) env
  `(catch ,(compile-form tag env) ,@(compile-body body env)))

(define-compiler COND (&rest clauses) env
  `(cond ,@(mapcar (lambda (clause)
		     (unless (consp clause)
		       (ERROR 'PROGRAM-ERROR))
		     (if (and (CONSTANTP (first clause))
			      (not (null (first clause))))
			 `(t ,@(compile-body (rest clause) env))
			 (mapcar (lambda (form) (compile-form form env))
				 clause)))
		   clauses)))

;;; TODO: eval-when
(define-compiler EVAL-WHEN (situations &rest body) env
  `(progn ,@(compile-body body env)))

(define-compiler FLET (fns &rest body) env
  (let ((new-env (augment-environment env :function (mapcar #'first fns))))
    (dolist (fn fns)
      (setf (lexical-function (first fn) new-env)
	    (compile-form `(LAMBDA ,@(rest fn)) env)))
    `(progn ,@(compile-body body new-env))))

(defun compile-funcall (fn args env)
  (if (and (consp fn) (eq (first fn) 'QUOTE) (symbolp (second fn)))
      (if (eq (second fn) 'FUNCALL)
	  (compile-form `(FUNCALL ,@args) env)
	  `(,(second fn) ,@(compile-forms args env)))
      (let ((fn (compile-form fn env))
	    (args (compile-forms args env)))
	(cond
	  ((subrp fn)
	   `(,(intern (function-name fn)) ,@args))
	  ((byte-code-function-p fn)
	   `(funcall ,fn ,@args))
	  (t
	   `(FUNCALL ,fn ,@args))))))

(define-compiler FUNCALL (fn &rest args) env
  (compile-funcall fn args env))

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
    `(progn ,@(compile-body body new-env))))

(defun lambda-list-parameters (lambda-list)
  (let ((result nil))
    (dolist (var lambda-list (nreverse result))
      (unless (lambda-list-keyword-p var)
	(push (cond
		((symbolp var)			    var)
		((consp var)	(let ((var (car var)))
				  (cond
				    ((symbolp var)  var)
				    ((consp var)    (car var))
				    (t		    (ERROR 'PROGRAM-ERROR)))))
		(t				    (ERROR 'PROGRAM-ERROR)))
	      result)))))

(if (fboundp 'compiled-function-constants)
    (progn
      (defvar compiled-function-accessors
	'(compiled-function-arglist compiled-function-instructions
	  compiled-function-constants compiled-function-stack-depth
	  compiled-function-doc-string compiled-function-interactive))
      (defun cfref (fn i)
	(funcall (nth i compiled-function-accessors) fn)))
    (defun cfref (fn i)
      (aref fn i)))

(let ((fn (vector))
      (env (vector)))
  (defvar *trampoline-template*
    (byte-compile `(lambda (&rest args) (let ((env ,env)) (apply ,fn args)))))
  (defvar *trampoline-constants*
    (cfref *trampoline-template* 2))
  (defvar *trampoline-fn-pos*
    (position fn *trampoline-constants*))
  (defvar *trampoline-env-pos*
    (position env *trampoline-constants*)))

(defvar *trampoline-length*
  (condition-case c
      (length *trampoline-template*)
    (error 6)))

(defmacro defun-trampoline ()
  `(defun trampoline (fn env)
     (let* ((consts (copy-sequence ,*trampoline-constants*))
	    (tramp
	     (make-byte-code
	      ,@(let ((args nil))
		  (dotimes (i *trampoline-length* (nreverse args))
		    (push (if (eq i 2)
			      'consts
			      `',(cfref *trampoline-template* i))
			  args))))))
       (aset consts *trampoline-fn-pos* fn)
       (aset consts *trampoline-env-pos* env)
       tramp)))

(defun-trampoline)

(defun env-with-vars (env vars decls)
  (if vars
      (let ((new-env (augment-environment env :variable vars)))
	(dolist (var vars)
	  (setf (lexical-value var new-env) (new-register))
	  (push var *bound*))
	new-env)
      env))

(defun* compile-lambda (form env)
  (MULTIPLE-VALUE-BIND (body decls) (cddr form)
    (let* ((vars (second form))
	   (new-env (env-with-vars env vars decls)))
      (let ((compiled-body (compile-body body new-env)))
	(cond
	  ((null *free*)
	   (expand-lambda vars compiled-body new-env))
	  ((every (lambda (var) (member (car var) *bound*)) *free*)
	   (let ((i -1))
	     (dolist (var *free*)
	       (NSUBST `(aref env ,(incf i)) (cdr var) compiled-body)))
	   (expand-lambda
	    vars
	    `((let ((env (vector
			  ,@(mapcar
			     (lambda (var)
			       (when (variable-bound-p (car var) new-env)
				 (cdr var)))
			     *free*))))
		,@compiled-body))
	    new-env))
	  (t
	   `(trampoline ,(expand-lambda vars compiled-body new-env) env)))))))

(defun partition-bindings (bindings env)
  (let ((local-bindings nil)
	(closure-bindings nil)
	(special-bindings nil))
    (dolist (binding bindings)
      (let ((list (if (symbolp binding)
		      (list binding nil)
		      binding)))
	(cond
	  ((MEMBER (first list) *free* (kw KEY) #'car)
	   (push list closure-bindings))
	  (t
	   (push list local-bindings)))))
    (cl:values local-bindings closure-bindings special-bindings)))

(defun first-or-identity (x)
  (if (atom x) x (car x)))

(define-compiler LET (bindings &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((vars (mapcar #'first-or-identity bindings))
	   (new-env (env-with-vars env vars decls))
	   (compiled-body (compile-body body new-env)))
      (MULTIPLE-VALUE-BIND (local-bindings closure-bindings special-bindings)
	  (partition-bindings bindings new-env)
	(print (format "free: %s, bound: %s" *free* *bound*))
	(let ((let-bindings
	       (append
		(mapcar (lambda (list)
			  `(,(compile-variable (first list) new-env)
			    ,(compile-form (second list) env)))
			local-bindings)
		(mapcar (lambda (list)
			  `(,(first list)
			    ,(compile-form (second list) env)))
			special-bindings)))
	      (body (cond
		      ((null *free*)
		       compiled-body)
		      ((every (lambda (var)
				(memq (car var) *bound*))
			      *free*)
		       `((setf
			  ,@(mappend
			     (lambda (list)
			       `(,(compile-variable (first list) new-env)
				 ,(compile-form (second list) env)))
			     closure-bindings))
			 ,@compiled-body)))))
	  (if let-bindings
	      `(let ,let-bindings ,@body)
	      `(progn ,@body)))))))

(define-compiler LET* (bindings &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((vars (lexical-binding-variables bindings env))
	   (new-env (env-with-vars env vars decls)))
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
	 ,@(compile-body body new-env)))))

;;; TODO: load-time-value

(define-compiler LOCALLY (&rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    ;; TODO: process decls
    `(progn ,@(compile-body forms env))))

(define-compiler MACROLET (macros &body forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    ;; TODO: bind macros, process decls
    (compile-body body env)))

(define-compiler MULTIPLE-VALUE-BIND (vars form &body forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((new-env (env-with-vars env vars decls))
	   (compiled-body (compile-body body new-env)))
      (case (length vars)
	(0 `(progn
	     ,(compile-form form env :values 0)
	     ,@compiled-body))
	(1 `(let ((,(compile-variable (first vars) new-env)
		   ,(compile-form form env :values 1)))
	     ,@compiled-body))
	(t `(let* ((,(compile-variable (first vars) new-env)
		    ,(compile-form form env :values t))
		   ,@(mapcar (lambda (var)
			       `(,(compile-variable var new-env)
				 (pop mvals)))
			     (rest vars)))
	     ,@compiled-body))))))

(define-compiler MULTIPLE-VALUE-CALL (fn &rest forms) env
  (if (null forms)
      (compile-form `(FUNCALL ,fn) env)
      `(APPLY ,(compile-form fn env)
	      (append ,@(mapcar (lambda (form)
				  (compile-form
				   `(MULTIPLE-VALUE-LIST ,form)
				   env :values t))
				forms)))))

(define-compiler MULTIPLE-VALUE-LIST (form) env
  (let ((val (new-register)))
    `(let ((,val ,(compile-form form env :values t)))
       (if (zerop nvals)
	   nil
	   (cons ,val mvals)))))

(define-compiler MULTIPLE-VALUE-PROG1 (form &rest forms) env
  (let ((val (new-register))
	(ntemp (new-register))
	(mtemp (new-register)))
    `(let* ((,val ,(compile-form form env :values t))
	    (,ntemp nvals)
	    (,mtemp mvals))
       ,@(compile-body forms env :values 0)
       (setq nvals ,ntemp mvals ,mtemp)
       ,val)))

(define-compiler NOT (form) env
  `(null ,(compile-form form env)))

(define-compiler NULL (form) env
  `(null ,(compile-form form env)))

(define-compiler PROGN (&rest body) env
  `(progn ,@(compile-body body env)))

;;; TODO: progv

(define-compiler QUOTE (form) env
  (compile-literal form))

(define-compiler RETURN-FROM (tag &optional form) env
  (let ((block (block-information tag env)))
    (if block
	(let ((block-tag (cdr block)))
	  (pushnew block-tag *blocks-mentioned*)
	  `(throw ',block-tag ,(compile-form form env)))
	(ERROR "No block for (RETURN-FROM ~S)" form))))

(define-compiler SETQ (var val &rest more) env
  (multiple-value-bind (type localp) (variable-information var env)
    (ecase type
      ((:lexical :special nil)
       `(setf ,(compile-variable var env) ,(compile-form val env)
	      ,@(when more
		  (rest (compile-form `(SETQ ,@more)))))))))

(defun compile-symbol-macrolet (forms)
  (let ((macros (second forms))
	(body (cddr forms)))
    (compile-body body env)))

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
     ,@(compile-body cleanups env)))

(define-compiler VALUES (&rest forms) env
  (let ((n (length forms)))
    (case n
      (0	`(setq nvals 0 mvals nil))
      (1	`(progn
		   (setq nvals 1 mvals nil)
		   ,(compile-form (car forms) env)))
      (t	`(progn
		   (setq nvals ,n
		         mvals ',(compile-forms (cdr forms) env))
		   ,(compile-form (car forms) env))))))
