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

(cl:defun COMPILE-FILE (input-file
			&REST keys
			&KEY OUTPUT-FILE
			     (VERBOSE *COMPILE-VERBOSE*)
			     (PRINT *COMPILE-PRINT*)
			     EXTERNAL-FORMAT)
  (let* ((*PACKAGE* *PACKAGE*)
	 (*READTABLE* *READTABLE*)
	 (*COMPILE-FILE-PATHNAME* (MERGE-PATHNAMES input-file))
	 (*COMPILE-FILE-TRUENAME* (TRUENAME *COMPILE-FILE-PATHNAME*))
	 (output (apply #'COMPILE-FILE-PATHNAME input-file keys))
	 (warnings-p nil)
	 (failure-p nil)
	 (*compile-file-mode* :not-compile-time))
    (WITH-COMPILATION-UNIT ()
      (WITH-OPEN-FILE (stream *COMPILE-FILE-PATHNAME*)
	(let ((eof (gensym)))
;	  (do ((form #1=(READ stream nil eof) #1#))
	  (do ((form (READ stream nil eof) (READ stream nil eof)))
	      ((eq form eof))
	    (FORMAT T "~%; Compile ~S" form)
	    (FORMAT T "~%; -> ~S" (compile2 form))))))
    (cl:values (TRUENAME output) warnings-p failure-p)))



(defvar *compile-file-mode* nil
  "Indicates whether file compilation is in effect, and if so, which
   mode: :compile-time-too or :not-compile-time.")

(defvar *genreg-counter* 0)

(defun genreg ()
  (prog1 (make-symbol (format "R%d" *genreg-counter*))
    (incf *genreg-counter*)))

(defvar *registers* (list (genreg)))
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
      (setf (cdr *next-register*) (list (genreg))))
    (setf *next-register* (cdr *next-register*))))

(defun lambda-expr-p (form)
  (and (consp form)
       (eq (car form) 'LAMBDA)))

(defmacro* with-fresh-context (&body body)
  `(let ((*next-register* *registers*)
	 (*free* nil)
	 (*bound* nil)
	 (*blocks-mentioned* nil))
     ,@body))

(defun compile1 (form)
  (byte-compile (compile2 form)))

(defun compile2 (form)
  (with-fresh-context
    (compile-form form *global-environment*)))

(defun cl-compiler-macroexpand (form env)
  (let ((exp1 t) (exp2 nil))
    (while (or exp1 exp2)
      (MULTIPLE-VALUE-SETQ (form exp1) (MACROEXPAND form env))
      (when (consp form)
	(let ((fn (COMPILER-MACRO-FUNCTION (car form) env)))
	  (if fn
	      (let ((new (funcall fn form env)))
		(setq exp2 (not (eq form new))
		      form new))
	      (setq exp2 nil))))))
  form)

(defun* compile-form (form &optional env &key (values 1))
  (when (and (consp form) (symbolp (first form)))
    (let* ((name (first form))
	   (fn (gethash name *form-compilers*)))
      (when fn
	(return-from compile-form (apply fn env (rest form))))))
  (setq form (cl-compiler-macroexpand form env))
  (cond
    ((symbolp form)
     (unless (eq values 0)
       (let ((val (compile-variable form env)))
	 (if (eq values t)
	     (if (null val)
		 `(setq nvals 1 mvals nil)
		 `(progn (setq nvals 1 mvals nil) ,val))
	     val))))
    ((atom form)
     (unless (eq values 0)
       (let ((val (compile-literal form)))
	 (if (eq values t)
	     (if (null val)
		 `(setq nvals 1 mvals nil)
		 `(progn (setq nvals 1 mvals nil) ,val))
	     val))))
    ((lambda-expr-p (first form))
     (let* ((lexp (first form))
	    (vars (cadr lexp))
	    (body (cddr lexp)))
       `(,(compile-lambda vars body env t) ,@(compile-forms (rest form) env))))
    ((symbolp (first form))
     (let* ((name (first form))
	    (fn (gethash name *form-compilers*)))
       (if fn
	   (apply fn env (rest form))
	   (compile-funcall `(QUOTE ,name) (rest form) env))))
    (t
     (ERROR "Syntax error: ~S" form))))

(defun compile-forms (args env)
  (mapcar (lambda (arg) (compile-form arg env)) args))

(defun* compile-body (forms env &key (values t))
  (if (null forms)
      nil
      (do* ((forms forms (cdr forms))
;	    (form #1=(car forms) #1#)
	    (form (car forms) (car forms))
	    (result nil))
	   ((null (cdr forms))
	    (push (compile-form form env :values values) result)
	    (nreverse result))
	(let ((comp (compile-form form env :values 0)))
	  (when comp
	    (push comp result))))))

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

(define-compiler "=" (&rest args) env
  (case (length args)
    (0	(ERROR "'=' needs at least one argument"))
    (1	'T)
    (2	`(binary= ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "=" *cl-package*)) args env))))

(define-compiler "/=" (&rest args) env
  (case (length args)
    (0	(ERROR "'/=' needs at least one argument"))
    (1	'T)
    (2	`(null (binary= ,@(compile-forms args env))))
    (t	(compile-funcall `(QUOTE ,(INTERN "/=" *cl-package*)) args env))))

(define-compiler "<" (&rest args) env
  (case (length args)
    (0	(ERROR "'<' needs at least one argument"))
    (1	'T)
    (2	`(binary< ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "<" *cl-package*)) args env))))

(define-compiler "<=" (&rest args) env
  (case (length args)
    (0	(ERROR "'<=' needs at least one argument"))
    (1	'T)
    (2	`(binary<= ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "<=" *cl-package*)) args env))))

(define-compiler ">" (&rest args) env
  (case (length args)
    (0	(ERROR "'>' needs at least one argument"))
    (1	'T)
    (2	(let ((forms (compile-forms args env)))
	  `(binary< ,(second forms) ,(first forms))))
    (t	(compile-funcall `(QUOTE ,(INTERN ">" *cl-package*)) args env))))

(define-compiler ">=" (&rest args) env
  (case (length args)
    (0	(ERROR "'>=' needs at least one argument"))
    (1	'T)
    (2	(let ((forms (compile-forms args env)))
	  `(binary<= ,(second forms) ,(first forms))))
    (t	(compile-funcall `(QUOTE ,(INTERN ">=" *cl-package*)) args env))))

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
	(body-form compiled-body))))

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

(define-compiler EVAL-WHEN (situations &rest body) env
  (let ((execute (or (memq (kw EXECUTE) situations)
		     (memq 'EVAL situations)))
	(compile-toplevel (or (memq (kw COMPILE-TOPLEVEL) situations)
			      (memq 'COMPILE) situations))
	(load-toplevel (or (memq (kw LOAD-TOPLEVEL) situations)
			   (memq 'LOAD) situations)))
    (cond
      ((or (and compile-toplevel
		(not load-toplevel)
		*compile-file-mode*)
	   (and (not compile-toplevel)
		(not load-toplevel)
		execute
		(eq *compile-file-mode* :compile-time-too)))
       (eval-with-env `(PROGN ,@body) env))
      (*compile-file-mode*
       (let ((*compile-file-mode*
	      (cond
		(compile-toplevel
		 (when load-toplevel :compile-time-too))
		(load-toplevel
		 (if execute *compile-file-mode* :not-compile-time)))))
	 (body-form (compile-body body env))))
      (execute
       (body-form (compile-body body env))))))

(define-compiler FLET (fns &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :function (mapcar #'first fns)))
	  (bindings nil))
      (dolist (fn fns)
	(let ((reg (new-register)))
	  (setf (lexical-function (first fn) new-env) reg)
	  (push `(,reg ,(compile-lambda (cadr fn) (cddr fn) env)) bindings)))
      (let ((compiled-body (compile-body body new-env)))
	(cond
	  ((null *free*))
	  ((create-environment-p env new-env)
	   (setq compiled-body (compile-environment compiled-body new-env)))
	  (t
	   (setq compiled-body
		 (compile-env-inits compiled-body
				    (mapcar
				     (lambda (fn)
				       (lexical-function (car fn) new-env))
				     fns)
				    new-env))))
	`(let ,bindings ,@compiled-body)))))

(defun compile-funcall (fn args env)
  (let ((compiled-args (compile-forms args env)))
    (if (and (consp fn) (eq (first fn) 'QUOTE) (symbolp (second fn)))
	(let ((name (second fn)))
	  (if (eq name 'FUNCALL)
	      (compile-form `(FUNCALL ,@args) env)
	      (multiple-value-bind (type localp decls)
		  (function-information name env)
		(if localp
		    (progn
		      (when (and (not (memq name *bound*))
				 (not (MEMBER name *free* (kw KEY) #'car)))
			(push (cons name (lexical-function name env))
			      *free*))
		      `(funcall ,(lexical-function name env)
			        ,@compiled-args))
		    `(,name ,@(compile-forms args env))))))
	(let ((fn (compile-form fn env)))
	  (cond
	    ((subrp fn)
	     `(,(intern (function-name fn)) ,@args))
	    ((byte-code-function-p fn)
	     `(funcall ,fn ,@args))
	    (t
	     `(FUNCALL ,fn ,@args)))))))

(define-compiler FUNCALL (fn &rest args) env
  (compile-funcall fn args env))

(define-compiler FUNCTION (name) env
  (if (lambda-expr-p name)
      (compile-lambda (cadr name) (cddr name) env)
      (multiple-value-bind (type localp decl) (function-information name env)
	(cond
	  (localp		(when (and (not (memq name *bound*))
					   (not (MEMBER name *free*
							(kw KEY) #'car)))
				  (push (cons name (lexical-function name env))
					*free*))
				(lexical-function name env))
	  ((symbolp name)	(symbol-function name))
	  ((setf-name-p name)	(FDEFINITION name))
	  (t			(ERROR "Syntax error: (FUNCTION ~S)" name))))))

(define-compiler GO (tag) env
  (let ((info (tagbody-information tag env)))
    (if info
	`(throw ',info ',tag)
	(ERROR "No tagbody for (GO ~S)" tag))))

(define-compiler IF (condition then &optional else) env
  (let ((compiled-condition (compile-form condition env))
	(compiled-then (compile-form then env)))
    (if (equal compiled-condition compiled-then)
	`(or ,compiled-condition
	     ,@(when else
	         (list (compile-form else env))))
	`(if ,compiled-condition
	     ,compiled-then
	     ,@(when else
	         (list (compile-form else env)))))))

(define-compiler LABELS (fns &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :function (mapcar #'first fns)))
	  (bindings nil)
	  (inits nil))
      (dolist (fn fns)
	(let ((reg (new-register)))
	  (setf (lexical-function (first fn) new-env) reg)
	  (push reg bindings)))
      (dolist (fn fns)
	(setq inits `(,(lexical-function (first fn) new-env)
		      ,(compile-lambda (cadr fn) (cddr fn) new-env)
		      ,@inits)))
      (let ((compiled-body (compile-body body new-env)))
	(cond
	  ((null *free*))
	  ((create-environment-p env new-env)
	   (setq compiled-body (compile-environment compiled-body new-env)))
	  (t
	   (setq compiled-body
		 (compile-env-inits compiled-body
				    (mapcar
				     (lambda (fn)
				       (lexical-function (car fn) new-env))
				     fns)
				    new-env))))
	`(let ,bindings (setf ,@inits) ,@compiled-body)))))

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

(defun variable-bound-p (var env)
  (multiple-value-bind (type localp decls) (variable-information var env)
    type))

(defun create-environment-p (env new-env)
  (let ((vars (mapcar #'car *free*)))
    (and (every (lambda (var) (not (variable-bound-p var env))) vars)
	 (some (lambda (var) (variable-bound-p var new-env)) vars))))

(defun initial-environment (env)
  (mapcar (lambda (var)
	    (when (variable-bound-p (car var) env)
	      (cdr var)))
	  *free*))

(defun compile-environment (body env)
  (let ((inits (initial-environment env))
	(nfree (length *free*))
	make-env)
    (cond
      ((<= nfree 2)
       (setq make-env (if (eq nfree 1) `(cons ,@inits nil) `(cons ,@inits)))
       (MAPC (lambda (var accessor)
	       (setq body (NSUBST `(,accessor env) (cdr var) body)))
	     *free* '(car cdr)))
      (t
       (setq make-env `(vector ,@inits))
       (let ((i -1))
	 (dolist (var *free*)
	   (setq body (NSUBST `(aref env ,(incf i)) (cdr var) body))))))
    (prog1 `((let ((env ,make-env)) ,@body))
      (setq *free* nil))))

(defun compile-env-inits (body vars env)
  (let ((inits nil))
    (dolist (var *free*)
      (when (memq (cdr var) vars)
	    ;(and (memq (car var) vars)
	    ;	 (eq (compile-variable (car var) env) (cdr var)))
	(let ((reg (new-register)))
	  (setq inits `(,reg ,(cdr var) ,@inits))
	  (setq body (NSUBST reg (cdr var) body))
	  (setf (cdr var) reg))))
    (if inits
	`((setf ,@inits) ,@body)
	body)))

(defun compile-trampoline (lambda-list body env)
  `(trampoline ,(expand-lambda
		 lambda-list
		 (compile-env-inits body (compile-forms lambda-list env) env)
		 env)
               env))

(defun compile-lambda (lambda-list forms env &optional keep-bindings)
  (MULTIPLE-VALUE-BIND (body decls doc) (parse-body forms t)
    (let* ((vars (lambda-list-variables lambda-list))
	   (*bound* (when keep-bindings *bound*))
	   (new-env (env-with-vars env vars decls))
	   (compiled-body (compile-body body new-env)))
      (dolist (decl decls)
	(when (eq (first decl) 'INTERACTIVE)
	  (push `(interactive ,@(rest decl)) compiled-body)))
      (when doc
	(push doc compiled-body))
      (cond
	((null *free*)
	 (expand-lambda lambda-list compiled-body new-env))
	((create-environment-p env new-env)
	 (expand-lambda
	  lambda-list (compile-environment compiled-body new-env) new-env))
	(t
	 (compile-trampoline lambda-list compiled-body new-env))))))

(defun partition-bindings (bindings env)
  (let ((lexical-bindings nil)
	(special-bindings nil))
    (dolist (binding bindings)
      (let ((list (if (symbolp binding)
		      (list binding nil)
		      binding)))
	(cond
	  (t
	   (push list lexical-bindings)))))
    (cl:values lexical-bindings special-bindings)))

(defun first-or-identity (x)
  (if (atom x) x (car x)))

(defun body-form (body)
  (if (and (consp body) (null (cdr body)))
      (car body)
      `(progn ,@body)))

(defun side-effect-free-p (form)
  (or (atom form)
      (let ((fn (car form)))
	(or (eq fn 'quote)
	    (and (symbolp fn)
		 (get fn 'side-effect-free)
		 (every #'side-effect-free-p (cdr form)))))))

(define-compiler LET (bindings &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((vars (mapcar #'first-or-identity bindings))
	   (new-env (env-with-vars env vars decls))
	   (compiled-body (compile-body body new-env)))
      (MULTIPLE-VALUE-BIND (lexical-bindings special-bindings)
	  (partition-bindings bindings new-env)
	(let* ((let-bindings
		(append
		 (mapcar (lambda (list)
			   `(,(compile-variable (first list) new-env)
			     ,(compile-form (second list) env)))
			 lexical-bindings)
		 (mapcar (lambda (list)
			   `(,(first list)
			     ,(compile-form (second list) env)))
			 special-bindings)))
	       (body (cond
		       ((null *free*)
			compiled-body)
		       ((create-environment-p env new-env)
			(compile-environment compiled-body new-env))
		       (t
			(compile-env-inits
			 compiled-body
			 (compile-forms (mapcar #'car lexical-bindings)
					new-env)
			 new-env)))))
	  (dolist (list let-bindings)
	    (when (side-effect-free-p (second list))
	      (case (tree-count (first list) body)
		(0 (setq let-bindings (DELETE list let-bindings)))
		(1 (setq body (NSUBST (second list) (first list) body))
		   (setq let-bindings (DELETE list let-bindings))))))
	  (if let-bindings
	      `(let ,let-bindings ,@body)
	      (body-form body)))))))

(define-compiler LET* (bindings &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (compile-form (if (null bindings)
		      `(LOCALLY ,@body)
		      `(LET (,(first bindings))
			(LET* ,(rest bindings)
			  ,@body)))
		  env)))

;;; TODO: load-time-value

(define-compiler LOCALLY (&rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    ;; TODO: process decls
    (body-form (compile-body forms env))))

(define-compiler MACROLET (macros &body forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :macro (mapcar #'first macros))))
      (dolist (macro macros)
	(setf (MACRO-FUNCTION (first macro) new-env)
	      (enclose `(LAMBDA (form env)
			 ;; TODO: destructuring-bind
			 (APPLY (LAMBDA ,@(rest macro)) (CDR form)))
		       env (first macro))))
      (body-form (compile-body body new-env)))))

(define-compiler MULTIPLE-VALUE-BIND (vars form &body forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((new-env (env-with-vars env vars decls))
	   (compiled-body (compile-body body new-env)))
      (unless (null *free*)
	(setq compiled-body
	      (if (create-environment-p env new-env)
		  (compile-environment compiled-body new-env)
		  (compile-env-inits compiled-body
				     (compile-forms vars new-env)
				     new-env))))
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
  (body-form (compile-body body env)))

(define-compiler PROGV (symbols values &body body) env
  `(do-progv ,(compile-form symbols env)
             ,(compile-form values env)
	     ,(compile-lambda () body env)))

(define-compiler QUOTE (form) env
  (compile-literal form))

(define-compiler RETURN-FROM (tag &optional form) env
  (let ((block (block-information tag env)))
    (if block
	(let ((block-tag (cdr block)))
	  (pushnew block-tag *blocks-mentioned*)
	  `(throw ',block-tag ,(compile-form form env)))
	(ERROR "No block for (RETURN-FROM ~S)" form))))

(define-compiler SETQ (&rest forms) env
  (when (oddp (length forms))
    (ERROR "Odd number of forms in SETQ"))
  (body-form
   (mapcar2
    (lambda (var val)
      (unless (symbolp var)
	(ERROR "Setting non-symbol ~S" var))
      (multiple-value-bind (type localp) (variable-information var env)
	(ecase type
	  (:lexical
	   `(setf ,(compile-variable var env) ,(compile-form val env)))
	  ((:special nil)
	   (when (null type)
	     (WARN "Setting undefined variable ~S" var))
	   `(setq ,var ,(compile-form val env)))
	  (:symbol-macro
	   (compile-form `(SETF ,var ,val) env))
	  (:constant
	   (ERROR "Setting constant ~S" var)))))
    forms)))

(define-compiler SYMBOL-MACROLET (macros &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :symbol-macro
					(mapcar #'first macros))))
      (dolist (macro macros)
	(setf (lexical-value (first macro) new-env)
	      (enclose `(LAMBDA (form env) (QUOTE ,(second macro)))
		       env (first macro))))
      (body-form (compile-body body new-env)))))

(defun compile-tagbody-forms (forms tagbody start env)
  (let* ((nofirst (eq start (car forms)))
	 (clause (unless nofirst (list (list start)))))
    (do ((clauses nil)
	 (forms forms (cdr forms)))
	((null forms)
	 (unless (eq (first (car (last clause))) 'throw)
	   (setq clause (append clause `((throw ',tagbody nil)))))
	 (setq clauses (append clauses `(,clause)))
	 clauses)
      (let ((form (first forms)))
	(cond
	  ((atom form)
	   (unless nofirst
	     (setq clause (append clause `((throw ',tagbody ',form))))
	     (setq nofirst nil))
	   (when clause
	     (setq clauses (append clauses `(,clause))))
	   (setq clause `((,form))))
	  (t
	   (setq clause (append clause `(,(compile-form form env))))))))))

(define-compiler TAGBODY (&rest forms) env
  (let* ((tagbody (gensym))
	 (new-env (augment-environment
		   env :tagbody
		   (cons tagbody (remove-if-not #'go-tag-p forms))))
	 (pc (new-register))
	 (start (if (go-tag-p (car forms)) (car forms) (gensym))))
    (let ((last (last forms)))
      (if (and (consp last)
	       (setq last (car last))
	       (eq (first last) 'GO)
	       (eq (second last) start)
	       (notany #'go-tag-p (rest forms)))
	  `(while t ,@(compile-body (butlast (rest forms)) env :values 0))
	  `(let ((,pc ',start))
	    (while (setq ,pc (catch ',tagbody
			       (case ,pc
				 ,@(compile-tagbody-forms
				    forms tagbody start new-env))))))))))

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
