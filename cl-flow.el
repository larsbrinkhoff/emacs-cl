;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 5, Data and Control Flow.

(IN-PACKAGE "EMACS-CL")

(defvar *setf-definitions* (make-hash-table))

(defun APPLY (fn &rest args)
  (cond
    ((COMPILED-FUNCTION-P fn)
     (apply #'apply fn args))
    ((INTERPRETED-FUNCTION-P fn)
     (eval-lambda-form (append (list (aref fn 1))
			       (butlast args)
			       (car (last args)))
		       (aref fn 2)))
    ((functionp fn)
     (apply #'apply fn args))
    (t
     (apply #'APPLY (FDEFINITION fn) args))))

(cl:defmacro DEFUN (name lambda-list &body body)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (SETF (FDEFINITION (QUOTE ,name))
           (FUNCTION (LAMBDA ,lambda-list (BLOCK ,name ,@body))))
     (SETF (function-name (FDEFINITION (QUOTE ,name)))
	   (QUOTE ,name))
     (QUOTE ,name)))

(defun setf-name-p (name)
  (and (consp name)
       (eq (car name) 'SETF)
       (symbolp (cadr name))
       (null (cddr name))))

(defun not-function-name-error (name)
  (type-error name '(OR SYMBOL (CONS (EQL SETF) (CONS SYMBOL NULL)))))

(defun FDEFINITION (name)
  (cond
    ((symbolp name)
     (unless (fboundp name)
       (ERROR 'UNDEFINED-FUNCTION (kw NAME) name))
     (if (or (SPECIAL-OPERATOR-P name) (MACRO-FUNCTION name))
	 nil
	 (symbol-function name)))
    ((setf-name-p name)
     (let ((fn (gethash (second name) *setf-definitions*)))
       (if (null fn)
	   (ERROR 'UNDEFINED-FUNCTION (kw NAME) name)
	   fn)))
    (t
     (not-function-name-error name))))

(defsetf FDEFINITION (name) (fn)
  `(cond
    ((symbolp ,name)
     (fset ,name ,fn))
    ((setf-name-p name)
     (puthash (second ,name) ,fn *setf-definitions*))
    (t
     (not-function-name-error ,name))))

(defun FBOUNDP (name)
  (cond
    ((symbolp name)
     (fboundp name))
    ((setf-name-p name)
     (not (null (gethash (second name) *setf-definitions*))))
    (t
     (not-function-name-error name))))
    
(defun FMAKUNBOUND (name)
  (cond
    ((symbolp name)
     (fmakunbound name)
     (remhash name *macro-functions*))
    ((setf-name-p name)
     (remhash (second name) *setf-definitions*))
    (t
     (not-function-name-error name)))
  name)
    
;;; Special operators: FLET, LABELS, MACROLET

(defun FUNCALL (fn &rest args)
  (cond
    ((COMPILED-FUNCTION-P fn)
     (apply fn args))
    ((INTERPRETED-FUNCTION-P fn)
     (eval-lambda-form (cons (aref fn 1) args) (aref fn 2)))
    ((functionp fn)
     (apply fn args))
    (t
     (APPLY (FDEFINITION fn) args))))

;;; Special operator: FUNCTION

(defun FUNCTION-LAMBDA-EXPRESSION (fn)
  (cond
    ((INTERPRETED-FUNCTION-P fn)	(VALUES (aref fn 1) T nil))
    ((subrp fn)				(VALUES nil nil nil))
    ((byte-code-function-p fn)		(VALUES nil nil nil))
    ((FUNCTIONP fn)			(VALUES nil T nil))
    (t					(error "type error"))))

(defun FUNCTIONP (object)
  (or (byte-code-function-p object)
      (subrp object)
      (INTERPRETED-FUNCTION-P object)))

(defun COMPILED-FUNCTION-P (object)
  (or (byte-code-function-p object)
      (subrp object)))

(defvar *constants* '(nil T PI))

(defmacro* DEFCONSTANT (name initial-value &optional documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (defconst ,name ,initial-value)
    (pushnew ',name *constants*)
    ',name))

(cl:defmacro DEFCONSTANT (name initial-value &optional documentation)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (DEFVAR ,name ,initial-value)
     (PUSHNEW (QUOTE ,name) *constants*)
     (QUOTE ,name)))

(DEFCONSTANT CALL-ARGUMENT-LIMIT 50)

(DEFCONSTANT LAMBDA-LIST-KEYWORDS
  '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

(DEFCONSTANT LAMBDA-PARAMETERS-LIMIT 50)

(cl:defmacro DEFVAR (name &optional (initial-value nil valuep) documentation)
  (with-gensyms (val)
    `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
       ,@(when valuep
	   `((LET ((,val ,initial-value))
	       (UNLESS (BOUNDP (QUOTE ,name))
		 (SETQ ,name ,val)))))
       (QUOTE ,name))))

(cl:defmacro DEFPARAMETER (name initial-value &optional documentation)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (SETQ ,name ,initial-value)
     (QUOTE ,name)))

; (dl:defmacro DESTRUCTURING-BIND (lambda-list form &body body)
;   (with-gensyms (name)
;     `(MACROLET ((,name ,lambda-list ,@body))
;        (,name ,form))))

(defun lambda-list-keyword-p (x)
  (member x LAMBDA-LIST-KEYWORDS))

(defun flatten (list)
  (mappend (lambda (x) (if (consp x) (flatten x) (list x))) list))

(defun destructure (lambda-list struct)
  (if (null lambda-list)
      nil
      (let ((result nil)
	    (current nil))
	(setq current (pop lambda-list))
	(when (eq current '&whole)
	  (push `(SETQ ,(pop lambda-list) ,struct) result)
	  (setq current (pop lambda-list)))
	(push `(WHEN (ATOM ,struct) (ERROR)) result)
	(while (and current (not (lambda-list-keyword-p current)))
	  (cond
	    ((symbolp current)
	     (push `(SETQ ,current (CAR ,struct)) result))
	    ((consp current)
	     (let ((subtree (gensym)))
	       (push `(SETQ ,subtree (CAR ,struct)) result)
	       (setq result (append (reverse (destructure current subtree))
				    result)))))
	  (push `(SETQ ,struct (CDR ,struct)) result)
	  (setq current (pop lambda-list)))
	(nreverse result))))

(cl:defmacro DESTRUCTURING-BIND (lambda-list form &body body)
  (with-gensyms (val)
    `(LET ,(remove-if #'lambda-list-keyword-p (flatten lambda-list))
       (LET ((,val ,form))
	 ,@(destructure lambda-list val))
       ,@body)))

;;; Special Operators: LET, LET*

;;; Special Operator: PROGV

;;; Special Operator: SETQ

(cl:defmacro PSETQ (&rest forms)
  (if (oddp (length forms))
      (error "syntax error")
      (do ((forms forms (cddr forms))
	   (vars nil)
	   (vals nil))
	  ((null forms)
	   (let ((temps (map-to-gensyms vars)))
	     `(LET ,(MAPCAR #'LIST temps vals)
	        (SETQ ,@(MAPCAN #'LIST vars temps)))))
	(push (first forms) vars)
	(push (second forms) vals))))

;;; Special Operator: BLOCK

;;; Special Operator: CATCH

;;; Special Operator: GO

;;; Special Operator: RETURN-FROM

(cl:defmacro RETURN (&optional form)
  `(RETURN-FROM nil ,form))

;;; Special Operator: TAGBODY

;;; Special Operator: THROW

;;; Special Operator: UNWIND-PROTECT

(defun expand-tagbody-forms (body start end)
  (do ((clauses nil)
       (clause (list (list start)))
       (forms body (cdr forms)))
      ((null forms)
       (setq clause (append clause (list (list 'go end))))
       (setq clauses (append clauses `(,clause)))
       clauses)
    (let ((form (first forms)))
      (cond
	((atom form)
	 (setq clause (append clause `((go ,form))))
	 (setq clauses (append clauses `(,clause)))
	 (setq clause `((,form))))
	(t
	 (setq clause (append clause `(,form))))))))

(defmacro* tagbody (&body body)
  (let ((pc (gensym))
	(start (gensym))
	(end (gensym))
	(throw-tag (gensym)))
    `(let ((,pc ',start))
      (macrolet ((go (tag)
		   (list 'throw
			 (list 'quote ',throw-tag)
			 (list 'quote tag))))
	(while (not (eq ,pc ',end))
	  (setq ,pc
		(catch ',throw-tag
		  (case ,pc
		    ,@(expand-tagbody-forms body start end))))))
      nil)))

;;; Constant Variable: NIL

(fset 'NOT (symbol-function 'not))

(DEFCONSTANT T 'T)

(fset 'EQ (symbol-function 'eq))

(defun EQL (x y)
  (or (eq x y)
      (cond
	((and (CHARACTERP x) (CHARACTERP y))
	 (eq (CHAR-CODE x) (CHAR-CODE y)))
	((and (bignump x) (bignump y))
	 (and (eq (length x) (length y))
	      (every #'eq x y)))
	((and (ratiop x) (ratiop y))
	 (and (EQL (NUMERATOR x) (NUMERATOR y))
	      (EQL (DENOMINATOR x) (DENOMINATOR y))))
	((and (COMPLEXP x) (COMPLEXP y))
	 (and (EQL (REALPART x) (REALPART y))
	      (EQL (IMAGPART x) (IMAGPART y))))
	(t
	 nil))))

(defun EQUAL (x y)
  (or (EQL x y)
      (cond
	((and (consp x) (consp y))
	 (and (EQUAL (car x) (car y))
	      (EQUAL (cdr x) (cdr y))))
	((and (STRINGP x) (STRINGP y))
	 (and (eq (LENGTH x) (LENGTH y))
	      (every #'eq x y)))
	((and (BIT-VECTOR-P x) (BIT-VECTOR-P y))
	 (and (eq (LENGTH x) (LENGTH y))
	      (every #'eq x y)))
	;; TODO: pathnames
	(t
	 nil))))

(defun EQUALP (x y)
  (or (EQUAL x y)
      (cond
	((and (CHARACTERP x) (CHARACTERP y))
	 (CHAR-EQUAL x y))
	((and (NUMBERP x) (NUMBERP y))
	 (cl:= x y))
	((and (consp x) (consp y))
	 (and (EQUAL (car x) (car y))
	      (EQUAL (cdr x) (cdr y))))
	((and (ARRAYP x) (ARRAYP y))
	 (and (equal (ARRAY-DIMENSIONS x) (ARRAY-DIMENSIONS y))
	      ;; TODO
	      'maybe))
	;; TODO: structures and hash tables
	(t
	 nil))))

(cl:defun IDENTITY (object)
  object)

(defun COMPLEMENT (fn)
  (let ((env (augment-environment nil :variable '(fn))))
    (setf (lexical-value 'fn env) fn)
    (enclose '(LAMBDA (x) (NOT (FUNCALL fn x))) env
	     (format "\"complement of %s\"" (PRIN1-TO-STRING fn)))))

(defun CONSTANTLY (value)
  (let ((env (augment-environment nil :variable '(value))))
    (setf (lexical-value 'value env) value)
    (enclose '(LAMBDA (&rest x) value) env
	     (format "\"constantly %s\"" (PRIN1-TO-STRING value)))))

(defun* EVERY (predicate &rest sequences)
  (apply #'mapcar
	 (lambda (&rest elts)
	   (unless (APPLY predicate elts)
	     (return-from EVERY nil)))
	 sequences)
  T)

(defun* SOME (predicate &rest sequences)
  (apply #'mapcar
	 (lambda (&rest elts)
	   (let ((val (APPLY predicate elts)))
	     (when val
	       (return-from SOME val))))
	 sequences)
  nil)

(defun NOTEVERY (predicate &rest sequences)
  (not (apply #'EVERY predicate sequences)))

(defun NOTANY (predicate &rest sequences)
  (not (apply #'SOME predicate sequences)))

(cl:defmacro AND (&rest forms)
  (if (null forms)
      T
      `(IF ,(first forms) (AND ,@(rest forms)))))

(cl:defmacro COND (&rest clauses)
  (if (null clauses)
      nil
      (let ((clause (first clauses)))
	(case (length clause)
	  (0	`(COND ,@(rest clauses)))
	  (1	`(OR ,(first clause) (COND ,@(rest clauses))))
	  (t	`(IF ,(first clause) (PROGN ,@(rest clause))
				     (COND ,@(rest clauses))))))))

;;; Special Operator: IF

(cl:defmacro OR (&rest forms)
  (if (null forms)
      nil
      (with-gensyms (x)
	`(LET ((,x ,(first forms)))
	   (IF ,x ,x (OR ,@(rest forms)))))))

(cl:defmacro WHEN (condition &body body)
  `(IF ,condition (PROGN ,@body)))

(cl:defmacro UNLESS (condition &body body)
  `(IF ,condition nil (PROGN ,@body)))

(cl:defmacro CASE (form &rest clauses)
  (let ((val (gensym))
	(seen-otherwise nil))
    `(LET ((,val ,form))
       (COND
	 ,@(mapcar (lambda (clause)
		     (when seen-otherwise
		       (error "syntax error"))
		     (setq seen-otherwise
			   (member (first clause) '(T OTHERWISE)))
		     (cond
		       (seen-otherwise
			`(T ,@(rest clause)))
		       ((atom (first clause))
			`((EQL ,val ,(first clause))
			  ,@(rest clause)))
		       (t
			`((MEMBER ,val (QUOTE ,(first clause)))
			  ,@(rest clause)))))
		   clauses)))))

(cl:defmacro CCASE (place &rest clauses)
  (with-gensyms (again)
    `(RESTART-BIND ((STORE-VALUE (LAMBDA (object)
				   (SETF ,place object)
				   (GO ,again))))
       (TAGBODY
         ,again
         (ECASE ,place ,@clauses)))))

(cl:defmacro ECASE (form &rest clauses)
  (with-gensyms (val)
    `(LET ((,val ,form))
       (CASE ,val
	 ,@clauses
	 (T (type-error ,val (QUOTE (MEMBER ,@(mappend
					       (lambda (x)
						 (ensure-list (first x)))
					       clauses)))))))))

(cl:defmacro TYPECASE (form &rest clauses)
  (let ((val (gensym))
	(seen-otherwise nil))
    `(LET ((,val ,form))
       (COND
	 ,@(mapcar (lambda (clause)
		     (when seen-otherwise
		       (error "syntax error"))
		     (setq seen-otherwise
			   (member (first clause) '(T OTHERWISE)))
		     `(,(if seen-otherwise
			    T
			    `(TYPEP ,val (QUOTE ,(first clause))))
		       ,@(rest clause)))
		   clauses)))))

(cl:defmacro CTYPECASE (place &rest clauses)
  (with-gensyms (again)
    `(RESTART-BIND ((STORE-VALUE (LAMBDA (object)
				   (SETF ,place object)
				   (GO ,again))))
       (TAGBODY
         ,again
         (ETYPECASE ,place ,@clauses)))))

(cl:defmacro ETYPECASE (form &rest clauses)
  (with-gensyms (val)
    `(LET ((,val ,form))
       (TYPECASE ,val
	 ,@clauses
	 (T (type-error ,val (QUOTE (OR ,@(mapcar #'first clauses)))))))))

(defmacro* MULTIPLE-VALUE-BIND (vars form &body body)
  (case (length vars)
    (0	`(progn ,form ,@body))
    (1	`(let ((,(first vars) ,form)) ,@body))
    (t	(let ((n -1))
	  `(let* ((,(first vars) ,form)
		  ,@(mapcar (lambda (var) `(,var (nth ,(incf n) mvals)))
			    (rest vars)))
	     ,@body)))))

(cl:defmacro MULTIPLE-VALUE-BIND (vars form &body body)
  (case (length vars)
    (0	`(PROGN ,form ,@body))
    (1	`(LET ((,(first vars) ,form)) ,@body))
    (t	`(MULTIPLE-VALUE-CALL (LAMBDA ,vars ,@body) ,form))))

;;; MULTIPLE-VALUE-CALL is a special operator.

(defmacro* MULTIPLE-VALUE-LIST (form)
  (let ((val (gensym)))
    `(let ((,val ,form))
       (if (zerop nvals)
	   nil
	   (cons ,val mvals)))))

(cl:defmacro MULTIPLE-VALUE-LIST (form)
  `(MULTIPLE-VALUE-CALL (FUNCTION LIST) ,form))

;;; MULTIPLE-VALUE-PROG1 is a special operator.

(defmacro* MULTIPLE-VALUE-SETQ (vars form)
  (if (null vars)
      form
      (let ((n -1))
	`(setq ,(first vars) ,form
	       ,@(mappend (lambda (var) `(,var (nth ,(incf n) mvals)))
			  (rest vars))))))

(cl:defmacro MULTIPLE-VALUE-SETQ (vars form)
  (let ((vals (gensym))
	(n -1))
    `(let ((,vals (MULTIPLE-VALUE-LIST ,form)))
       (SETQ ,@(mapcar (lambda (var) `(,var (nth ,(incf n) ,vals))) vars)))))

(defun VALUES (&rest vals)
  (VALUES-LIST vals))

(defun VALUES-LIST (list)
  (setq nvals (length list))
  (setq mvals (cdr-safe list))
  (car-safe list))

(DEFCONSTANT MULTIPLE-VALUES-LIMIT 20)

(defmacro* NTH-VALUE (n form)
  (if (eq n 0)
      `(VALUES ,form)
      `(progn
	,form
	(VALUES (nth ,(1- n) mvals)))))

(cl:defmacro NTH-VALUE (n form)
  `(MULTIPLE-VALUE-CALL (LAMBDA (&rest vals) (NTH ,n vals)) ,form))

(defun expand-prog (let bindings body)
  (MULTIPLE-VALUE-BIND (body decl) (parse-body body)
    `(BLOCK nil
       (,let ,bindings
	 ,@decl
	 (TAGBODY ,@body)))))

(cl:defmacro PROG (bindings &body body)
  (expand-prog 'LET bindings body))

(cl:defmacro PROG* (bindings &body body)
  (expand-prog 'LET* bindings body))

(cl:defmacro PROG1 (form1 &rest forms)
  (with-gensyms (val)
    `(LET ((,val ,form1))
       ,@forms
       ,val)))

(cl:defmacro PROG2 (form1 form2 &rest forms)
  (with-gensyms (val)
    `(PROGN
       ,form1
       (LET ((,val ,form2))
	 ,@forms
	 ,val))))

;;; Special Operator: PROGN

(cl:defmacro DEFINE-MODIFY-MACRO (name lambda-list fn &optional documentation)
  (with-gensyms (place env temps values variables setter getter)
    `(DEFMACRO ,name (,place ,@lambda-list &ENVIRONMENT ,env)
       ,documentation
       (MULTIPLE-VALUE-BIND (,temps ,values ,variables ,setter ,getter)
	   (GET-SETF-EXPANSION ,place ,env)
	 (BACKQUOTE
	   (LET* ((COMMA-AT (MAPCAR (FUNCTION LIST) ,temps ,values))
		  ((COMMA (FIRST ,variables))
		   ;; TODO: only params from lambda-list
		   (,fn (COMMA ,getter) ,@lambda-list)))
	     (COMMA ,setter)))))))

(defmacro* DEFSETF (access-fn &rest args)
  (case (length args)
    (0 (error "syntax error"))
    (1 (short-form-defsetf access-fn (first args)))
    (t (apply #'long-form-defsetf access-fn args))))

(defun short-form-defsetf (access-fn update-fn)
  `(DEFINE-SETF-EXPANDER ,access-fn (&rest args)
     (let ((var (gensym))
	   (temps (map-to-gensyms args)))
       (VALUES temps
	       args
	       (list var)
	       (append '(,update-fn) temps (list var))
	       (list* ',access-fn temps)))))

(defun* long-form-defsetf (access-fn lambda-list variables &body body)
  (let ((args (remove-if (lambda (x) (member x LAMBDA-LIST-KEYWORDS))
			 lambda-list)))
    `(DEFINE-SETF-EXPANDER ,access-fn ,lambda-list
       (let* ((var (gensym))
	     (temps (map-to-gensyms ',args))
	     (,(first variables) var))
	 (VALUES temps
		 (list ,@args)
		 (list var)
		 (apply (lambda ,lambda-list ,@body) temps)
		 (cons ',access-fn temps))))))

(defvar *setf-expanders* (make-hash-table))

(defmacro* DEFINE-SETF-EXPANDER (access-fn lambda-list &body body)
  (setq lambda-list (copy-list lambda-list))
  (remf lambda-list '&environment)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',access-fn *setf-expanders*)
           (lambda ,lambda-list ,@body))
     ',access-fn))

(cl:defmacro DEFINE-SETF-EXPANDER (access-fn lambda-list &body body)
  (setq lambda-list (copy-list lambda-list))
  (remf lambda-list '&ENVIRONMENT)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (SETF (GETHASH ',access-fn *setf-expanders*)
           (LAMBDA ,lambda-list ,@body))
     (QUOTE ,access-fn)))

(DEFSETF FDEFINITION (name) (fn)
  `(COND
    ((SYMBOLP ,name)
     (SETF (SYMBOL-FUNCTION ,name) ,fn))
    ((setf-name-p ,name)
     (SETF (gethash (second ,name) *setf-definitions*) ,fn))
    (T
     (error "type error"))))

(defun GET-SETF-EXPANSION (place &optional env)
  (setq place (MACROEXPAND place))
  (cond
   ((consp place)
    (let* ((name (first place))
	   (fn (gethash (first place) *setf-expanders*)))
      (if fn
	  (apply fn (rest place))
	  (let ((temps (map-to-gensyms (rest place)))
		(var (gensym)))
	    (VALUES temps
		    (rest place)
		    (list var)
		    `(FUNCALL (FUNCTION (SETF ,name)) ,var ,@temps)
		    `(,name ,@temps))))))
   ((symbolp place)
    (let ((var (gensym)))
      (VALUES nil nil (list var) `(SETQ ,place ,var) place)))
   (t
    (error))))

(defmacro* SETF (place value &rest more &environment env)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    `(let* (,@(MAPCAR #'list temps values)
	    (,(first variables) ,value))
       ,setter
       ,@(when more
	   `((SETF ,@more))))))

(cl:defmacro SETF (place value &rest more) ;TODO: &environment
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    `(LET* (,@(MAPCAR #'list temps values)
	    (,(first variables) ,value))
       ,setter
       ,@(when more
	   `((SETF ,@more))))))

(cl:defmacro PSETF (place value &rest more) ;TODO: &environment
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    `(LET* (,@(MAPCAR #'list temps values)
	    (,(first variables) ,value))
       ,@(when more
	   `((SETF ,@more)))
       ,setter
       nil)))

(cl:defmacro SHIFTF (place x &rest more) ;TODO: &environment
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    (with-gensyms (val)
      `(LET* (,@(MAPCAR #'list temps values)
	      (,val ,getter)
	      (,(first variables) ,(if (null more) x `(SHIFTF ,x ,@more))))
	 ,setter
	 ,val))))

(cl:defmacro ROTATEF (&rest places) ;TODO: &environment
  (if (or (null places) (null (rest places)))
      nil
      (let ((place (first places))
	    (places (rest places))
	    (val (gensym)))
      (MULTIPLE-VALUE-BIND (temps values variables setter getter)
	  (GET-SETF-EXPANSION place env)
	`(LET* (,@(MAPCAR #'list temps values)
		(,val ,getter)
		(,(first variables) (SHIFTF ,@places ,val)))
	   ,setter
	   nil)))))
    
;;; CONTROL-ERROR, PROGRAM-ERROR, and UNDEFINED-FUNCTION are defined
;;; in cl-conditions.el.
