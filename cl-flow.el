;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;;
;;; This file implements operators in chapter 5, Data and Control Flow.

(defvar *setf-definitions* (make-hash-table))

(defun fdefinition (name)
  (cond
    ((symbop name)
     (symbol-function name))
    ((and (consp name) (eq (first name) 'setf) (eq (cddr name) nil))
     (gethash (second name) *setf-definitions*))
    (t
     (error))))

(defsetf fdefinition (name) (fn)
  `(cond
    ((symbolp ,name)
     (setf (symbol-function ,name) ,fn))
    ((and (consp ,name) (eq (first ,name) 'setf) (eq (cddr ,name) nil))
     (setf (gethash (second ,name) *setf-definitions*) ,fn))
    (t
     (error "type error"))))

(defun cl:funcall (fn &rest args)
  (cond
    ((symbolp fn)
     (apply fn args))
    ((and (consp fn) (eq (first fn) 'setf) (null (cddr fn)))
     (apply (fdefinition fn) args))
    (t
     (error))))

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

;; (defvar *multiple-values-variable* nil)

;; (defmacro* cl:multiple-value-bind (vars form &body body)
;;   (let ((vals (gensym))
;; 	(empty (gensym))
;; 	(old (gensym)))
;;     `(let ((,vals ',empty)
;; 	   (,old *multiple-values-variable*))
;;       (setq *multiple-values-variable* ',vals)
;;       (let ((,(car vars) ,form)
;; 	    ,@(cdr vars))
;; 	(unless (eq ,vals ',empty)
;; 	  (dolist (var ',vars)
;; 	    (set var (car-safe ,vals))
;; 	    (setq ,vals (cdr-safe ,vals))))
;; 	(setq *multiple-values-variable* ,old)
;; 	,@body))))

;; (defmacro cl:multiple-value-call (function &rest forms)
;;   `(apply ,function
;;     (append ,@(mapcan (lambda (form)
;; 			`((cl:multiple-value-list ,form)))
;; 		      forms))))

;; (defmacro cl:multiple-value-list (form)
;;   (let ((vals (gensym))
;; 	(empty (gensym))
;; 	(list (gensym)))
;;     `(let* ((,vals ',empty)
;; 	    (*multiple-values-variable* ',vals)
;; 	    (,list (list ,form)))
;;       (unless (eq ,vals ',empty)
;; 	(setq ,list ,vals))
;;       ,list)))

;; ;;; multiple-value-prog1 first-form form* => first-form-results

;; (defmacro* cl:multiple-value-setq (vars form &body body)
;;   (let ((vals (gensym))
;; 	(empty (gensym)))
;;     `(let ((,vals ',empty)
;; 	   (*multiple-values-variable* ',vals))
;;       (setq ,(car vars) ,form)
;;       (unless (eq ,vals ',empty)
;; 	(dolist (var ',vars)
;; 	  (set var (car-safe ,vals))
;; 	  (setq ,vals (cdr-safe ,vals))))
;;       ,@body)))

;; (defun cl:values (&rest vals)
;;   (cl:values-list vals))

;; (defun cl:values-list (list)
;;   (when *multiple-values-variable*
;;     (set *multiple-values-variable* list))
;;   (car list))

(defmacro* DEFSETF (access-fn &rest args)
  (if (<= (length args) 2)
      `(DEFINE-SETF-EXPANDER ,access-fn (&rest args2)
	(let ((fn ',(first args))
	      (var (gensym))
	      (temps (mapcar (lambda (x) (gensym)) args2)))
	  (values temps
		  args2
		  (list var)
		  `(,fn ,@temps ,var)
		  ())))
      `(long-form-defsetf ,access-fn ,@args)))

(defmacro* long-form-defsetf (access-fn lambda-list variables &body body)
  `(DEFINE-SETF-EXPANDER ,access-fn ,lambda-list
    (values ()
            ()
            ',variables
            ,(cons 'progn body))))

(defvar *setf-expanders* (make-hash-table))

(defmacro* DEFINE-SETF-EXPANDER (access-fn lambda-list &body body)
  (setq lambda-list (copy-list lambda-list))
  (remf lambda-list '&environment)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (gethash ',access-fn *setf-expanders*)
          (lambda ,lambda-list ,@body))))

(defun GET-SETF-EXPANSION (place &optional env)
  (let ((fn (gethash (first place) *setf-expanders*)))
    (if fn
	(apply fn (rest place))
	(let ((temps (mapcar (lambda (x) (gensym)) (rest place)))
	      (var (gensym)))
	  (values temps
		  (rest place)
		  (list var)
		  `(cl:funcall '(setf ,(first place)) ,var ,@temps)
		  ())))))

(defmacro* SETF (place value &environment env)
  (multiple-value-bind (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    `(let* ,(cl:mapcar #'list temps values)
      (let ((,(first variables) ,value))
	,setter))))
