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

(defun set-fdefinition (name new)
  (cond
    ((symbolp name)
     (setf (symbol-function name) new))
    ((and (consp name) (eq (first name) 'setf) (eq (cddr name) nil))
     (setf (gethash (second name) *setf-definitions*) new))
    (t
     (error))))
(defsetf fdefinition set-symbol-package)

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
