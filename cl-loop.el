;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements the LOOP macro from chapter 6, Iteration.

(IN-PACKAGE "EMACS-CL")

(cl:defmacro LOOP (&rest forms)
  (if (every #'consp forms)
      `(DO () (nil) ,@forms)
      (expand-extended-loop forms)))

; (defmacro* define-loop-object (name (&optional parent) ,&rest slots)
;   `(DEFSTRUCT (,(symcat name "-clause")
; 	       (:include ,(or parent 'loop-clause))
; 	       (:constructor ,(symcat "make-" name "-clause"))
;	       (:predicate ,(symcat name "-clause-p")))
;     ,@slots))

; (DEFSTRUCT loop-clause)
; (define-loop-object named () name)

; (defvar *loop-clause-table* (make-hash-table :test #'equal))

; (defmacro* define-loop-clause (name lambda-list &body body)
;   `(setf (gethash ,(symbol-name name) *loop-clause-table*)
;          (lambda ,lambda-list (progn ,@body))))

; (defun sym= (sym1 sym2)
;   (string= (symbol-name sym1) (symbol-name sym2)))

; (defun next-form ()
;   (pop *loop-forms*))

; (defun next-form-is (sym)
;   (and (symbolp (first *loop-forms*))
;        (string= (symbol-name (first *loop-forms*)) (symbol-name sym))))

; (define-loop-clause named ()
;   (make-named-clause (kw name) (next-form forms)))

; (define-loop-clause for ()
;   (let* ((var (next-form))
; 	 (next (next-form)))
;     (cond
;       ((sym= next 'fixnum)
;        ...))))

;;; TODO: complex form of LOOP
(defun expand-extended-loop (forms)
  (error "TODO"))
;   (let ((objs nil)
; 	(*loop-forms* forms))
;     (while *loop-forms*
;       (push (parse-clause) objs))
;     `(BLOCK ,(let ((named (FIND-IF #'named-clause-p objs)))
; 	       (when named (named-clause-name named))))))

; (defun parse-clause ()
;   (let ((fn (gethash (SYMBOL-NAME (first *loop-forms*)) *loop-clause-table*)))
;     (funcall fn)))

;;; TODO: Local Macro LOOP-FINISH
