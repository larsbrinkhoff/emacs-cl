;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 6, Iteration.

(IN-PACKAGE "EMACS-CL")

(defun var-inits (vars)
  (mapcar (lambda (var)
	    (if (symbolp var)
		var
		`(,(first var) ,(second var))))
	  vars))

(defun var-steps (vars)
  (mappend (lambda (var)
	     (when (and (consp var) (= (length var) 3))
	       `(,(first var) ,(third var))))
	   vars))

(defun expand-do (let setq vars test result body)
  (with-gensyms (block start)
    `(,let ,(var-inits vars)
       (BLOCK ,block
	 (TAGBODY
	   ,start
	   (WHEN ,test (RETURN-FROM ,block (PROGN ,@result)))
	   ,@body
	   (,setq ,@(var-steps vars))
	   (GO ,start))))))

(cl:defmacro DO (vars (test &rest result) &body body)
  (expand-do 'LET 'PSETQ vars test result body))

(cl:defmacro DO* (vars (test &rest result) &body body)
  (expand-do 'LET* 'SETQ vars test result body))

(cl:defmacro DOTIMES ((var count &optional result) &body body)
  (with-gensyms (end)
    `(DO ((,var 0 (,(INTERN "1+" "CL") ,var))
	  (,end ,count))
         ((EQL ,var ,end)
	  ,result)
       ,@body)))

(cl:defmacro DOLIST ((var list &optional result) &body body)
  `(PROGN
     (MAPC (LAMBDA (,var) ,@body) ,list)
     ,result))

(cl:defmacro LOOP (&rest forms)
  (if (every #'consp forms)
      `(DO () (nil) (PROGN ,@forms))
      (error "TODO")))

;;; TODO: LOOP-FINISH
