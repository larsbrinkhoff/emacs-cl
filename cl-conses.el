;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 14, Conses.

(IN-PACKAGE "CL")

(mapc (lambda (to from)
	(fset to (symbol-function from)))
      '(CONS CONSP ATOM)
      '(cons consp atom))

(defun RPLACA (cons object)
  (setcar cons object)
  cons)

(defun RPLACD (cons object)
  (setcdr cons object)
  cons)

(defun CAR (object)
  (cond
    ((consp object)	(car object))
    ((null object)	nil)
    (t			(error "type error"))))

(defsetf CAR (cons) (car)
  `(setcar ,cons ,car))

(defun CDR (object)
  (cond
    ((consp object)	(cdr object))
    ((null object)	nil)
    (t			(error "type error"))))

(defsetf CDR (cons) (car)
  `(setcdr ,cons ,cdr))

(defun build-cxr (string index)
  (case (aref string index)
    (65		`(CAR ,(build-cxr string (1+ index))))
    (68		`(CDR ,(build-cxr string (1+ index))))
    (t		'object)))

(macrolet ((def (sym)
	     (let ((name (symbol-name sym)))
	       `(progn
		 (defun ,sym (object)
		   ,(build-cxr name 1))
		 (defsetf ,sym (cons) (obj)
		   (list ',(if (eq (aref name 1) 65) 'setcar 'setcdr)
			 ,(build-cxr name 2) obj))))))
  (def CAAR) (def CADR) (def CDAR) (def CDDR)
  (def CAAAR) (def CAADR) (def CADAR) (def CADDR)
  (def CDAAR) (def CDADR) (def CDDAR) (def CDDDR)
  (def CAAAAR) (def CAAADR) (def CAADAR) (def CAADDR)
  (def CADAAR) (def CADADR) (def CADDAR) (def CADDDR)
  (def CDAAAR) (def CDAADR) (def CDADAR) (def CDADDR)
  (def CDDAAR) (def CDDADR) (def CDDDAR) (def CDDDDR))

(defun COPY-TREE (tree)
  (if (CONSP tree)
      (CONS (COPY-TREE (CAR tree)) (COPY-TREE (CDR tree)))
      tree))

(defun* MAKE-LIST (size &key initial-element)
  (make-list size initial-element))

(defun ENDP (object)
  (cond
    ((null object)	'T)
    ((consp object)	nil)
    (t			(error "type error"))))

(fset 'NULL (symbol-function 'null))

(defun MAPCAR (fn &rest seqs)
  (if (null (cdr seqs))
      (mapcar fn (car seqs))
      (cl-mapcar-many fn seqs)))

(defun MAPCAN (fn &rest seqs)
  (apply #'nconc
   (if (null (cdr seqs))
       (mapcar fn (car seqs))
       (cl-mapcar-many fn seqs))))
