;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 14, Conses.

(IN-PACKAGE "CL")

(mapc (lambda (to from)
	(setf (symbol-function to) (symbol-function from)))
      '(CONS CONSP ATOM RPLACA RPLACD CAR CDR CAAR CADR CDAR CDDR CAAAR CAADR
	CADAR CADDR CDAAR CDADR CDDAR CDDDR CAAAAR CAAADR CAADAR CAADDR CADAAR
	CADADR CADDAR CADDDR CDAAAR CDAADR CDADAR CDADDR CDDAAR CDDADR CDDDAR
	CDDDDR)
      '(cons consp atom rplaca rplacd car cdr caar cadr cdar cddr caaar caadr
	cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
	cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar
	cddddr))

(defun COPY-TREE (tree)
  (if (CONSP tree)
      (CONS (COPY-TREE (CAR tree)) (COPY-TREE (CDR tree)))
      tree))

(defun* MAKE-LIST (size &key initial-element)
  (make-list size initial-element))

(defun MAPCAR (fn &rest seqs)
  (if (null (cdr seqs))
      (mapcar fn (car seqs))
      (cl-mapcar-many fn seqs)))

(defun MAPCAN (fn &rest seqs)
  (apply #'nconc
   (if (null (cdr seqs))
       (mapcar fn (car seqs))
       (cl-mapcar-many fn seqs))))
