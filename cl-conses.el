;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 14, Conses.

;;; CONS ok as is

;;; CONSP ok as is

;;; ATOM ok as is

;;; RPLACA, RPLACD ok as is

(defun* cl:make-list (size &key initial-element)
  (make-list size initial-element))

(defun cl:mapcar (fn &rest seqs)
  (if (null (cdr seqs))
      (mapcar fn (car seqs))
      (cl-mapcar-many fn seqs)))

(defun cl:mapcan (fn &rest seqs)
  (apply #'nconc
   (if (null (cdr seqs))
       (mapcar fn (car seqs))
       (cl-mapcar-many fn seqs))))
