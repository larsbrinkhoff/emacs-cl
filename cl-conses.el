;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 14, Conses.

(IN-PACKAGE "CL")

(setf (symbol-function CAR) (symbol-function car))

(setf (symbol-function CDR) (symbol-function cdr))

(setf (symbol-function CONS) (symbol-function cons))

(setf (symbol-function CONSP) (symbol-function consp))

(setf (symbol-function ATOM) (symbol-function atom))

(setf (symbol-function RPLACA) (symbol-function rplaca))

(setf (symbol-function RPLACD) (symbol-function rplacd))

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
