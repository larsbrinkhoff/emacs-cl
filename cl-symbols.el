;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 10, Symbols.

(defun KEYWORDP (sym)
  (and (symbolp sym)
       (SYMBOL-PACKAGE sym)
       (equal (package-name (SYMBOL-PACKAGE sym)) "KEYWORD")))

(defun COPY-SYMBOL (sym &optional copy-properties)
  (let ((new (make-symbol (symbol-name sym))))
    (when copy-properties
      (when (boundp sym)
	(setf (symbol-value new) (symbol-value sym)))
      (when (fboundp sym)
	(setf (symbol-function new) (symbol-function sym)))
      (setf (symbol-plist new) (copy-list (symbol-plist sym))))
    new))

;;; No closures in Emacs Lisp!
; (let ((table (make-hash-table :test 'eq :weakness t)))
;   (defun SYMBOL-PACKAGE (sym)
;     (gethash sym table))
;   (defun set-SYMBOL-PACKAGE (sym package)
;     (if (null package)
; 	(remhash sym table)
; 	(setf (gethash sym table) package))
;     package))

(defvar *symbol-package-table* (make-hash-table :test 'eq :weakness t))

(defun SYMBOL-PACKAGE (sym)
  (gethash sym *symbol-package-table*))

(defsetf SYMBOL-PACKAGE (sym) (package)
  `(if (null ,package)
       (progn (remhash ,sym *symbol-package-table*) ,package)
       (setf (gethash ,sym *symbol-package-table*) ,package)))
