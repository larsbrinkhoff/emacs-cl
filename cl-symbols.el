;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 10, Symbols.

(setf (symbol-function 'SYMBOLP) (symbol-function 'symbolp))

(defun KEYWORDP (sym)
  (and (SYMBOLP sym)
       (SYMBOL-PACKAGE sym)
       (equal (package-name (SYMBOL-PACKAGE sym)) "KEYWORD")))

(setf (symbol-function 'MAKE-SYMBOL) (symbol-function 'make-symbol))

(defun COPY-SYMBOL (sym &optional copy-properties)
  (let ((new (make-symbol (symbol-name sym))))
    (when copy-properties
      (when (boundp sym)
	(setf (symbol-value new) (symbol-value sym)))
      (when (fboundp sym)
	(setf (symbol-function new) (symbol-function sym)))
      (setf (symbol-plist new) (copy-list (symbol-plist sym))))
    new))

(defun GENSYM (&optional x)
  (multiple-value-bind (prefix suffix)
      (cond
	((null x)	(values "G" (1- (incf *GENSYM-COUNTER*))))
	((STRINGP x)	(values x (1- (incf *GENSYM-COUNTER*))))
	((INTEGERP x)	(values "G" x))
	(t		(error "type error")))
    (MAKE-SYMBOL (FORMAT NIL "~S~D" prefix suffix))))

(defvar *GENSYM-COUNTER* 1)

(defvar *gentemp-counter* 1)

(defun* GENTEMP (&optional (prefix "T") (package-designator *package*))
  (loop
   (multiple-value-bind (symbol status)
       (INTERN (FORMAT NIL "~S~D" prefix *gentemp-counter*) package)
     (unless status
       (return-from GENTEMP symbol))
     (incf *gentemp-counter*))))

(setf (symbol-function 'SYMBOL-FUNCTION) (symbol-function 'symbol-function))

(defsetf SYMBOL-FUNCTION (symbol) (fn)
  `(setf (symbol-function ,symbol) ,fn))

(setf (symbol-function 'SYMBOL-NAME) (symbol-function 'symbol-name))

(defvar *symbol-package-table* (make-hash-table :test 'eq :weakness t))

(defun SYMBOL-PACKAGE (sym)
  (gethash sym *symbol-package-table*))

(defsetf SYMBOL-PACKAGE (sym) (package)
  `(if (null ,package)
       (progn (remhash ,sym *symbol-package-table*) ,package)
       (setf (gethash ,sym *symbol-package-table*) ,package)))

;;; TODO: symbol-plist, symbol-value, get, remprop, boundp, makunbound, set
