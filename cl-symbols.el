;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 10, Symbols.

(fset 'SYMBOLP (symbol-function 'symbolp))

(defun KEYWORDP (sym)
  (and (SYMBOLP sym)
       (SYMBOL-PACKAGE sym)
       (equal (PACKAGE-NAME (SYMBOL-PACKAGE sym)) "KEYWORD")))

(fset 'MAKE-SYMBOL (symbol-function 'make-symbol))

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
   (multiple-value-bind (symbol found)
       (INTERN (FORMAT NIL "~S~D" prefix *gentemp-counter*) package)
     (unless found
       (return-from GENTEMP symbol))
     (incf *gentemp-counter*))))

(fset 'SYMBOL-FUNCTION (symbol-function 'symbol-function))

(defsetf SYMBOL-FUNCTION (symbol) (fn)
  `(fset ,symbol ,fn))

(fset 'SYMBOL-NAME (symbol-function 'symbol-name))

(defvar *symbol-package-table* (make-hash-table :test 'eq :weakness t))

(defun SYMBOL-PACKAGE (sym)
  (gethash sym *symbol-package-table*))

(defsetf SYMBOL-PACKAGE (sym) (package)
  `(if (null ,package)
       (progn (remhash ,sym *symbol-package-table*) ,package)
       (setf (gethash ,sym *symbol-package-table*) ,package)))

(fset 'SYMBOL-PLIST (symbol-function 'symbol-plist))

(defsetf SYMBOL-PLIST (symbol) (plist)
  `(setplist ,symbol ,plist))

(fset 'SYMBOL-VALUE (symbol-function 'symbol-value))

(defsetf SYMBOL-VALUE (symbol) (val)
  `(set ,symbol ,val))

(defun GET (symbol property &optional default)
  (let ((val (member property (symbol-plist symbol))))
    (if val
	(car val)
	default)))

(defsetf GET (symbol prop &optional default) (val)
  `(put ,symbol ,property ,val))

(defun REMPROP (symbol property)
  (setplist symbol (delete property (symbol-plist symbol))))

(fset 'BOUNDP (symbol-function 'boundp))

(fset 'MAKUNBOUND (symbol-function 'makunbound))

(fset 'SET (symbol-function 'set))

