;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 10, Symbols.

(IN-PACKAGE "EMACS-CL")

;;; Note that the Emacs Lisp symbol nil doubles as the Common Lisp
;;; symbol NIL.  This requires special attention in SYMBOL-NAME.

;;; The SYMBOL system class is built in.

(fset 'SYMBOLP (symbol-function 'symbolp))

(defun KEYWORDP (sym)
  (and (SYMBOLP sym)
       (eq (SYMBOL-PACKAGE sym) *keyword-package*)))

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
    (MAKE-SYMBOL (FORMAT nil "~A~D" prefix suffix))))

(defvar *GENSYM-COUNTER* 1)

(defvar *gentemp-counter* 1)

(cl:defun GENTEMP (&OPTIONAL (prefix "T") (package *PACKAGE*))
  (catch 'GENTEMP
    (loop
      (MULTIPLE-VALUE-BIND (symbol found)
	  (INTERN (FORMAT nil "~A~D" prefix *gentemp-counter*) package)
	(unless found
	  (throw 'GENTEMP (cl:values symbol)))
	(incf *gentemp-counter*)))))

(defun SYMBOL-FUNCTION (symbol)
  (unless (symbolp symbol)
    (type-error symbol 'SYMBOL))
  (unless (fboundp symbol)
    (ERROR 'UNDEFINED-FUNCTION (kw NAME) symbol))
  (symbol-function symbol))

(defsetf SYMBOL-FUNCTION (symbol) (fn)
  `(fset ,symbol ,fn))

(DEFSETF SYMBOL-FUNCTION (symbol) (fn)
  `(fset ,symbol ,fn))

(defun SYMBOL-NAME (symbol)
  (if symbol
      (symbol-name symbol)
      "NIL"))

(defvar *symbol-package-table* (make-hash-table :test 'eq :weakness t))

(defun SYMBOL-PACKAGE (sym)
  (gethash sym *symbol-package-table*))

(defsetf SYMBOL-PACKAGE (sym) (package)
  `(if (null ,package)
       (progn (remhash ,sym *symbol-package-table*) ,package)
       (setf (gethash ,sym *symbol-package-table*) ,package)))

(fset 'SYMBOL-PLIST (symbol-function 'symbol-plist))

(DEFSETF SYMBOL-PLIST (symbol) (plist)
  `(setplist ,symbol ,plist))

(fset 'SYMBOL-VALUE (symbol-function 'symbol-value))

(defsetf SYMBOL-VALUE (symbol) (val)
  `(set ,symbol ,val))

(DEFSETF SYMBOL-VALUE (symbol) (val)
  `(SET ,symbol ,val))

(defun GET (symbol property &optional default)
  (let ((val (member property (symbol-plist symbol))))
    (if val
	(car val)
	default)))

(DEFSETF GET (symbol property &optional default) (val)
  `(put ,symbol ,property ,val))

(defun REMPROP (symbol indicator)
  (setplist symbol (delete-property (symbol-plist symbol) indicator)))

(fset 'BOUNDP (symbol-function 'boundp))

(fset 'MAKUNBOUND (symbol-function 'makunbound))

(fset 'SET (symbol-function 'set))

;;; UNBOUND-VARIABLE in cl-conditions.el.
