;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 11, Packages.

(defstruct (package (:constructor mk-package ())
		    (:predicate packagep)
		    (:copier nil))
  name
  nicknames
  shadowing-symbols
  use-list
  used-by-list
  table
  doc)

(defconst not-found (cons nil nil))

(defun* FIND-SYMBOL (string &optional (p *package*))
  (let* ((package (FIND-PACKAGE p))
	 (table (package-table
		 (or package
		     (error (format "package \"%s\" not found" p)))))
	 (symbol (gethash string table not-found)))
    (if (eq symbol not-found)
	(values nil nil)
	(values symbol :external))))

(defvar *all-packages* nil)

(defun FIND-PACKAGE (name)
  (if (packagep name)
      name
      (let ((string (STRING name)))
	(find-if 
	 (lambda (p)
	   (or (string= string (package-name p))
	       (find string (package-nicknames p) :test 'equal)))
	 *all-packages*))))

(defun FIND-ALL-SYMBOLS (name)
  (let ((string (STRING name))
	(syms nil))
    (dolist (p *all-packages* syms)
      (multiple-value-bind (sym status) (FIND-SYMBOL string p)
	(if (or (eq status :internal) (eq status :external))
	    (push sym syms))))))

;;; import

(defun LIST-ALL-PACKAGES ()
  (copy-list *all-packages*))

;;; rename-package

;;; shadow

;;; shadowing-import

(defun DELETE-PACKAGE (package)
  (dolist (p (package-use-list package))
    (setf (package-used-by-list p) (delete package (package-used-by-list p))))
  (setf *all-packages* (delete package *all-packages*)))

(defun* MAKE-PACKAGE (name &key nicknames use)
  (let ((package (mk-package))
	(use-packages (mapcar (lambda (p) (FIND-PACKAGE p)) use)))
    (setf (package-table package) (make-hash-table :test 'equal)
	  (package-name package) (STRING name)
	  (package-nicknames package) nicknames
	  (package-use-list package) use-packages)
    (dolist (p use-packages)
      (push package (package-used-by-list p)))
    (push package *all-packages*)
    package))

;;; with-package-iterator

;;; unexport

(defun* UNINTERN (symbol &optional (package *package*))
  (when (eq (SYMBOL-PACKAGE symbol) package)
    (setf (SYMBOL-PACKAGE symbol) nil))
  (let* ((table (package-table package))
	 (name (symbol-name symbol))
	 (sym (gethash name table not-found)))
    (unless (eq sym not-found)
      (remhash name table)
      t)))

(defmacro IN-PACKAGE (package)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *package* (FIND-PACKAGE ,package))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun* UNUSE-PACKAGE (packages-to-unuse &optional (package *package*))
  (let ((package (FIND-PACKAGE package)))
    (dolist (p (ensure-list packages-to-unuse))
      (let ((p (FIND-PACKAGE p)))
	(setf (package-use-list package)
	      (delete p (package-use-list package))
	      (package-used-by-list p)
	      (delete package (package-used-by-list p))))))
  t)

(defun* USE-PACKAGE (packages-to-use &optional (package *package*))
  (let ((package (FIND-PACKAGE package)))
    (dolist (p (ensure-list packages-to-use))
      (pushnew (FIND-PACKAGE p) (package-use-list package))))
  t)

(defmacro DEFPACKAGE (name &body options)
  (let ((nicknames nil)
	(documentation nil)
	(use-list nil)
	(shadow-list nil)
	(doc nil))
    (dolist (option options)
      (ecase (first option)
	(:nicknames     (dolist (i (rest option)) (pushnew i nicknames)))
	(:documentation (setq doc (second option)))
	(:use           (dolist (i (rest option)) (pushnew i use-list)))
	(:shadow	nil)
	(:shadowing-import-from nil)
	(:import-from	nil)
	(:export	nil)
	(:intern	nil)
	(:size		nil)))
    (let ((package (gensym)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	(let ((,package (MAKE-PACKAGE ,name
				      :nicknames ,nicknames
				      :use ,use-list)))
	  (setf (package-doc doc))
	  ,package)))))

(defmacro* DO-SYMBOLS ((var &optional (package *package*) result)
		       &body body)
  (let ((ignore (gensym)))
    `(progn
      (maphash (lambda (,ignore ,var) ,@body) (package-table ,package))
      ,result)))

;;; do-external-symbols

;;; do-all-symbols

(defun* INTERN (name &optional (package-designator *package*))
  (let ((package (FIND-PACKAGE package-designator)))
    (when (null package)
      (error (format "package \"%s\" not found" package-designator)))
    (multiple-value-bind (symbol status) (FIND-SYMBOL name package)
      (if status
	  (values symbol status)
	  (let ((symbol (make-symbol name)))
	    (setf (SYMBOL-PACKAGE symbol) package)
	    (setf (gethash name (package-table package)) symbol)
	    (values symbol nil))))))

;;; package-error

;;; package-error-package

(MAKE-PACKAGE "COMMON-LISP" :nicknames '("CL"))
(MAKE-PACKAGE "COMMON-LISP-USER" :nicknames '("CL-USER") :use '("CL"))
(MAKE-PACKAGE "KEYWORD")
(MAKE-PACKAGE "EMACS-LISP" :nicknames '("EL"))

(defvar *package* (FIND-PACKAGE "CL-USER"))

(dolist (sym
	  '(ADJUST-ARRAY ADJUSTABLE-ARRAY-P AREF ARRAY-DIMENSION
	    ARRAY-DIMENSIONS ARRAY-ELEMENT-TYPE ARRAY-HAS-FILL-POINTER-P
	    ARRAYP ASH BACKQUOTE COMMA COMMA-AT COMMA-DOT CONCATENATE COPY-SEQ
	    COPY-STRUCTURE COPY-SYMBOL DEFINE-SETF-EXPANDER DEFPACKAGE DEFSETF
	    DEFSTRUCT DEFUN DELETE-PACKAGE DO-SYMBOLS ELT FIND-SYMBOL
	    FIND-ALL-SYMBOLS FIND-PACKAGE FLOAT FUNCTION GET-SETF-EXPANSION
	    IN-PACKAGE INTEGERP INTERN KEYWORDP LENGTH LIST-ALL-PACKAGES LOGAND
	    LOGANDC1 LOGANDC2 LOGEQV LOGIOR LOGNAND LOGNOR LOGNOT LOGORC1
	    LOGORC2 LOGXOR MAKE-ARRAY MAKE-PACKAGE MAX MIN MINUSP NUMBERP
	    PEEK-CHAR PRINT RANDOM READ-CHAR READ-LINE SETF SIMPLE-BIT-VECTOR-P
	    SIMPLE-VECTOR-P STRING STRINGP SUBTYPEP SYMBOL-PACKAGE TYPEP
	    UNREAD-CHAR UNUSE-PACKAGE UPGRADED-ARRAY-ELEMENT-TYPE USE-PACKAGE
	    UNINTERN VECTORP WRITE-CHAR WRITE-LINE WRITE-STRING QUOTE ZEROP))
  (setf (SYMBOL-PACKAGE sym) (FIND-PACKAGE "COMMON-LISP")))
