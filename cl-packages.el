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

(defun* find-symbol (string &optional (p *package*))
  (let* ((package (find-package p))
	 (table (package-table
		 (or package
		     (error (format "package \"%s\" not found" p)))))
	 (symbol (gethash string table not-found)))
    (if (eq symbol not-found)
	(values nil nil)
	(values symbol :external))))

(defvar *all-packages* nil)

(defun find-package (name)
  (if (packagep name)
      name
      (let ((string (cl:string name)))
	(find-if 
	 (lambda (p)
	   (or (string= string (package-name p))
	       (find string (package-nicknames p) :test 'equal)))
	 *all-packages*))))

(defun find-all-symbols (name)
  (let ((string (cl:string name))
	(syms nil))
    (dolist (p *all-packages* syms)
      (multiple-value-bind (sym status) (find-symbol string p)
	(if (or (eq status :internal) (eq status :external))
	    (push sym syms))))))

;;; import

(defun list-all-packages ()
  (copy-list *all-packages*))

;;; rename-package

;;; shadow

;;; shadowing-import

(defun delete-package (package)
  (dolist (p (package-use-list package))
    (setf (package-used-by-list p) (delete package (package-used-by-list p))))
  (setf *all-packages* (delete package *all-packages*)))

(defun* make-package (name &key nicknames use)
  (let ((package (mk-package))
	(use-packages (mapcar (lambda (p) (find-package p)) use)))
    (setf (package-table package) (make-hash-table :test 'equal)
	  (package-name package) (cl:string name)
	  (package-nicknames package) nicknames
	  (package-use-list package) use-packages)
    (dolist (p use-packages)
      (push package (package-used-by-list p)))
    (push package *all-packages*)
    package))

;;; with-package-iterator

;;; unexport

(defun* cl:unintern (symbol &optional (package *package*))
  (when (eq (symbol-package symbol) package)
    (setf (symbol-package symbol) nil))
  (let* ((table (package-table package))
	 (name (symbol-name symbol))
	 (sym (gethash name table not-found)))
    (unless (eq sym not-found)
      (remhash name table)
      t)))

(defmacro in-package (package)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *package* (find-package ,package))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun* unuse-package (packages-to-unuse &optional (package *package*))
  (let ((package (find-package package)))
    (dolist (p (ensure-list packages-to-unuse))
      (let ((p (find-package p)))
	(setf (package-use-list package)
	      (delete p (package-use-list package))
	      (package-used-by-list p)
	      (delete package (package-used-by-list p))))))
  t)

(defun* use-package (packages-to-use &optional (package *package*))
  (let ((package (find-package package)))
    (dolist (p (ensure-list packages-to-use))
      (pushnew (find-package p) (package-use-list package))))
  t)

(defmacro defpackage (name &body options)
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
	(let ((,package (make-package ,name
				      :nicknames ,nicknames
				      :use ,use-list)))
	  (setf (package-doc doc))
	  ,package)))))

(defmacro* do-symbols ((var &optional (package *package*) result)
		       &body body)
  (let ((ignore (gensym)))
    `(progn
      (maphash (lambda (,ignore ,var) ,@body) (package-table ,package))
      ,result)))

;;; do-external-symbols

;;; do-all-symbols

(defun* cl:intern (name &optional (package-designator *package*))
  (let ((package (find-package package-designator)))
    (when (null package)
      (error (format "package \"%s\" not found" package-designator)))
    (multiple-value-bind (symbol status) (find-symbol name package)
      (if status
	  (values symbol status)
	  (let ((symbol (make-symbol name)))
	    (setf (symbol-package symbol) package)
	    (setf (gethash name (package-table package)) symbol)
	    (values symbol nil))))))

;;; package-error

;;; package-error-package

(make-package "COMMON-LISP" :nicknames '("CL"))
(make-package "COMMON-LISP-USER" :nicknames '("CL-USER") :use '("CL"))
(make-package "KEYWORD")
(make-package "EMACS-LISP" :nicknames '("EL"))

(defvar *package* (find-package "CL-USER"))

(dolist (sym '(ADJUST-ARRAY ADJUSTABLE-ARRAY-P AREF ARRAY-DIMENSION
	       ARRAY-DIMENSIONS ARRAY-ELEMENT-TYPE ARRAY-HAS-FILL-POINTER-P
	       ARRAYP ASH BACKQUOTE COMMA COMMA-AT COMMA-DOT COPY-SEQ
	       DEFINE-SETF-EXPANDER DEFSETF ELT FLOAT FUNCTION
	       GET-SETF-EXPANSION INTEGERP LENGTH LOGAND LOGANDC1 LOGANDC2
	       LOGEQV LOGIOR LOGNAND LOGNOR LOGNOT LOGORC1 LOGORC2 LOGXOR
	       MAKE-ARRAY MAX MIN MINUSP NUMBERP PRINT RANDOM SETF
	       SIMPLE-BIT-VECTOR-P SIMPLE-VECTOR-P STRINGP SUBTYPEP TYPEP
	       UPGRADED-ARRAY-ELEMENT-TYPE VECTORP QUOTE ZEROP))
  (setf (symbol-package sym) (find-package "COMMON-LISP")))
