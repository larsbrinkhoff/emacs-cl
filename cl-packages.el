;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 11, Packages.

(IN-PACKAGE "EMACS-CL")

;;; A note about the EMACS-LISP package: This package isn't
;;; implemented the same way all other packages are.  It doesn't have
;;; a hash table or a list of exported symbols.  Instead, symbols are
;;; searched with intern-soft, and all symbols are exported.

(defconst kw:EXTERNAL (keyword "EXTERNAL"))
(defconst kw:INHERITED (keyword "INHERITED"))

;;; The PACKAGE system class is built in.

(defun PACKAGE-NAME (package)
  (aref (FIND-PACKAGE package) 1))

(defun PACKAGE-NICKNAMES (package)
  (aref (FIND-PACKAGE package) 2))

(defun PACKAGE-SHADOWING-SYMBOLS (package)
  (aref (FIND-PACKAGE package) 3))

(defun PACKAGE-USE-LIST (package)
  (aref (FIND-PACKAGE package) 4))

(defun PACKAGE-USED-BY-LIST (package)
  (aref (FIND-PACKAGE package) 5))

(defun package-table (package)
  (aref package 6))

(defun package-exported (package)
  (aref package 7))

(defun PACKAGEP (package)
  (vector-and-typep package 'PACKAGE))

(defvar *all-packages* nil)

(defun* EXPORT (symbols &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (sym symbols (VALUES T))
      (MULTIPLE-VALUE-BIND (s status) (FIND-SYMBOL (SYMBOL-NAME sym) package)
	(cond
	  ((eq status kw:INHERITED)
	   (IMPORT sym package))
	  ((null status)
	   (ERROR 'PACKAGE-ERROR (kw PACKAGE) package))))
      (pushnew sym (aref package 7)))))

(defun FIND-PACKAGE (name)
  (if (PACKAGEP name)
      name
      (let ((string (STRING name)))
	(find-if 
	 (lambda (p)
	   (or (STRING= string (PACKAGE-NAME p))
	       (find string (PACKAGE-NICKNAMES p) :test 'equal)))
	 *all-packages*))))

(defun* MAKE-PACKAGE (name &key nicknames use)
  (let ((package (make-vector 8 'PACKAGE))
	(use-packages (mapcar #'FIND-PACKAGE use)))
    (aset package 1 (STRING name))
    (aset package 2 (mapcar #'STRING nicknames))
    (aset package 3 nil)
    (aset package 4 use-packages)
    (aset package 6 (make-hash-table :test 'equal))
    (aset package 7 nil)
    (dolist (p use-packages)
      (push package (aref p 5)))
    (push package *all-packages*)
    package))

(defvar *keyword-package* (MAKE-PACKAGE "KEYWORD"))
(defvar *emacs-lisp-package* (MAKE-PACKAGE "EMACS-LISP" :nicknames '("EL")))
(defvar *emacs-cl-package* (MAKE-PACKAGE "EMACS-CL" :nicknames '("E-CL")))
(defvar *cl-package* (MAKE-PACKAGE "COMMON-LISP" :nicknames '("CL")))
(MAKE-PACKAGE "COMMON-LISP-USER" :nicknames '("CL-USER")
	      :use '("CL" "E-CL" "EL"))

(defconst not-found (cons nil nil))

(defun* FIND-SYMBOL (string &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (cond
      ((null package)
       (error (format "package \"%s\" not found" package-designator)))
      ;; Special EMACS-LISP magic: EMACS-LISP doesn't have a separate
      ;; table, use intern-soft to find symbols instead.
      ((eq package *emacs-lisp-package*)
       (let ((symbol (intern-soft string)))
	 (if symbol
	     (progn
	       (unless (SYMBOL-PACKAGE symbol)
		 (setf (SYMBOL-PACKAGE symbol) *emacs-lisp-package*))
	       (VALUES symbol kw:EXTERNAL))
	     (VALUES nil nil))))
      (t
       (let* ((table (package-table package))
	      (symbol (gethash string table not-found)))
	 (if (not (eq symbol not-found))
	     (VALUES symbol
		     (if (member symbol (package-exported package))
			 kw:EXTERNAL
			 (kw INTERNAL)))
	     (dolist (p (PACKAGE-USE-LIST package) (VALUES nil nil))
	       (MULTIPLE-VALUE-BIND (symbol found) (FIND-SYMBOL string p)
		 (when (and found
			    ;; Special EMACS-LISP magic: EMACS-LISP doesn't
			    ;; have a list of exported symbols.
			    (or (eq p *emacs-lisp-package*)
				(member symbol (package-exported p))))
		   (return-from FIND-SYMBOL
		     (VALUES symbol kw:INHERITED)))))))))))

(defun FIND-ALL-SYMBOLS (name)
  (let ((string (STRING name))
	(syms nil))
    (dolist (p *all-packages* (VALUES syms))
      (MULTIPLE-VALUE-BIND (sym status) (FIND-SYMBOL string p)
	(if (or (eq status :internal) (eq status kw:EXTERNAL))
	    (push sym syms))))))

(defun* IMPORT (symbols &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (symbol symbols (VALUES T))
      (MULTIPLE-VALUE-BIND (sym found)
	  (FIND-SYMBOL (SYMBOL-NAME symbol) package)
	(when (and found (not (eq sym symbol)))
	  (error "package error")))
      (setf (gethash (SYMBOL-NAME symbol) (package-table package)) symbol)
      (when (null (SYMBOL-PACKAGE symbol))
	(setf (SYMBOL-PACKAGE symbol) package)))))

(defun LIST-ALL-PACKAGES ()
  (copy-list *all-packages*))

(defun RENAME-PACKAGE (package-designator name &optional new-nicknames)
  (let ((package (FIND-PACKAGE package-designator)))
    (aset package 1 (if (PACKAGEP name)
			(PACKAGE-NAME name)
			(STRING name)))
    (aset package 2 (mapcar #'STRING new-nicknames))))

(defun* SHADOW (symbol-names &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (name symbol-names (VALUES T))
      (MULTIPLE-VALUE-BIND (sym status) (FIND-SYMBOL name package)
	(when (or (null status) (eq status kw:INHERITED))
	  (setq sym (nth-value 0 (INTERN name package))))
	(pushnew sym (aref package 3))))))

(defun* SHADOWING-IMPORT (symbols &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (symbol symbols (VALUES T))
      (MULTIPLE-VALUE-BIND (sym found)
	  (FIND-SYMBOL (SYMBOL-NAME symbol) package)
	(when found
	  (UNINTERN sym package)))
      (IMPORT symbol package))))

(defun DELETE-PACKAGE (package-designator)
  (let ((package (FIND-PACKAGE package-designator)))
    (if (null (aref package 1))
	nil
	(let ((package (FIND-PACKAGE package-designator)))
	  (unless package
	    (error "package error"))
	  (when (PACKAGE-USED-BY-LIST package)
	    (error "package error"))
	  (dolist (p (PACKAGE-USE-LIST package))
	    (aset p 5 (delete package (PACKAGE-USED-BY-LIST p))))
	  (setq *all-packages* (delete package *all-packages*))
	  (aset package 1 nil)
	  T))))

(defun package-symbols (package types)
  (unless (null package)
    (let ((result nil))
      (maphash (lambda (ignore symbol)
		 (MULTIPLE-VALUE-BIND (sym status)
		     (FIND-SYMBOL (SYMBOL-NAME symbol) package)
		   (when (and status (member status types))
		     (push (cons sym status) result))))
	       (package-table package))
      (when (member (kw INHERITED) types)
	(dolist (p (PACKAGE-USE-LIST package))
	  (dolist (s (package-exported p))
	    (push (cons s (kw INHERITED)) result))))
      result)))

(cl:defmacro WITH-PACKAGE-ITERATOR ((name packages &rest types) &body body)
  (with-gensyms (p s)
    `(LET* ((,p (MAPCAR (FUNCTION FIND-PACKAGE) (ensure-list ,packages)))
	    (,s (package-symbols (CAR ,p) (QUOTE ,types))))
       (MACROLET ((,name ()
		    (QUOTE
		      (IF (AND (NULL ,s) (NULL (CDR ,p)))
			  nil
			  (PROGN
			    (WHEN (NULL ,s)
			      (SETQ ,p (CDR ,p))
			      (SETQ ,s (package-symbols (CAR ,p)
							(QUOTE ,types))))
			    (LET ((cons (POP ,s)))
			      (VALUES T (CAR cons) (CDR cons) (CAR ,p))))))))
	  ,@body))))

(cl:defun UNEXPORT (symbols &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (symbol symbols)
      (MULTIPLE-VALUE-BIND (sym found)
	  (FIND-SYMBOL (symbol-name symbol) package)
	(if (and found (eq sym symbol))
	    (aset package 7 (delete symbol (aref package 7)))
	    (ERROR 'PACKAGE-ERROR (kw PACKAGE) package)))))
  (VALUES T))

(defun* UNINTERN (symbol &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (when (eq (SYMBOL-PACKAGE symbol) package)
      (setf (SYMBOL-PACKAGE symbol) nil))
    (let* ((table (package-table package))
	   (name (symbol-name symbol))
	   (sym (gethash name table not-found)))
      (unless (eq sym not-found)
	(remhash name table)))
    (aset package 3 (delete symbol (aref package 3)))
    (aset package 7 (delete symbol (aref package 7))))
  'T)

(defmacro IN-PACKAGE (package)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *PACKAGE* (FIND-PACKAGE ,package))))

(cl:defmacro IN-PACKAGE (package)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (SETQ *PACKAGE* (FIND-PACKAGE ,package))))

(defun* UNUSE-PACKAGE (packages-to-unuse &optional (package *PACKAGE*))
  (let ((package (FIND-PACKAGE package)))
    (do-list-designator (p packages-to-unuse)
      (let ((p (FIND-PACKAGE p)))
	(aset package 4 (delete p (PACKAGE-USE-LIST package)))
	(aset p 5 (delete package (PACKAGE-USED-BY-LIST p))))))
  T)

(defun* USE-PACKAGE (packages-to-use &optional (package *PACKAGE*))
  (let ((package (FIND-PACKAGE package)))
    (do-list-designator (p packages-to-use)
      (aset package 4 (cons (FIND-PACKAGE p) (PACKAGE-USE-LIST package)))))
  T)

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
	  ,package)))))

(defun el-maphash (fn hash)
  (maphash (lambda (k v) (FUNCALL fn k v)) hash))

(defmacro* DO-SYMBOLS ((var &optional package result) &body body)
  (with-gensyms (p1 p2 ignore)
    `(let* ((,p1 ,package)
	    (,p2 (if ,p1 (FIND-PACKAGE ,p1) *PACKAGE*)))
       (el-maphash (lambda (,ignore ,var) ,@body)
	           (package-table ,p2))
       ,result)))

(cl:defmacro DO-SYMBOLS ((var &optional package result) &body body)
  (with-gensyms (p1 p2 ignore)
    `(LET* ((,p1 ,package)
	    (,p2 (IF ,p1 (FIND-PACKAGE ,p1) *PACKAGE*)))
       (el-maphash (LAMBDA (,ignore ,var) ,@body)
	           (package-table ,p2))
       ,result)))

(cl:defmacro DO-EXTERNAL-SYMBOLS ((var &optional package result) &body body)
  (with-gensyms (p1 p2)
    `(LET* ((,p1 ,package)
	    (,p2 (IF ,p1 (FIND-PACKAGE ,p1) *PACKAGE*)))
       (DOLIST (,var (aref ,p2 7) ,result)
	 ,@body))))

(cl:defmacro DO-ALL-SYMBOLS ((var &optional result) &body body)
  (with-gensyms (p ignore)
    `(DOLIST (,p *all-packages* ,result)
       (el-maphash (LAMBDA (,ignore ,var) ,@body)
	           (package-table ,p)))))

(defun* INTERN (name &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (when (null package)
      (error (format "package \"%s\" not found" package-designator)))
    (MULTIPLE-VALUE-BIND (symbol found) (FIND-SYMBOL name package)
      (if found
	  (VALUES symbol found)
	  (let ((symbol (if (eq package *emacs-lisp-package*)
			    (intern name)
			    (make-symbol name))))
	    (setf (SYMBOL-PACKAGE symbol) package)
	    (unless (eq package *emacs-lisp-package*)
	      (setf (gethash name (package-table package)) symbol))
	    (when (eq package *keyword-package*)
	      (set symbol symbol)
	      (pushnew symbol (aref package 7)))
	    (VALUES symbol nil))))))

(defvar *PACKAGE* (FIND-PACKAGE "CL-USER"))

;;; PACKAGE-ERROR and PACKAGE-ERROR-PACKAGE are defined in cl-conditions.el.



;;; Bootstrap magic: take the list of symbols created by the old
;;; keyword function, and import them into the KEYWORD package.
(dolist (sym *initial-keywords*)
  (IMPORT sym *keyword-package*)
  (EXPORT sym *keyword-package*))

;;; Redefine the keyword function (initially defined in utils.el).
(defun keyword (name)
  (NTH-VALUE 0 (INTERN (upcase name) *keyword-package*)))
