;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 11, Packages.

; (DEFSTRUCT (PACKAGE (:constructor mk-package ())
; 		    (:predicate PACKAGEP)
; 		    (:copier nil))
;   NAME
;   NICKNAMES
;   SHADOWING-SYMBOLS
;   USE-LIST
;   USED-BY-LIST
;   table)

(defconst not-found (cons nil nil))

(defun* FIND-SYMBOL (string &optional (p *PACKAGE*))
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
  (IF (PACKAGEP name)
      name
      (let ((string (STRING name)))
	(find-if 
	 (lambda (p)
	   (or (string= string (PACKAGE-NAME p))
	       (find string (PACKAGE-NICKNAMES p) :test 'equal)))
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
  (dolist (p (PACKAGE-USE-LIST package))
    (aset p 5 (delete package (PACKAGE-USED-BY-LIST p))))
  (setq *all-packages* (delete package *all-packages*)))

(defun* MAKE-PACKAGE (name &key nicknames use)
  (let ((package (make-vector 7 'PACKAGE))
	(use-packages (mapcar #'FIND-PACKAGE use)))
    (aset package 1 (STRING name))
    (aset package 2 nicknames)
    (aset package 3 NIL)
    (aset package 4 use-packages)
    (aset package 6 (make-hash-table :test 'equal))
    (dolist (p use-packages)
      (aset p 5 (cons package (aref p 5))))
    (push package *all-packages*)
    package))

;;; with-package-iterator

;;; unexport

(defun* UNINTERN (symbol &optional (package *PACKAGE*))
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
    (setq *PACKAGE* (FIND-PACKAGE ,package))))

(defun* UNUSE-PACKAGE (packages-to-unuse &optional (package *PACKAGE*))
  (let ((package (FIND-PACKAGE package)))
    (dolist (p (ensure-list packages-to-unuse))
      (let ((p (FIND-PACKAGE p)))
	(aset package 4 (delete p (PACKAGE-USE-LIST package)))
	(aset p 5 (delete package (PACKAGE-USED-BY-LIST p))))))
  t)

(defun* USE-PACKAGE (packages-to-use &optional (package *PACKAGE*))
  (let ((package (FIND-PACKAGE package)))
    (dolist (p (ensure-list packages-to-use))
      (aset package 4 (cons (FIND-PACKAGE p) (PACKAGE-USE-LIST package)))))
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
	  ,package)))))

(defmacro* DO-SYMBOLS ((var &optional (package *PACKAGE*) result)
		       &body body)
  (let ((ignore (gensym)))
    `(progn
      (maphash (lambda (,ignore ,var) ,@body) (package-table ,package))
      ,result)))

;;; do-external-symbols

;;; do-all-symbols

(defun* INTERN (name &optional (package-designator *PACKAGE*))
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

(defun PACKAGE-NAME (package)
  (aref package 1))

(defun PACKAGE-NICKNAMES (package)
  (aref package 2))

(defun PACKAGE-SHADOWING-SYMBOLS (package)
  (aref package 3))

(defun PACKAGE-USE-LIST (package)
  (aref package 4))

(defun PACKAGE-USED-BY-LIST (package)
  (aref package 5))

(defun package-table (package)
  (aref package 6))

(defun PACKAGEP (package)
  (cl-truth (vector-and-typep package 'PACKAGE)))

(MAKE-PACKAGE "COMMON-LISP" :nicknames '("CL"))
(MAKE-PACKAGE "COMMON-LISP-USER" :nicknames '("CL-USER") :use '("CL"))
(MAKE-PACKAGE "KEYWORD")
(MAKE-PACKAGE "EMACS-LISP" :nicknames '("EL"))

(defvar *PACKAGE* (FIND-PACKAGE "CL-USER"))

;;; package-error

;;; package-error-package

(dolist (sym
	  '(*GENSYM-COUNTER* *MACROEXPAND-HOOK* *READ-BASE* *READTABLE*
	    *PACKAGE* ADJUST-ARRAY ADJUSTABLE-ARRAY-P ALPHA-CHAR-P
	    ALPHANUMERICP AND AREF ARRAY ARRAY-DIMENSION ARRAY-DIMENSIONS
	    ARRAY-ELEMENT-TYPE ARRAY-HAS-FILL-POINTER-P ARRAYP ASH ATAN ATOM
	    BACKQUOTE BASE-CHAR BASE-STRING BIGNUM BIT BIT-VECTOR BIT-VECTOR-P
	    BLOCK BOOLEAN BOUNDP BYTE BYTE-POSITION BYTE-SIZE CAR CAAAAR
	    CAAADR CAAAR CAADAR CAADDR CAADR CAAR CADAAR CADADR CADAR CADDAR
	    CADDDR CADDR CADR CATCH CHAR CDAAAR CDAADR CDAAR CDADAR CDADDR CDADR
	    CDAR CDDAAR CDDADR CDDAR CDDDAR CDDDDR CDDDR CDDR CDR CHAR-CODE
	    CHAR-CODE-LIMIT CHAR-DOWNCASE CHAR-NAME CHAR-UPCASE CHAR= CHARACTER
	    CHARACTERP CHECK-TYPE CODE-CHAR COMMA COMMA-AT COMMA-DOT
	    COMPILED-FUNCTION COMPILED-FUNCTION-P COMPILER-MACRO-FUNCTION
	    COMPLEX COMPLEXP CONCATENATE CONJUGATE CONS CONSP COPY-READTABLE
	    COPY-SEQ COPY-STRUCTURE COPY-SYMBOL COPY-TREE DEFCONSTANT
	    DEFINE-COMPILER-MACRO DEFINE-SETF-EXPANDER DEFINE-SYMBOL-MACRO
	    DEFMACRO DEFPACKAGE DEFSETF DEFSTRUCT DEFUN DELETE-PACKAGE
	    DENOMINATOR DEPOSIT-FIELD DIGIT-CHAR-P DO-SYMBOLS DOUBLE-FLOAT
	    DPB ELT EQ EQL EQUAL EVAL-WHEN EXTENDED-CHAR FDEFINITION
	    FIND-ALL-SYMBOLS FIND-PACKAGE FIND-SYMBOL FIXNUM FLET FLOAT
	    FUNCTION FUNCTIONP GENSYM GENTEMP GET GET-DISPATCH-MACRO-CHARACTER
	    GET-MACRO-CHARACTER GET-SETF-EXPANSION GO HASH-TABLE IF IMAGPART
	    IN-PACKAGE INTEGER INTEGER-LENGTH INTEGERP INTERN KEYWORD KEYWORDP
	    LABELS LDB LDB-TEST LENGTH LET LET* LIST LIST-ALL-PACKAGES
	    LOAD-TIME-VALUE LOCALLY LOGAND LOGANDC1 LOGANDC2 LOGBITP LOGCOUNT
	    LOGEQV LOGIOR LOGNAND LOGNOR LOGNOT LOGORC1 LOGORC2 LOGTEST LOGXOR
	    LONG-FLOAT LOWER-CASE-P MACRO-FUNCTION MACROEXPAND MACROEXPAND-1
	    MACROLET MAKE-ARRAY MAKE-LIST MAKE-PACKAGE MAKE-SYMBOL MAKE-STRING
	    MAKUNBOUND
	    MAPCAN MAPCAR MASK-FIELD MAX MEMBER MIN MINUSP MOD
	    MOST-NEGATIVE-FIXNUM MOST-POSITIVE-FIXNUM MULTIPLE-VALUE-CALL
	    MULTIPLE-VALUE-PROG1 NAME-CHAR NIL NOT NULL NUMBER NUMBERP
	    NUMERATOR OR PACKAGE PACKAGE-NAME PACKAGE-NICKNAMES
	    PACKAGE-SHADOWING-SYMBOLS PACKAGE-USE-LIST PACKAGE-USED-BY-LIST
	    PACKAGEP PARSE-INTEGER PEEK-CHAR PHASE PRINT PROGN PROGV QUOTE
	    RANDOM RATIO RATIONAL RATIONALP READ-CHAR READ-DELIMITED-LIST
	    READ-FROM-STRING READ-LINE READ-PRESERVING-WHITESPACE READTABLE
	    READTABLE-CASE READTABLEP REAL REALP REALPART REMPROP RETURN-FROM
	    RPLACA RPLACD SCHAR SET SET-DISPATCH-MACRO-CHARACTER SET-MACRO-CHARACTER
	    SET-SYNTAX-FROM-CHAR SETF SETQ SHORT-FLOAT SIMPLE-BIT-VECTOR
	    SIMPLE-BIT-VECTOR-P SIMPLE-STRING SIMPLE-STRING-P SIMPLE-VECTOR
	    SIMPLE-VECTOR-P SINGLE-FLOAT SIGNED-BYTE SPECIAL-OPERATOR-P
	    STANDARD-CHAR STRING STRINGP SUBTYPEP SYMBOL SYMBOL-FUNCTION
	    SYMBOL-MACROLET SYMBOL-NAME SYMBOL-PACKAGE SYMBOL-PLIST
	    SYMBOL-VALUE SYMBOLP T TAGBODY THE THROW TYPE-OF TYPEP UNREAD-CHAR
	    UNSIGNED-BYTE UNUSE-PACKAGE UNWIND-PROTECT
	    UPGRADED-ARRAY-ELEMENT-TYPE UPGRADED-COMPLEX-PART-TYPE
	    UPPER-CASE-P USE-PACKAGE UNINTERN VALUES VECTORP WRITE-CHAR
	    WRITE-LINE WRITE-STRING ZEROP))
  (setf (SYMBOL-PACKAGE sym) (FIND-PACKAGE "COMMON-LISP")))
