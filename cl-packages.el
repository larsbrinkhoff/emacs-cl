;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 11, Packages.

;;; A note about the EMACS-LISP package: This package isn't
;;; implemented the same way all other packages are.  It doesn't have
;;; a hash table or a list of exported symbols.  Instead, symbols are
;;; searched with intern-soft, and all symbols are exported.

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

(defun package-exported (package)
  (aref package 7))

(defun PACKAGEP (package)
  (vector-and-typep package 'PACKAGE))

(defvar *all-packages* nil)

(defun* EXPORT (symbols &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (sym symbols 'T)
      (multiple-value-bind (sym status) (FIND-SYMBOL (SYMBOL-NAME sym) package)
	(when (eq status *:inherited*)
	  (IMPORT sym package)))
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
(defvar *common-lisp-package* (MAKE-PACKAGE "COMMON-LISP" :nicknames '("CL")))
(MAKE-PACKAGE "COMMON-LISP-USER" :nicknames '("CL-USER") :use '("CL" "EL"))

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
	       (values symbol *:external*))
	     (values nil nil))))
      (t
       (let* ((table (package-table package))
	      (symbol (gethash string table not-found)))
	 (if (not (eq symbol not-found))
	     (values symbol
		     (if (member symbol (package-exported package))
			 *:external*
			 *:internal*))
	     (dolist (p (PACKAGE-USE-LIST package) (values nil nil))
	       (multiple-value-bind (symbol found) (FIND-SYMBOL string p)
		 (when (and found
			    ;; Special EMACS-LISP magic: EMACS-LISP doesn't
			    ;; have a list of exported symbols.
			    (or (eq p *emacs-lisp-package*)
				(member symbol (package-exported p))))
		   (return-from FIND-SYMBOL
		     (values symbol *:inherited*)))))))))))

(defun FIND-ALL-SYMBOLS (name)
  (let ((string (STRING name))
	(syms nil))
    (dolist (p *all-packages* syms)
      (multiple-value-bind (sym status) (FIND-SYMBOL string p)
	(if (or (eq status :internal) (eq status *:external*))
	    (push sym syms))))))

(defun* IMPORT (symbols &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (symbol symbols 'T)
      (multiple-value-bind (sym found)
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

(defun* SHADOW (symbol-names &optional (package-designator *PACKAGE))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (name symbol-names)
      (multiple-value-bind (sym status) (FIND-SYMBOL name package)
	(when (or (null status) (eq status *:inherited*))
	  (setq sym (nth-value 0 (INTERN name package))))
	(pushnew sym (aref package 3)))))
  'T)

(defun* SHADOWING-IMPORT (symbols &optional (package-designator *PACKAGE))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (symbol symbols)
      (multiple-value-bind (sym found) (FIND-SYMBOL (SYMBOL-NAME sym package))
	(when found
	  (UNINTERN sym package)))
      (IMPORT symbol package))))

(defun DELETE-PACKAGE (package)
  (dolist (p (PACKAGE-USE-LIST package))
    (aset p 5 (delete package (PACKAGE-USED-BY-LIST p))))
  (setq *all-packages* (delete package *all-packages*)))

;;; with-package-iterator

;;; unexport

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

(defmacro* DO-SYMBOLS ((var &optional (package *PACKAGE*) result)
		       &body body)
  (let ((ignore (gensym)))
    `(progn
      (maphash (lambda (,ignore ,var) ,@body) (package-table ,package))
      ,result)))

; (cl:defmacro DO-SYMBOLS ((var &optional (package *PACKAGE*) result)
; 		      &body body)
;   42)

;   (let ((ignore (gensym)))
;     `(progn
;       (maphash (lambda (,ignore ,var) ,@body) (package-table ,package))
;       ,result)))

;;; do-external-symbols

;;; do-all-symbols

(defun* INTERN (name &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (when (null package)
      (error (format "package \"%s\" not found" package-designator)))
    (multiple-value-bind (symbol found) (FIND-SYMBOL name package)
      (if found
	  (values symbol found)
	  (let ((symbol (if (eq package *emacs-lisp-package*)
			    (intern name)
			    (make-symbol name))))
	    (setf (SYMBOL-PACKAGE symbol) package)
	    (unless (eq package *emacs-lisp-package*)
	      (setf (gethash name (package-table package)) symbol))
	    (when (eq package *keyword-package*)
	      (set symbol symbol))
	    (values symbol nil))))))

(defconst *:internal* (nth-value 0 (INTERN "INTERNAL" *keyword-package*)))
(defconst *:external* (nth-value 0 (INTERN "EXTERNAL" *keyword-package*)))
(defconst *:inherited* (nth-value 0 (INTERN "INHERITED" *keyword-package*)))

(defvar *PACKAGE* (FIND-PACKAGE "CL-USER"))

;;; package-error

;;; package-error-package

(defun populate-packages ()
  (let ((cl-table (make-hash-table :test 'equal)))
    (aset *common-lisp-package* 3 nil)		;shadowing symbols
    (aset *common-lisp-package* 6 cl-table)	;hash table
    (aset *common-lisp-package* 7 nil)		;exported symbols

    (dolist (sym '(&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY &OPTIONAL
&REST &WHOLE *FEATURES* *GENSYM-COUNTER* *MACROEXPAND-HOOK* *PACKAGE*
*READ-BASE* *READ-EVAL* *READTABLE* ABS ACONS ADJUST-ARRAY ADJUSTABLE-ARRAY-P
ALPHA-CHAR-P ALPHANUMERICP AND APPEND APPLY AREF ARRAY ARRAY-DIMENSION
ARRAY-DIMENSIONS ARRAY-ELEMENT-TYPE ARRAY-HAS-FILL-POINTER-P ARRAYP ASH ASSOC
ASSOC-IF ASSOC-IF-NOT ATAN ATOM BASE-CHAR BASE-STRING BIGNUM BIT BIT-VECTOR
BIT-VECTOR-P BLOCK BOOLEAN BOUNDP BUTLAST BYTE BYTE-POSITION BYTE-SIZE CAR
CAAAAR CAAADR CAAAR CAADAR CAADDR CAADR CAAR CADAAR CADADR CADAR CADDAR CADDDR
CADDR CADR CATCH CHAR CDAAAR CDAADR CDAAR CDADAR CDADDR CDADR CDAR CDDAAR
CDDADR CDDAR CDDDAR CDDDDR CDDDR CDDR CDR CHAR-CODE CHAR-CODE-LIMIT
CHAR-DOWNCASE CHAR-NAME CHAR-UPCASE CHAR= CHARACTER CHARACTERP CHECK-TYPE CLOSE
CODE-CHAR COMPILED-FUNCTION COMPILED-FUNCTION-P COMPILER-MACRO-FUNCTION
COMPLEMENT COMPLEX COMPLEXP CONCATENATE CONJUGATE CONS CONSP COPY-ALIST
COPY-LIST COPY-READTABLE COPY-SEQ COPY-STRUCTURE COPY-SYMBOL COPY-TREE
DEFCONSTANT DEFINE-COMPILER-MACRO DEFINE-SETF-EXPANDER DEFINE-SYMBOL-MACRO
DEFMACRO DEFPACKAGE DEFSETF DEFSTRUCT DEFUN DELETE-PACKAGE DENOMINATOR
DEPOSIT-FIELD DIGIT-CHAR-P DO-SYMBOLS FIFTH DOUBLE-FLOAT DPB ELT EQ EQL EQUAL
EVAL-WHEN EXTENDED-CHAR EIGHTH EXPORT FDEFINITION FILE-POSITION
FIND-ALL-SYMBOLS FIND-PACKAGE FIND-SYMBOL FIRST FIXNUM FLET FLOAT FOURTH
FUNCALL FUNCTION FUNCTIONP GENSYM GENTEMP GET GET-DISPATCH-MACRO-CHARACTER
GET-MACRO-CHARACTER GET-SETF-EXPANSION GET-OUTPUT-STREAM-STRING GO HASH-TABLE
IDENTITY IF IMAGPART IMPORT IN-PACKAGE INTEGER INTEGER-LENGTH INTEGERP INTERN
KEYWORD KEYWORDP LABELS LAST LDB LDB-TEST LENGTH LET LET*
LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-VERSION LIST LIST*
LIST-ALL-PACKAGES LIST-LENGTH LISTP LOAD-TIME-VALUE LOCALLY LOGAND LOGANDC1
LOGANDC2 LOGBITP LOGCOUNT LOGEQV LOGIOR LOGNAND LOGNOR LOGNOT LOGORC1 LOGORC2
LOGTEST LOGXOR LONG-FLOAT LOWER-CASE-P MACRO-FUNCTION MACROEXPAND MACROEXPAND-1
MACROLET MAKE-ARRAY MAKE-LIST MAKE-PACKAGE MAKE-SYMBOL MAKE-STRING
MAKE-STRING-INPUT-STREAM MAKE-STRING-OUTPUT-STREAM MAKUNBOUND MAP MAPCAN MAPCAR
MASK-FIELD MAX MEMBER MIN MINUSP MOD MOST-NEGATIVE-FIXNUM MOST-POSITIVE-FIXNUM
MULTIPLE-VALUE-CALL MULTIPLE-VALUE-PROG1 NAME-CHAR NBUTLAST NCONC NINTH NOT
NRECONC NSUBLIS NSUBST NSUBST-IF NSUBST-IF-NOT NTH NULL NUMBER NUMBERP
NUMERATOR OPEN OR PACKAGE PACKAGE-NAME PACKAGE-NICKNAMES
PACKAGE-SHADOWING-SYMBOLS PACKAGE-USE-LIST PACKAGE-USED-BY-LIST PACKAGEP
PAIRLIS PARSE-INTEGER PEEK-CHAR PHASE PRIN1 PRINT PROGN PROGV QUOTE RANDOM
RASSOC RASSOC-IF RASSOC-IF-NOT RATIO RATIONAL RATIONALP READ-CHAR
READ-DELIMITED-LIST READ-FROM-STRING READ-LINE READ-PRESERVING-WHITESPACE
READTABLE READTABLE-CASE READTABLEP REAL REALP REALPART REMPROP RETURN-FROM
RPLACA RPLACD SCHAR SECOND SET SET-DISPATCH-MACRO-CHARACTER SET-MACRO-CHARACTER
SET-SYNTAX-FROM-CHAR SETF SETQ SEVENTH SHADOW SHORT-FLOAT SIMPLE-BIT-VECTOR
SIMPLE-BIT-VECTOR-P SIMPLE-STRING SIMPLE-STRING-P SIMPLE-VECTOR SIMPLE-VECTOR-P
SINGLE-FLOAT SIGNED-BYTE SIXTH SPECIAL-OPERATOR-P STANDARD-CHAR STRING= STRING
STRINGP SUBLIS SUBST SUBST-IF SUBST-IF-NOT SUBTYPEP SYMBOL SYMBOL-FUNCTION
SYMBOL-MACROLET SYMBOL-NAME SYMBOL-PACKAGE SYMBOL-PLIST SYMBOL-VALUE SYMBOLP T
TAGBODY TENTH TERPRI THE THIRD THROW TREE-EQUAL TYPE-OF TYPEP UNREAD-CHAR
UNSIGNED-BYTE UNUSE-PACKAGE UNWIND-PROTECT UPGRADED-ARRAY-ELEMENT-TYPE
UPGRADED-COMPLEX-PART-TYPE UPPER-CASE-P USE-PACKAGE UNINTERN VALUES VECTOR
VECTORP WITH-INPUT-FROM-STRING WITH-OPEN-FILE WITH-OPEN-STREAM WRITE-CHAR
WRITE-LINE WRITE-STRING ZEROP))
      (setf (gethash (SYMBOL-NAME sym) cl-table) sym)
      (setf (SYMBOL-PACKAGE sym) *common-lisp-package*)
      (push sym (aref *common-lisp-package* 7)))

    ;; NIL is a special case, because its Emacs Lisp symbol-name isn't
    ;; equal to its Common Lisp SYMBOL-NAME.
    (setf (gethash "NIL" (package-table *common-lisp-package*)) nil)
    (setf (SYMBOL-PACKAGE nil) *common-lisp-package*)
    (push nil (aref *common-lisp-package* 7))

    ;; Internal CL symbols.
    (dolist (sym '(BACKQUOTE COMMA COMMA-AT COMMA-DOT INTERPRETED-FUNCTION
		   INTERPRETED-FUNCTION-P))
      (setf (gethash (SYMBOL-NAME sym) cl-table) sym)
      (setf (SYMBOL-PACKAGE sym) *common-lisp-package*))

    ;; Symbols prefixed with "cl:" in Emacs Lisp.
    (dolist (name '("=" "/=" "<" ">" "<=" ">=" "*" "+" "-" "/" "1+" "1-"))
      (let ((to (make-symbol name))
	    (from (intern (concat "cl:" name))))
	(setf (gethash name cl-table) to)
	(setf (SYMBOL-PACKAGE to) *common-lisp-package*)
	(if (boundp from)
	    (set to (symbol-value from)))
	(fset to (symbol-function from))
	(push to (aref *common-lisp-package* 7))))

    (dolist (sym '(** *** ++ +++ // ///))
      (setf (gethash (symbol-name sym) cl-table) sym)
      (setf (SYMBOL-PACKAGE sym) *common-lisp-package*)
      (set sym nil)
      (push sym (aref *common-lisp-package* 7)))

    (setq *global-environment*
	  (vector 'environment nil nil nil nil nil))))


;;; Local variables:
;;; fill-column: 79
;;; End:
