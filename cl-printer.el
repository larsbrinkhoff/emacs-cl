;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 22, Printer.

(IN-PACKAGE "EMACS-CL")

;;; TODO: Function COPY-PPRINT-DISPATCH

;;; TODO: Macro FORMATTER

;;; TODO: Function PPRINT-DISPATCH

;;; TODO: Local Macro PPRINT-EXIT-IF-LIST-EXHAUSTED

;;; TODO: Function PPRINT-FILL
;;; TODO:          PPRINT-LINEAR
;;; TODO:          PPRINT-TABULAR

;;; TODO: Function PPRINT-INDENT

;;; TODO: Macro PPRINT-LOGICAL-BLOCK

;;; TODO: Function PPRINT-NEWLINE

;;; TODO: Local Macro PPRINT-POP

;;; TODO: Function PPRINT-TAB

;;; TODO: Standard Generic Function PRINT-OBJECT

(defvar *object-identities* (make-hash-table :test #'eq :weakness t))

(defvar *identity-counter* 12345)

(defun object-identity (object)
  ;; TODO: Perhaps flush a non-weak hash table occasionally.
  (or (gethash object *object-identities*)
      (setf (gethash object *object-identities*) (incf *identity-counter*))))

(defmacro* PRINT-UNREADABLE-OBJECT ((object stream &rest keys) &body body)
  `(print-unreadable-object ,object ,stream (lambda () ,@body) ,@keys))

(cl:defmacro PRINT-UNREADABLE-OBJECT ((object stream &rest keys) &body body)
  `(print-unreadable-object ,object ,stream (LAMBDA () ,@body) ,@keys))

(cl:defun print-unreadable-object (object stream fn &key type identity)
  (when *PRINT-READABLY*
    (ERROR 'PRINT-NOT-READABLE (kw OBJECT) object))
  (WRITE-STRING "#<" stream)
  (when type
    (PRIN1 (TYPE-OF object) stream)
    (WRITE-STRING " " stream))
  (FUNCALL fn)
  (when identity
    (WRITE-STRING " {" stream)
    (PRIN1 (object-identity object))
    (WRITE-STRING "}" stream))
  (WRITE-STRING ">" stream)
  nil)

;;; TODO: Function SET-PPRINT-DISPATCH

(defun external-symbol-p (symbol)
  (eq (NTH-VALUE 1 (FIND-SYMBOL (SYMBOL-NAME symbol) (SYMBOL-PACKAGE symbol)))
      (kw EXTERNAL)))

(defun print-symbol-name (symbol stream)
  (let* ((name (SYMBOL-NAME symbol))
	 (read-sym (READ-FROM-STRING name))
	 (escape (if (and (symbolp read-sym)
			  (string= name (SYMBOL-NAME read-sym)))
		     "" "|")))
    (WRITE-STRING escape stream)
    (WRITE-STRING name stream)
    (WRITE-STRING escape stream)))

(cl:defun WRITE (object &key
		 (array *PRINT-ARRAY*)
		 (base *PRINT-BASE*)
		 (case *PRINT-CASE*)
		 (circle *PRINT-CIRCLE*)
		 (escape *PRINT-ESCAPE*)
		 (gensym *PRINT-GENSYM*)
		 (length *PRINT-LENGTH*)
		 (level *PRINT-LEVEL*)
		 (lines *PRINT-LINES*)
		 (miser-width *PRINT-MISER-WIDTH*)
		 (pprint-dispatch *PRINT-PPRINT-DISPATCH*)
		 (pretty *PRINT-PRETTY*)
		 (radix *PRINT-RADIX*)
		 (readably *PRINT-READABLY*)
		 (right-margin *PRINT-RIGHT-MARGIN*)
		 stream)
  (let ((stream (output-stream stream))
	(*PRINT-ARRAY* array)
	(*PRINT-BASE* base)
	(*PRINT-CASE* case)
	(*PRINT-CIRCLE* circle)
	(*PRINT-ESCAPE* escape)
	(*PRINT-GENSYM* gensym)
	(*PRINT-LENGTH* length)
	(*PRINT-LEVEL* level)
	(*PRINT-LINES* lines)
	(*PRINT-MISER-WIDTH* miser-width)
	(*PRINT-PPRINT-DISPATCH* pprint-dispatch)
	(*PRINT-PRETTY* pretty)
	(*PRINT-RADIX* radix)
	(*PRINT-READABLY* readably)
	(*PRINT-RIGHT-MARGIN* right-margin))
    (cond
      ((INTEGERP object)
       (prin1-integer object stream))
      ((floatp object)
       (prin1-float object stream))
      ((symbolp object)
       (cond
	 ((eq (NTH-VALUE 0 (FIND-SYMBOL (SYMBOL-NAME object) *PACKAGE*))
	      object)
	  (print-symbol-name object stream))
	 ((null (SYMBOL-PACKAGE object))
	  (WRITE-STRING "#:" stream)
	  (print-symbol-name object stream))
	 ((eq (SYMBOL-PACKAGE object) *keyword-package*)
	  (WRITE-STRING ":" stream)
	  (print-symbol-name object stream))
	 (t
	  (WRITE-STRING (PACKAGE-NAME (SYMBOL-PACKAGE object)) stream)
	  (WRITE-STRING (if (external-symbol-p object) ":" "::") stream)
	  (print-symbol-name object stream))))
      ((CHARACTERP object)
       (WRITE-STRING "#\\" stream)
       (WRITE-STRING (or (CHAR-NAME object) (string (CHAR-CODE object)))
		     stream))
      ((consp object)
       (WRITE-STRING "(" stream)
       (PRIN1 (car object) stream)
       (while (consp (cdr object))
	 (WRITE-STRING " " stream)
	 (setq object (cdr object))
	 (PRIN1 (car object) stream))
       (unless (null (cdr object))
	 (WRITE-STRING " . " stream)
	 (PRIN1 (cdr object) stream))
       (WRITE-STRING ")" stream))
      ((FUNCTIONP object)
       (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)))
      ((ratiop object)
       (PRIN1 (NUMERATOR object) stream)
       (WRITE-STRING "/" stream)
       (PRIN1 (DENOMINATOR object) stream))
      ((COMPLEXP object)
       (WRITE-STRING "#C(" stream)
       (PRIN1 (REALPART object) stream)
       (WRITE-STRING " " stream)
       (PRIN1 (IMAGPART object) stream)
       (WRITE-STRING ")" stream))
      ((BIT-VECTOR-P object)
       (WRITE-STRING "#*" stream)
       (dotimes (i (LENGTH object))
	 (PRIN1 (AREF object i) stream)))
      ((STRINGP object)
       (when *PRINT-ESCAPE*
	 (WRITE-STRING "\"" stream))
       (dotimes (i (LENGTH object))
	 (let ((char (CHAR-CODE (CHAR object i))))
	   (if *PRINT-ESCAPE*
	       (case char
		 (34	(WRITE-STRING "\\\"" stream))
		 (92	(WRITE-STRING "\\\\" stream))
		 (t	(WRITE-STRING (string char) stream)))
	       (WRITE-STRING (string char) stream))))
       (when *PRINT-ESCAPE*
	 (WRITE-STRING "\"" stream)))
      ((VECTORP object)
       (WRITE-STRING "#(" stream)
       (dotimes (i (LENGTH object))
	 (when (> i 0)
	   (WRITE-STRING " " stream))
	 (PRIN1 (AREF object i) stream))
       (WRITE-STRING ")" stream))
      ((PACKAGEP object)
       (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t)
         (PRIN1 (PACKAGE-NAME object) stream)))
      ((READTABLEP object)
       (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)))
      ((STREAMP object)
       (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)
         (cond
	   ((STREAM-filename object)
	    (WRITE-STRING object stream))
	   ((bufferp (STREAM-content object))
	    (WRITE-STRING (buffer-name (STREAM-content object)) stream))
	   ((STRINGP (STREAM-content object))
	    (WRITE-STRING (string 34) stream)
	    (WRITE-STRING (STREAM-content object) stream)
	    (WRITE-STRING (string 34) stream)))))
      ((or (TYPEP object 'SIMPLE-CONDITION)
	   ;; TODO: these two won't be necessary later
	   (TYPEP object 'SIMPLE-ERROR)
	   (TYPEP object 'SIMPLE-WARNING))
       (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)
         (PRINC (apply #'FORMAT nil
		       (SIMPLE-CONDITION-FORMAT-CONTROL object)
		       (SIMPLE-CONDITION-FORMAT-ARGUMENTS object)))))
      ((TYPEP object 'CONDITION)
       (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)))
      ((restartp object)
       (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)
         (PRIN1 (RESTART-NAME object) stream)
	 (when (RESTART-condition object)
	   (WRITE-STRING " " stream)
	   (PRIN1 (RESTART-condition object) stream))))
      ((PATHNAMEP object)
       (WRITE-STRING "#P" stream)
       (PRIN1 (NAMESTRING object) stream))
      (t
       (error)))
    (VALUES object)))

(defun write-char-to-*standard-output* (char)
  (WRITE-CHAR (CODE-CHAR char) *STANDARD-OUTPUT*))

(defun prin1-integer (number stream)
  (when *PRINT-RADIX*
    (case *PRINT-BASE*
      (2	(WRITE-STRING "#b" stream))
      (8	(WRITE-STRING "#o" stream))
      (10)
      (16	(WRITE-STRING "#x" stream))
      (t	(WRITE-STRING "#" stream)
		(let* ((base *PRINT-BASE*) (*PRINT-BASE* 10)) (PRIN1 base)))))
  (cond
    ((ZEROP number)
     (WRITE-STRING "0" stream))
    ((MINUSP number)
     (WRITE-STRING "-" stream)
     (setq number (cl:- number))))
  (print-digits number stream)
  (when (and *PRINT-RADIX* (eq *PRINT-BASE* 10))
    (WRITE-STRING "." stream)))

(defun print-digits (number stream)
  (when (PLUSP number)
    (MULTIPLE-VALUE-BIND (number digit) (TRUNCATE number *PRINT-BASE*)
      (print-digits number stream)
      (WRITE-CHAR (AREF "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" digit)
		  stream))))

(defun write-char-to-stream (char)
  (WRITE-CHAR (CODE-CHAR char) stream))

(defun prin1-float (float stream)
  (let ((standard-output #'write-char-to-*standard-output*))
    (prin1 float)))

(defun PRIN1 (object &optional stream)
  (WRITE object (kw STREAM) stream (kw ESCAPE) t))

(defun PRINT (object &optional stream)
  (TERPRI stream)
  (PRIN1 object stream)
  (WRITE-CHAR (ch 32) stream)
  object)

(defun PPRINT (object &optional stream)
  (TERPRI stream)
  (WRITE object (kw STREAM) stream (kw ESCAPE) t (kw PRETTY) t))

(defun PRINC (object &optional stream)
  (WRITE object (kw STREAM) stream (kw ESCAPE) nil (kw READABLY) nil))

(cl:defun WRITE-TO-STRING (object &rest keys)
  (WITH-OUTPUT-TO-STRING (stream)
    (apply (cl:function WRITE) object (kw STREAM) stream keys)))

(cl:defun PRIN1-TO-STRING (object)
  (WITH-OUTPUT-TO-STRING (stream)
    (funcall (cl:function PRIN1) object stream)))

(cl:defun PRINC-TO-STRING (object)
  (WITH-OUTPUT-TO-STRING (stream)
    (funcall (cl:function PRINC) object stream)))

(defvar *PRINT-ARRAY* nil)

(defvar *PRINT-BASE* 10)

(defvar *PRINT-RADIX* nil)

(defvar *PRINT-CASE* nil)

(defvar *PRINT-CIRCLE* nil)

(defvar *PRINT-ESCAPE* nil)

(defvar *PRINT-GENSYM* nil)

(defvar *PRINT-LEVEL* nil)

(defvar *PRINT-LENGTH* nil)

(defvar *PRINT-LINES* nil)

(defvar *PRINT-MISER-WIDTH* nil)

(defvar *PRINT-PPRINT-DISPATCH* nil)

(defvar *PRINT-PRETTY* nil)

(defvar *PRINT-READABLY* nil)

(defvar *PRINT-RIGHT-MARGIN* nil)

;;; PRINT-NOT-READABLE and PRINT-NOT-READABLE-OBJECT defined in
;;; cl-conditions.el.

;;; TODO: Function FORMAT
