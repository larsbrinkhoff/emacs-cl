;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 18, Hash Tables.

(IN-PACKAGE "EMACS-CL")

(if (eq (type-of (make-hash-table)) 'hash-table)
    (progn
      (defun* MAKE-HASH-TABLE (&key test size rehash-size rehash-threshold)
	(make-hash-table :test test :size size))

      (defmacro htab (hash)
	hash)

      (defun HASH-TABLE-P (object)
	(hash-table-p object))

      (defun HASH-TABLE-TEST (hash)
	;; TODO
	'EQ))

    ;; If there isn't a real hash-table type, make one using defstruct.
    (progn
      (DEFSTRUCT (HASH-TABLE (:copier nil) (:constructor mkhash (TABLE TEST)))
        TABLE TEST)

      (defun* MAKE-HASH-TABLE (&key test size rehash-size rehash-threshold)
	(mkhash (make-hash-table :test test :size size) test))

      (defun htab (hash)
	(HASH-TABLE-TABLE hash))))

(defun HASH-TABLE-COUNT (hash)
  (hash-table-count (htab hash)))

(defun HASH-TABLE-REHASH-SIZE (hash)
  ;; TODO
  0)

(defun HASH-TABLE-REHASH-THRESHOLD (hash)
  ;; TODO
  0)

(defun HASH-TABLE-SIZE (hash)
  ;; TODO
  0)

(defun* GETHASH (key hash &optional default)
  (gethash key (htab hash) default))

(unless (fboundp 'puthash)
  (defun puthash (key value table)
    (setf (gethash key table) value)))

(DEFINE-SETF-EXPANDER GETHASH (key hash &optional default)
  (with-gensyms (keytemp hashtemp val)
    (VALUES (list keytemp hashtemp)
	    (list key hash)
	    (list val)
	    `(puthash ,keytemp ,val ,hashtemp)
	    `(GETHASH ,keytemp ,hashtemp))))

(defun REMHASH (key hash)
  (remhash key (htab hash)))

(defun MAPHASH (fn hash)
  (maphash fn (htab hash))
  nil)

(defun hashlist (hash)
  (let ((list nil))
    (maphash (lambda (k v) (push (cons k v) list)) hash)
    list))

(cl:defmacro WITH-HASH-TABLE-ITERATOR ((name hash) &body body)
  (with-gensyms (list)
    `(LET ((,list (hashlist ,hash)))
       (MACROLET ((,name ()
		    (QUOTE (IF (NULL ,list) (VALUES nil nil nil)
			       (LET ((cons (POP ,list)))
				 (VALUES T (CAR cons) (CDR cons)))))))
	 ,@body))))

(defun CLRHASH (hash)
  (clrhash (htab hash))
  hash)

;;; TODO: SXHASH
