;;;; -*- emacs-lisp -*-

(require 'cl)

(defun keywordp (sym)
  (and (symbolp sym)
       (equal (package-name (symbol-package sym)) "KEYWORD")))

(defun copy-symbol (sym &optional copy-properties)
  (let ((new (make-symbol (symbol-name sym))))
    (when copy-properties
      (when (boundp sym)
	(setf (symbol-value new) (symbol-value sym)))
      (when (fboundp sym)
	(setf (symbol-function new) (symbol-function sym)))
      (setf (symbol-plist new) (copy-list (symbol-plist sym))))
    new))

;;; No closures in Emacs Lisp!
; (let ((table (make-hash-table :test 'eq :weakness t)))
;   (defun symbol-package (sym)
;     (gethash sym table))
;   (defun set-symbol-package (sym package)
;     (if (null package)
; 	(remhash sym table)
; 	(setf (gethash sym table) package))
;     package))

(defvar *symbol-package-table* (make-hash-table :test 'eq :weakness t))

(defun symbol-package (sym)
  (gethash sym *symbol-package-table*))

(defun set-symbol-package (sym package)
  (if (null package)
      (remhash sym *symbol-package-table*)
      (setf (gethash sym *symbol-package-table*) package))
  package)

(defsetf symbol-package set-symbol-package)
