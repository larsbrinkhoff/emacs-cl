;;;; -*- emacs-lisp -*-

(defvar * nil)
(defvar ** nil)
(defvar ***)
(defvar + nil)
(defvar ++ nil)
(defvar +++)
(defvar / nil)
(defvar // nil)
(defvar ///)
(defvar - nil)

(defun top-level-loop ()
  (loop
   (princ (format "%s> " (package-name *package*)))
   (setq +++ ++
	 ++ +
	 + -
	 - (read)
	 /// //
	 *** **
	 // /
	 ** *
	 / (list (eval -))
	 * (first /))
   (dolist (val /)
     (princ val))
   (princ "\n")))
