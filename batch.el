;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; Batch-mode REPL.  This code is used by the "emacs-cl" script.

(defun batch-repl ()
  (setq *STANDARD-OUTPUT* (make-princ-stream)
	*ERROR-OUTPUT* *STANDARD-OUTPUT*
	*TRACE-OUTPUT* *STANDARD-OUTPUT*)
  (setq *STANDARD-INPUT* (make-read-char-exclusive-input-stream))
  (setq *TERMINAL-IO* (MAKE-TWO-WAY-STREAM *STANDARD-INPUT* *STANDARD-OUTPUT*)
	*QUERY-IO* *TERMINAL-IO*)
  (loop
   (FORMAT T "~%~A> " (PACKAGE-NAME *PACKAGE*))
   (dolist (x (emacs-cl-eval-interactively (READ)))
     (PPRINT x))))
