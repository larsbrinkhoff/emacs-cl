;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; Batch-mode REPL.

(defun batch-repl ()
  (setq *STANDARD-OUTPUT* (make-princ-stream)
	*ERROR-OUTPUT* *STANDARD-OUTPUT*
	*TRACE-OUTPUT* *STANDARD-OUTPUT*)
  (setq *STANDARD-INPUT* (make-read-char-exclusive-input-stream))
  (setq *TERMINAL-IO* (MAKE-TWO-WAY-STREAM *STANDARD-INPUT* *STANDARD-OUTPUT*)
	*QUERY-IO* *TERMINAL-IO*)
  (loop
   (FORMAT T "~%~A> " (PACKAGE-NAME *PACKAGE*))
   (let ((+-sym (INTERN "+" "CL"))
	 (--sym (INTERN "-" "CL"))
	 (*-sym (INTERN "*" "CL"))
	 (/-sym (INTERN "/" "CL")))
     (setq +++ ++ ++ (SYMBOL-VALUE +-sym))
     (set +-sym (SYMBOL-VALUE --sym))
     (set --sym (READ))
     (setq /// // // (SYMBOL-VALUE /-sym))
     (set /-sym (restart-bind ((ABORT (lambda ()
					(return-from emacs-cl-eval-print))))
		  (MULTIPLE-VALUE-LIST (EVAL (symbol-value --sym)))))
     (setq *** ** ** (SYMBOL-VALUE *-sym))
     (set *-sym (first (symbol-value /-sym)))
     (dolist (x (symbol-value /-sym))
       (PPRINT x)))))
