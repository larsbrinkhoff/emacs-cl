;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; A major mode implementing a Lisp listener for Emacs CL.

(defvar emacs-cl-prompt-marker nil
  "Position of last prompt.")

(defun emacs-cl ()
  "Starts an Emacs CL listener."
  (interactive)
  (make-variable-buffer-local 'emacs-cl-prompt-marker)
  (switch-to-buffer (generate-new-buffer "*Emacs CL*"))
  (emacs-cl-mode)
  (setq *STANDARD-OUTPUT* (make-buffer-output-stream (current-buffer))
	*ERROR-OUTPUT* *STANDARD-OUTPUT*
	*TRACE-OUTPUT* *STANDARD-OUTPUT*)
  (setq *STANDARD-INPUT*
	(MAKE-ECHO-STREAM (make-read-char-exclusive-input-stream)
			  *STANDARD-OUTPUT*))
  (setq *TERMINAL-IO* (MAKE-TWO-WAY-STREAM *STANDARD-INPUT* *STANDARD-OUTPUT*)
	*QUERY-IO* *TERMINAL-IO*)
  (insert "Emacs CL> ")
  (setq emacs-cl-prompt-marker (point-marker)))

(defun emacs-cl-mode ()
  "Starts an Emacs CL listener."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'emacs-cl-mode)
  (setq mode-name "Emacs CL")
  (use-local-map emacs-cl-mode-map)
  (run-hooks 'emacs-cl-mode-hooks))

(defvar emacs-cl-mode-map nil
  "Local keymap for Emacs CL listener buffers.")

(unless emacs-cl-mode-map
  (setq emacs-cl-mode-map (make-keymap))
  (substitute-key-definition 'newline 'emacs-cl-newline
			     emacs-cl-mode-map global-map))

(defun* emacs-cl-eval-print (form)
  (let ((*-sym (NTH-VALUE 0 (INTERN "*" "CL")))
	(/-sym (NTH-VALUE 0 (INTERN "/" "CL")))
	(values
	 (restart-bind ((ABORT (lambda () (return-from emacs-cl-eval-print))))
	   (MULTIPLE-VALUE-LIST (EVAL form)))))
    (setq /// // // (SYMBOL-VALUE /-sym))
    (set /-sym values)
    (setq *** ** ** (SYMBOL-VALUE *-sym))
    (set *-sym (first values))
    (dolist (x values)
      (PPRINT x))))

(defun emacs-cl-newline ()
  (interactive)
  (when (>= (point) emacs-cl-prompt-marker)
    (goto-char (point-max))
    (when (> (point) emacs-cl-prompt-marker)
      (let ((+-sym (NTH-VALUE 0 (INTERN "+" "CL")))
	    (--sym (NTH-VALUE 0 (INTERN "-" "CL"))))
	(setq +++ ++ ++ (SYMBOL-VALUE +-sym))
	(set +-sym (SYMBOL-VALUE --sym))
	(set --sym (READ-FROM-STRING
		    (buffer-substring emacs-cl-prompt-marker (point))))
	(if debug-on-error
	    (emacs-cl-eval-print (SYMBOL-VALUE --sym))
	    (condition-case condition
		(emacs-cl-eval-print (SYMBOL-VALUE --sym))
	      (error (insert (format "\nError: %s" condition)))))))
    (insert "\nEmacs CL> ")
    (setq emacs-cl-prompt-marker (point-marker))))
