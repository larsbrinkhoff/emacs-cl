;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; A major mode implementing a Lisp listener.

(defvar emacs-cl-prompt-position nil
  "Position of last prompt.")

(defun emacs-cl ()
  "Starts an Emacs CL listener."
  (interactive)
  (make-variable-buffer-local 'emacs-cl-prompt-position)
  (switch-to-buffer (generate-new-buffer "*Emacs CL*"))
  (emacs-cl-mode)
  (setq *STANDARD-OUTPUT* (make-buffer-output-stream (current-buffer)))
  (insert "Emacs CL> ")
  (setq emacs-cl-prompt-position (point)))

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

(defun emacs-cl-newline ()
  (interactive)
  (when (>= (point) emacs-cl-prompt-position)
    (goto-char (point-max))
    (when (> (point) emacs-cl-prompt-position)
      (insert "\n")
      (let ((+-sym (nth-value 0 (INTERN "+" "CL")))
	    (--sym (nth-value 0 (INTERN "-" "CL")))
	    (*-sym (nth-value 0 (INTERN "*" "CL")))
	    (/-sym (nth-value 0 (INTERN "/" "CL"))))
	(setq +++ ++ ++ (SYMBOL-VALUE +-sym))
	(set +-sym (SYMBOL-VALUE --sym))
	(set --sym (READ-FROM-STRING
		    (buffer-substring emacs-cl-prompt-position (point))))
	(setq /// // // (SYMBOL-VALUE /-sym))
	(if debug-on-error
	    (set /-sym (list (EVAL (SYMBOL-VALUE --sym))))
	    (condition-case condition
		(set /-sym (list (EVAL (SYMBOL-VALUE --sym))))
	      (error (insert (format "Error: %s" condition)))))
	(setq *** ** ** (SYMBOL-VALUE *-sym))
	(set *-sym (first (SYMBOL-VALUE /-sym)))
	(PRINT (SYMBOL-VALUE *-sym))
	(dolist (x (rest (SYMBOL-VALUE /-sym)))
	  (princ "\n")
	  (PRINT x))))
    (insert "\nEmacs CL> ")
    (setq emacs-cl-prompt-position (point))))
