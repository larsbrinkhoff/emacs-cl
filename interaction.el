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
      (setq +++ ++
	    ++ cl:+
	    cl:+ cl:-
	    cl:- (READ-FROM-STRING
		  (buffer-substring emacs-cl-prompt-position (point))))
      (setq /// // // cl:/)
      (if debug-on-error
	  (setq cl:/ (list (EVAL cl:-)))
	  (condition-case condition
	      (setq cl:/ (list (EVAL cl:-)))
	    (error (insert (format "Error: %s" condition)))))
      (setq *** ** ** cl:* cl:* (first cl:/))
      (PRINT cl:*)
      (dolist (x (rest cl:/))
	(princ "\n")
	(PRINT x)))
    (insert "\nEmacs CL> ")
    (setq emacs-cl-prompt-position (point))))
