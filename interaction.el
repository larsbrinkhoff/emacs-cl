;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; A major mode implementing an Emacs Common Lisp listener.

(defvar emacs-cl-prompt-marker nil
  "Position of last prompt.")

(defvar emacs-cl-history '("")
  "Common Lisp listener command history.")

(defvar emacs-cl-history-index 0
  "Common Lisp listener command history index.")

(defun emacs-cl ()
  "Starts a Common Lisp listener."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Emacs Common Lisp*"))
  (emacs-cl-mode)
  (setq *STANDARD-OUTPUT* (make-buffer-output-stream (current-buffer))
	*ERROR-OUTPUT* *STANDARD-OUTPUT*
	*TRACE-OUTPUT* *STANDARD-OUTPUT*)
  (setq *STANDARD-INPUT*
	(MAKE-ECHO-STREAM (make-read-char-exclusive-input-stream)
			  *STANDARD-OUTPUT*))
  (setq *TERMINAL-IO* (MAKE-TWO-WAY-STREAM *STANDARD-INPUT* *STANDARD-OUTPUT*)
	*QUERY-IO* *TERMINAL-IO*)
  (insert (PACKAGE-NAME *PACKAGE*) "> ")
  (setq emacs-cl-prompt-marker (point-marker)))

(defun emacs-cl-mode ()
  "Major mode for an Emacs Common Lisp listener.

  \\[emacs-cl-newline]		Process current line
  \\[emacs-cl-history-previous]		Previous line in history
  \\[emacs-cl-history-next]		Next line in history"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'emacs-cl-mode)
  (setq mode-name "Emacs Common Lisp")
  (use-local-map emacs-cl-mode-map)
  (make-variable-buffer-local 'emacs-cl-prompt-marker)
  (make-variable-buffer-local 'emacs-cl-history)
  (make-variable-buffer-local 'emacs-cl-history-index)
  (run-hooks 'emacs-cl-mode-hooks))

(defvar emacs-cl-mode-map nil
  "Local keymap for Emacs Common Lisp listener buffers.")

(unless emacs-cl-mode-map
  (setq emacs-cl-mode-map (make-keymap))
  (substitute-key-definition 'newline 'emacs-cl-newline
			     emacs-cl-mode-map global-map)
  (define-key emacs-cl-mode-map "\M-p" 'emacs-cl-history-previous)
  (define-key emacs-cl-mode-map "\M-n" 'emacs-cl-history-next))

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
      (let* ((start (1+ (point)))
	     (ignore (PPRINT x))
	     (end (point)))
	;(put-text-property start end 'face 'underline)
	(put-text-property start end 'mouse-face 'modeline)
	;(put-text-property start end 'keymap ...)
	(put-text-property start end 'emacs-cl-object x)))))

(defun emacs-cl-newline ()
  (interactive)
  (when (>= (point) emacs-cl-prompt-marker)
    (goto-char (point-max))
    (when (> (point) emacs-cl-prompt-marker)
      (let ((+-sym (NTH-VALUE 0 (INTERN "+" "CL")))
	    (--sym (NTH-VALUE 0 (INTERN "-" "CL")))
	    (line (buffer-substring emacs-cl-prompt-marker (point))))
	(setf (nth 0 emacs-cl-history) line)
	(setq +++ ++ ++ (SYMBOL-VALUE +-sym))
	(set +-sym (SYMBOL-VALUE --sym))
	(catch 'error
	  (set --sym
	       (HANDLER-BIND ((ERROR (lambda (c)
				       (insert "\nError: ")
				       (PRIN1 c)
				       (throw 'error nil))))
		 (READ-FROM-STRING line))))
	  (if debug-on-error
	      (emacs-cl-eval-print (SYMBOL-VALUE --sym))
	      (condition-case condition
		  (emacs-cl-eval-print (SYMBOL-VALUE --sym))
		(error (insert (format "\nError: %s" condition))))))))
    (insert "\n" (PACKAGE-NAME *PACKAGE*) "> ")
    (setq emacs-cl-prompt-marker (point-marker))
    (push "" emacs-cl-history)
    (setq emacs-cl-history-index 0))

(defun emacs-cl-history-previous ()
  (interactive)
  (when (and (>= (point) emacs-cl-prompt-marker)
	     (< emacs-cl-history-index (1- (length emacs-cl-history))))
    (when (zerop emacs-cl-history-index)
      (setf (nth 0 emacs-cl-history)
	    (buffer-substring emacs-cl-prompt-marker (point))))
    (delete-region emacs-cl-prompt-marker (point))
    (incf emacs-cl-history-index)
    (insert (nth emacs-cl-history-index emacs-cl-history))))

(defun emacs-cl-history-next ()
  (interactive)
  (when (and (>= (point) emacs-cl-prompt-marker)
	     (plusp emacs-cl-history-index))
    (delete-region emacs-cl-prompt-marker (point))
    (decf emacs-cl-history-index)
    (insert (nth emacs-cl-history-index emacs-cl-history))))
