(require 'cl)
(require 'byte-compile "bytecomp")

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 5000)

;;; Fake an IN-PACKAGE macro.
(defmacro IN-PACKAGE (name) nil)

(defvar *cl-files*
'("utils"
  "func"

  "cl-evaluation"
  "cl-flow"
  "cl-numbers"
  "cl-conses"
  "cl-strings"
  "cl-arrays"
  "cl-sequences"
  "cl-structures"
  "cl-iteration"

  "cl-characters"
  "cl-symbols"
  "cl-packages"

  "cl-types"
  "cl-typep"
  "cl-subtypep"

  "cl-hash"
  "cl-streams"
  "cl-reader"
  "cl-printer"
  "cl-environment"
  "cl-system"
  "cl-files"
  "interaction"
  "cl-eval"

  "cl-loop"
  "cl-format"
  "cl-conditions"

  "populate"))

(defun load-cl ()
  (interactive)
  (let ((load-path (cons "~/src/emacs-cl" load-path))
	(debug-on-error t)
	(byte-compile-warnings nil))
    (mapc #'load *cl-files*)
    (populate-packages)
    (garbage-collect)
    (message "Emacs CL is loaded")))

(defun compile-cl ()
  (interactive)
  (let ((byte-compile-warnings t))
    (dolist (file *cl-files*)
      (byte-compile-file (concat file ".el")))))

(load-cl)
(IN-PACKAGE "CL-USER")
