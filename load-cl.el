(let ((load-path (cons "~/src/emacs-cl" load-path)))
  (require 'cl)
  (defmacro in-package (name) nil)

  (load "cl-numbers")
  (load "cl-flow")

  (load "cl-characters")
  (load "cl-strings")
  (load "cl-packages")
  (load "cl-symbols")
  (load "cl-streams")
  (load "cl-reader")

  (load "cl-typep")
  (load "cl-types")
  nil)
