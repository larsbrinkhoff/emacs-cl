(require 'cl)
(defmacro IN-PACKAGE (name) nil)

(let ((load-path (cons "~/src/emacs-cl" load-path))
      (debug-on-error t))
  (load "utils")

  (load "cl-flow")
  (load "cl-numbers")
  (load "cl-conses")
  (load "cl-arrays")
  (load "cl-sequences")
  (load "cl-structures")

  (load "cl-characters")
  (load "cl-strings")
  (load "cl-symbols")
  (load "cl-packages")
  (load "cl-typep")
  (load "cl-streams")

  (load "cl-types")
  (load "cl-subtypep")

  (load "cl-reader")
  (load "cl-printer")
  (load "cl-evaluation")
  (load "cl-environment")

  (load "interaction"))

(IN-PACKAGE "CL-USER")

;(maphash (lambda (k v) (unintern k)) (package-table *common-lisp-package*))
