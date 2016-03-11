;;;; -*- lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.

(defpackage :emacs-cl-tests (:use "CL"))
(in-package :emacs-cl-tests)

(unless (null (read-from-string "(#-emacs-cl 1)"))
  (print "error reading from \"(#-emacs-cl 1)\""))

(unless (equal (read-from-string "(1 #| 2 |#)") '(1))
  (print "error reading from \"(1 #| 2 |#)\""))

(print "tests.lisp loaded")
