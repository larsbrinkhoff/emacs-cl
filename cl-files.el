;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 20, Files.

(IN-PACKAGE "EMACS-CL")

(defun DIRECTORY (pathspec)
  ;; TODO...
  (directory-files pathspec))

(defun PROBE-FILE (pathspec)
  ;; TODO...
  (if (file-exists-p pathspec)
      pathspec
      nil))

(defun ENSURE-DIRECTORIES-EXIST (pathspec &optional verbose)
  (let ((dir (file-name-directory pathspec)))
    (VALUES pathspec
	    (if (file-exists-p dir)
		(progn (make-directory dir t) T)
		nil))))

(defun TRUENAME (filespec)
  (file-truename filespec))

(defun FILE-AUTHOR (pathspec)
  (user-login-name (nth 2 (file-attributes pathspec))))

;;; Difference between Unix time and Common Lisp universal time is 70 years.
;;; TODO: Make this more exact.
(defconst universal-time-offset (cl:* 3600 24 365 70))

(defun FILE-WRITE-DATE (pathspec)
  (let* ((x (nth 5 (file-attributes pathspec)))
	 (y (first x))
	 (z (second x)))
    (when (null x)
      (error "file error"))
    (cl:+ (binary* y 65536) z universal-time-offset)))

(defun RENAME-FILE (filespec new-name)
  (rename-file filespec new-name t)
  ;; TODO...
  (VALUES new-name filespec new-name))

(defun DELETE-FILE (filespec)
  (if (file-exists-p filespec)
      (delete-file filespec)
      (error "file error"))
  T)
