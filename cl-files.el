;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 20, Files.

(IN-PACKAGE "EMACS-CL")

(defun file-error (pathname)
  (ERROR 'FILE-ERROR (kw PATHNAME) pathname))

(defun wild-directories (name dir pathname files)
  (if (null dir)
      (nconc (DIRECTORY (MERGE-PATHNAMES name pathname)) files)
      (let ((component (first dir)))
	(setq dir (rest dir))
	(cond
	  ((eq component (kw WILD))
	   (dolist (file (directory-files name) files)
	     (unless (or (string= file ".") (string= file ".."))
	       (setq file (concat name file "/"))
	       (when (file-directory-p file)
		 (setq files (wild-directories file dir pathname files))))))
	  ((eq component (kw WILD-INFERIORS))
	   (setq files (wild-directories name dir pathname files))
	   (dolist (file (directory-files name) files)
	     (unless (or (string= file ".") (string= file ".."))
	       (setq file (concat name file "/"))
	       (when (file-directory-p file)
		 (setq files (wild-directories
			      file (cons (kw WILD-INFERIORS) dir)
			      pathname files))))))
	  ((eq component (kw UP))
	   (wild-directories (concat name "../") dir pathname files))
	  ((eq component (kw BACK))
	   (ERROR ":BACK isn't supported"))
	  (t
	   (let ((file (concat name component "/")))
	     (if (file-directory-p file)
		 (wild-directories file dir pathname files)
		 files)))))))

(defun DIRECTORY (pathname-designator)
  (let ((pathname (PATHNAME pathname-designator)))
    (if (WILD-PATHNAME-P pathname (kw DIRECTORY))
	(let* ((dir (PATHNAME-DIRECTORY pathname))
	       (x (pop dir))
	       (name (cond
		       ((eq x (kw ABSOLUTE)) "/")
		       ((or (null x) (eq x (kw RELATIVE))) "./")
		       (t (error "error")))))
	  (wild-directories name dir pathname nil))
	(let ((result nil)
	      (dir (MAKE-PATHNAME (kw DIRECTORY)
				  (PATHNAME-DIRECTORY pathname))))
	  (dolist (file (directory-files (DIRECTORY-NAMESTRING pathname)))
	    (setq file (MERGE-PATHNAMES file dir))
	    (when (PATHNAME-MATCH-P file pathname)
	      (push file result)))
	  result))))

(defun PROBE-FILE (pathspec)
  ;; TODO...
  (if (file-exists-p pathspec)
      pathspec
      nil))

(defun ENSURE-DIRECTORIES-EXIST (pathspec &optional verbose)
  (let ((dir (file-name-directory (NAMESTRING pathspec))))
    (cl:values pathspec
	       (if (file-exists-p dir)
		   (progn (make-directory dir t) T)
		   nil))))

(defun TRUENAME (filespec)
  (PATHNAME (file-truename (NAMESTRING filespec))))

(defun FILE-AUTHOR (pathspec)
  (user-login-name (nth 2 (file-attributes pathspec))))

(defun FILE-WRITE-DATE (pathspec)
  (let* ((x (nth 5 (file-attributes pathspec)))
	 (y (first x))
	 (z (second x)))
    (when (null x)
      (file-error pathspec))
    (cl:+ (binary* y 65536) z universal-time-offset)))

(defun RENAME-FILE (filespec new-name)
  (rename-file filespec new-name t)
  ;; TODO...
  (cl:values new-name filespec new-name))

(defun DELETE-FILE (filespec)
  (if (file-exists-p filespec)
      (delete-file filespec)
      (file-error filespec))
  T)

;;; FILE-ERROR and FILE-ERROR-PATHNAME are defined in cl-conditions.el.
