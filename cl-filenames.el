;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 19, Filenames.

(IN-PACKAGE "EMACS-CL")

(DEFSTRUCT (PATHNAME (:predicate PATHNAMEP)
		     (:copier nil)
		     (:constructor mkpathname (HOST DEVICE DIRECTORY
					       NAME TYPE VERSION)))
  HOST
  DEVICE
  DIRECTORY
  NAME
  TYPE
  VERSION)

(DEFSTRUCT (LOGICAL-PATHNAME (:predicate nil)
			     (:copier nil)
			     (:constructor nil)
			     (:include PATHNAME)))

(defun PATHNAME (pathspec)
  (cond
    ((PATHNAMEP pathspec)
     pathspec)
    ((STRINGP pathspec)
     ;; TODO: parse logical pathnames
     (VALUES (PARSE-NAMESTRING pathspec)))
    ((STREAMP pathspec)
     (PATHNAME (STREAM-filename pathspec)))
    (t
     (type-error pathspec '(OR PATHNAME STRING STREAM)))))

(cl:defun MAKE-PATHNAME (&key host device directory name
			      type version defaults case)
  (unless defaults
    (setq defaults (mkpathname (PATHNAME-HOST *DEFAULT-PATHNAME-DEFAULTS*)
			       nil nil nil nil nil)))
  (MERGE-PATHNAMES (mkpathname host device directory name type version)
		   defaults))

;;; PATHNAMEP defined by defstruct.

;;; PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY,
;;; PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION defined by defstruct.

;;; TODO: LOAD-LOGICAL-PATHNAME-TRANSLATIONS

(defvar *logical-pathname-translations* (make-hash-table))

(defun LOGICAL-PATHNAME-TRANSLATIONS (host)
  (gethash host *logical-pathname-translations*))

(defsetf LOGICAL-PATHNAME-TRANSLATIONS (host) (trans)
  `(puthash ,host ,trans *logical-pathname-translations*))

(DEFSETF LOGICAL-PATHNAME-TRANSLATIONS (host) (trans)
  `(puthash ,host ,trans *logical-pathname-translations*))

;;; TODO: LOGICAL-PATHNAME

(defun maybe-empty (component)
  (cond
    ((null component)			"")
    ((eq component (kw UNSPECIFIC))	"")
    ((eq component (kw NEWEST))		"")
    ((eq component (kw WILD))		"*")
    ((eq component (kw PREVIOUS))	"~")
    ((INTEGERP component)		(format ".~%d~" component))
    (t					component)))

(defun NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator))
	 (dir (DIRECTORY-NAMESTRING pathname))
	 (name (FILE-NAMESTRING pathname))
	 (type (maybe-empty (PATHNAME-TYPE pathname)))
	 (ver (maybe-empty (PATHNAME-VERSION pathname))))
    (concat
     dir
     (if (or (zerop (length dir)) (zerop (length name))) "" "/")
     name
     (if (or (zerop (length name)) (zerop (length type))) "" ".")
     type ver)))

(defun FILE-NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator)))
    (maybe-empty (PATHNAME-NAME pathname))))

(defun DIRECTORY-NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator))
	 (dir (PATHNAME-DIRECTORY pathname))
	 (string (if (and (consp dir)
			  (eq (first dir) (kw ABSOLUTE)))
		     "/" "")))
    (setq string (concat string (second dir)))
    (dolist (x (cddr dir) string)
      (setq string (concat string "/" x)))))

(defun HOST-NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator)))
    (maybe-empty (PATHNAME-HOST pathname))))

;;; TODO: ENOUGH-NAMESTRING

(defun slashp (char)
  (eq (CHAR-CODE char) 47))

(defun parse-dir (string)
  (when string
    (when (eq (CHAR-CODE (CHAR string 0)) 126)
      (setq string (expand-file-name string)))
    (let* ((start 0)
	   (dir (if (slashp (CHAR string 0))
		    (progn (incf start)
			   (list (kw ABSOLUTE)))
		    (list (kw RELATIVE)))))
      (do ((i start)
	   (j 1 (1+ j)))
	  ((eq j (LENGTH string))
	   (when (> j i)
	     (push (SUBSEQ string i j) dir)))
	(when (slashp (CHAR string j))
	  (let ((component (SUBSEQ string i j)))
	    (cond
	      ((STRING= component "*")		(push (kw WILD) dir))
	      ((STRING= component "..")		(push (kw UP) dir))
	      ((STRING= component "."))		;Nothing.
	      (t				(push component dir))))
	  (setq i (1+ j))))
      (nreverse dir))))

(defun parse-ver (name string)
  (if (STRING= name "")
      nil
      (cond
	((STRING= string "")		(kw NEWEST))
	((STRING= string "~")		(kw PREVIOUS))
	((and (eq (CHAR-CODE (CHAR string 0)) 46)
	      (eq (CHAR-CODE (CHAR string 1)) 126)
	      (eq (CHAR-CODE (CHAR string (1- (LENGTH string)))) 126))
					(PARSE-INTEGER string (kw START) 2
						       (kw JUNK-ALLOWED) t))
	(t				(error "invalid version")))))

(defun maybe-wild (string)
  (cond
    ((null string)		nil)
    ((STRING= string "")	nil)
    ((STRING= string "*")	(kw WILD))
    (t				string)))

(cl:defun PARSE-NAMESTRING (thing &optional host default-pathname
			          &key (start 0) end junk-allowed)
  (cond
    ((STRINGP thing)
     ;; TODO: parse logical pathnames
     (let* ((string (SUBSEQ thing start end))
	    (dir (parse-dir (file-name-directory thing)))
	    (name+ver (file-name-nondirectory thing))
	    (name-ver (file-name-sans-versions name+ver))
	    (ver (parse-ver name-ver (substring name+ver (length name-ver))))
	    (name (maybe-wild (file-name-sans-extension name+ver)))
	    (type (maybe-wild (file-name-extension name+ver))))
       (VALUES (mkpathname nil nil dir name type ver)
	       (or end (LENGTH thing)))))))

(defvar *DEFAULT-PATHNAME-DEFAULTS* (PARSE-NAMESTRING default-directory))

(cl:defun WILD-PATHNAME-P (pathname-designator &optional field)
  (let ((pathname (PATHNAME pathname-designator)))
    (cond
      ((eq field (kw HOST))
       (eq (PATHNAME-HOST pathname) (kw WILD)))
      ((eq field (kw DEVICE))
       (eq (PATHNAME-DEVICE pathname) (kw WILD)))
      ((eq field (kw DIRECTORY))
       (or (memq (kw WILD) (PATHNAME-DIRECTORY pathname))
	   (memq (kw WILD-INFERIORS) (PATHNAME-DIRECTORY pathname))))
      ((eq field (kw NAME))
       (eq (PATHNAME-NAME pathname) (kw WILD)))
      ((eq field (kw TYPE))
       (eq (PATHNAME-TYPE pathname) (kw WILD)))
      ((eq field (kw VERSION))
       (eq (PATHNAME-VERSION pathname) (kw WILD)))
      ((null field)
       (some (lambda (f) (WILD-PATHNAME-P pathname f))
	     `(,(kw HOST) ,(kw DEVICE) ,(kw DIRECTORY)
	       ,(kw NAME) ,(kw TYPE) ,(kw VERSION))))
      (t
       (type-error field `(MEMBER ,(kw HOST) ,(kw DEVICE) ,(kw DIRECTORY)
			          ,(kw NAME) ,(kw TYPE) ,(kw VERSION)))))))

(defmacro wild-test (fn pathname wildcard)
  `(or (eq (,fn ,wildcard) (kw WILD))
       (EQUAL (,fn ,wildcard) (,fn ,pathname))))

(defun directories-match-p (pathname wildcard)
  (print (format "%s %s" pathname wildcard))
  (cond
    ((null pathname)
     (null wildcard))
    ((null wildcard)
     nil)
    ((eq (first wildcard) (kw WILD-INFERIORS))
     (if (null (rest wildcard))
	 T
	 (some (lambda (p) (directories-match-p p (rest wildcard)))
	       (maplist #'IDENTITY pathname))))
    ((wild-test first pathname wildcard)
     (directories-match-p (rest pathname) (rest wildcard)))))

(defvar *all-wild-pathname* (mkpathname (kw WILD) (kw WILD) (kw WILD)
					(kw WILD) (kw WILD) (kw WILD)))

(defun PATHNAME-MATCH-P (pathname-designator wildcard)
  (let ((pathname (PATHNAME pathname))
	(wildcard (MERGE-PATHNAMES wildcard *all-wild-pathname*)))
    (and (wild-test PATHNAME-HOST pathname wildcard)
	 (wild-test PATHNAME-DEVICE pathname wildcard)
	 (or (wild-test PATHNAME-DIRECTORY pathname wildcard)
	     (directories-match-p (PATHNAME-DIRECTORY pathname)
				  (PATHNAME-DIRECTORY wildcard)))
	 (wild-test PATHNAME-NAME pathname wildcard)
	 (wild-test PATHNAME-TYPE pathname wildcard)
	 (wild-test PATHNAME-VERSION pathname wildcard))))

;;; TODO: TRANSLATE-LOGICAL-PATHNAME

(defun wild-or-nil (x y)
  (if (or (null x) (eq x (kw WILD)))
      y
      x))

(defun translate-dir (source from to)
  (cond
    ((null to)
     nil)
    ((eq (first to) (kw WILD))
     (let ((pos (position (kw WILD) from)))
       (if pos
	   (cons (nth pos source)
		 (translate-dir (nthcdr (incf pos) source)
				(nthcdr pos from) (rest to)))
	   (ERROR 'ERROR))))
    (t
     (cons (first to) (translate-dir source from (rest to))))))

(defun TRANSLATE-PATHNAME (source from-wildcard to-wildcard)
  (let ((source (PATHNAME source))
	(from-wildcard (PATHNAME from-wildcard))
	(to-wildcard (PATHNAME to-wildcard)))
    (mkpathname
     (wild-or-nil (PATHNAME-HOST to-wildcard) (PATHNAME-HOST source))
     (wild-or-nil (PATHNAME-DEVICE to-wildcard) (PATHNAME-DEVICE source))
     (translate-dir (PATHNAME-DIRECTORY source)
		    (PATHNAME-DIRECTORY from-wildcard)
		    (PATHNAME-DIRECTORY to-wildcard))
     (wild-or-nil (PATHNAME-NAME to-wildcard) (PATHNAME-NAME source))
     (wild-or-nil (PATHNAME-TYPE to-wildcard) (PATHNAME-TYPE source))
     (wild-or-nil (PATHNAME-VERSION to-wildcard) (PATHNAME-VERSION source)))))

(cl:defun MERGE-PATHNAMES (pathname-d &optional (default
						 *DEFAULT-PATHNAME-DEFAULTS*)
					        (default-version (kw NEWEST)))
  (let ((pathname (PATHNAME pathname-d)))
    ;; TODO: read spec more closely.
    (mkpathname (or (PATHNAME-HOST pathname) (PATHNAME-HOST default))
		(or (PATHNAME-DEVICE pathname) (PATHNAME-DEVICE default))
		;; TODO: Special handling of directory component.
		(or (PATHNAME-DIRECTORY pathname) (PATHNAME-DIRECTORY default))
		(or (PATHNAME-NAME pathname) (PATHNAME-NAME default))
		(or (PATHNAME-TYPE pathname) (PATHNAME-TYPE default))
		(or (PATHNAME-VERSION pathname)
		    (if (PATHNAME-NAME pathname)
			default-version
			(or (PATHNAME-VERSION default) default-version))))))
