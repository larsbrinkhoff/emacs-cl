;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 19, Filenames.

(IN-PACKAGE "EMACS-CL")

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

(defun mkpathname (host device directory name type version)
  (vector 'PATHNAME host device directory name type version))

(cl:defun MAKE-PATHNAME (&key HOST DEVICE DIRECTORY NAME
			      TYPE VERSION DEFAULTS CASE)
  (unless DEFAULTS
    (setq DEFAULTS (mkpathname (PATHNAME-HOST *DEFAULT-PATHNAME-DEFAULTS*)
			       nil nil nil nil nil)))
  (MERGE-PATHNAMES (mkpathname HOST DEVICE DIRECTORY NAME TYPE VERSION)
		   DEFAULTS))

(defun PATHNAMEP (object)
  (vector-and-typep object 'PATHNAME))

(defun PATHNAME-HOST (pathname-designator)
  (let ((pathname (PATHNAME pathname-designator)))
    (aref pathname 1)))

(defun PATHNAME-DEVICE (pathname-designator)
  (let ((pathname (PATHNAME pathname-designator)))
    (aref pathname 2)))

(defun PATHNAME-DIRECTORY (pathname-designator)
  (let ((pathname (PATHNAME pathname-designator)))
    (aref pathname 3)))

(defun PATHNAME-NAME (pathname-designator)
  (let ((pathname (PATHNAME pathname-designator)))
    (aref pathname 4)))

(defun PATHNAME-TYPE (pathname-designator)
  (let ((pathname (PATHNAME pathname-designator)))
    (aref pathname 5)))

(defun PATHNAME-VERSION (pathname-designator)
  (let ((pathname (PATHNAME pathname-designator)))
    (aref pathname 6)))

;;; TODO: LOAD-LOGICAL-PATHNAME-TRANSLATIONS

(defvar *logical-pathname-translations* (make-hash-table))

(defun LOGICAL-PATHNAME-TRANSLATIONS (host)
  (gethash host *logical-pathname-translations*))

(defsetf LOGICAL-PATHNAME-TRANSLATIONS (host) (trans)
  `(puthash ,host ,trans *logical-pathname-translations*))

(DEFSETF LOGICAL-PATHNAME-TRANSLATIONS (host) (trans)
  `(puthash ,host ,trans *logical-pathname-translations*))

;;; *DEFAULT-PATHNAME-DEFAULTS* defined below.

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
     name
     (if (zerop (length type)) "" ".")
     type ver)))

(defun FILE-NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator)))
    (maybe-empty (PATHNAME-NAME pathname))))

(defun DIRECTORY-NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator))
	 (dir (PATHNAME-DIRECTORY pathname))
	 (string (if (and (consp dir) (eq (first dir) (kw ABSOLUTE)))
		     "/" "")))
    (dolist (x (rest dir) string)
      (setq string
	    (concat string
		    (cond
		      ((STRINGP x)	x)
		      ((eq x (kw UP))	"..")
		      ((eq x (kw BACK))	(ERROR 'ERROR))
		      (t		(type-error
					 x `(OR STRING
					     (MEMBER ,(kw UP) ,(kw BACK))))))
		    "/")))))

(defun HOST-NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator)))
    (maybe-empty (PATHNAME-HOST pathname))))

(defun dir-subtract (dir1 dir2)
  (cond
    ((null dir1)
     (cons (kw RELATIVE) dir2))
    ((null dir2)
     nil)
    ((EQUAL (first dir1) (first dir2))
     (dir-subtract (rest dir1) (rest dir2)))))

(cl:defun ENOUGH-NAMESTRING (pathname-designator &optional
			     (defaults *DEFAULT-PATHNAME-DEFAULTS*))
  ;; It is required that
  ;;   (merge-pathnames (enough-namestring pathname defaults) defaults)
  ;;   == (merge-pathnames (parse-namestring pathname nil defaults) defaults)
  (let ((pathname (PATHNAME pathname-designator)))
    (let ((candidates (list (NAMESTRING pathname)))
	  (shortest nil))
      (when (and (EQUAL (PATHNAME-HOST pathname) (PATHNAME-HOST defaults))
		 (EQUAL (PATHNAME-DEVICE pathname) (PATHNAME-DEVICE defaults))
		 (consp (PATHNAME-DIRECTORY pathname))
		 (eq (first (PATHNAME-DIRECTORY pathname)) (kw ABSOLUTE)))
	(let ((dir (dir-subtract (PATHNAME-DIRECTORY defaults)
				 (PATHNAME-DIRECTORY pathname))))
	  (when dir
	    (push (NAMESTRING (MAKE-PATHNAME (kw DIRECTORY) dir
					     (kw DEFAULTS) pathname))
		  candidates))))
      (FIND-IF (lambda (len)
		 (or (null shortest)
		     (when (< len shortest)
		       (setq shortest len))))
	       candidates (kw KEY) #'LENGTH))))

(defun slashp (char)
  (ch= char 47))

(defun parse-dir (string)
  (when string
    (when (ch= (CHAR string 0) 126)
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

(defvar *DEFAULT-PATHNAME-DEFAULTS*
  (mkpathname nil nil (parse-dir default-directory) nil nil nil))

(defun parse-ver (name string)
  (if (STRING= name "")
      nil
      (cond
	((STRING= string "")		(kw NEWEST))
	((STRING= string "~")		(kw PREVIOUS))
	((and (ch= (CHAR string 0) 46)
	      (ch= (CHAR string 1) 126)
	      (ch= (CHAR string (1- (LENGTH string))) 126))
					(PARSE-INTEGER string (kw START) 2
						       (kw JUNK-ALLOWED) t))
	(t				(error "invalid version")))))

(defun maybe-wild (string)
  (cond
    ((null string)		nil)
    ((STRING= string "")	nil)
    ((STRING= string "*")	(kw WILD))
    (t				string)))

(cl:defun PARSE-NAMESTRING (thing &optional host
			    (default *DEFAULT-PATHNAME-DEFAULTS*)
			    &key (START 0) END JUNK-ALLOWED)
  (cond
    ((STREAMP thing)
     (PARSE-NAMESTRING (STREAM-filename thing) host default (kw START) START
		       (kw END) END (kw JUNK-ALLOWED) JUNK-ALLOWED))
    ((PATHNAMEP thing)
     (if (EQUAL (PATHNAME-HOST thing) host)
	 (VALUES thing START)
	 (ERROR 'ERROR)))
    ((STRINGP thing)
     ;; TODO: parse logical pathnames
     (let* ((string (SUBSEQ thing START END))
	    (dir (parse-dir (file-name-directory thing)))
	    (name+ver (file-name-nondirectory thing))
	    (name-ver (file-name-sans-versions name+ver))
	    (ver (parse-ver name-ver (substring name+ver (length name-ver))))
	    (name (maybe-wild (file-name-sans-extension name-ver)))
	    (type (maybe-wild (file-name-extension name+ver))))
       (VALUES (mkpathname nil nil dir name type ver)
	       (or END (LENGTH thing)))))
    (t
     (type-error thing '(OR PATHNAME STRING STREAM)))))

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
  (let ((pathname (PATHNAME pathname-designator))
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

(cl:defun MERGE-PATHNAMES (pathname-d &optional (default-d
						 *DEFAULT-PATHNAME-DEFAULTS*)
					        (default-version (kw NEWEST)))
  (let ((pathname (PATHNAME pathname-d))
	(default (PATHNAME default-d)))
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
