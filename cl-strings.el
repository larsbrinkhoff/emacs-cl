;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 16, Strings.

(IN-PACKAGE "EMACS-CL")

;;; System Class STRING

;;; Type BASE-STRING

;;; Type SIMPLE-STRING

;;; Type SIMPLE-BASE-STRING

(defun SIMPLE-STRING-P (object)
  (stringp object))

(defun CHAR (string index)
  (cond
    ((SIMPLE-STRING-P string)
     (SCHAR string index))
    ((STRINGP string)
     (SCHAR (aref string 2) index))
    (t
     (error "type error"))))

(defsetf CHAR (string index) (char)
  `(cond
    ((SIMPLE-STRING-P ,string)
     (setf (SCHAR ,string ,index) ,char))
    ((STRINGP ,string)
     (setf (SCHAR (aref ,string 2) ,index) ,char))
    (t
     (error "type error"))))

(defun SCHAR (string index)
  (CODE-CHAR (aref string index)))

(defsetf SCHAR (string index) (char)
  `(aset ,string ,index (CHAR-CODE ,char)))

(defun STRING (x)
  (cond
    ((STRINGP x)	x)
    ((SYMBOLP x)	(SYMBOL-NAME x))
    ((CHARACTERP x)	(MAKE-STRING 1 (kw INITIAL-ELEMENT) x))
    (t			(error))))

(cl:defun STRING-UPCASE (string &key (start 0) end)
  (NSTRING-UPCASE (COPY-SEQ string) (kw START) start (kw END) end))

(cl:defun STRING-DOWNCASE (string &key (start 0) end)
  (NSTRING-DOWNCASE (COPY-SEQ string) (kw START) start (kw END) end))

(cl:defun STRING-CAPITALIZE (string &key (start 0) (end (LENGTH string)))
  (NSTRING-CAPITALIZE (COPY-SEQ string) (kw START) start (kw END) end))

(cl:defun NSTRING-UPCASE (string &key (start 0) end)
  (unless end
    (setq end (LENGTH string)))
  (do ((i start (1+ i)))
      ((eq i end) string)
    (setf (CHAR string i) (CHAR-UPCASE (CHAR string i)))))

(cl:defun NSTRING-DOWNCASE (string &key (start 0) end)
  (unless end
    (setq end (LENGTH string)))
  (do ((i start (1+ i)))
      ((eq i end) string)
    (setf (CHAR string i) (CHAR-DOWNCASE (CHAR string i)))))

(cl:defun NSTRING-CAPITALIZE (string &key (start 0) (end (LENGTH string)))
  (do* ((i start (1+ i))
	(in-word-p nil))
       ((eq i end)
	string)
    (let* ((char (CHAR string i))
	   (alnump (ALPHANUMERICP char)))
      (when alnump
	(setf (CHAR string i)
	      (if in-word-p (CHAR-DOWNCASE char) (CHAR-UPCASE char))))
      (setq in-word-p alnump))))

(defun STRING-TRIM (chars string)
  (STRING-LEFT-TRIM chars (STRING-RIGHT-TRIM chars string)))

(defun STRING-LEFT-TRIM (chars string)
  (let ((i 0)
	(len (LENGTH string)))
    (while (and (FIND (CHAR string i) chars)
		(< i len))
      (incf i))
    (SUBSEQ string i)))

(defun STRING-RIGHT-TRIM (chars string)
  (let* ((i (1- (LENGTH string))))
    (while (and (FIND (CHAR string i) chars)
		(>= i 0))
      (decf i))
    (SUBSEQ string 0 (1+ i))))

(cl:defun STRING= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string= (substring string1 start1 end1)
	   (substring string2 start2 end2)))

(cl:defun STRING/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (not (STRING= string1 string2 :start1 start1 :end1 end1
		                :start2 start2 :end2 end2)))

(cl:defun STRING< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((len1 (LENGTH string1))
	(len2 (LENGTH string2)))
    (block nil
      (loop
       (when (eq start1 len1)
	 (return (if (eq start2 len2) nil start1)))
       (when (eq start2 len2)
	 (return nil))
       (when (CHAR< (CHAR string1 start1) (CHAR string2 start2))
	 (return start1))
       (when (CHAR> (CHAR string1 start1) (CHAR string2 start2))
	 (return nil))
       (incf start1)
       (incf start2)))))

(cl:defun STRING> (string1 string2 &key start1 end1 start2 end2)
  (let ((i 0)
	(len1 (LENGTH string1))
	(len2 (LENGTH string2)))
    (loop
      (when (eq i len1)
	(return-from STRING> nil))
      (when (eq i len2)
	(return-from STRING> i))
      (when (CHAR< (CHAR string1 i) (CHAR string2 i))
	(return-from STRING> nil))
      (when (CHAR> (CHAR string1 i) (CHAR string2 i))
	(return-from STRING> i))
      (incf i))))

(cl:defun STRING<= (string1 string2 &key start1 end1 start2 end2)
  (STRING> string2 string1 :start1 start2 :end1 end2
			   :start2 start1 :end2 end1))

(cl:defun STRING>= (string1 string2 &key start1 end1 start2 end2)
  (STRING< string2 string1 :start1 start2 :end1 end2
			   :start2 start1 :end2 end1))

(cl:defun STRING-EQUAL (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string= (substring (STRING-UPCASE string1) start1 end1)
	   (substring (STRING-UPCASE string2) start2 end2)))

(cl:defun STRING-NOT-EQUAL (string1 string2 &key (start1 0) end1
			                         (start2 0) end2)
  (not (STRING-EQUAL string1 string2 :start1 start1 :end1 end1
				     :start2 start2 :end2 end2)))

(cl:defun STRING-LESSP (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (STRING< (substring (STRING-UPCASE string1) start1 end1)
	   (substring (STRING-UPCASE string2) start1 end1)))

(cl:defun STRING-GREATERP (string1 string2 &key (start1 0) end1
						(start2 0) end2)
  (STRING> (substring (STRING-UPCASE string1) start1 end1)
	   (substring (STRING-UPCASE string2) start1 end1)))

(cl:defun STRING-NOT-GREATERP (string1 string2 &key (start1 0) end1
						    (start2 0) end2)
  (STRING<= (substring (STRING-UPCASE string1) start1 end1)
	    (substring (STRING-UPCASE string2) start1 end1)))

(cl:defun STRING-NOT-LESSP (string1 string2 &key (start1 0) end1
						 (start2 0) end2)
  (STRING>= (substring (STRING-UPCASE string1) start1 end1)
	    (substring (STRING-UPCASE string2) start1 end1)))

(defun STRINGP (object)
  (or (stringp object)
      (vector-and-typep object 'STRING)))

(cl:defun MAKE-STRING (size &key initial-element element-type)
  (make-string size (if initial-element (CHAR-CODE initial-element) 0)))
