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
     (type-error string 'STRING))))

(defsetf CHAR (string index) (char)
  `(cond
    ((SIMPLE-STRING-P ,string)
     (setf (SCHAR ,string ,index) ,char))
    ((STRINGP ,string)
     (setf (SCHAR (aref ,string 2) ,index) ,char))
    (t
     (type-error ,string 'STRING))))

(unless (fboundp 'char-octet)
  (defmacro char-octet (x) x))

(defun SCHAR (string index)
  ;; TODO: make use of XEmacs' character type
  (CODE-CHAR (char-octet (aref string index))))

(defsetf SCHAR (string index) (char)
  ;; TODO: make use of XEmacs' character type
  `(aset ,string ,index (CHAR-CODE ,char)))

(defun STRING (x)
  (cond
    ((STRINGP x)	x)
    ((SYMBOLP x)	(SYMBOL-NAME x))
    ((CHARACTERP x)	(MAKE-STRING 1 (kw INITIAL-ELEMENT) x))
    (t			(error))))

(cl:defun STRING-UPCASE (string &KEY (START 0) END)
  (NSTRING-UPCASE (COPY-SEQ string) (kw START) START (kw END) END))

(cl:defun STRING-DOWNCASE (string &KEY (START 0) END)
  (NSTRING-DOWNCASE (COPY-SEQ string) (kw START) START (kw END) END))

(cl:defun STRING-CAPITALIZE (string &KEY (START 0) (END (LENGTH string)))
  (NSTRING-CAPITALIZE (COPY-SEQ string) (kw START) START (kw END) END))

(cl:defun NSTRING-UPCASE (string &KEY (START 0) END)
  (unless END
    (setq END (LENGTH string)))
  (do ((i START (1+ i)))
      ((eq i END) string)
    (setf (CHAR string i) (CHAR-UPCASE (CHAR string i)))))

(cl:defun NSTRING-DOWNCASE (string &KEY (START 0) END)
  (unless END
    (setq END (LENGTH string)))
  (do ((i START (1+ i)))
      ((eq i END) string)
    (setf (CHAR string i) (CHAR-DOWNCASE (CHAR string i)))))

(cl:defun NSTRING-CAPITALIZE (string &KEY (START 0) (END (LENGTH string)))
  (do* ((i START (1+ i))
	(in-word-p nil))
       ((eq i END)
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

(cl:defun STRING= (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (string= (substring string1 START1 END1)
	   (substring string2 START2 END2)))

(cl:defun STRING/= (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (not (STRING= string1 string2 :START1 START1 :END1 END1
		                :START2 START2 :END2 END2)))

(cl:defun STRING< (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (let ((len1 (LENGTH string1))
	(len2 (LENGTH string2)))
    (block nil
      (loop
       (when (eq START1 len1)
	 (return (if (eq START2 len2) nil START1)))
       (when (eq START2 len2)
	 (return nil))
       (when (CHAR< (CHAR string1 START1) (CHAR string2 START2))
	 (return START1))
       (when (CHAR> (CHAR string1 START1) (CHAR string2 START2))
	 (return nil))
       (incf START1)
       (incf START2)))))

(cl:defun STRING> (string1 string2 &KEY START1 END1 START2 END2)
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

(cl:defun STRING<= (string1 string2 &KEY START1 END1 START2 END2)
  (STRING> string2 string1 :START1 START2 :END1 END2
			   :START2 START1 :END2 END1))

(cl:defun STRING>= (string1 string2 &KEY START1 END1 START2 END2)
  (STRING< string2 string1 :START1 START2 :END1 END2
			   :START2 START1 :END2 END1))

(cl:defun STRING-EQUAL (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (string= (substring (STRING-UPCASE string1) START1 END1)
	   (substring (STRING-UPCASE string2) START2 END2)))

(cl:defun STRING-NOT-EQUAL (string1 string2 &KEY (START1 0) END1
			                         (START2 0) END2)
  (not (STRING-EQUAL string1 string2 :START1 START1 :END1 END1
				     :START2 START2 :END2 END2)))

(cl:defun STRING-LESSP (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (STRING< (substring (STRING-UPCASE string1) START1 END1)
	   (substring (STRING-UPCASE string2) START1 END1)))

(cl:defun STRING-GREATERP (string1 string2 &KEY (START1 0) END1
						(START2 0) END2)
  (STRING> (substring (STRING-UPCASE string1) START1 END1)
	   (substring (STRING-UPCASE string2) START1 END1)))

(cl:defun STRING-NOT-GREATERP (string1 string2 &KEY (START1 0) END1
						    (START2 0) END2)
  (STRING<= (substring (STRING-UPCASE string1) START1 END1)
	    (substring (STRING-UPCASE string2) START1 END1)))

(cl:defun STRING-NOT-LESSP (string1 string2 &KEY (START1 0) END1
						 (START2 0) END2)
  (STRING>= (substring (STRING-UPCASE string1) START1 END1)
	    (substring (STRING-UPCASE string2) START1 END1)))

(defun STRINGP (object)
  (or (stringp object)
      (vector-and-typep object 'STRING)))

(cl:defun MAKE-STRING (size &KEY initial-element element-type)
  (make-string size (if initial-element (CHAR-CODE initial-element) 0)))
