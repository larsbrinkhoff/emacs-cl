;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 23, Reader.

(IN-PACKAGE "CL")

(defvar *backquote-level* 0)

(DEFSTRUCT (READTABLE (:predicate READTABLEP) (:copier nil))
  CASE
  SYNTAX-TYPE
  MACRO-TABLE
  DISPATCH-TABLE)

(defun* GET-DISPATCH-MACRO-CHARACTER (disp-char sub-char
				      &optional (readtable *READTABLE*))
  (let ((string (concat (list (CHAR-CODE disp-char) (CHAR-CODE sub-char)))))
    (gethash string (READTABLE-DISPATCH-TABLE readtable))))

(defun* SET-DISPATCH-MACRO-CHARACTER (disp-char sub-char new-function
				      &optional (readtable *READTABLE*))
  (let ((string (concat (list (CHAR-CODE disp-char) (CHAR-CODE sub-char)))))
    (setf (gethash string (READTABLE-DISPATCH-TABLE readtable))
	  new-function))
  T)

(defun* GET-MACRO-CHARACTER (char &optional (readtable *READTABLE*))
  (values
   (aref (READTABLE-MACRO-TABLE readtable) (CHAR-CODE char))
   (eq (char-syntx char readtable) :non-terminating-macro)))

(defun* SET-MACRO-CHARACTER (char new-function
			     &optional non-terminating-p
			               (readtable *READTABLE*))
  (setf (aref (READTABLE-MACRO-TABLE readtable) (CHAR-CODE char)) new-function)
  (setf (aref (READTABLE-SYNTAX-TYPE readtable) (CHAR-CODE char))
	(if non-terminating-p
	    :non-terminating-macro
	    :terminating-macro))
  T)

(defun* SET-SYNTAX-FROM-CHAR (to-char from-char
			      &optional (to-readtable *READTABLE*)
			                (from-readtable *standard-readtable*))
  (setf (aref (READTABLE-SYNTAX-TYPE to-readtable) (CHAR-CODE to-char))
	(char-syntx from-char from-readtable))
  T)

(defun* char-syntx (char &optional (readtable *READTABLE*))
  (aref (READTABLE-SYNTAX-TYPE readtable) (CHAR-CODE char)))

(defvar *standard-readtable*
  (let ((readtable (MAKE-READTABLE)))
    (setf (READTABLE-CASE readtable) :upcase)

    (setf (READTABLE-SYNTAX-TYPE readtable) (make-vector 256 :constituent))
    (setf (aref (READTABLE-SYNTAX-TYPE readtable) 32) :whitespace)
    (setf (aref (READTABLE-SYNTAX-TYPE readtable) 92) :single-escape)
    (setf (aref (READTABLE-SYNTAX-TYPE readtable) 124) :multiple-escape)
    (SET-SYNTAX-FROM-CHAR (CODE-CHAR 9) (CODE-CHAR 32) readtable readtable)
    (SET-SYNTAX-FROM-CHAR (CODE-CHAR 10) (CODE-CHAR 32) readtable readtable)
    (SET-SYNTAX-FROM-CHAR (CODE-CHAR 12) (CODE-CHAR 32) readtable readtable)
    (SET-SYNTAX-FROM-CHAR (CODE-CHAR 13) (CODE-CHAR 32) readtable readtable)

    (setf (READTABLE-MACRO-TABLE readtable) (make-vector 256 nil))
    (SET-MACRO-CHARACTER (CODE-CHAR 34) #'double-quote-reader nil readtable)
    (SET-MACRO-CHARACTER (CODE-CHAR 35) #'sharp-reader T readtable)
    (SET-MACRO-CHARACTER (CODE-CHAR 39) #'quote-reader nil readtable)
    (SET-MACRO-CHARACTER (CODE-CHAR 40) #'left-paren-reader nil readtable)
    (SET-MACRO-CHARACTER (CODE-CHAR 41) #'right-paren-reader nil readtable)
    (SET-MACRO-CHARACTER (CODE-CHAR 44) #'comma-reader nil readtable)
    (SET-MACRO-CHARACTER (CODE-CHAR 59) #'semicolon-reader nil readtable)
    (SET-MACRO-CHARACTER (CODE-CHAR 96) #'backquote-reader nil readtable)

    (setf (READTABLE-DISPATCH-TABLE readtable) (make-hash-table :test #'equal))
    (macrolet ((sharp-macro (n fn)
		 `(SET-DISPATCH-MACRO-CHARACTER (CODE-CHAR 35) (CODE-CHAR ,n)
		                                ,fn readtable)))
      (sharp-macro 92 #'sharp-backslash-reader)
      (sharp-macro 39 #'sharp-quote-reader)
      (sharp-macro 40 #'sharp-left-paren-reader)
      (sharp-macro ?* #'sharp-asterisk-reader)
      (sharp-macro ?: #'sharp-colon-reader)
      (sharp-macro ?. #'sharp-dot-reader)
      (sharp-macro ?b #'sharp-b-reader)
      (sharp-macro ?B #'sharp-b-reader)
      (sharp-macro ?o #'sharp-o-reader)
      (sharp-macro ?O #'sharp-o-reader)
      (sharp-macro ?x #'sharp-x-reader)
      (sharp-macro ?X #'sharp-x-reader)
      (sharp-macro ?r #'sharp-r-reader)
      (sharp-macro ?R #'sharp-r-reader)
      (sharp-macro ?c #'sharp-c-reader)
      (sharp-macro ?C #'sharp-c-reader)
      (sharp-macro ?a #'sharp-a-reader)
      (sharp-macro ?A #'sharp-a-reader)
      (sharp-macro ?s #'sharp-s-reader)
      (sharp-macro ?S #'sharp-s-reader)
      (sharp-macro ?p #'sharp-p-reader)
      (sharp-macro ?P #'sharp-p-reader)
      (sharp-macro ?= #'sharp-equal-reader)
      (sharp-macro 35 #'sharp-sharp-reader)
      (sharp-macro ?+ #'sharp-plus-reader)
      (sharp-macro ?- #'sharp-minus-reader)
      (sharp-macro 124 #'sharp-bar-reader)
      (sharp-macro ?< #'sharp-less-reader)
      (sharp-macro 32 #'sharp-space-reader)
      (sharp-macro 41 #'sharp-right-paren-reader))

    readtable))

(defun whitespacep (char)
  (eq (char-syntx char) :whitespace))

(defun constituentp (char)
  (eq (char-syntx char) :constituent))

(defun double-quote-reader (stream double-quote-char)
  (do ((string "")
       (char #1=(READ-CHAR stream T nil T) #1#))
      ((CHAR= char double-quote-char)
       (values string))
    (setq string
	  (concat string
		  (list (CHAR-CODE
			 (if (eq (char-syntx char) :single-escape)
			     (READ-CHAR stream T nil T)
			     char)))))))

(defun sharp-reader (stream char1)
  (let* ((char2 (READ-CHAR stream T nil T))
	 (fn (GET-DISPATCH-MACRO-CHARACTER char1 char2)))
    (if (null fn)
	(error "invalid dispatching macro: %s" string)
	(funcall fn stream char2 nil))))

(defun quote-reader (stream ch)
  (values (list 'QUOTE (READ stream T nil T))))

(defun left-paren-reader (stream char)
  (do ((list nil)
       (char #1=(PEEK-CHAR T stream) #1#))
      ((EQL char (CODE-CHAR 41))
       (READ-CHAR stream)
       (values (nreverse list)))
    (push (READ stream T nil T) list)))

(defun right-paren-reader (stream char)
  (error "unbalanced '%c'" char))

(defun comma-reader (stream char)
  (unless (plusp *backquote-level*)
    (error "comma outside backquote"))
  (let ((next-char (READ-CHAR stream T nil T)))
    (let ((*backquote-level* (1- *backquote-level*)))
      (cond
	((EQL next-char (CODE-CHAR 64))
	 (values (list 'COMMA-AT (READ stream T nil T))))
	((EQL next-char (CODE-CHAR 46))
	 (values (list 'COMMA-DOT (READ stream T nil T))))
	(t
	 (UNREAD-CHAR next-char stream)
	 (values (list 'COMMA (READ stream T nil T))))))))

(defun semicolon-reader (stream ch)
  (do ()
      ((CHAR= (READ-CHAR stream nil (CODE-CHAR 10) T) (CODE-CHAR 10))
       (values))))

(defun backquote-reader (stream char)
  (let* ((*backquote-level* (1+ *backquote-level*))
	 (form (READ stream T nil T)))
    (values (list 'BACKQUOTE form))))

(defun sharp-backslash-reader (stream char n)
  (do ((string "")
       (char #1=(READ-CHAR stream nil ?  T) #1#))
      ((not (constituentp char))
       (UNREAD-CHAR char stream)
       (values (if (= (length string) 1)
		   (CODE-CHAR (aref string 0))
		   (NAME-CHAR string))))
    (setq string (concat string (list (CHAR-CODE char))))))

(defun sharp-quote-reader (stream char n)
  (values (list 'FUNCTION (READ stream T nil T))))

(defun sharp-left-paren-reader (stream char n)
  (values (CONCATENATE 'VECTOR (READ-DELIMITED-LIST (CODE-CHAR 41) stream))))

(defun sharp-asterisk-reader (stream char n) nil)

(defun sharp-colon-reader (stream char n)
  (do ((string "")
       (char #1=(READ-CHAR stream nil ?  T) #1#))
      ((not (constituentp char))
       (UNREAD-CHAR char stream)
       (values (make-symbol string)))
    (setq string (concat string (list (CHAR-CODE char))))))

(defvar *read-eval* T)

(defun sharp-dot-reader (stream char n)
  (if *read-eval*
      (values (eval (READ stream T nil T)))
      (error "reader error: #. disabled")))

(defun sharp-b-reader (stream char n) nil)
(defun sharp-o-reader (stream char n) nil)
(defun sharp-x-reader (stream char n) nil)
(defun sharp-r-reader (stream char n) nil)

(defun sharp-c-reader (stream char n)
  (let ((list (READ stream T nil T)))
    (if (and (consp list) (= (length list) 2))
	(values (COMPLEX (first list) (second list)))
	(error "syntax error"))))

(defun sharp-a-reader (stream char n) nil)
(defun sharp-s-reader (stream char n) nil)
(defun sharp-p-reader (stream char n) nil)
(defun sharp-equal-reader (stream char n) nil)
(defun sharp-sharp-reader (stream char n) nil)

(defun eval-feature-test (expr)
  (cond
    ((symbolp expr)
     (member expr *FEATURES*))
    ((atom expr)
     (error "syntax error"))
    ((eq (first expr) (INTERN "NOT" *keyword-package*))
     (not (eval-feature-test (second expr))))
    ((eq (first expr) (INTERN "AND" *keyword-package*))
     (every #'eval-feature-test (rest expr)))
    ((eq (first expr) (INTERN "OR" *keyword-package*))
     (some #'eval-feature-test (rest expr)))
    (t
     (error "syntax error"))))

(defun sharp-plus-reader (stream char n)
  (if (eval-feature-test (let ((*PACKAGE* *keyword-package*))
			   (READ stream T nil T)))
      (values (READ stream T nil T))
      (progn
	(READ stream T nil T)
	(values))))

(defun sharp-minus-reader (stream char n)
  (if (not (eval-feature-test (let ((*PACKAGE* *keyword-package*))
				(READ stream T nil T))))
      (values (READ stream T nil T))
      (progn
	(READ stream T nil T)
	(values))))

(defun sharp-bar-reader (stream char n)
  (do ((last nil)
       (level 1)
       (char #1=(READ-CHAR stream T nil T) #1#))
      ((= level 0)
       (UNREAD-CHAR char stream)
       (values))
    (when (and (EQL last (CODE-CHAR 35)) (EQL char (CODE-CHAR 124)))
      (incf level))
    (when (and (EQL last (CODE-CHAR 124)) (EQL char (CODE-CHAR 35)))
      (decf level))
    (setq last char)))

(defun sharp-less-reader (stream char n)
  (error "syntax error"))
(defun sharp-space-reader (stream char n)
  (error "syntax error"))
(defun sharp-right-paren-reader (stream char n)
  (error "syntax error"))

(defun* COPY-READTABLE (&optional (from *READTABLE*) to)
  (unless from
    (setq from *standard-readtable*))
  (unless to
    (setq to (MAKE-READTABLE)))
  (setf (READTABLE-CASE to) (READTABLE-CASE from))
  (setf (READTABLE-SYNTAX-TYPE to)
	(copy-sequence (READTABLE-SYNTAX-TYPE from)))
  (setf (READTABLE-MACRO-TABLE to)
	(copy-sequence (READTABLE-MACRO-TABLE from)))
  (setf (READTABLE-DISPATCH-TABLE to)
	(let ((hash (make-hash-table :test #'equal)))
	  (maphash (lambda (key val) (setf (gethash key hash) val))
		   (READTABLE-DISPATCH-TABLE from))
	  hash))
  to)

(defvar *READTABLE* (COPY-READTABLE nil))

(defun char-convert-case (char)
  (ecase (READTABLE-CASE *READTABLE*)
    (:preserve	char)
    (:upcase	(CHAR-UPCASE char))
    (:downcase	(CHAR-DOWNCASE char))
    (:invert	(error "not implemented"))))

(defun* READ (&optional stream (eof-error-p T) eof-value recursive-p)
  (let (char
	(escape nil)
	(package nil)
	(colons 0)
	(token nil))
    (tagbody
      STEP-1
       (setq char (READ-CHAR stream eof-error-p eof-value recursive-p))
       (when (EQL char eof-value)
	 (return-from READ eof-value))

      (case (char-syntx char)
	(:whitespace
	 (go STEP-1))
	((:terminating-macro :non-terminating-macro)
	 (multiple-value-bind (fn nt) (GET-MACRO-CHARACTER char)
	   (let ((list (multiple-value-list (funcall fn stream char))))
	     (if (null list)
		 (go STEP-1)
		 (return-from READ (first list))))))
	(:single-escape
	 (setq escape t)
	 (setq char (READ-CHAR stream T nil T))
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-8))
	(:multiple-escape
	 (go STEP-9))
	(:constituent
	 (setq char (char-convert-case char))))

      STEP-8a
      (cond
	((CHAR= char (CODE-CHAR 58))
	 (incf colons)
	 (when (or (and package (not (zerop (length token))))
		   (> colons 2))
	   (error "too many colons"))
	 (if (= colons 1)
	     (setq package token))
	 (setq token nil))
	(t
	 (setq token (concat token (list (CHAR-CODE char))))))
      STEP-8
      (setq char (READ-CHAR stream nil nil T))
      (when (null char)
	(go STEP-10))
      (case (char-syntx char)
	((:constituent :non-terminating-macro)
	 (setq char (char-convert-case char))
	 (go STEP-8a))
	(:single-escape
	 (setq escape t)
	 (setq char (READ-CHAR stream T nil T))
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-8))
	(:multiple-escape
	 (go STEP-9))
	(:terminating-macro
	 (UNREAD-CHAR char stream)
	 (go STEP-10))
	(:whitespace
	 (when nil
	   (UNREAD-CHAR char stream))
	 (go STEP-10)))

      STEP-9
      (setq escape t)
      (setq char (READ-CHAR stream nil nil T))
      (when (null char)
	(error "end of file"))
      (case (char-syntx char)
	((:constituent :non-terminating-macro :terminating-macro :whitespace)
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-9))
	(:single-escape
	 (setq char (READ-CHAR stream T nil T))
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-9))
	(:multiple-escape
	 (when (null token)
	   (setq token ""))
	 (go STEP-8)))

      STEP-10
      (return-from READ (process-token package colons token escape)))))

(defun* process-token (package colons token escape)
  (when (and (zerop colons) (not escape))
    (let ((n (parse-number token)))
      (when n
	(return-from process-token n))))
  (when (null package)
    (case colons
      (0 (setq package *PACKAGE*))
      (1 (setq package "KEYWORD"))
      (2 (error "too many colons"))))
  (when (null token)
    (error "token terminated by colon"))
  (multiple-value-bind (sym status) (FIND-SYMBOL token package)
    (cond
      ((or (eq status *:external*)
	   (eq status *:inherited*))
       (return-from READ sym))
      ((eq status *:internal*)
       (if (< colons 2)
	   (error "internal symbol")
	   sym))
      ((null status)
       (return-from READ (nth-value 0 (INTERN token package)))))))

(defvar *READ-BASE* 10)

(defun potential-number-p (string)
  (and
   (every (lambda (char)
	    (or (DIGIT-CHAR-P (CODE-CHAR char) *READ-BASE*)
		(find char "+-/.^_DEFLSdefls")))
	  string)
   (or (some ;;(lambda (char) (DIGIT-CHAR-P (CODE-CHAR char)))
	     (compose DIGIT-CHAR-P CODE-CHAR)
	     string)
       (and (some (lambda (char)
		    (DIGIT-CHAR-P (CODE-CHAR char) *READ-BASE*))
		  string)
	    (not (find 46 string))))
   (let ((char (aref string 0)))
     (or (DIGIT-CHAR-P (CODE-CHAR char) *READ-BASE*)
	 (find char "+-.^_")))
   (not (find (aref string (1- (length string))) "+-"))))

(defun* parse-number (string)
  (when (potential-number-p string)
    (multiple-value-bind (integer end)
	(PARSE-INTEGER string :radix *READ-BASE* :junk-allowed T)
      (when (and integer (= end (length string)))
	(return-from parse-number integer))
      (when (and integer
		 (CHAR= (CHAR string end) (CODE-CHAR 47)))
	(multiple-value-bind (denumerator end2)
	    (PARSE-INTEGER string :radix *READ-BASE* :start (1+ end)
			   :junk-allowed T)
	  (when (and denumerator (= end2 (length string)))
	    (return-from parse-number (cl:/ integer denumerator))))))))

(defun* READ-DELIMITED-LIST (delimiter &optional (stream *STANDARD-INPUT*)
			                         recursive-p)
  (do ((list nil)
       (char #1=(PEEK-CHAR T stream T nil recursive-p) #1#))
      ((CHAR= char delimiter)
       (READ-CHAR stream t nil recursive-p)
       (nreverse list))
    (push (READ stream t nil recursive-p) list)))

(defun* READ-FROM-STRING (string &optional (eof-error-p T) eof-value
				 &key (start 0) end preserve-whitespace)
  (let ((stream (MAKE-STRING-INPUT-STREAM string start end)))
    (if preserve-whitespace
	(READ-PRESERVING-WHITESPACE stream eof-error-p eof-value)
	(READ stream eof-error-p eof-value))))
