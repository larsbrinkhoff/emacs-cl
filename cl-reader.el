;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 23, Reader.

(defvar *backquote-level* 0)

(defstruct (readtable (:predicate readtablep) (:copier nil))
  case
  syntax-type
  macro-table
  dispatch-table)

(defun* get-dispatch-macro-character (disp-char sub-char
				      &optional (readtable *readtable*))
  (let ((string (concat (list (char-code disp-char) (char-code sub-char)))))
    (gethash string (readtable-dispatch-table readtable))))

(defun* set-dispatch-macro-character (disp-char sub-char new-function
				      &optional (readtable *readtable*))
  (let ((string (concat (list (char-code disp-char) (char-code sub-char)))))
    (setf (gethash string (readtable-dispatch-table readtable))
	  new-function))
  t)

(defun* get-macro-character (char &optional (readtable *readtable*))
  (values
   (aref (readtable-macro-table readtable) (char-code char))
   (eq (char-syntx char readtable) :non-terminating-macro)))

(defun* set-macro-character (char new-function
			    &optional non-terminating-p
			              (readtable *readtable*))
  (setf (aref (readtable-macro-table readtable) (char-code char)) new-function)
  (setf (aref (readtable-syntax-type readtable) (char-code char))
	(if non-terminating-p
	    :non-terminating-macro
	    :terminating-macro))
  t)

(defun* set-syntax-from-char (to-char from-char
			      &optional (to-readtable *readtable*)
			                (from-readtable *standard-readtable*))
  (setf (aref (readtable-syntax-type to-readtable) (char-code to-char))
	(char-syntx from-char from-readtable))
  t)

(defun* char-syntx (char &optional (readtable *readtable*))
  (aref (readtable-syntax-type readtable) (char-code char)))

(defvar *standard-readtable*
  (let ((readtable (make-readtable)))
    (setf (readtable-case readtable) :upcase)

    (setf (readtable-syntax-type readtable) (make-vector 256 :constituent))
    (setf (aref (readtable-syntax-type readtable) 32) :whitespace)
    (setf (aref (readtable-syntax-type readtable) 92) :single-escape)
    (setf (aref (readtable-syntax-type readtable) 124) :multiple-escape)
    (set-syntax-from-char (code-char 9) (code-char 32) readtable readtable)
    (set-syntax-from-char (code-char 10) (code-char 32) readtable readtable)
    (set-syntax-from-char (code-char 12) (code-char 32) readtable readtable)
    (set-syntax-from-char (code-char 13) (code-char 32) readtable readtable)

    (setf (readtable-macro-table readtable) (make-vector 256 nil))
    (set-macro-character (code-char 34) #'double-quote-reader nil readtable)
    (set-macro-character (code-char 35) #'sharp-reader t readtable)
    (set-macro-character (code-char 39) #'quote-reader nil readtable)
    (set-macro-character (code-char 40) #'left-paren-reader nil readtable)
    (set-macro-character (code-char 41) #'right-paren-reader nil readtable)
    (set-macro-character (code-char 44) #'comma-reader nil readtable)
    (set-macro-character (code-char 59) #'semicolon-reader nil readtable)
    (set-macro-character (code-char 96) #'backquote-reader nil readtable)

    (setf (readtable-dispatch-table readtable) (make-hash-table :test #'equal))
    (macrolet ((sharp-macro (n fn)
		 `(set-dispatch-macro-character (code-char 35) (code-char ,n)
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
       (char #1=(READ-CHAR stream t nil t) #1#))
      ((char= char double-quote-char)
       (values string))
    (setq string
	  (concat string
		  (list (char-code
			 (if (eq (char-syntx char) :single-escape)
			     (READ-CHAR stream t nil t)
			     char))))))

(defun sharp-reader (stream char1)
  (let* ((char2 (READ-CHAR stream t nil t))
	 (fn (get-dispatch-macro-character char1 char2)))
    (if (null fn)
	(error "invalid dispatching macro: %s" string)
	(funcall fn stream char2 nil))))

(defun quote-reader (stream ch)
  (values (list 'QUOTE (cl:read stream t nil t))))

(defun left-paren-reader (stream char)
  (do ((list nil)
       (char #1=(PEEK-CHAR t stream) #1#))
      ((eql char (code-char 41))
       (values (nreverse list)))
    (push (cl:read stream t nil t) list)))

(defun right-paren-reader (stream char)
  (error "unbalanced '%c'" char))

(defun comma-reader (stream char)
  (unless (plusp *backquote-level*)
    (error "comma outside backquote"))
  (let ((next-char (READ-CHAR stream t nil t)))
    (let ((*backquote-level* (1- *backquote-level*)))
      (cond
	((eql next-char (code-char 64))
	 (values (list 'COMMA-AT (cl:read stream t nil t))))
	((eql next-char (code-char 46))
	 (values (list 'COMMA-DOT (cl:read stream t nil t))))
	(t
	 (UNREAD-CHAR next-char stream)
	 (values (list 'COMMA (cl:read stream t nil t))))))))

(defun semicolon-reader (stream ch)
  (do ()
      ((char= (READ-CHAR stream nil (code-char 10) t) (code-char 10))
       (values))))

(defun backquote-reader (stream char)
  (let* ((*backquote-level* (1+ *backquote-level*))
	 (form (cl:read stream t nil t)))
    (values (list 'BACKQUOTE form))))

(defun sharp-backslash-reader (stream char n)
  (do ((string "")
       (char #1=(READ-CHAR stream nil ?  t) #1#))
      ((not (constituentp char))
       (UNREAD-CHAR char stream)
       (values (if (= (length string) 1)
		   (aref string 0)
		   (name-char string))))
    (setq string (concat string (list (char-code char))))))

(defun sharp-quote-reader (stream char n)
  (values (list 'FUNCTION (cl:read stream t nil t))))

(defun sharp-left-paren-reader (stream char n)
  (values (cl:concatenate 'vector
			  (read-delimited-list (code-char 41) stream))))

(defun sharp-asterisk-reader (stream char n) nil)

(defun sharp-colon-reader (stream char n)
  (do ((string "")
       (char #1=(READ-CHAR stream nil ?  t) #1#))
      ((not (constituentp char))
       (UNREAD-CHAR char stream)
       (values (make-symbol string)))
    (setq string (concat string (list (char-code char))))))

(defvar *read-eval* t)

(defun sharp-dot-reader (stream char n)
  (if *read-eval*
      (values (eval (cl:read stream t nil t)))
      (error "reader error: #. disabled")))

(defun sharp-b-reader (stream char n) nil)
(defun sharp-o-reader (stream char n) nil)
(defun sharp-x-reader (stream char n) nil)
(defun sharp-r-reader (stream char n) nil)

(defun sharp-c-reader (stream char n)
  (let ((list (cl:read stream t nil t)))
    (if (and (consp list) (= (length list) 2))
	(values (complex (first list) (second list)))
	(error "syntax error"))))

(defun sharp-a-reader (stream char n) nil)
(defun sharp-s-reader (stream char n) nil)
(defun sharp-p-reader (stream char n) nil)
(defun sharp-equal-reader (stream char n) nil)
(defun sharp-sharp-reader (stream char n) nil)
(defun sharp-plus-reader (stream char n) nil)
(defun sharp-minus-reader (stream char n) nil)

(defun sharp-bar-reader (stream char n)
  (do ((last nil)
       (level 1)
       (char #1=(READ-CHAR stream t nil t) #1#))
      ((= level 0)
       (UNREAD-CHAR char stream)
       (values))
    (when (and (eql last (code-char 35)) (eql char (code-char 124)))
      (incf level))
    (when (and (eql last (code-char 124)) (eql char (code-char 35)))
      (decf level))
    (setq last char)))

(defun sharp-less-reader (stream char n)
  (error "syntax error"))
(defun sharp-space-reader (stream char n)
  (error "syntax error"))
(defun sharp-right-paren-reader (stream char n)
  (error "syntax error"))

(defun* copy-readtable (&optional (from *readtable*) to)
  (unless from
    (setq from *standard-readtable*))
  (unless to
    (setq to (make-readtable)))
  (setf (readtable-case to) (readtable-case from))
  (setf (readtable-syntax-type to)
	(copy-sequence (readtable-syntax-type from)))
  (setf (readtable-macro-table to)
	(copy-sequence (readtable-macro-table from)))
  (setf (readtable-dispatch-table to)
	(let ((hash (make-hash-table :test #'equal)))
	  (maphash (lambda (key val) (setf (gethash key hash) val))
		   (readtable-dispatch-table from))
	  hash))
  to)

(defvar *readtable* (copy-readtable nil))

(defun char-convert-case (char)
  (case (readtable-case *readtable*)
    (:preserve char)
    (:upcase (char-upcase char))
    (:downcase (char-downcase char))
    (:invert (error "not implemented"))))

(defun* cl:read (&optional stream (eof-error-p t) eof-value recursive-p)
  (let (char
	(escape nil)
	(package nil)
	(colons 0)
	(token nil))
    (tagbody
      STEP-1
       (setq char (READ-CHAR stream eof-error-p eof-value recursive-p))
       (when (eql char eof-value)
	 (return-from cl:read eof-value))

      (case (char-syntx char)
	(:whitespace
	 (go STEP-1))
	((:terminating-macro :non-terminating-macro)
	 (multiple-value-bind (fn nt) (get-macro-character char)
	   (let ((list (multiple-value-list (funcall fn stream char))))
	     (if (null list)
		 (go STEP-1)
		 (return-from cl:read (first list))))))
	(:single-escape
	 (setq escape t)
	 (setq char (READ-CHAR stream t nil t))
	 (setq token (concat token (list (char-code char))))
	 (go STEP-8))
	(:multiple-escape
	 (go STEP-9))
	(:constituent
	 (setq char (char-convert-case char))))

      STEP-8a
      (cond
	((char= char (code-char 58))
	 (incf colons)
	 (when (or (and package (not (zerop (length token))))
		   (> colons 2))
	   (error "too many colons"))
	 (if (= colons 1)
	     (setq package token))
	 (setq token nil))
	(t
	 (setq token (concat token (list (char-code char))))))
      STEP-8
      (setq char (READ-CHAR stream nil nil t))
      (when (null char)
	(go STEP-10))
      (case (char-syntx char)
	((:constituent :non-terminating-macro)
	 (setq char (char-convert-case char))
	 (go STEP-8a))
	(:single-escape
	 (setq escape t)
	 (setq char (READ-CHAR stream t nil t))
	 (setq token (concat token (list (char-code char))))
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
      (setq char (READ-CHAR stream nil nil t))
      (when (null char)
	(error "end of file"))
      (case (char-syntx char)
	((:constituent :non-terminating-macro :terminating-macro :whitespace)
	 (setq token (concat token (list (char-code char))))
	 (go STEP-9))
	(:single-escape
	 (setq char (READ-CHAR stream t nil t))
	 (setq token (concat token (list (char-code char))))
	 (go STEP-9))
	(:multiple-escape
	 (when (null token)
	   (setq token ""))
	 (go STEP-8)))

      STEP-10
      (return-from cl:read (process-token package colons token escape)))))

(defun* process-token (package colons token escape)
  (when (and (zerop colons) (not escape))
    (let ((n (parse-number token)))
      (when n
	(return-from process-token n))))
  (when (null package)
    (case colons
      (0 (setq package *package*))
      (1 (setq package "KEYWORD"))
      (2 (error "too many colons"))))
  (when (null token)
    (error "token terminated by colon"))
  (multiple-value-bind (sym status) (find-symbol token package)
    (case status
      (:external
       (return-from cl:read sym))
      ((:internal :inherited)
       (error))
      ((nil)
       (return-from cl:read (cl:intern token package))))))

(defvar *read-base* 10)

(defun potential-number-p (string)
  (and
   (every (lambda (char)
	    (or (digit-char-p char *read-base*)
		(find char "+-/.^_DEFLSdefls")))
	  string)
   (or (some #'digit-char-p string)
       (and (some (lambda (char) (digit-char-p char *read-base*)) string)
	    (not (find (code-char 46) string))))
   (let ((char (aref string 0)))
     (or (digit-char-p char *read-base*)
	 (find char "+-.^_")))
   (not (find (aref string (1- (length string))) "+-"))))

(defun* parse-number (string)
  (when (potential-number-p string)
    (multiple-value-bind (integer end)
	(parse-integer string :radix *read-base* :junk-allowed t)
      (when (and integer (= end (length string)))
	(return-from parse-number integer))
      (when (and integer
		 (char= (char string end) (code-char 47)))
	(multiple-value-bind (denumerator end2)
	    (parse-integer string :radix *read-base* :start (1+ end)
			   :junk-allowed t)
	  (when (and denumerator (= end2 (length string)))
	    (return-from parse-number (cl:/ integer denumerator))))))))

(defun* read-delimited-list (delimiter &optional (stream *standard-input*)
			                         recursive-p)
  (do ((list nil)
       (char #1=(PEEK-CHAR t stream t nil recursive-p) #1#))
      ((char= char delimiter)
       (READ-CHAR stream t nil recursive-p)
       (nreverse list))
    (push (cl:read stream t nil recursive-p) list)))

(defun* cl:read-from-string (string &optional (eof-error-p t) eof-value
				    &key (start 0) end preserve-whitespace)
  (let ((stream (make-string-input-stream string start end)))
    (if preserve-whitespace
	(read-preserving-whitespace stream eof-error-p eof-value)
	(cl:read stream eof-error-p eof-value))))

