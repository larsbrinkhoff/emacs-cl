;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 23, Reader.

(defstruct (readtable (:predicate readtablep) (:copier nil))
  case
  syntax-type
  macro-table
  dispatch-table)

(defun* get-dispatch-macro-character (disp-char sub-char
				      &optional (readtable *readtable*))
  (let ((string (concat (list disp-char) (list sub-char))))
    (gethash string (readtable-dispatch-table readtable))))

(defun* set-dispatch-macro-character (disp-char sub-char new-function
				      &optional (readtable *readtable*))
  (let ((string (concat (list disp-char) (list sub-char))))
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
    (set-syntax-from-char 9 32 readtable readtable)
    (set-syntax-from-char 10 32 readtable readtable)
    (set-syntax-from-char 12 32 readtable readtable)
    (set-syntax-from-char 13 32 readtable readtable)

    (setf (readtable-macro-table readtable) (make-vector 256 nil))
    (set-macro-character 34 #'double-quote-reader nil readtable)
    (set-macro-character ?# #'sharp-reader t readtable)
    (set-macro-character ?' #'quote-reader nil readtable)
    (set-macro-character 40 #'left-paren-reader nil readtable)
    (set-macro-character 41 #'right-paren-reader nil readtable)
    (set-macro-character ?, #'comma-reader nil readtable)
    (set-macro-character 59 #'semicolon-reader nil readtable)
    (set-macro-character ?` #'backquote-reader nil readtable)

    (setf (readtable-dispatch-table readtable) (make-hash-table :test #'equal))
    (set-dispatch-macro-character ?# 92 #'sharp-backslash-reader readtable)
    (set-dispatch-macro-character ?# ?' #'sharp-quote-reader readtable)
    (set-dispatch-macro-character ?# 40 #'sharp-left-paren-reader readtable)
    (set-dispatch-macro-character ?# ?* #'sharp-asterisk-reader readtable)
    (set-dispatch-macro-character ?# ?: #'sharp-colon-reader readtable)
    (set-dispatch-macro-character ?# ?. #'sharp-dot-reader readtable)
    (set-dispatch-macro-character ?# ?b #'sharp-b-reader readtable)
    (set-dispatch-macro-character ?# ?B #'sharp-b-reader readtable)
    (set-dispatch-macro-character ?# ?o #'sharp-o-reader readtable)
    (set-dispatch-macro-character ?# ?O #'sharp-o-reader readtable)
    (set-dispatch-macro-character ?# ?x #'sharp-x-reader readtable)
    (set-dispatch-macro-character ?# ?X #'sharp-x-reader readtable)
    (set-dispatch-macro-character ?# ?r #'sharp-r-reader readtable)
    (set-dispatch-macro-character ?# ?R #'sharp-r-reader readtable)
    (set-dispatch-macro-character ?# ?c #'sharp-c-reader readtable)
    (set-dispatch-macro-character ?# ?C #'sharp-c-reader readtable)
    (set-dispatch-macro-character ?# ?a #'sharp-a-reader readtable)
    (set-dispatch-macro-character ?# ?A #'sharp-a-reader readtable)
    (set-dispatch-macro-character ?# ?s #'sharp-s-reader readtable)
    (set-dispatch-macro-character ?# ?S #'sharp-s-reader readtable)
    (set-dispatch-macro-character ?# ?p #'sharp-p-reader readtable)
    (set-dispatch-macro-character ?# ?P #'sharp-p-reader readtable)
    (set-dispatch-macro-character ?# ?= #'sharp-equal-reader readtable)
    (set-dispatch-macro-character ?# ?# #'sharp-sharp-reader readtable)
    (set-dispatch-macro-character ?# ?+ #'sharp-plus-reader readtable)
    (set-dispatch-macro-character ?# ?- #'sharp-minus-reader readtable)
    (set-dispatch-macro-character ?# ?| #'sharp-bar-reader readtable)
    (set-dispatch-macro-character ?# ?< #'sharp-less-reader readtable)
    (set-dispatch-macro-character ?# 32 #'sharp-space-reader readtable)
    (set-dispatch-macro-character ?# 41 #'sharp-right-paren-reader readtable)

    readtable))

(defun whitespacep (char)
  (eq (char-syntx char) :whitespace))

(defun constituentp (char)
  (eq (char-syntx char) :constituent))

(defun double-quote-reader (stream double-quote-char)
  (do ((string "")
       (char #1=(cl:read-char stream t nil t) #1#))
      ((char= char double-quote-char)
       (values string))
    (setq string
	  (concat string
		  (list (if (eq (char-syntx char) :single-escape)
			    (cl:read-char stream t nil t)
			    char))))))

(defun sharp-reader (stream char1)
  (let* ((char2 (cl:read-char stream t nil t))
	 (fn (get-dispatch-macro-character char1 char2)))
    (if (null fn)
	(error "invalid dispatching macro: %s" string)
	(funcall fn stream char2 nil))))

(defun quote-reader (stream ch)
  (values (list (cl:intern "QUOTE" "CL") (cl:read stream t nil t))))

(defun left-paren-reader (stream char)
  (do ((list nil)
       (char #1=(peek-char t stream) #1#))
      ((eql char 41)
       (values (nreverse list)))
    (push (cl:read stream t nil t) list)))

(defun right-paren-reader (stream char)
  (error "unbalanced '%c'" char))

(defun comma-reader (stream char)
  nil)

(defun semicolon-reader (stream ch)
  (do ()
      ((char= (cl:read-char stream nil (code-char 10) t) (code-char 10))
       (values))))

(defun backquote-reader (stream char)
  nil)

(defun sharp-backslash-reader (stream char n)
  (do ((string "")
       (char #1=(cl:read-char stream nil ?  t) #1#))
      ((not (constituentp char))
       (unread-char char stream)
       (values (if (= (length string) 1)
		   (aref string 0)
		   (name-char string))))
    (setq string (concat string (list char)))))

(defun sharp-quote-reader (stream char n)
  (values (list (cl:intern "FUNCTION" "CL") (cl:read stream t nil t))))

(defun sharp-left-paren-reader (stream char n)
  (values (cl:concatenate 'vector
			  (read-delimited-list (code-char 41) stream))))

(defun sharp-asterisk-reader (stream char n) nil)

(defun sharp-colon-reader (stream char n)
  (do ((string "")
       (char #1=(cl:read-char stream nil ?  t) #1#))
      ((not (constituentp char))
       (unread-char char stream)
       (values (make-symbol string)))
    (setq string (concat string (list char)))))

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
       (char #1=(cl:read-char stream t nil t) #1#))
      ((= level 0)
       (unread-char char stream)
       (values))
    (when (and (eql last ?#) (eql char ?|))
      (incf level))
    (when (and (eql last ?|) (eql char ?#))
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
       (setq char (cl:read-char stream eof-error-p eof-value recursive-p))
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
	 (setq char (cl:read-char stream t nil t))
	 (setq token (concat token (list char)))
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
	 (setq token (concat token (list char)))))
      STEP-8
      (setq char (cl:read-char stream nil nil t))
      (when (null char)
	(go STEP-10))
      (case (char-syntx char)
	((:constituent :non-terminating-macro)
	 (setq char (char-convert-case char))
	 (go STEP-8a))
	(:single-escape
	 (setq escape t)
	 (setq char (cl:read-char stream t nil t))
	 (setq token (concat token (list char)))
	 (go STEP-8))
	(:multiple-escape
	 (go STEP-9))
	(:terminating-macro
	 (unread-char char stream)
	 (go STEP-10))
	(:whitespace
	 (when nil
	   (unread-char char stream))
	 (go STEP-10)))

      STEP-9
      (setq escape t)
      (setq char (cl:read-char stream nil nil t))
      (when (null char)
	(error "end of file"))
      (case (char-syntx char)
	((:constituent :non-terminating-macro :terminating-macro :whitespace)
	 (setq token (concat token (list char)))
	 (go STEP-9))
	(:single-escape
	 (setq char (cl:read-char stream t nil t))
	 (setq token (concat token (list char)))
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
       (char #1=(peek-char t stream t nil recursive-p) #1#))
      ((char= char delimiter)
       (cl:read-char stream t nil recursive-p)
       (nreverse list))
    (push (cl:read stream t nil recursive-p) list)))

(defun* cl:read-from-string (string &optional (eof-error-p t) eof-value
				    &key (start 0) end preserve-whitespace)
  (let ((stream (make-string-input-stream string start end)))
    (if preserve-whitespace
	(read-preserving-whitespace stream eof-error-p eof-value)
	(cl:read stream eof-error-p eof-value))))

