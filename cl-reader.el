;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 23, Reader.

(IN-PACKAGE "EMACS-CL")

(defvar *backquote-level* 0)

(DEFSTRUCT (READTABLE (:predicate READTABLEP) (:copier nil))
  CASE
  SYNTAX-TYPE
  MACRO-TABLE
  DISPATCH-TABLE)

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

(cl:defun MAKE-DISPATCH-MACRO-CHARACTER (char &optional non-terminating-p
					      (readtable *READTABLE*))
  (SET-MACRO-CHARACTER char #'dispatch-reader non-terminating-p readtable)
  T)

(defun* read1 (stream eof-error-p eof-value recursive-p preserve-whitespace)
  (let (char
	(escape nil)
	(package nil)
	(colons 0)
	(token nil))
    (tagbody
      STEP-1
       (setq char (READ-CHAR stream eof-error-p eof-value recursive-p))
       (when (EQL char eof-value)
	 (return-from read1 eof-value))

      (case (char-syntx char)
	(:whitespace
	 (go STEP-1))
	((:terminating-macro :non-terminating-macro)
	 (let* ((fn (GET-MACRO-CHARACTER char))
		(list (MULTIPLE-VALUE-LIST (funcall fn stream char))))
	   (if (null list)
	       (go STEP-1)
	       (return-from read1 (VALUES (first list))))))
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
	((ch= char 58)
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
	 (when preserve-whitespace
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
      (unless *READ-SUPPRESS*
	(return-from read1 (process-token package colons token escape))))))

(cl:defun READ (&optional stream (eof-error-p T) eof-value recursive-p)
  (read1 stream eof-error-p eof-value recursive-p nil))

(cl:defun READ-PRESERVING-WHITESPACE (&optional stream (eof-error-p T)
				      eof-value recursive-p)
  (read1 stream eof-error-p eof-value recursive-p t))

(defmacro* unless-read-suppress-let ((var form) &body body)
  `(let ((,var ,form))
     (unless *READ-SUPPRESS*
       ,@body)))

(defun* READ-DELIMITED-LIST (delimiter &optional (stream *STANDARD-INPUT*)
			                         recursive-p)
  (do ((list nil)
       (char #1=(PEEK-CHAR T stream T nil recursive-p) #1#))
      ((CHAR= char delimiter)
       (READ-CHAR stream t nil recursive-p)
       (nreverse list))
    (unless-read-suppress-let (object (READ stream t nil recursive-p))
      (push object list))))

(cl:defun READ-FROM-STRING (string &optional (eof-error-p T) eof-value
			           &key (START 0) END PRESERVE-WHITESPACE)
  (let ((stream (MAKE-STRING-INPUT-STREAM string START END)))
    (if PRESERVE-WHITESPACE
	(READ-PRESERVING-WHITESPACE stream eof-error-p eof-value)
	(READ stream eof-error-p eof-value))))

;;; READTABLE-CASE defined by defstruct.

;;; READTABLEP defined by defstruct.

(defun* SET-DISPATCH-MACRO-CHARACTER (disp-char sub-char new-function
				      &optional (readtable *READTABLE*))
  (let ((string (concat (list (CHAR-CODE disp-char) (CHAR-CODE sub-char)))))
    (setf (gethash string (READTABLE-DISPATCH-TABLE readtable))
	  new-function))
  T)

(defun* GET-DISPATCH-MACRO-CHARACTER (disp-char sub-char
				      &optional (readtable *READTABLE*))
  (let ((string (concat (list (CHAR-CODE disp-char) (CHAR-CODE sub-char)))))
    (gethash string (READTABLE-DISPATCH-TABLE readtable))))

(defun* SET-MACRO-CHARACTER (char new-function
			     &optional non-terminating-p
			               (readtable *READTABLE*))
  (setf (aref (READTABLE-MACRO-TABLE readtable) (CHAR-CODE char)) new-function)
  (setf (aref (READTABLE-SYNTAX-TYPE readtable) (CHAR-CODE char))
	(if non-terminating-p
	    :non-terminating-macro
	    :terminating-macro))
  T)

(defun* char-syntx (char &optional (readtable *READTABLE*))
  (aref (READTABLE-SYNTAX-TYPE readtable) (CHAR-CODE char)))

(defun* GET-MACRO-CHARACTER (char &optional (readtable *READTABLE*))
  (VALUES (aref (READTABLE-MACRO-TABLE readtable) (CHAR-CODE char))
	  (eq (char-syntx char readtable) :non-terminating-macro)))

(defun* SET-SYNTAX-FROM-CHAR (to-char from-char
			      &optional (to-readtable *READTABLE*)
			                (from-readtable *standard-readtable*))
  (setf (aref (READTABLE-SYNTAX-TYPE to-readtable) (CHAR-CODE to-char))
	(char-syntx from-char from-readtable))
  T)

(cl:defmacro WITH-STANDARD-IO-SYNTAX (&body body)
  `(LET ((*PACKAGE*			*cl-package*)
	 (*PRINT-ARRAY*			T)
	 (*PRINT-BASE*			10)
	 (*PRINT-CASE*			(kw UPCASE))
	 (*PRINT-CIRCLE*		nil)
	 (*PRINT-ESCAPE*		T)
	 (*PRINT-GENSYM*		T)
	 (*PRINT-LENGTH*		nil)
	 (*PRINT-LEVEL*			nil)
	 (*PRINT-LINES*			nil)
	 (*PRINT-MISER-WIDTH*		nil)
;	 (*PRINT-PPRINT-DISPATCH*	#<The standard pprint dispatch table>)
	 (*PRINT-PRETTY*		nil)
	 (*PRINT-RADIX*			nil)
	 (*PRINT-READABLY*		T)
	 (*PRINT-RIGHT-MARGIN*		nil)
	 (*READ-BASE*			10)
	 (*READ-DEFAULT-FLOAT-FORMAT*	'SINGLE-FLOAT)
	 (*READ-EVAL*			T)
	 (*READ-SUPPRESS*		nil)
	 (*READTABLE*			*standard-readtable*))
     ,@body))

(defvar *READ-BASE* 10)

(defvar *READ-DEFAULT-FLOAT-FORMAT* 'SINGLE-FLOAT)

(defvar *READ-EVAL* T)

(defvar *READ-SUPPRESS* nil)

(defvar *standard-readtable*
  (let ((readtable (MAKE-READTABLE)))
    (setf (READTABLE-CASE readtable) (kw UPCASE))

    (setf (READTABLE-SYNTAX-TYPE readtable) (make-vector 256 :constituent))
    (setf (aref (READTABLE-SYNTAX-TYPE readtable) 32) :whitespace)
    (setf (aref (READTABLE-SYNTAX-TYPE readtable) 92) :single-escape)
    (setf (aref (READTABLE-SYNTAX-TYPE readtable) 124) :multiple-escape)
    (dolist (char (mapcar #'CODE-CHAR '(9 10 12 13)))
      (SET-SYNTAX-FROM-CHAR char (ch 32) readtable readtable))

    (setf (READTABLE-MACRO-TABLE readtable) (make-vector 256 nil))
    (SET-MACRO-CHARACTER (ch 34) #'double-quote-reader nil readtable)
    (MAKE-DISPATCH-MACRO-CHARACTER (ch 35) T readtable)
    (SET-MACRO-CHARACTER (ch 39) #'quote-reader nil readtable)
    (SET-MACRO-CHARACTER (ch 40) #'left-paren-reader nil readtable)
    (SET-MACRO-CHARACTER (ch 41) #'right-paren-reader nil readtable)
    (SET-MACRO-CHARACTER (ch 44) #'comma-reader nil readtable)
    (SET-MACRO-CHARACTER (ch 59) #'semicolon-reader nil readtable)
    (SET-MACRO-CHARACTER (ch 96) #'backquote-reader nil readtable)

    (setf (READTABLE-DISPATCH-TABLE readtable) (make-hash-table :test #'equal))
    (macrolet ((sharp-macro (n fn)
		 `(SET-DISPATCH-MACRO-CHARACTER (ch 35) (CODE-CHAR ,n)
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

(defvar *READTABLE* (COPY-READTABLE nil))

;;; READER-ERROR defined in cl-conditions.el.

(defun whitespacep (char)
  (eq (char-syntx char) :whitespace))

(defun constituentp (char)
  (eq (char-syntx char) :constituent))



(defun dispatch-reader (stream char1)
  (do* ((param nil)
	(char #1=(READ-CHAR stream T nil T) #1#)
	(digit #2=(DIGIT-CHAR-P char 10) #2#))
      ((not digit)
       (let ((fn (GET-DISPATCH-MACRO-CHARACTER char1 char)))
	 (if (null fn)
	     (ERROR 'READER-ERROR)
	     (funcall fn stream char param))))
    (setq param (binary+ (binary* (or param 0) 10) digit))))

(defun double-quote-reader (stream double-quote-char)
  (do ((string "")
       (char #1=(READ-CHAR stream T nil T) #1#))
      ((CHAR= char double-quote-char)
       (VALUES (if *READ-SUPPRESS* nil string)))
    (when (eq (char-syntx char) :single-escape)
      (setq char (READ-CHAR stream T nil T)))
    (unless *READ-SUPPRESS*
      (setq string (concat string (list (CHAR-CODE char)))))))

(defun quote-reader (stream ch)
  (let ((object (READ stream T nil T)))
    (unless *READ-SUPPRESS*
      (VALUES (list 'QUOTE object)))))

(defun* left-paren-reader (stream char)
  (do ((list nil)
       (char #1=(PEEK-CHAR T stream) #1#))
      ((ch= char 41)
       (READ-CHAR stream)
       (VALUES (nreverse list)))
    (unless-read-suppress-let (object (READ stream T nil T))
      (if (and (symbolp object) (string= (SYMBOL-NAME object) "."))
	  (let ((cdr (READ stream T nil T)))
	    (unless (ch= (READ-CHAR stream) 41)
	      (error "syntax error"))
	    (return-from left-paren-reader
	      (VALUES (append (nreverse list) cdr))))
	  (push object list)))))

(defun right-paren-reader (stream char)
  (unless *READ-SUPPRESS*
    (error "unbalanced '%c'" char)))

(defun comma-reader (stream char)
  (unless (or (plusp *backquote-level*) *READ-SUPPRESS*)
    (error "comma outside backquote"))
  (let ((next-char (READ-CHAR stream T nil T)))
    (let ((*backquote-level* (1- *backquote-level*)))
      (cond
	((ch= next-char 64)
	 (unless-read-suppress-let (object (READ stream T nil T))
	   (VALUES (list 'COMMA-AT object))))
	((ch= next-char 46)
	 (unless-read-suppress-let (object (READ stream T nil T))
	   (VALUES (list 'COMMA-DOT object))))
	(t
	 (UNREAD-CHAR next-char stream)
	 (unless-read-suppress-let (object (READ stream T nil T))
	   (VALUES (list 'COMMA object))))))))

(defun semicolon-reader (stream ch)
  (do ()
      ((ch= (READ-CHAR stream nil (ch 10) T) 10)
       (VALUES))))

(defun backquote-reader (stream char)
  (let* ((*backquote-level* (1+ *backquote-level*))
	 (form (READ stream T nil T)))
    (unless *READ-SUPPRESS*
      (VALUES (list 'BACKQUOTE form)))))

(defun no-param (char n)
  (when n
    (WARN "Parameter %D ignored in #%C." n char)))

(defun sharp-backslash-reader (stream char n)
  (no-param (ch 92) n)
  (do ((string "")
       (char #1=(READ-CHAR stream nil (ch 32) T) #1#))
      ((not (constituentp char))
       (UNREAD-CHAR char stream)
       (VALUES (cond
		 (*READ-SUPPRESS*	 nil)
		 ((= (length string) 1)	 (CHAR string 0))
		 (t			 (NAME-CHAR string)))))
    (unless *READ-SUPPRESS*
      (setq string (concat string (list (CHAR-CODE char)))))))

(defun sharp-quote-reader (stream char n)
  (no-param (ch 39) n)
  (unless-read-suppress-let (object (READ stream T nil T))
    (VALUES (list 'FUNCTION object))))

(defun sharp-left-paren-reader (stream char n)
  (unless-read-suppress-let (list (READ-DELIMITED-LIST (ch 41) stream))
    (VALUES (if (and n (plusp n))
		(MAP-INTO (MAKE-ARRAY n (kw INITIAL-ELEMENT) (car (last list)))
			  #'IDENTITY list)
		(CONCATENATE 'VECTOR list)))))

(defun bit-vector (contents n)
  (let* ((len (or n (length contents)))
	 (vec (make-bool-vector len (car (last contents)))))
    (dotimes (i (min len (length contents)) vec)
      (aset vec i (nth i contents)))))

(defun sharp-asterisk-reader (stream char n)
  (do ((contents nil)
       (char #1=(READ-CHAR stream nil (ch 32) T) #1#))
      ((not (constituentp char))
       (UNREAD-CHAR char stream)
       (VALUES (unless *READ-SUPPRESS* (bit-vector (nreverse contents) n))))
    (unless *READ-SUPPRESS*
      (push (ecase (CHAR-CODE char) (48 nil) (49 1)) contents))))

(defun sharp-colon-reader (stream char n)
  (no-param (ch 58) n)
  (do ((string "")
       (char #1=(READ-CHAR stream nil (ch 32) T) #1#))
      ((not (constituentp char))
       (UNREAD-CHAR char stream)
       (VALUES (unless *READ-SUPPRESS* (make-symbol string))))
    (setq string (concat string (list (CHAR-CODE char))))))

(defun sharp-dot-reader (stream char n)
  (no-param (ch 46) n)
  (unless-read-suppress-let (object (READ stream T nil T))
    (if *READ-EVAL*
	(VALUES (eval object))
	(ERROR 'READER-ERROR))))

(defun sharp-b-reader (stream char n) nil)
(defun sharp-o-reader (stream char n) nil)
(defun sharp-x-reader (stream char n) nil)
(defun sharp-r-reader (stream char n) nil)

(defun sharp-c-reader (stream char n)
  (no-param (ch 67) n)
  (let ((list (READ stream T nil T)))
    (unless *READ-SUPPRESS*
      (if (and (consp list) (= (length list) 2))
	  (VALUES (COMPLEX (first list) (second list)))
	  (error "syntax error")))))

(defun sharp-a-reader (stream char n) nil)
(defun sharp-s-reader (stream char n) nil)

(defun sharp-p-reader (stream char n)
  (no-param (ch 80) n)
  (unless-read-suppress-let (string (READ stream T nil T))
    (unless (STRINGP string)
      (ERROR 'READER-ERROR))
    (PARSE-NAMESTRING string)))

(defun sharp-equal-reader (stream char n) nil)
(defun sharp-sharp-reader (stream char n) nil)

(defun eval-feature-test (expr)
  (cond
    ((symbolp expr)
     (member expr *FEATURES*))
    ((atom expr)
     (error "syntax error"))
    ((eq (first expr) (kw NOT))
     (not (eval-feature-test (second expr))))
    ((eq (first expr) (kw AND))
     (every #'eval-feature-test (rest expr)))
    ((eq (first expr) (kw OR))
     (some #'eval-feature-test (rest expr)))
    (t
     (error "syntax error"))))

(defun sharp-plus-reader (stream char n)
  (no-param (ch 43) n)
  (if (eval-feature-test (let ((*PACKAGE* *keyword-package*))
			   (READ stream T nil T)))
      (VALUES (READ stream T nil T))
      (let ((*READ-SUPPRESS* T))
	(READ stream T nil T)
	(VALUES))))

(defun sharp-minus-reader (stream char n)
  (no-param (ch 45) n)
  (if (eval-feature-test (let ((*PACKAGE* *keyword-package*))
			   (READ stream T nil T)))
      (let ((*READ-SUPPRESS* T))
	(READ stream T nil T)
	(VALUES))
      (VALUES (READ stream T nil T))))

(defun sharp-bar-reader (stream char n)
  (no-param (ch 124) n)
  (do ((last nil)
       (level 1)
       (char #1=(READ-CHAR stream T nil T) #1#))
      ((= level 0)
       (UNREAD-CHAR char stream)
       (VALUES))
    (when (and (ch= last 35) (ch= char 124))
      (incf level))
    (when (and (ch= last 124) (ch= char 35))
      (decf level))
    (setq last char)))

(defun sharp-less-reader (stream char n)
  (ERROR "syntax error"))
(defun sharp-space-reader (stream char n)
  (ERROR "syntax error"))
(defun sharp-right-paren-reader (stream char n)
  (ERROR "syntax error"))



(cl:defmacro BACKQUOTE (form)
  (expand-bq form))

(defun expand-bq (form)
  (cond
    ((consp form)
     (case (car form)
       (COMMA
	(second form))
       ((COMMA-AT COMMA-DOT)
	(error "syntax error"))
       (t
	(cons 'APPEND (expand-bq-list form)))))
    ((VECTORP form)
     `(APPLY (FUNCTION VECTOR) ,(expand-bq (MAP 'LIST #'IDENTITY form))))
    (t
     `(QUOTE ,form))))

(defun* expand-bq-list (list)
  (let ((car (car list))
	(cdr (cdr list)))
    (cons
     (if (consp car)
	 (case (first car)
	   (COMMA			`(LIST ,(second car)))
	   ((COMMA-AT COMMA-DOT)	(second car))
	   (t				`(LIST ,(expand-bq car))))
	 (case car
	   (COMMA			(return-from expand-bq-list
					  (list (second list))))
	   ((COMMA-AT COMMA-DOT)	(error "syntax error"))
	   (t				`(LIST ,(expand-bq car)))))
     (if (consp cdr)
	 (expand-bq-list cdr)
	 `((QUOTE ,cdr))))))



(defun char-convert-case (char)
  (let ((case (READTABLE-CASE *READTABLE*)))
    (cond
      ((eq case (kw PRESERVE))	char)
      ((eq case (kw UPCASE))	(CHAR-UPCASE char))
      ((eq case (kw DOWNCASE))	(CHAR-DOWNCASE char))
      ((eq case (kw INVERT))	(error "TODO"))
      (t			(type-error case `(MEMBER ,(kw PRESERVE)
							  ,(kw UPCASE)
							  ,(kw DOWNCASE)
							  ,(kw INVERT)))))))

(defun* process-token (package colons token escape)
  (when (and (zerop colons) (not escape))
    (let ((n (parse-number token)))
      (when n
	(return-from process-token n))))
  (when (null package)
    (case colons
      (0 (setq package *PACKAGE*))
      (1 (setq package *keyword-package*))
      (2 (error "too many colons"))))
  (when (null token)
    (error "token terminated by colon"))
  (MULTIPLE-VALUE-BIND (sym status) (FIND-SYMBOL token package)
    (VALUES
      (cond
	((or (eq status (kw EXTERNAL)) (eq status (kw INHERITED)))
	 sym)
	((eq status (kw INTERNAL))
	 (if (and (< colons 2) (not (eq package *PACKAGE*)))
	     (error "internal symbol")
	     sym))
	((null status)
	 (NTH-VALUE 0 (INTERN token package)))))))

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
  ;; TODO: parse floats starting with a period.

  ;; First, is it a string of decimal digits followed by a period or an
  ;; exponent marker?  If so, can be either a decimal integer or a float.
  (MULTIPLE-VALUE-BIND (integer end)
      (PARSE-INTEGER string (kw RADIX) 10 (kw JUNK-ALLOWED) T)
    (when (and integer
	       (< end (LENGTH string))
	       (FIND (CHAR string end) ".DEFLSdefls"))
      (if (and (eq (1+ end) (LENGTH string))
	       (ch= (CHAR string end) 46))
	  (return-from parse-number (VALUES integer))
	  (let ((fraction 0)
		(exponent 0)
		(end2 end))
	    (when (ch= (CHAR string end) 46)
	      (MULTIPLE-VALUE-SETQ (fraction end2)
		(PARSE-INTEGER string (kw RADIX) 10 (kw START) (incf end)
			       (kw JUNK-ALLOWED) T)))
	    (when (< end2 (LENGTH string))
	      (unless (FIND (CHAR string end2) "DEFLSdefls")
		(ERROR 'READ-ERROR))
	      (setq exponent (PARSE-INTEGER string (kw RADIX) 10
					    (kw START) (1+ end2))))
	    (case *READ-DEFAULT-FLOAT-FORMAT*
	      ((SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT))
	      (t (ERROR 'PARSE-ERROR)))
	    (return-from parse-number
	      (VALUES
	       (* (+ (FLOAT integer)
		     (* (if (MINUSP integer) -1 1)
			(FLOAT fraction)
			(expt 10.0 (- end end2))))
		  (expt 10.0 (FLOAT exponent)))))))))
  
  ;; Second, try parsing as a number in current input radix.  It can
  ;; be either an integer or a ratio.
  (MULTIPLE-VALUE-BIND (integer end)
      (PARSE-INTEGER string (kw RADIX) *READ-BASE* (kw JUNK-ALLOWED) T)
    (unless integer
      (return-from parse-number (VALUES nil)))
    (cond
      ((= end (LENGTH string))
       (return-from parse-number (VALUES integer)))
      ((ch= (CHAR string end) 47)
       (MULTIPLE-VALUE-BIND (denumerator end2)
	   (PARSE-INTEGER string (kw RADIX) *READ-BASE*
			  (kw START) (1+ end) (kw JUNK-ALLOWED) T)
	 (when (and denumerator (= end2 (LENGTH string)))
	   (VALUES (cl:/ integer denumerator))))))))
