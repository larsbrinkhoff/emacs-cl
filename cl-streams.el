;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 21, Streams.

(IN-PACKAGE "EMACS-CL")

(defvar *STANDARD-INPUT* nil)

(defvar *STANDARD-OUTPUT* nil)

(defvar *ERROR-OUTPUT* nil)

(defvar *TERMINAL-IO* nil)

(DEFSTRUCT (STREAM (:predicate STREAMP) (:copier nil))
  filename
  content
  index
  read-fn
  write-fn)

(defun* PEEK-CHAR (&optional peek-type stream (eof-error-p T)
			     eof-value recursive-p)
  (loop
   (let ((char (READ-CHAR stream eof-error-p eof-value recursive-p)))
     (cond
       ((EQL char eof-value)
	(return-from PEEK-CHAR eof-value))
       ((or (eq peek-type nil)
	    (and (eq peek-type T) (not (whitespacep char)))
	    (and (not (eq peek-type T)) (CHAR= char peek-type)))
	(UNREAD-CHAR char stream)
	(return-from PEEK-CHAR char))))))

(defun input-stream (designator)
  (case designator
    ((nil)	*STANDARD-INPUT*)
    ((t)	*TERMINAL-IO*)
    (t		designator)))

(defun output-stream (designator)
  (case designator
    ((nil)	*STANDARD-OUTPUT*)
    ((t)	*TERMINAL-IO*)
    (t		designator)))

(defun* READ-CHAR (&optional stream-designator (eof-error-p T)
			     eof-value recursive-p)
  (let* ((stream (input-stream stream-designator))
	 (ch (funcall (STREAM-read-fn stream) stream)))
    (if (eq ch :eof)
	(if eof-error-p
	    (error "end of file")
	    eof-value)
	(CODE-CHAR ch))))

;;; TODO: READ-CHAR-NO-HANG

(defun TERPRI (&optional stream-designator)
  (let ((stream (output-stream stream-designator)))
    (WRITE-CHAR (CODE-CHAR 10) stream))
  nil)

;;; TODO: FRESH-LINE

(defun UNREAD-CHAR (char &optional stream-designator)
  (let ((stream (input-stream stream-designator)))
    (when (> (STREAM-index stream) 0)
      (decf (STREAM-index stream)))))

(defun WRITE-CHAR (char &optional stream-designator)
  (let ((stream (output-stream stream-designator)))
    (funcall (STREAM-write-fn stream) (CHAR-CODE char) stream)
    char))

(defun* READ-LINE (&optional stream-designator (eof-error-p T)
			     eof-value recursive-p)
  (let ((stream (input-stream stream-designator))
	(line ""))
    (loop
     (let ((char (READ-CHAR stream eof-error-p eof-value recursive-p)))
       (cond
	 ((eq char eof-value)
	  (return-from READ-LINE
	    (VALUES (if (= (length line) 0) eof-value line) t)))
	 ((eq (CHAR-CODE char) 10)
	  (return-from READ-LINE (VALUES line nil))))
       (setq line (concat line (list (CHAR-CODE char))))))))

(cl:defun WRITE-STRING (string &optional stream-designator &key (start 0) end)
  (do ((stream (output-stream stream-designator))
       (i 0 (1+ i)))
      ((= i (LENGTH string)) string)
    (WRITE-CHAR (CHAR string i) stream)))

(cl:defun WRITE-LINE (string &optional stream-designator &key (start 0) end)
  (let ((stream (output-stream stream-designator)))
    (WRITE-STRING string stream :start start :end end)
    (TERPRI stream)
    string))

;;; TODO: read-sequence

;;; TODO: write-sequence

;;; TODO: file-length

(defun FILE-POSITION (stream)
  (STREAM-index stream))

(cl:defun OPEN (filespec &key (direction (kw INPUT)) (element-type 'CHARACTER)
		              if-exists if-does-not-exist
			      (external-format (kw DEFAULT)))
  (MAKE-STREAM (kw filename) (when (eq direction :output) filespec)
	       (kw content) (let ((buffer (create-file-buffer filespec)))
			      (when (eq direction :input)
				(save-current-buffer
				  (set-buffer buffer)
				  (insert-file-contents-literally filespec)))
			      buffer)
	       (kw index) 0
	       (kw read-fn)
	         (lambda (stream)
		   (save-current-buffer
		     (set-buffer (STREAM-content stream))
		     (if (= (STREAM-index stream) (buffer-size))
			 :eof
			 (char-after (incf (STREAM-index stream))))))
	       (kw write-fn)
	         (lambda (char stream)
		   (save-current-buffer
		     (set-buffer (STREAM-content stream))
		     (goto-char (incf (STREAM-index stream)))
		     (insert char)))))

;;; TODO: stream-external-format

; (defmacro* WITH-OPEN-FILE ((stream filespec &rest options) &body body)
;   `(WITH-OPEN-STREAM (,stream (OPEN ,filespec ,@options))
;      ,@body))

(cl:defmacro WITH-OPEN-FILE ((stream filespec &rest options) &body body)
  `(WITH-OPEN-STREAM (,stream (OPEN ,filespec ,@options))
     ,@body))

(cl:defun CLOSE (stream &key abort)
  (when (STREAM-filename stream)
    (save-current-buffer
      (set-buffer (STREAM-content stream))
      (write-region 1 (1+ (buffer-size)) (STREAM-filename stream))))
  (when (bufferp (STREAM-content stream))
    (kill-buffer (STREAM-content stream)))
  T)

; (defmacro* WITH-OPEN-STREAM ((var stream) &body body)
;   `(let ((,var ,stream))
;     (unwind-protect
; 	 (progn ,@body)
;       (CLOSE ,var))))

(cl:defmacro WITH-OPEN-STREAM ((var stream) &body body)
  `(LET ((,var ,stream))
     (UNWIND-PROTECT
	  (PROGN ,@body)
       (CLOSE ,var))))

;;; TODO: listen

;;; TODO: clear-input

;;; TODO: finish-output, force-output, clear-output

;;; TODO: y-or-n-p, yes-or-no-p

;;; TODO: make-synonym-stream

;;; TODO: synonym-stream-symbol

;;; TODO: broadcast-stream-streams

;;; TODO: make-broadcast-stream

(defun MAKE-TWO-WAY-STREAM (input output)
  (MAKE-STREAM (kw content) (cons input output)
	       (kw index) 0
	       (kw read-fn)
	         (lambda (stream)
		   (READ-CHAR (car (STREAM-content stream))))
	       (kw write-fn)
	         (lambda (char stream)
		   (WRITE-CHAR char (cdr (STREAM-content stream))))))

(defun TWO-WAY-STREAM-INPUT-STREAM (stream)
  (car (STREAM-content stream)))

(defun TWO-WAY-STREAM-OUTPUT-STREAM (stream)
  (cdr (STREAM-content stream)))

;;; TODO: echo-stream-input-stream, echo-stream-output-stream

;;; TODO: make-echo-stream

;;; TODO: concatenated-stream-streams

;;; TODO: make-concatenated-stream

(defun GET-OUTPUT-STREAM-STRING (stream)
  (STREAM-content stream))

(cl:defun MAKE-STRING-INPUT-STREAM (string &optional (start 0) end)
  (MAKE-STREAM (kw content) (let ((substr (substring string start end)))
			      (if (> (length substr) 0)
				  substr
				  :eof))
	       (kw index) 0
	       (kw read-fn)
	         (lambda (stream)
		   (cond
		     ((eq (STREAM-content stream) :eof)
		      :eof)
		     ((= (STREAM-index stream)
			 (length (STREAM-content stream)))
		      (setf (STREAM-content stream) :eof))
		     (t
		      (aref (STREAM-content stream)
			    (1- (incf (STREAM-index stream)))))))
	       (kw write-fn)
	         (lambda (c s) (error "write to input stream"))))

(cl:defun MAKE-STRING-OUTPUT-STREAM (&key (element-type 'CHARACTER))
  (MAKE-STREAM (kw content) ""
	       (kw index) 0
	       (kw read-fn)
	         (lambda (s) (error "read from output stream"))
	       (kw write-fn)
	         (lambda (char stream)
		   (setf (STREAM-content stream)
			 (concat (STREAM-content stream)
				 (list char))))))

; (defmacro* WITH-INPUT-FROM-STRING ((var string &key index (start 0) end)
; 				   &body body)
;   `(WITH-OPEN-STREAM (,var (MAKE-STRING-INPUT-STREAM ,string ,start ,end))
;      ,@body))

(cl:defmacro WITH-INPUT-FROM-STRING ((var string &key index start end)
				     &body body)
  (when (null start)
    (setq start 0))
  `(WITH-OPEN-STREAM (,var (MAKE-STRING-INPUT-STREAM ,string ,start ,end))
     ,@body))

(cl:defmacro WITH-OUTPUT-TO-STRING ((var &optional string &key element-type)
				    &body body)
  (when (null element-type)
    (setq element-type '(QUOTE CHARACTER)))
  (if string
      `(WITH-OPEN-STREAM (,var (make-fill-pointer-output-stream ,string))
	 ,@body)
      `(WITH-OPEN-STREAM (,var (MAKE-STRING-OUTPUT-STREAM
				,(kw ELEMENT-TYPE) ,element-type))
	 ,@body
	 (GET-OUTPUT-STREAM-STRING ,var))))

;;; STREAM-ERROR-STREAM defined by defstruct.


(defun make-buffer-output-stream (buffer)
  (MAKE-STREAM (kw content) buffer
	       (kw index) 0
	       (kw read-fn) (lambda (s) (error "read from output stream"))
	       (kw write-fn) (lambda (char stream)
			       (insert char)
			       (when (eq char 10)
				 (sit-for 0)))))

(defun make-read-char-exclusive-input-stream ()
  (MAKE-STREAM (kw content) nil
	       (kw index) 0
	       (kw read-fn) (lambda (s) (read-char-exclusive))
	       (kw write-fn) (lambda (c s) (error "write to input stream"))))

(defun make-fill-pointer-output-stream (string)
  (MAKE-STREAM (kw content) string
	       (kw index) 0
	       (kw read-fn) (lambda (s) (error "read from output stream"))
	       (kw write-fn) (lambda (char stream)
			       (VECTOR-PUSH-EXTEND
				(CODE-CHAR char)
				(STREAM-content stream)))))
