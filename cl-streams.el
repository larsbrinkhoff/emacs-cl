;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 21, Streams.

(defvar *STANDARD-INPUT* nil)

(defvar *STANDARD-OUTPUT* nil)

(defvar *TERMINAL-IO*)

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
	    (CHAR= char peek-type))
	(UNREAD-CHAR char stream)
	(return-from PEEK-CHAR char))))))

(defun resolve-input-stream-designator (designator)
  (case designator
    ((nil)	*STANDARD-INPUT*)
    ((t)	*TERMINAL-IO*)
    (t		designator)))

(defun resolve-output-stream-designator (designator)
  (case designator
    ((nil)	*STANDARD-OUTPUT*)
    ((t)	*TERMINAL-IO*)
    (t		designator)))

(defun* READ-CHAR (&optional stream-designator (eof-error-p T)
			     eof-value recursive-p)
  (let* ((stream (resolve-input-stream-designator stream-designator))
	 (ch (funcall (STREAM-read-fn stream) stream)))
    (if (eq ch :eof)
	(if eof-error-p
	    (error "end of file")
	    eof-value)
	(CODE-CHAR ch))))

(defun UNREAD-CHAR (char &optional stream-designator)
  (let ((stream (resolve-input-stream-designator stream-designator)))
    (when (> (STREAM-index stream) 0)
      (decf (STREAM-index stream)))))

(defun WRITE-CHAR (char &optional stream-designator)
  (let ((stream (resolve-output-stream-designator stream-designator)))
    (funcall (STREAM-write-fn stream) char stream)
    char))

(defun* READ-LINE (&optional stream-designator (eof-error-p T)
			     eof-value recursive-p)
  (let ((stream (resolve-input-stream-designator stream-designator))
	(line ""))
    (loop
     (let ((char (READ-CHAR stream eof-error-p eof-value recursive-p)))
       (cond
	 ((eq char eof-value)
	  (return-from READ-LINE
	    (values (if (= (length line) 0) eof-value line) t)))
	 ((= char (CODE-CHAR 10))
	  (return-from READ-LINE (values line nil))))
       (setq line (concat line (list (CHAR-CODE char))))))))

(defun* WRITE-STRING (string &optional stream-designator &key (start 0) end)
  (do ((stream (resolve-output-stream-designator stream-designator))
       (i 0 (1+ i)))
      ((= i (length string)) string)
    (WRITE-CHAR (aref string i) stream)))

(defun* WRITE-LINE (string &optional stream-designator &key (start 0) end)
  (let ((stream (resolve-output-stream-designator stream-designator)))
    (WRITE-STRING string stream :start start :end end)
    (WRITE-CHAR 10 stream)
    string))

;;; TODO: read-sequence

;;; TODO: write-sequence

;;; TODO: file-length

(defun FILE-POSITION (stream)
  (STREAM-index stream))

(defun* OPEN (filespec &key (direction :input) (element-type 'character)
		            if-exists if-does-not-exist
			    (external-format :default))
  (MAKE-STREAM :filename (when (eq direction :output) filespec)
	       :content (let ((buffer (create-file-buffer filespec)))
			  (when (eq direction :input)
			    (save-current-buffer
			      (set-buffer buffer)
			      (insert-file-contents-literally filespec)))
			  buffer)
	       :index 0
	       :read-fn (lambda (stream)
			  (save-current-buffer
			    (set-buffer (STREAM-content stream))
			    (if (= (STREAM-index stream) (buffer-size))
				:eof
				(char-after (incf (STREAM-index stream))))))
	       :write-fn (lambda (char stream)
			   (save-current-buffer
			     (set-buffer (STREAM-content stream))
			     (goto-char (incf (STREAM-index stream)))
			     (insert char)))))

;;; TODO: stream-external-format

(defmacro* WITH-OPEN-FILE ((stream filespec &rest options) &body body)
  `(WITH-OPEN-STREAM (,stream (OPEN ,filespec ,@options))
     ,@body))

(defun* CLOSE (stream &key abort)
  (when (STREAM-filename stream)
    (save-current-buffer
      (set-buffer (STREAM-content stream))
      (write-region 1 (1+ (buffer-size)) (STREAM-filename stream))))
  (when (bufferp (STREAM-content stream))
    (kill-buffer (STREAM-content stream)))
  t)

(defmacro* WITH-OPEN-STREAM ((var stream) &body body)
  `(let ((,var ,stream))
    (unwind-protect
	 (progn ,@body)
      (CLOSE ,var))))

;;; TODO: listen

;;; TODO: clear-input

;;; TODO: finish-output, force-output, clear-output

;;; TODO: y-or-n-p, yes-or-no-p

;;; TODO: make-synonym-stream

;;; TODO: synonym-stream-symbol

;;; TODO: broadcast-stream-streams

;;; TODO: make-broadcast-stream

;;; TODO: make-two-way-stream

;;; TODO: two-way-stream-input-stream, two-way-stream-output-stream

;;; TODO: echo-stream-input-stream, echo-stream-output-stream

;;; TODO: make-echo-stream

;;; TODO: concatenated-stream-streams

;;; TODO: make-concatenated-stream

(defun GET-OUTPUT-STREAM-STRING (stream)
  (STREAM-content stream))

(defun* MAKE-STRING-INPUT-STREAM (string &optional (start 0) end)
  (MAKE-STREAM :content (let ((substr (substring string start end)))
			  (if (> (length substr) 0)
			      substr
			      :eof))
	       :index 0
	       :read-fn (lambda (stream)
			  (cond
			    ((eq (STREAM-content stream) :eof)
			     :eof)
			    ((= (STREAM-index stream)
				(length (STREAM-content stream)))
			     (setf (STREAM-content stream) :eof))
			    (t
			     (aref (STREAM-content stream)
				   (1- (incf (STREAM-index stream)))))))
	       :write-fn (lambda (c s) (error "write to input stream"))))

(defun* MAKE-STRING-OUTPUT-STREAM (&key element-type)
  (MAKE-STREAM :content ""
	       :index 0
	       :read-fn (lambda (s) (error "read from output stream"))
	       :write-fn
	         (lambda (char stream)
		   (setf (STREAM-content stream)
			 (concat (STREAM-content stream)
				 (list (CHAR-CODE char)))))))

(defmacro* WITH-INPUT-FROM-STRING ((var string &key index (start 0) end)
				   &body body)
  `(WITH-OPEN-STREAM (,var (MAKE-STRING-INPUT-STREAM ,string ,start ,end))
     ,@body))

;;; TODO: with-output-to-string (needs strings with fill pointers)
;;; (which we now have)

;;; TODO: stream-error-stream
