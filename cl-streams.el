;;;; -*- emacs-lisp -*-
;;;;
;;;; Copyright (C) 2003 Lars Brinkhoff.
;;;;
;;;; This file implements operators in chapter 21, Streams.

(defvar *STANDARD-INPUT* nil)

(defvar *STANDARD-OUTPUT* nil)

(defvar *TERMINAL-IO*)

(defstruct (stream (:predicate streamp) (:copier nil))
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
       ((eq char eof-value)
	(return-from PEEK-CHAR eof-value))
       ((or (eq peek-type nil)
	    (and (eq peek-type t) (not (whitespacep char)))
	    (char= char peek-type))
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
	 (ch (funcall (stream-read-fn stream) stream)))
    (if (eq ch :eof)
	(IF eof-error-p
	    (error "end of file")
	    eof-value)
	(CODE-CHAR ch))))

(defun UNREAD-CHAR (char &optional stream-designator)
  (let ((stream (resolve-input-stream-designator stream-designator)))
    (when (> (stream-index stream) 0)
      (decf (stream-index stream)))))

(defun WRITE-CHAR (char &optional stream-designator)
  (let ((stream (resolve-output-stream-designator stream-designator)))
    (funcall (stream-write-fn stream) char stream)
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

(defun file-position (stream)
  (stream-index stream))

(defun* open (filespec &key (direction :input) (element-type 'character)
		            if-exists if-does-not-exist
			    (external-format :default))
  (make-stream :filename (when (eq direction :output) filespec)
	       :content (let ((buffer (create-file-buffer filespec)))
			  (when (eq direction :input)
			    (save-current-buffer
			      (set-buffer buffer)
			      (insert-file-contents-literally filespec)))
			  buffer)
	       :index 0
	       :read-fn (lambda (stream)
			  (save-current-buffer
			    (set-buffer (stream-content stream))
			    (if (= (stream-index stream) (buffer-size))
				:eof
				(char-after (incf (stream-index stream))))))
	       :write-fn (lambda (char stream)
			   (save-current-buffer
			     (set-buffer (stream-content stream))
			     (goto-char (incf (stream-index stream)))
			     (insert char)))))

;;; TODO: stream-external-format

(defmacro* with-open-file ((stream filespec &rest options) &body body)
  `(with-open-stream (,stream (open ,filespec ,@options))
     ,@body))

(defun* close (stream &key abort)
  (when (stream-filename stream)
    (save-current-buffer
      (set-buffer (stream-content stream))
      (write-region 1 (1+ (buffer-size)) (stream-filename stream))))
  (when (bufferp (stream-content stream))
    (kill-buffer (stream-content stream)))
  t)

(defmacro* with-open-stream ((var stream) &body body)
  `(let ((,var ,stream))
    (unwind-protect
	 (progn ,@body)
      (close ,var))))

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

(defun get-output-stream-string (stream)
  (stream-content stream))

(defun* make-string-input-stream (string &optional (start 0) (end NIL))
  (make-stream :content (let ((substr (substring string start (OR end nil))))
			  (if (> (length substr) 0)
			      substr
			      :eof))
	       :index 0
	       :read-fn (lambda (stream)
			  (cond
			    ((eq (stream-content stream) :eof)
			     :eof)
			    ((= (stream-index stream)
				(length (stream-content stream)))
			     (setf (stream-content stream) :eof))
			    (t
			     (aref (stream-content stream)
				   (1- (incf (stream-index stream)))))))
	       :write-fn (lambda (c s) (error "write to input stream"))))

(defun* make-string-output-stream (&key element-type)
  (make-stream :content ""
	       :index 0
	       :read-fn (lambda (s) (error "read from output stream"))
	       :write-fn
	         (lambda (char stream)
		   (setf (stream-content stream)
			 (concat (stream-content stream)
				 (list (CHAR-CODE char)))))))

(defmacro* with-input-from-string ((var string &key index (start 0) end)
				   &body body)
  `(with-open-stream (,var (make-string-input-stream ,string ,start ,end))
     ,@body))

;;; TODO: with-output-to-string (needs strings with fill pointers)

;;; TODO: stream-error-stream
