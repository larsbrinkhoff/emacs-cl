;;;; -*- elisp -*-

(require 'cl)

(defstruct (stream (:predicate streamp) (:copier nil))
  content
  index
  read-fn
  write-fn)

(defun* peek-char (&optional peek-type stream (eof-error-p t)
			     eof-value recursive-p)
  (loop
   (let ((char (cl:read-char stream eof-error-p eof-value recursive-p)))
     (cond
       ((eq char eof-value)
	(return-from peek-char eof-value))
       ((or (eq peek-type nil)
	    (and (eq peek-type t) (not (whitespacep char)))
	    (char= char peek-type))
	(unread-char char stream)
	(return-from peek-char char))))))

(defun* cl:read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (let ((ch (funcall (stream-read-fn stream) stream)))
    (if (eq ch :eof)
	(if eof-error-p
	    (error "end of file")
	    eof-value)
	ch)))

(defun unread-char (char &optional stream)
  (when (> (stream-index stream) 0)
    (decf (stream-index stream))))

(defun cl:write-char (char &optional stream)
  (funcall (stream-write-fn stream) char stream)
  char)

(defun* read-line (&optional stream (eof-error-p t) eof-value recursive-p)
  (let ((line ""))
    (loop
     (let ((char (cl:read-char stream eof-error-p eof-value recursive-p)))
       (cond
	 ((eq char eof-value)
	  (return-from read-line
	    (values (if (= (length line) 0) eof-value line) t)))
	 ((= char 10)
	  (return-from read-line (values line nil))))
       (setq line (concat line (list char)))))))

(defun* write-string (string &optional stream &key (start 0) end)
  (message "length %d" (length string))
  (do ((i 0 (1+ i)))
      ((= i (length string)) string)
    (cl:write-char (aref string i) stream)))

(defun* write-line (string &optional stream &key (start 0) end)
  (write-string string stream :start start :end end)
  (cl:write-char 10 stream)
  string)

(defun file-position (stream)
  (stream-index stream))

(defun get-output-stream-string (stream)
  (stream-content stream))

(defun* make-string-input-stream (string &optional (start 0) end)
  (make-stream :content (let ((substr (substring string start end)))
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
			 (concat (stream-content stream) (list char))))))
