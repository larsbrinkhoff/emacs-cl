;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements the FORMAT function from chapter 22, Printer.

(IN-PACKAGE "EMACS-CL")

;;; Some tests:
;;; (FORMAT nil "~D~?~D" 1 "~X" '(10) 3)
;;; (FORMAT nil "foo~[1~;2~:;3~]bar" -1)
;;; (FORMAT nil "foo~@[~D~]bar~D" 1 2)
;;; (FORMAT nil "foo~:[1~;2~]bar" t)
;;; (FORMAT nil "foo~{ ~D~} bar" '(1 2 3))
;;; (FORMAT nil "foo~@{ ~D~} bar" 1 2 3)
;;; (FORMAT nil "foo~:{ ~D~} bar" '((1 4) (2) (3)))
;;; (FORMAT nil "foo~:@{ ~D~} bar" '(1) '(2) '(3))

(defvar *format-directives* (make-hash-table :test #'eq))

(defmacro* define-format-directive (char lambda-list &body body)
  `(setf (gethash ,char *format-directives*)
         (cl:lambda ,lambda-list ,@body)))

(DEFSTRUCT (format-state (:conc-name nil)
			 (:predicate nil)
			 (:copier nil)
			 (:constructor make-format-state (format-string
							  format-args)))
  format-args
  format-string
  (arg-index 0)
  (format-index 0)
  conditional-arg
  conditional-index
  iteration-kind
  iteration-index
  (conditional-printing-p t)
  (iterative-printing-p t)
  (nesting-stack nil))

(defun printing-p (state)
  (and (conditional-printing-p state)
       (iterative-printing-p state)))

(defun next-arg (state)
  (when (printing-p state)
    (prog1
	(nth (arg-index state) (format-args state))
      (incf (arg-index state)))))

(defun next-char (state)
  (prog1
      (CHAR (format-string state) (format-index state))
    (incf (format-index state))))

(defun push-nesting (state type &optional option)
  (ecase type
    (:conditional
     (push (conditional-arg state) (nesting-stack state))
     (push (conditional-index state) (nesting-stack state))
     (push (conditional-printing-p state) (nesting-stack state)))
    (:iteration
     (unless option
       (push (format-args state) (nesting-stack state))
       (push (arg-index state) (nesting-stack state)))
     (push (iterative-printing-p state) (nesting-stack state))
     (push (iteration-kind state) (nesting-stack state))
     (push (iteration-index state) (nesting-stack state)))))

(defun pop-nesting (state type &optional option)
  (ecase type
    (:conditional
     (setf (conditional-printing-p state) (pop (nesting-stack state)))
     (setf (conditional-index state) (pop (nesting-stack state)))
     (setf (conditional-arg state) (pop (nesting-stack state))))
    (:iteration
     (setf (iteration-index state) (pop (nesting-stack state)))
     (setf (iteration-kind state) (pop (nesting-stack state)))
     (setf (iterative-printing-p state) (pop (nesting-stack state)))
     (unless option
       (setf (arg-index state) (pop (nesting-stack state)))
       (setf (format-args state) (pop (nesting-stack state)))))))



;;; Basic Output ----------------------------------------

(defun printing-char-p (char)
  (and (not (ch= char 32)) (GRAPHIC-CHAR-P char)))

(define-format-directive ?C (stream state atp colonp)
  (when (printing-p state)
    (let ((char (next-arg state)))
      (cond
	(colonp
	 (if (printing-char-p char)
	     (WRITE-CHAR char stream)
	     (WRITE-STRING (CHAR-NAME char) stream)))
	(atp
	 (WRITE char (kw STREAM) stream (kw READABLY) t))
	(t
	 (WRITE-CHAR char stream))))))

(define-format-directive ?% (stream state atp colonp &optional (n 1))
  (when (printing-p state)
    (dotimes (i n)
      (TERPRI stream))))

(define-format-directive ?& (stream state atp colonp &optional (n 1))
  (when (printing-p state)
    (when (plusp n)
      (FRESH-LINE stream)
      (dotimes (i (1- n))
	(TERPRI stream)))))

(define-format-directive ?| (stream state atp colonp &optional (n 1))
  (when (printing-p state)
    (dotimes (i n)
      (WRITE-CHAR (ch 12) stream))))

(define-format-directive ?~ (stream state atp colonp &optional (n 1))
  (when (printing-p state)
    (dotimes (i n)
      (WRITE-CHAR (ch 126) stream))))

;;; Radix Control ----------------------------------------

(define-format-directive ?R (stream state atp colonp &optional radix mincol
				    padchar commachar comma-interval)
  (when (printing-p state)
    (let ((num (next-arg state)))
      (if (null radix)
	  nil ;TODO
	  (WRITE num (kw STREAM) stream (kw BASE) radix (kw RADIX) nil)))))

(defun print-in-radix (num stream atp radix)
  (when (and atp (not (ZEROP num)))
    (WRITE-CHAR (ch 43) stream))
  (WRITE num (kw STREAM) stream (kw ESCAPE) nil (kw RADIX) nil
	     (kw BASE) radix (kw READABLY) nil))
      
(define-format-directive ?D (stream state atp colonp &optional mincol padchar
				    commachar comma-interval)
  (when (printing-p state)
    (print-in-radix (next-arg state) stream atp 10)))

(define-format-directive ?B (stream state atp colonp &optional mincol padchar
				    commachar comma-interval)
  (when (printing-p state)
    (print-in-radix (next-arg state) stream atp 2)))

(define-format-directive ?O (stream state atp colonp &optional mincol padchar
				    commachar comma-interval)
  (when (printing-p state)
    (print-in-radix (next-arg state) stream atp 8)))

(define-format-directive ?X (stream state atp colonp &optional mincol padchar
				    commachar comma-interval)
  (when (printing-p state)
    (print-in-radix (next-arg state) stream atp 16)))

;;; Floating Point Printers ----------------------------------------

;;; TODO: ~F
;;; TODO: ~E
;;; TODO: ~G
;;; TODO: ~$

;;; Printer Operations ----------------------------------------

(define-format-directive ?A (stream state atp colonp
			     &optional mincol colinc minpad padchar)
  (when (printing-p state)
    (PRINC (next-arg state) stream)))

(define-format-directive ?S (stream state atp colonp
			     &optional mincol colinc minpad padchar)
  (when (printing-p state)
    (PRIN1 (next-arg state) stream)))

(define-format-directive ?W (stream state atp colonp)
  (when (printing-p state)
    (let ((*PRINT-PRETTY* colonp)
	  (*PRINT-LEVEL* (if atp nil *PRINT-LEVEL*))
	  (*PRINT-LENGTH* (if atp nil *PRINT-LENGTH*)))
      (WRITE (next-arg state) (kw STREAM) stream))))

;;; Pretty Printer Operations ----------------------------------------

(define-format-directive ?_ (stream state atp colonp &rest args)
  (when (printing-p state)
    (PPRINT-NEWLINE (if atp
			(if colonp (kw MANDATORY) (kw MISER))
			(if colonp (kw FILL) (kw LINEAR)))
		    stream)))

;;; TODO: ~<

(define-format-directive ?I (stream state atp colonp &optional (n 0))
  (PPRINT-INDENT (if colonp (kw CURRENT) (kw BLOCK)) n))

(define-format-directive ?/ (stream state atp colonp &rest args)
  (when (printing-p state)
    (let ((name "")
	  (package *cl-user-package*)
	  char)
      (while (not (ch= (setq char (next-char state)) 47))
	(when (and (ch= char 58) (eq package *cl-user-package*))
	  (setq package name)
	  (setq name "")
	  (when (ch= (setq char (next-char state)) 58)
	    (setq char (next-char state))))
	(setq name (concat name (string (char-upcase-code char)))))
      (MULTIPLE-VALUE-BIND (symbol found) (FIND-SYMBOL name package)
	(APPLY (symbol-function symbol)
	       stream (next-arg state) colonp atp args)))))

;;; Layout Control ----------------------------------------

;;; TODO: ~T
;;; TODO: ~<
;;; TODO: ~>

;;; Control-Flow Operations ----------------------------------------

(define-format-directive ?* (stream state atp colonp &optional n)
  (when (printing-p state)
    (cond
      (atp	(setf (arg-index state) (or n 0)))
      (colonp	(decf (arg-index state) (or n 1)))
      (t	(incf (arg-index state) (or n 1))))))

(defun check-condition (state &optional colonp)
  (setf (conditional-printing-p state)
	(or (and colonp (not (null (conditional-arg state))))
	    (eq (conditional-arg state) (conditional-index state))))
  (when (conditional-printing-p state)
    (setf (conditional-arg state) nil)))

(define-format-directive 91 (stream state atp colonp &optional n)     ; ~[
  (push-nesting state :conditional)
  (cond
    (colonp
     (setf (conditional-arg state) (if (next-arg state) 1 0))
     (setf (conditional-index state) 0)
     (check-condition state))
    (atp
     (when (setf (conditional-printing-p state) (next-arg state))
       (decf (arg-index state))))
    (t
     (setf (conditional-arg state) (or n (next-arg state)))
     (setf (conditional-index state) 0)
     (check-condition state))))

(define-format-directive 93 (stream state atp colonp)		      ; ~]
  (pop-nesting state :conditional))

(define-format-directive ?{ (stream state atp colonp &optional n)
  (if atp
      (progn
	(push-nesting state :iteration :at)
	(setf (iteration-kind state) :at))
      (let ((arg (next-arg state)))
	(push-nesting state :iteration)
	(setf (format-args state) arg)
	(setf (iteration-kind state) nil)
	(setf (arg-index state) 0)))
  (setf (iteration-index state) (format-index state))
  (when colonp
    (let ((arg (next-arg state)))
      (push-nesting state :iteration)
      (setf (format-args state) arg))
    (setf (arg-index state) 0)
    (setf (iteration-kind state) :colon)))

(define-format-directive ?} (stream state atp colonp &optional n)
  (cond
    ((eq (iteration-kind state) :colon)
     (pop-nesting state :iteration)
     (if (>= (arg-index state) (length (format-args state)))
	 (pop-nesting state :iteration (iteration-kind state))
	 (let ((arg (next-arg state)))
	   (setf (format-index state) (iteration-index state))
	   (push-nesting state :iteration)
	   (setf (format-args state) arg)
	   (setf (arg-index state) 0)
	   (setf (iteration-kind state) :colon))))
    ((>= (arg-index state) (length (format-args state)))
     (pop-nesting state :iteration (iteration-kind state)))
    (t
     (setf (format-index state) (iteration-index state)))))

(define-format-directive ?? (stream state atp colonp)
  (when (printing-p state)
    (let* ((format (next-arg state))
	   (args (next-arg state)))
      (apply #'FORMAT stream format args))))

;;; Miscellaneous Operations ----------------------------------------

;;; TODO: ~(

;;; TODO: ~)

(define-format-directive ?P (stream state atp colonp)
  (when (printing-p state)
    (when colonp
      (decf (arg-index state)))
    (let ((pluralp (not (eq (next-arg state) 1))))
      (if atp
	  (WRITE-STRING (if pluralp "ies" "y") stream)
	  (when pluralp
	    (WRITE-CHAR (ch 115) stream))))))

;;; Miscellaneous Pseudo-Operations ----------------------------------------

(define-format-directive 59 (stream state atp colonp)		      ; ~;
  (incf (conditional-index state))
  (check-condition state colonp))

(define-format-directive ?^ (stream state atp colonp &optional n1 n2 n3)
  (when (and (printing-p state)
	     (cond
	       ((null n1)	(>= (arg-index state)
				    (length (format-args state))))
	       ((null n2)	(zerop n1))
	       ((null n3)	(eq n1 n2))
	       (t		(cl:<= n1 n2 n3))))
    (if (iteration-index state)
	(progn
	  (setf (iterative-printing-p state) nil)
	  (when colonp
	    (setf (arg-index state) (length (format-args state)))))
	(setf (format-index state) (LENGTH (format-string state))))))

(define-format-directive 10 (stream state atp colonp)
  (when (printing-p state)
    (when atp
      (TERPRI stream))
    (when (not colonp)
      (let ((char (CHARACTER " ")))
	(while (and (< (format-index state) (LENGTH (format-string state)))
		    (progn
		      (setq char (next-char state))
		      (whitespacep char))))
	(unless (whitespacep char)
	  (decf (format-index state)))))))



(cl:defmacro FORMATTER (format)
  (unless (STRINGP format)
    (type-error format 'STRING))
  ;; TODO: better implementation
  (let ((env (augment-environment nil :variable '(format))))
    (setf (lexical-value 'format env) format)
    (enclose '(LAMBDA (*STANDARD-OUTPUT* &REST args)
	        (APPLY (FUNCTION FORMAT) T format args)
	        nil)
	     env)))

(defun FORMAT (stream-designator format &rest args)
  (let ((stream (or (and (eq stream-designator 'T) *STANDARD-OUTPUT*)
		    stream-designator
		    (MAKE-STRING-OUTPUT-STREAM)))
	(i 0))
    (if (FUNCTIONP format)
	(APPLY format stream args)
	(let ((state (make-format-state format args)))
	  (while (< (format-index state) (LENGTH format))
	    (let ((char (next-char state)))
	      (cond
		((ch= char 126)
		 (let ((colonp nil) (atp nil))
		   (while (FIND (setq char (next-char state)) ":@")
		     (cond
		       ((ch= char ?:)	(setq colonp t))
		       ((ch= char ?@)	(setq atp t))))
		   (let ((fn (gethash (char-upcase-code char)
				      *format-directives*)))
		     (apply fn stream state atp colonp nil))))
		((printing-p state)
		 (WRITE-CHAR char stream)))))))
    (if stream-designator
	nil
	(GET-OUTPUT-STREAM-STRING stream))))
