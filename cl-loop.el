;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements the LOOP macro from chapter 6, Iteration.

(IN-PACKAGE "EMACS-CL")

(cl:defmacro LOOP (&rest forms)
  (if (every #'consp forms)
      `(DO () (nil) ,@forms)
      (expand-extended-loop forms)))

(defstruct (loop-state
	     (:conc-name nil)
	     (:constructor make-loop-state (loop-forms)))
  loop-forms
  loop-name
  loop-bindings
  loop-collect
  loop-prologue
  loop-tests
  loop-setters
  loop-body
  loop-steps
  loop-epilogue
  loop-result)

(defun copy-loop-state (state)
  (let ((s (make-loop-state (loop-forms state))))
    (setf (loop-collect s) (loop-collect state))
    s))

(defun merge-loop-states (s1 s2)
  (setf (loop-forms s1)    (loop-forms s2))
  (when (loop-name s2)     (setf (loop-name s1) (loop-name s2)))
  (setf (loop-bindings s1) (append (loop-bindings s2) (loop-bindings s1)))
  (when (loop-collect s2)  (setf (loop-collect s1) (loop-collect s2)))
  (setf (loop-prologue s1) (append (loop-prologue s2) (loop-prologue s1)))
  (setf (loop-tests s1)    (append (loop-tests s2) (loop-tests s1)))
  (setf (loop-setters s1)  (append (loop-setters s2) (loop-setters s1)))
  (setf (loop-steps s1)    (append (loop-steps s2) (loop-steps s1)))
  (setf (loop-epilogue s1) (append (loop-epilogue s2) (loop-epilogue s1)))
  (when (loop-result s2)   (setf (loop-result s1) (loop-result s2)))
  s1)

(defun parse-loop-clause (state)
  (let ((k (symbol-name (pop (loop-forms state)))))
    (cond
      ((string= k "NAMED")
       (setq (loop-name state) (pop (loop-forms state))))

      ((string= k "WITH")
       (let ((var (pop (loop-forms state))))
	 (push
	  `((,var
	     ,(when (string= (symbol-name (first (loop-forms state))) "=")
	        (pop (loop-forms state))
		(pop (loop-forms state)))))
	  (loop-bindings state))))

      ((or (string= k "FOR") (string= k "AS"))
       (let* ((var (pop (loop-forms state)))
	      (k (symbol-name (pop (loop-forms state)))))
	 (cond
	   ((string= k "IN")
	    (let ((list (gensym)))
	      (push `((,list ,(pop (loop-forms state))) ,var)
		    (loop-bindings state))
	      (push `(ENDP ,list) (loop-tests state))
	      (push `(SETQ ,var (CAR ,list)) (loop-setters state))
	      (push `(SETQ ,list (CDR ,list)) (loop-steps state))))

	   ((string= k "BEING")
	    (setq k (symbol-name (pop (loop-forms state))))
	    (when (or (string= k "THE") (string= k "EACH"))
	      (setq k (symbol-name (pop (loop-forms state)))))
	    ;; Discard IN or OF.
	    (pop (loop-forms state))
	    (let ((list (gensym)))
	      (push `((,list (package-symbols
			      (FIND-PACKAGE ,(pop (loop-forms state)))
			      ,(cond
				((or (string= k "SYMBOL")
				     (string= k "SYMBOLS"))
				 `(QUOTE (,(kw EXTERNAL)
					  ,(kw INTERNAL)
					  ,(kw INHERITED))))
				((or (string= k "PRESENT-SYMBOL")
				     (string= k "PRESENT-SYMBOLS"))
				 `(QUOTE (,(kw EXTERNAL)
					  ,(kw INTERNAL))))
				((or (string= k "EXTERNAL-SYMBOL")
				     (string= k "EXTERNAL-SYMBOLS"))
				 `(QUOTE (,(kw EXTERNAL)))))))
		      ,var)
		    (loop-bindings state))
	      (push `(NULL ,list) (loop-tests state))
	      (push `(SETQ ,var (CAR ,list)) (loop-setters state))
	      (push `(SETQ ,list (CDR ,list)) (loop-steps state)))))))

      ((or (string= k "COLLECT") (string= k "COLLECTING"))
       (let ((form (pop (loop-forms state))))
	 (unless (loop-collect state)
	   (setf (loop-collect state) (gensym)))
	 (push `((,(loop-collect state) nil)) (loop-bindings state))
	 (push `(PUSH ,form ,(loop-collect state)) (loop-body state))
	 (setf (loop-result state) `(NREVERSE ,(loop-collect state)))))

      ((or (string= k "IF") (string= k "WHEN") (string= k "UNLESS"))
       (let ((condition (pop (loop-forms state)))
	     (then nil)
	     (else nil))
	 (when (string= k "UNLESS")
	   (setq condition `(NOT ,condition)))
	 (let ((s (parse-loop-clause (copy-loop-state state))))
	   (setq then (loop-body s))
	   (setf (loop-body s) nil)
	   (setq state (merge-loop-states state s)))
	 (when (string= (symbol-name (first (loop-forms state))) "ELSE")
	   (pop (loop-forms state))
	   (let ((s (parse-loop-clause (copy-loop-state state))))
	     (setq else (loop-body s))
	     (setf (loop-body s) nil)
	     (setq state (merge-loop-states state s))))
	 (push `(IF ,condition
		    (PROGN ,@then)
		    ,(when else `(PROGN ,@else)))
	       (loop-body state))))

      ((or (string= k "DO") (string= k "DOING"))
       (while (consp (first (loop-forms state)))
	 (push (pop (loop-forms state)) (loop-body state)))))

    state))

; (loop for a in x
;       for b in y
;       with c = t
;       if a
;         collect (funcall (if c #'char-upcase #'char-downcase)
; 			 (elt (elt l a) b))
;       else
;         collect #\space
;       if c
;         do (setq c ()))

; (loop for foo being the external-symbols in :cl collect (string-upcase foo))

; (loop for x being the external-symbols in :cl collect (string x))

(defun expand-extended-loop (forms)
  (let ((state (make-loop-state forms))
	(start (gensym))
	(end (gensym)))
    (do ()
	((null (loop-forms state)))
      (setq state (parse-loop-clause state)))
    `(BLOCK ,(loop-name state)
      ,(expand-bindings (nreverse (loop-bindings state))
        `(PROGN
	  ,@(nreverse (loop-prologue state))
	  (CATCH (QUOTE ,end)
	    (MACROLET ((LOOP-FINISH () (QUOTE (THROW (QUOTE ,end) nil))))
	      (TAGBODY
		 ,start
		 (WHEN (OR ,@(loop-tests state)) (LOOP-FINISH))
		 ,@(nreverse (loop-setters state))
		 ,@(nreverse (loop-body state))
		 ,@(nreverse (loop-steps state))
		 (GO ,start))))
	  ,@(nreverse (loop-epilogue state))
	  ,(loop-result state))))))

(defun expand-bindings (bindings body)
  (if (null bindings)
      body
      `(LET ,(first bindings) ,(expand-bindings (rest bindings) body))))
