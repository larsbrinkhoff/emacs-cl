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
	     (:constructor make-loop-state
			   (loop-forms &optional loop-accumulator)))
  loop-forms
  loop-name
  loop-bindings
  loop-accumulator
  loop-prologue
  loop-tests
  loop-setters
  loop-body
  loop-steps
  loop-epilogue
  loop-result)

(defun copy-loop-state (state)
  (let ((s (make-loop-state (loop-forms state))))
    (setf (loop-accumulator s) (loop-accumulator state))
    s))

(defun merge-loop-states (s1 s2)
  (setf (loop-forms s1)    (loop-forms s2))
  (when (loop-name s2)     (setf (loop-name s1) (loop-name s2)))
  (setf (loop-bindings s1) (append (loop-bindings s2) (loop-bindings s1)))
  (when (loop-accumulator s2)
    (setf (loop-accumulator s1) (loop-accumulator s2)))
  (setf (loop-prologue s1) (append (loop-prologue s2) (loop-prologue s1)))
  (setf (loop-tests s1)    (append (loop-tests s2) (loop-tests s1)))
  (setf (loop-setters s1)  (append (loop-setters s2) (loop-setters s1)))
  (setf (loop-steps s1)    (append (loop-steps s2) (loop-steps s1)))
  (setf (loop-epilogue s1) (append (loop-epilogue s2) (loop-epilogue s1)))
  (when (loop-result s2)   (setf (loop-result s1) (loop-result s2)))
  s1)

(defmacro merge-loop-state (state)
  `(progn
    (setq forms (loop-forms ,state))
    (when (loop-name ,state)
      (setq name (loop-name ,state)))
    (setq bindings
     (append (loop-bindings ,state) bindings))
    (when (loop-accumulator ,state)
      (setq accumulator (loop-accumulator ,state)))
    (setq prologue
     (append (loop-prologue ,state) prologue))
    (setq tests
     (append (loop-tests ,state) tests))
    (setq setters
     (append (loop-setters ,state) setters))
    (setq steps
     (append (loop-steps ,state) steps))
    (setq epilogue
     (append (loop-epilogue ,state) epilogue))
    (when (loop-result ,state)
      (setq result (loop-result ,state)))))


(defvar *loop-clause-handlers* (make-hash-table :test #'equal))

(defmacro* define-loop-clause (names () &body body)
  (with-gensyms (state)
    `(dolist (name ',(ensure-list names))
      (setf (gethash name *loop-clause-handlers*)
            (lambda (,state)
	      (let ((forms		(loop-forms ,state))
		    (name		(loop-name ,state))
		    (bindings		(loop-bindings ,state))
		    (accumulator	(loop-accumulator ,state))
		    (prologue		(loop-prologue ,state))
		    (tests		(loop-tests ,state))
		    (setters		(loop-setters ,state))
		    (body		(loop-body ,state))
		    (steps		(loop-steps ,state))
		    (epilogue		(loop-epilogue ,state))
		    (result		(loop-result ,state)))
		,@body
		(setf (loop-forms ,state) forms)
		(setf (loop-name ,state) name)
		(setf (loop-bindings ,state) bindings)
		(setf (loop-accumulator ,state) accumulator)
		(setf (loop-prologue ,state) prologue)
		(setf (loop-tests ,state) tests)
		(setf (loop-setters ,state) setters)
		(setf (loop-body ,state) body)
		(setf (loop-steps ,state) steps)
		(setf (loop-epilogue ,state) epilogue)
		(setf (loop-result ,state) result)
		,state))))))

(defmacro peek= (&rest names)
  `(member (symbol-name (first forms)) ',names))

(defmacro next= (&rest names)
  `(member (symbol-name (pop forms)) ',names))

(define-loop-clause "NAMED" ()
  (setq name (pop forms)))

(define-loop-clause "WITH" ()
  (let ((bs nil)
	(more t))
    (while more
      (let* ((var (pop forms))
	     (val (when (peek= "=")
		    (pop forms)
		    (pop forms))))
	(push `(,var ,val) bs))
      (setq more (prog1 (peek= "AND")
		   (pop forms))))
    (push bs bindings)))

(define-loop-clause ("FOR" "AS") ()
  (let* ((var (pop forms))
	 (k (symbol-name (pop forms))))
    (cond
      ((string= k "IN")
       (let ((list (gensym)))
	 (push `((,list ,(pop forms)) ,var) bindings)
	 (push `(ENDP ,list) tests)
	 (push `(SETQ ,var (CAR ,list)) setters)
	 (push `(SETQ ,list (CDR ,list)) steps)))

      ((string= k "ON")
       (push `((,var ,(pop forms))) bindings)
       (push `(ATOM ,var) tests)
       (push `(SETQ ,var (CDR ,var)) steps))

      ((string= k "=")
       (let* ((form1 (pop forms))
	      (form2 form1))
	 (push `((,var ,form1)) bindings)
	 (when (peek= "THEN")
	   (pop forms)
	   (setq form2 (pop forms)))
	 (push `(SETQ ,var ,form2) steps)))

      ((string= k "ACROSS")
       (with-gensyms (vector length)
	 (push `(,var (,index 0) (,vector (pop forms))) bindings)
	 (push `((,length (just-one (ARRAY-DIMENSIONS ,vector)))))
	 (push `(eq ,index ,length) tests)
	 (push `(SETQ ,var (AREF ,vector ,index)) setters)
	 (push `(INCF ,index) steps)))

      ((string= k "BEING")
       (setq k (symbol-name (pop forms)))
       (when (or (string= k "THE") (string= k "EACH"))
	 (setq k (symbol-name (pop forms))))
       ;; Discard IN or OF.
       (pop forms)
       (setq k (symbol-name (pop forms)))
       (let ((list (gensym)))
	 (push `((,list (package-symbols
			 (OR (FIND-PACKAGE ,(pop forms))
			     (ERROR (QUOTE PACKAGE-ERROR)))
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
			    `(QUOTE (,(kw EXTERNAL))))
			   (t
			    (ERROR "Invalid LOOP keyword: ~A" k)))))
		 ,var)
	       bindings)
	 (push `(NULL ,list) tests)
	 (push `(SETQ ,var (CAAR ,list)) setters)
	 (push `(SETQ ,list (CDR ,list)) steps))))))

(define-loop-clause ("COLLECT" "COLLECTING") ()
  (let ((form (pop forms)))
    (unless accumulator
      (setq accumulator (gensym)))
    (push `((,accumulator nil)) bindings)
    (push `(PUSH ,form ,accumulator) body)
    (setf result `(NREVERSE ,accumulator))))

(define-loop-clause ("APPEND" "APPENDING") ()
  (let ((form (pop forms)))
    (unless accumulator
      (setf accumulator (gensym)))
    (push `((,accumulator nil)) bindings)
    (push `(SETQ ,accumulator (APPEND ,accumulator ,form)) body)
    (setf result accumulator)))

(define-loop-clause ("NCONC" "NCONCING") ()
  (let ((form (pop forms)))
    (unless accumulator
      (setf accumulator (gensym)))
    (push `((,accumulator nil)) bindings)
    (push `(SETQ ,accumulator (NCONC ,accumulator ,form)) body)
    (setf result accumulator)))

(define-loop-clause ("COUNT" "COUNTING") ()
  (let ((form (pop forms)))
    (unless accumulator
      (setf accumulator (gensym)))
    (push `((,accumulator 0)) bindings)
    (push `(WHEN ,form (INCF ,accumulator)) body)
    (setf result accumulator)))

(define-loop-clause ("SUM" "SUMMING") ()
  (let ((form (pop forms)))
    (unless accumulator
      (setf accumulator (gensym)))
    (push `((,accumulator 0)) bindings)
    (push `(INCF ,accumulator ,form) body)
    (setf result accumulator)))

(define-loop-clause ("MAXIMIZE" "MAXIMIZING") ()
  (let ((form (pop forms))
	(val (gensym)))
    (unless accumulator
      (setf accumulator (gensym)))
    (push `((,accumulator nil)) bindings)
    (push `(SETQ ,accumulator (LET ((,val ,form))
				(IF ,accumulator
				    (MAX ,accumulator ,val)
				    ,val)))
	  body)
    (setf result accumulator)))

(define-loop-clause ("MINIMIZE" "MINIMIZING") ()
  (let ((form (pop forms))
	(val (gensym)))
    (unless accumulator
      (setf accumulator (gensym)))
    (push `((,accumulator nil)) bindings)
    (push `(SETQ ,accumulator (LET ((,val ,form))
				(IF ,accumulator
				    (MIN ,accumulator ,val)
				    ,val)))
	  body)
    (setf result accumulator)))

(define-loop-clause "WHILE" ()
  (push `(NOT ,(pop forms)) tests))

(define-loop-clause "UNTIL" ()
  (push (pop forms) tests))

(define-loop-clause ("IF" "WHEN") ()
  (let ((condition (pop forms))
	(then nil)
	(else nil))
   (let ((s (parse-loop-clause (make-loop-state forms accumulator))))
     (setq then (loop-body s))
     (setf (loop-body s) nil)
     (merge-loop-state s))
   (when (string= (symbol-name (first forms)) "ELSE")
     (pop forms)
     (let ((s (parse-loop-clause (make-loop-state forms accumulator))))
       (setq else (loop-body s))
       (setf (loop-body s) nil)
       (merge-loop-state s)))
   (push `(IF ,condition
	      (PROGN ,@then)
	      ,(when else `(PROGN ,@else)))
	 body)))

(define-loop-clause "UNLESS" ()
  (let ((condition `(NOT ,(pop forms)))
	(then nil)
	(else nil))
    (let ((s (parse-loop-clause (make-loop-state forms accumulator))))
      (setq then (loop-body s))
      (setf (loop-body s) nil)
      (merge-loop-state s))
    (when (string= (symbol-name (first forms)) "ELSE")
      (pop forms)
      (let ((s (parse-loop-clause (make-loop-state forms accumulator))))
	(setq else (loop-body s))
	(setf (loop-body s) nil)
	(merge-loop-state s)))
    (push `(IF ,condition
	       (PROGN ,@then)
	       ,(when else `(PROGN ,@else)))
	  body)))

(define-loop-clause ("DO" "DOING") ()
  (while (consp (first forms))
    (push (pop forms) body)))

(define-loop-clause "RETURN" ()
  (push `(RETURN-FROM ,name (pop forms))))

(define-loop-clause "INITIALLY" ()
  (while (consp (first forms))
    (push (pop forms) prologue)))

(define-loop-clause "FINALLY" ()
  (while (consp (first forms))
    (push (pop forms) epilogue)))

(defun parse-loop-clause (state)
  (let* ((k (symbol-name (pop (loop-forms state))))
	 (fn (gethash k *loop-clause-handlers*)))
    (funcall fn state)))

;     (cond
;       ((string= k "NAMED")
;        (setq (loop-name state) (pop (loop-forms state))))

;       ((string= k "WITH")
;        (let ((bindings nil)
; 	     (more t))
; 	 (while more
; 	   (let* ((var (pop (loop-forms state)))
; 		  (val (when (string= (symbol-name (first (loop-forms state)))
; 				      "=")
; 			 (pop (loop-forms state))
; 			 (pop (loop-forms state)))))
; 	     (push `(,var ,val) bindings))
; 	   (setq more
; 		 (prog1
; 		     (string= (symbol-name (first (loop-forms state))) "AND")
; 		   (pop (loop-forms state)))))))

;       ((or (string= k "FOR") (string= k "AS"))
;        (let* ((var (pop (loop-forms state)))
; 	      (k (symbol-name (pop (loop-forms state)))))
; 	 (cond
; 	   ((string= k "IN")
; 	    (let ((list (gensym)))
; 	      (push `((,list ,(pop (loop-forms state))) ,var)
; 		    (loop-bindings state))
; 	      (push `(ENDP ,list) (loop-tests state))
; 	      (push `(SETQ ,var (CAR ,list)) (loop-setters state))
; 	      (push `(SETQ ,list (CDR ,list)) (loop-steps state))))

; 	   ((string= k "ON")
; 	    (push `((,var ,(pop (loop-forms state)))) (loop-bindings state))
; 	    (push `(ATOM ,var) (loop-tests state))
; 	    (push `(SETQ ,var (CDR ,var)) (loop-steps state)))

; 	   ((string= k "=")
; 	    (let* ((form1 (pop (loop-forms state)))
; 		   (form2 form1))
; 	      (push `((,var ,form1)) (loop-bindings state))
; 	      (when (string= (symbol-name (first (loop-forms state))) "THEN")
; 		(pop (loop-forms state))
; 		(setq form2 (pop (loop-forms state))))
; 	      (push `(SETQ ,var ,form2) (loop-steps state))))

; 	   ((string= k "ACROSS")
; 	    (with-gensyms (vector length)
; 	      (push `(,var (,index 0) (,vector (pop (loop-forms state))))
; 		    (loop-bindings state))
; 	      (push `((,length (just-one (ARRAY-DIMENSIONS ,vector)))))
; 	      (push `(eq ,index ,length) (loop-tests state))
; 	      (push `(SETQ ,var (AREF ,vector ,index)) (loop-setters state))
; 	      (push `(INCF ,index) (loop-steps state))))

; 	   ((string= k "BEING")
; 	    (setq k (symbol-name (pop (loop-forms state))))
; 	    (when (or (string= k "THE") (string= k "EACH"))
; 	      (setq k (symbol-name (pop (loop-forms state)))))
; 	    ;; Discard IN or OF.
; 	    (pop (loop-forms state))
; 	    (let ((list (gensym)))
; 	      (push `((,list (package-symbols
; 			      (OR (FIND-PACKAGE ,(pop (loop-forms state)))
; 				  (ERROR (QUOTE PACKAGE-ERROR)))
; 			      ,(cond
; 				((or (string= k "SYMBOL")
; 				     (string= k "SYMBOLS"))
; 				 `(QUOTE (,(kw EXTERNAL)
; 					  ,(kw INTERNAL)
; 					  ,(kw INHERITED))))
; 				((or (string= k "PRESENT-SYMBOL")
; 				     (string= k "PRESENT-SYMBOLS"))
; 				 `(QUOTE (,(kw EXTERNAL)
; 					  ,(kw INTERNAL))))
; 				((or (string= k "EXTERNAL-SYMBOL")
; 				     (string= k "EXTERNAL-SYMBOLS"))
; 				 `(QUOTE (,(kw EXTERNAL))))
; 				(t
; 				 (ERROR "Invalid LOOP keyword: ~A" k)))))
; 		      ,var)
; 		    (loop-bindings state))
; 	      (push `(NULL ,list) (loop-tests state))
; 	      (push `(SETQ ,var (CAAR ,list)) (loop-setters state))
; 	      (push `(SETQ ,list (CDR ,list)) (loop-steps state)))))))

;       ((or (string= k "COLLECT") (string= k "COLLECTING"))
;        (let ((form (pop (loop-forms state))))
; 	 (unless (loop-accumulator state)
; 	   (setf (loop-accumulator state) (gensym)))
; 	 (push `((,(loop-accumulator state) nil)) (loop-bindings state))
; 	 (push `(PUSH ,form ,(loop-accumulator state)) (loop-body state))
; 	 (setf (loop-result state) `(NREVERSE ,(loop-accumulator state)))))

;       ((or (string= k "APPEND") (string= k "APPENDING"))
;        (let ((form (pop (loop-forms state))))
; 	 (unless (loop-accumulator state)
; 	   (setf (loop-accumulator state) (gensym)))
; 	 (push `((,(loop-accumulator state) nil)) (loop-bindings state))
; 	 (push `(SETQ ,(loop-accumulator state)
; 		      (APPEND ,(loop-accumulator state) ,form))
; 	       (loop-body state))
; 	 (setf (loop-result state) (loop-accumulator state))))

;       ((or (string= k "NCONC") (string= k "NCONCING"))
;        (let ((form (pop (loop-forms state))))
; 	 (unless (loop-accumulator state)
; 	   (setf (loop-accumulator state) (gensym)))
; 	 (push `((,(loop-accumulator state) nil)) (loop-bindings state))
; 	 (push `(SETQ ,(loop-accumulator state)
; 		      (NCONC ,(loop-accumulator state) ,form))
; 	       (loop-body state))
; 	 (setf (loop-result state) (loop-accumulator state))))

;       ((or (string= k "COUNT") (string= k "COUNTING"))
;        (let ((form (pop (loop-forms state))))
; 	 (unless (loop-accumulator state)
; 	   (setf (loop-accumulator state) (gensym)))
; 	 (push `((,(loop-accumulator state) 0)) (loop-bindings state))
; 	 (push `(WHEN ,form (INCF ,(loop-accumulator state)))
; 	       (loop-body state))
; 	 (setf (loop-result state) (loop-accumulator state))))

;       ((or (string= k "SUM") (string= k "SUMMING"))
;        (let ((form (pop (loop-forms state))))
; 	 (unless (loop-accumulator state)
; 	   (setf (loop-accumulator state) (gensym)))
; 	 (push `((,(loop-accumulator state) 0)) (loop-bindings state))
; 	 (push `(INCF ,(loop-accumulator state) ,form) (loop-body state))
; 	 (setf (loop-result state) (loop-accumulator state))))

;       ((or (string= k "MAXIMIZE") (string= k "MAXIMIZING"))
;        (let ((form (pop (loop-forms state)))
; 	     (val (gensym)))
; 	 (unless (loop-accumulator state)
; 	   (setf (loop-accumulator state) (gensym)))
; 	 (push `((,(loop-accumulator state) nil)) (loop-bindings state))
; 	 (push `(SETQ ,(loop-accumulator state)
; 		      (LET ((,val ,form))
; 			(IF ,(loop-accumulator state)
; 			    (MAX ,(loop-accumulator state) ,val)
; 			    ,val)))
; 	       (loop-body state))
; 	 (setf (loop-result state) (loop-accumulator state))))

;       ((or (string= k "MINIMIZE") (string= k "MINIMIZING"))
;        (let ((form (pop (loop-forms state)))
; 	     (val (gensym)))
; 	 (unless (loop-accumulator state)
; 	   (setf (loop-accumulator state) (gensym)))
; 	 (push `((,(loop-accumulator state) nil)) (loop-bindings state))
; 	 (push `(SETQ ,(loop-accumulator state)
; 		      (LET ((,val ,form))
; 			(IF ,(loop-accumulator state)
; 			    (MIN ,(loop-accumulator state) ,val)
; 			    ,val)))
; 	       (loop-body state))
; 	 (setf (loop-result state) (loop-accumulator state))))

;       ((string= k "WHILE")
;        (let ((form (pop (loop-forms state))))
; 	 (push `(NOT ,form) (loop-tests state))))

;       ((string= k "UNTIL")
;        (let ((form (pop (loop-forms state))))
; 	 (push form (loop-tests state))))

;       ((or (string= k "IF") (string= k "WHEN") (string= k "UNLESS"))
;        (let ((condition (pop (loop-forms state)))
; 	     (then nil)
; 	     (else nil))
; 	 (when (string= k "UNLESS")
; 	   (setq condition `(NOT ,condition)))
; 	 (let ((s (parse-loop-clause (copy-loop-state state))))
; 	   (setq then (loop-body s))
; 	   (setf (loop-body s) nil)
; 	   (setq state (merge-loop-states state s)))
; 	 (when (string= (symbol-name (first (loop-forms state))) "ELSE")
; 	   (pop (loop-forms state))
; 	   (let ((s (parse-loop-clause (copy-loop-state state))))
; 	     (setq else (loop-body s))
; 	     (setf (loop-body s) nil)
; 	     (setq state (merge-loop-states state s))))
; 	 (push `(IF ,condition
; 		    (PROGN ,@then)
; 		    ,(when else `(PROGN ,@else)))
; 	       (loop-body state))))

;       ((or (string= k "DO") (string= k "DOING"))
;        (while (consp (first (loop-forms state)))
; 	 (push (pop (loop-forms state)) (loop-body state))))

;       ((string= k "RETURN")
;        (push `(RETURN-FROM ,(loop-name state) (pop (loop-forms state)))))

;       ((string= k "INITIALLY")
;        (while (consp (first (loop-forms state)))
; 	 (push (pop (loop-forms state)) (loop-prologue state))))

;       ((string= k "FINALLY")
;        (while (consp (first (loop-forms state)))
; 	 (push (pop (loop-forms state)) (loop-epilogue state)))))

;     state))

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
