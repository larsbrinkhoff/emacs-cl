;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 17, Sequences.

(IN-PACKAGE "EMACS-CL")

;;; System Class SEQUENCE

(defun COPY-SEQ (sequence)
  (cond
    ((listp sequence)
     (copy-list sequence))
    ((SIMPLE-VECTOR-P sequence)
     (copy-sequence sequence))
    ((vector-and-typep sequence 'VECTOR)
     (let ((storage (aref sequence 2))
	   (vector (make-vector
		    (1+ (or (FILL-POINTER sequence) (LENGTH sequence)))
		    'SIMPLE-VECTOR)))
       (do ((i 1 (1+ i)))
	   ((= i (length vector)))
	 (aset vector i (aref storage (1- i))))
       vector))
    ((VECTORP sequence)
     (subseq (aref sequence 2) 0 (FILL-POINTER sequence)))
    (t
     (type-error sequence 'SEQUENCE))))

(defun ELT (sequence index)
  (cond
    ((listp sequence)
     (nth index sequence))
    ((VECTORP sequence)
     (if (ARRAY-HAS-FILL-POINTER-P sequence)
	 (if (cl:< index (FILL-POINTER sequence))
	     (AREF sequence index)
	     (error))
	 (AREF sequence index)))
    (t
     (error "type error"))))

(defsetf ELT (sequence index) (obj)
  `(if (listp ,sequence)
       (setf (nth ,index ,sequence) ,obj)
       (setf (AREF ,sequence ,index) ,obj)))

(DEFSETF ELT (sequence index) (obj)
  `(IF (LISTP ,sequence)
       (SETF (NTH ,index ,sequence) ,obj)
       (SETF (AREF ,sequence ,index) ,obj)))

(cl:defun FILL (seq obj &key (START 0) END)
  ;; TODO: use fillarray when possible
  (let ((len (LENGTH seq)))
    (unless END
      (setq END len))
    (do ((i START (1+ i)))
	((eq i len))
      (setf (ELT seq i) obj)))
  seq)

(cl:defun MAKE-SEQUENCE (type size &key INITIAL-ELEMENT)
  (cond
    ((SUBTYPEP type 'LIST)
     (make-list size INITIAL-ELEMENT))
    ((SUBTYPEP type 'BIT-VECTOR)
     (make-bool-vector size (ecase INITIAL-ELEMENT ((0 nil) nil) (1 t))))
    ((SUBTYPEP type 'STRING)
     (make-string size (if INITIAL-ELEMENT (CHAR-CODE INITIAL-ELEMENT) 0)))
    ((SUBTYPEP type 'VECTOR)
     (let ((vector (make-vector (1+ size) INITIAL-ELEMENT)))
       (aset vector 0 'SIMPLE-VECTOR)
       vector))
    (t
     (error))))

(defun SUBSEQ (seq start &optional end)
  (unless end
    (setq end (LENGTH seq)))
  (cond
    ((SIMPLE-STRING-P seq)
     (substring seq start end))
    ((STRINGP seq)
     (substring (aref seq 2) start end))
    ((listp seq)
     (if (eq start end)
	 nil
	 (let ((new (copy-list (nthcdr start seq))))
	   (setcdr (nthcdr (- end start 1) new) nil)
	   new)))
;     ((BIT-VECTOR-P seq)
;      (let* ((s (if (SIMPLE-BIT-VECTOR-P seq) seq (aref seq 2)))
; 	    (len (- end start))
; 	    (new (make-bool-vector len nil)))
;        (do ((i 0 (1+ i))
; 	    (j start (1+ j)))
; 	   ((eq i len))
; 	 (aset new i (aref s j)))
;        new))
;     ((VECTORP seq)
;      (let* ((len (- end start -1))
; 	    (new (make-vector len 'SIMPLE-VECTOR))
; 	    (s (if (SIMPLE-VECTOR-P seq)
; 		   (progn (incf start) seq)
; 		   (aref seq 2))))
;        (do ((i 1 (1+ i))
; 	    (j start (1+ j)))
; 	   ((eq i len))
; 	 (aset new i (aref seq j)))
;        new))
    ((VECTORP seq)
     (let ((len (- end start))
	   (i0 0))
       (when (eq (aref seq 0) 'SIMPLE-VECTOR)
	 (incf i0)
	 (incf len)
	 (incf start))
       (let ((new (if (BIT-VECTOR-P seq)
		      (make-bool-vector len nil)
		      (make-vector len 'SIMPLE-VECTOR)))
	     (storage (if (SIMPLE-VECTOR-P seq) seq (aref seq 2))))
	 (do ((i i0 (1+ i))
	      (j start (1+ j)))
	     ((eq i len))
	   (aset new i (aref storage j)))
	 new)))
    (t
     (type-error seq 'SEQUENCE))))

;;; TODO: SETF SUBSEQ

(defun* MAP (type fn &rest sequences)
  (let ((len (apply #'min (mapcar #'LENGTH sequences)))
	(i 0)
	(result nil))
    (loop
      (when (eq i len)
	(return-from MAP
	  (progn
	    (setq result (nreverse result))
	    (ecase type
	      (LIST	result)
	      (STRING	(if (null result)
			    ""
			    (apply #'string (mapcar #'CHAR-CODE result))))
	      (VECTOR	(apply #'vector 'SIMPLE-VECTOR result))))))
      (push (APPLY fn (mapcar (lambda (seq) (ELT seq i)) sequences)) result)
      (incf i))))

(defun* MAP-INTO (result fn &rest sequences)
  (let ((len (apply #'min (mapcar #'LENGTH (cons result sequences))))
	(i 0))
    (loop
      (when (eq i len)
	(return-from MAP-INTO result))
      (setf (ELT result i)
	    (APPLY fn (mapcar (lambda (seq) (ELT seq i)) sequences)))
      (incf i))))

;;; TODO: REDUCE

(cl:defun COUNT (obj seq &key FROM-END TEST TEST-NOT (START 0) END
			      (KEY (cl:function IDENTITY)))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST (cl:function EQL)))
  (COUNT-IF (lambda (x) (FUNCALL TEST obj x)) seq
	    (kw FROM-END) FROM-END (kw START) START
	    (kw END) END (kw KEY) KEY))

(cl:defun COUNT-IF (predicate seq &key FROM-END (START 0) END
		                       (KEY (cl:function IDENTITY)))
  (unless END
    (setq END (LENGTH seq)))
  (let ((n 0))
    (if FROM-END
	(do ((i (1- END) (1- i)))
	    ((eq i (1- START)))
	  (let ((elt (ELT seq i)))
	    (when (FUNCALL predicate (FUNCALL KEY elt))
	      (incf n))))
	(do ((i START (1+ i)))
	    ((eq i END))
	  (let ((elt (ELT seq i)))
	    (when (FUNCALL predicate (FUNCALL KEY elt))
	      (incf n)))))
    n))

(cl:defun COUNT-IF-NOT (predicate &rest args)
  (apply (cl:function COUNT-IF) (COMPLEMENT predicate) args))

(defun LENGTH (sequence)
  (cond
    ((or (listp sequence)
	 (SIMPLE-BIT-VECTOR-P sequence)
	 (SIMPLE-STRING-P sequence))
     (length sequence))
    ((SIMPLE-VECTOR-P sequence)
     (1- (length sequence)))
    ((VECTORP sequence)
     (if (ARRAY-HAS-FILL-POINTER-P sequence)
	 (FILL-POINTER sequence)
	 (length (aref sequence 2))))
    (t
     (error))))

(defun REVERSE (seq)
  (cond
   ((listp seq)
    (reverse seq))
   ((VECTORP seq)
    (NREVERSE (COPY-SEQ seq)))
   (t
    (type-error seq 'SEQUENCE))))

(defun NREVERSE (seq)
  (cond
    ((listp seq)
     (nreverse seq))
    ((VECTORP seq)
     (do* ((len (LENGTH seq))
	   (end (/ len 2))
	   (i 0 (1+ i))
	   (j (1- len) (1- j)))
	  ((eq i end)
	   seq)
       (rotatef (AREF seq i) (AREF seq j))))
    (t
     (type-error seq 'SEQUENCE))))

(cl:defun SORT (sequence predicate &key (KEY (cl:function IDENTITY)))
  (cond 
    ((listp sequence)
     (sort sequence (lambda (x y)
		      (FUNCALL predicate (FUNCALL KEY x) (FUNCALL KEY y)))))
    ((VECTORP sequence)
     (MAP-INTO sequence
	       #'IDENTITY
	       (SORT (MAP 'LIST #'IDENTITY sequence) predicate (kw KEY) KEY)))
    (t
     (error "type error"))))

(fset 'STABLE-SORT (symbol-function 'SORT))

(cl:defun FIND (obj seq &key FROM-END TEST TEST-NOT (START 0) END
		             (KEY (cl:function IDENTITY)))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST (cl:function EQL)))
  (FIND-IF (lambda (x) (FUNCALL TEST obj x)) seq
	   (kw FROM-END) FROM-END (kw START) START
	   (kw END) END (kw KEY) KEY))

(cl:defun FIND-IF (predicate seq &key FROM-END (START 0) END
		                      (KEY (cl:function IDENTITY)))
  (let ((len (LENGTH seq)))
    (unless END
      (setq END len))
    (catch 'FIND
      (if FROM-END
	  (do ((i (1- END) (1- i)))
	      ((eq i -1))
	    (let ((elt (ELT seq i)))
	      (when (FUNCALL predicate (FUNCALL KEY elt))
		(throw 'FIND elt))))
	  (do ((i START (1+ i)))
	      ((eq i END))
	    (let ((elt (ELT seq i)))
	      (when (FUNCALL predicate (FUNCALL KEY elt))
		(throw 'FIND elt)))))
      nil)))

(cl:defun FIND-IF-NOT (predicate &rest args)
  (apply (cl:function FIND-IF) (COMPLEMENT predicate) args))

(cl:defun POSITION (obj seq &key FROM-END TEST TEST-NOT (START 0) END
				 (KEY (cl:function IDENTITY)))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST (cl:function EQL)))
  (POSITION-IF (lambda (x) (FUNCALL TEST obj x)) seq
	       (kw FROM-END) FROM-END (kw START) START
	       (kw END) END (kw KEY) KEY))

(cl:defun POSITION-IF (predicate seq &key FROM-END (START 0) END
		                          (KEY (cl:function IDENTITY)))
  (let ((len (LENGTH seq)))
    (unless END
      (setq END len))
    (catch 'POSITION
      (if FROM-END
	  (do ((i (1- END) (1- i)))
	      ((eq i -1))
	    (let ((elt (ELT seq i)))
	      (when (FUNCALL predicate (FUNCALL KEY elt))
		(throw 'FIND i))))
	  (do ((i START (1+ i)))
	      ((eq i END))
	    (let ((elt (ELT seq i)))
	      (when (FUNCALL predicate (FUNCALL KEY elt))
		(throw 'FIND elt)))))
      nil)))

(cl:defun POSITION-IF-NOT (predicate &rest args)
  (apply (cl:function FIND-IF) (COMPLEMENT predicate) args))

(defun subseq-p (seq1 start1 end1 seq2 start2 end2 TEST KEY)
  (catch 'subseq-p
    (do ((i start1 (1+ i))
	 (j start2 (1+ j)))
	((or (eq i end1) (eq j end2))
	 (eq i end1))
      (unless (FUNCALL TEST (FUNCALL KEY (ELT seq1 i))
		            (FUNCALL KEY (ELT seq2 j)))
	(throw 'subseq-p nil)))))

(cl:defun SEARCH (seq1 seq2 &key FROM-END TEST TEST-NOT (KEY #'IDENTITY)
		                 (START1 0) (START2 0) END1 END2)
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (unless END1
    (setq END1 (LENGTH seq1)))
  (unless END2
    (setq END2 (LENGTH seq2)))
  (catch 'SEARCH
    (if FROM-END
	(do ((i (1- END2) (1- i)))
	    ((minusp i))
	  (when (subseq-p seq1 START1 END1 seq2 i END2 TEST KEY)
	    (throw 'SEARCH i)))
	(do ((i START2 (1+ i)))
	    ((eq i END2))
	  (when (subseq-p seq1 START1 END1 seq2 i END2 TEST KEY)
	    (throw 'SEARCH (+ i (- END1 START1) -1)))))
    nil))

;;; TODO: MISMATCH

(cl:defun REPLACE (seq1 seq2 &key (START1 0) (START2 0) END1 END2)
  (unless END1
    (setq END1 (LENGTH seq1)))
  (unless END2
    (setq END2 (LENGTH seq2)))
  (do ((i START1 (1+ i))
       (j START2 (1+ j)))
      ((or (eq i END1) (eq j END2)))
    (setf (ELT seq1 i) (ELT seq2 j)))
  seq1)

(cl:defun NSUBSTITUTE (new old seq &key FROM-END TEST TEST-NOT (START 0) END
					COUNT (KEY (cl:function IDENTITY)))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (NSUBSTITUTE-IF new (lambda (x) (FUNCALL TEST old x))
		  (kw FROM-END) FROM-END (kw TEST) TEST (kw START) START
		  (kw END) END (kw COUNT) COUNT (kw KEY) KEY))

(cl:defun NSUBSTITUTE-IF (obj predicate seq &key FROM-END (START 0) END COUNT
						 (KEY (cl:function IDENTITY)))
  (unless END
    (setq END (LENGTH seq)))
  (if FROM-END
      (do ((i (1- END) (1- i)))
	  ((or (minusp i) (<= COUNT 0)))
	(when (FUNCALL predicate (FUNCALL KEY (ELT seq i)))
	  (setf (ELT seq i) obj))
	(decf COUNT))
      (do ((i START (1+ i)))
	  ((or (eq i END) (<= COUNT 0)))
	(when (FUNCALL predicate (FUNCALL KEY (ELT seq i)))
	  (setf (ELT seq i) obj))
	(decf COUNT)))
  seq)

(cl:defun NSUBSTITUTE-IF-NOT (predicate &rest args)
  (apply (cl:function NSUBSTITUTE-IF) (COMPLEMENT predicate) args))

(cl:defun SUBSTITUTE (new old seq &rest keys)
  (apply (cl:function NSUBSTITUTE) new old (COPY-SEQ seq) keys))

(cl:defun SUBSTITUTE-IF (obj predicate seq &rest keys)
  (apply (cl:function NSUBSTITUTE-IF) obj predicate (COPY-SEQ seq) keys))

(cl:defun SUBSTITUTE-IF-NOT (obj predicate seq &rest keys)
  (apply (cl:function NSUBSTITUTE-IF)
	 obj (COMPLEMENT predicate) (COPY-SEQ seq) keys))

(defun CONCATENATE (type &rest sequences)
  (ecase type
    (LIST
     (let ((result nil))
       (dolist (seq sequences (nreverse result))
	 (dosequence (x seq)
           (push x result)))))
    (VECTOR
     (let ((vector (make-vector (1+ (reduce #'+ (mapcar #'LENGTH sequences)))
				'SIMPLE-VECTOR))
	   (i 0))
       (dolist (seq sequences)
	 (dosequence (x seq)
	   (aset vector (incf i) x)))
       vector))
    (STRING
     (let ((string (make-string (reduce #'+ (mapcar #'LENGTH sequences)) 0))
	   (i -1))
       (dolist (seq sequences)
	 (dosequence (x seq)
	   (aset string (incf i) (CHAR-CODE x))))
       string))))

;;; TODO: MERGE

;;; TODO: REMOVE
;;; TODO: REMOVE-IF

(cl:defun REMOVE-IF-NOT (predicate &rest args)
  (apply (cl:function REMOVE-IF) (COMPLEMENT predicate) args))

;;; TODO: DELETE
;;; TODO: DELETE-IF

(cl:defun DELETE-IF-NOT (predicate &rest args)
  (apply (cl:function DELETE-IF) (COMPLEMENT predicate) args))

;;; TODO: REMOVE-DUPLICATES
;;; TODO: DELETE-DUPLICATES



(defmacro* dovector ((var vector &optional result) &body body)
  (with-gensyms (i len vec)
    `(let* (,var (,i 0) (,vec ,vector) (,len (LENGTH ,vec)))
       (while (< ,i ,len)
	 (setq ,var (AREF ,vec ,i))
	 ,@body
	 (incf ,i))
       ,result)))

(defmacro* dosequence ((var sequence &optional result) &body body)
  (let ((seq (gensym)))
    `(let ((,seq ,sequence))
       (if (listp ,seq)
	   (dolist (,var ,seq ,result) ,@body)
	   (dovector (,var ,seq ,result) ,@body)))))
