;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.

;;; Various test cases for bignum addition.
(defun bignum-test ()
  (loop for (x y z) in
	'((67108864		67108864		[BIGNUM -134217728 0])
	  (134217727		1			[BIGNUM -134217728 0])
	  (-134217728		-1			[BIGNUM 134217727 -1])
	  (-134217728		-134217728		[BIGNUM 0 -1])
	  ([BIGNUM -1 0]	[BIGNUM -1 0]		[BIGNUM -2 1])
	  ([BIGNUM 0 -1]	[BIGNUM 0 -1]		[BIGNUM 0 -2])
	  ([BIGNUM 0 2]		[BIGNUM 0 -1]		[BIGNUM 0 1])
	  ([BIGNUM 0 -1]	[BIGNUM 0 2]		[BIGNUM 0 1])
	  ([BIGNUM 0 1]		[BIGNUM 0 -2]		[BIGNUM 0 -1])
	  ([BIGNUM 0 -2]	[BIGNUM 0 1]		[BIGNUM 0 -1])
	  ([BIGNUM 2 2]		[BIGNUM -1 -3]		1)
	  ([BIGNUM 2 2]		[BIGNUM -3 -3]		-1)
	  ([BIGNUM -54323701 6]	[BIGNUM 16292363 17]	[BIGNUM -38031338 23])
	  ([BIGNUM 119720045 12408]
				[BIGNUM 38283770 30621]
						    [BIGNUM -110431641 43029])
	  ([BIGNUM -134217728 2] -1			[BIGNUM 134217727 2])
	  ([BIGNUM 0 100000000]	[BIGNUM 0 100000000]	[BIGNUM 0 -68435456 0])
	  ([BIGNUM -24181363 103035877]
				[BIGNUM -24181363 103035877]
					       [BIGNUM -48362726 -62363701 0]))
	do (unless (equal (cl:+ x y) z)
	     (princ (format "%s + %s /= %s\n" x y z)))))

(defun funcall-test ()
  (let ((foo
	 (EVAL (READ-FROM-STRING
		"(defun foo (a &optional b (c 100) (d 101 dp)
		               &key f (g 102) (h 103 hp) ((i j) 104 jp))
		   (list a b c d dp f g h hp j jp))")))
	(data
	 `(((1) . (1 nil 100 101 nil nil 102 103 nil 104 nil))
	   ((1 2) . (1 2 100 101 nil nil 102 103 nil 104 nil))
	   ((1 2 3) . (1 2 3 101 nil nil 102 103 nil 104 nil))
	   ((1 2 3 4) . (1 2 3 4 t nil 102 103 nil 104 nil))
	   ((1 2 3 4 ,(kw f) 5) . (1 2 3 4 t 5 102 103 nil 104 nil)))))
    (dolist (x data)
      (let* ((args (car x))
	     (good (cdr x))
	     (result (APPLY foo args)))
	(dolist (i '(4 9 11))
	  (setf (nth i result) (not (null (nth i result)))))
	(unless (equal result good)
	  (princ (format "Interpreted FOO %s /= %s\n" args result)))))
    (COMPILE foo nil)
    (dolist (x data)
      (let* ((args (car x))
	     (good (cdr x))
	     (result (APPLY foo args)))
	(dolist (i '(4 9 11))
	  (setf (nth i result) (not (null (nth i result)))))
	(unless (equal result good)
	  (princ (format "Compiled FOO %s = %s\n" args (APPLY foo args)))
	  (princ (format "Compiled FOO %s /= %s\n" args result)))))))

(defun test-cl ()
  (bignum-test)
  ;(funcall-test)
  (LOAD "tests.lisp")
  (princ "\nAll tests completed.\n\n"))
