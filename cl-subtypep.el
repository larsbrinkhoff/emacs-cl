;;;; -*- emacs-lisp -*-

(in-package "CL")

(defvar *type-specs*
  '(bit fixnum bignum integer ratio rational single-float real complex number
    null boolean symbol cons list))

(defvar *type-val* (make-hash-table :test 'equal))

(defvar *representatives*
  '(0 1 2 [bignum 0] [ratio 1 2] 0.0 [complex 0 1]
    nil t defun (nil)))

(defun rep-bit (object)
  (let ((pos (position object *representatives*)))
    (if pos
	(ash 1 pos)
	(error))))

(defun register (object)
  (dolist (type *type-specs*)
    (when (cl:typep object type)
      (setf (gethash type *type-val*)
	    (logior (gethash type *type-val*) (rep-bit object))))))

(dolist (type *type-specs*)
  (setf (gethash type *type-val*) 0))

(dolist (object *representatives*)
  (register object))

(defun type-val (type)
  (if (member type *type-specs*)
      (gethash type *type-val*)
      (ecase (first type)
	(and	(reduce #'logand (mapcar #'type-val (rest type))))
	(or	(reduce #'logior (mapcar #'type-val (rest type))))
	(not	(lognot (type-val (second type)))))))

(defun find-new-types (type)
  (when (consp type)
    (case (first type)
      ((and or not)
       (mapc #'find-new-types (rest type)))
      ((integer rational float short-float single-float double-float
	long-float real)
       (push type *type-specs*)
       (unless (or (eq (second type) '*)
		   (eq (third type) '*)
		   (find-if (lambda (object) (cl:typep object type))
			    *representatives*))
	 (let ((object (/ (+ (second type) (third type)) 2)))
	   (push object *representatives*)))
       (let ((low (or (second type) '*)))
	 (cond
	   ((eq low '*))
	   ((consp low)
	    (let ((object (first low)))
	      (pushnew object *representatives*)))
	   (t
	    (let ((object (1- low)))
	      (pushnew object *representatives*)))))
       (let ((high (or (third type) '*)))
	 (cond
	   ((eq high '*))
	   ((consp high)
	    (let ((object (first high)))
	      (pushnew object *representatives*)))
	   (t
	    (let ((object (1+ high)))
	      (pushnew object *representatives*))))))
      ((member eql)
       (push type *type-specs*)
       (dolist (object (rest type))
	 (pushnew object *representatives*)))
      (t
       (error)))))

(defun cl:subtypep (type1 type2 &optional env)
  (let ((*type-specs* *type-specs*)
	(*type-val* *type-val*)
	(*representatives* *representatives*))
    (find-new-types type1)
    (find-new-types type2)
    (dolist (type *type-specs*)
      (setf (gethash type *type-val*) 0))
    (dolist (object *representatives*)
      (register object))
    (print *representatives*)
    (dolist (type *type-specs*)
      (print (format "%s => %s" type (type-val type))))
    (zerop (logand (type-val type1) (lognot (type-val type2))))))
