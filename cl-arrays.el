(defun* make-array (dimensions &key element-type initial-element
		    initial-contents adjustable fill-pointer
		    displaced-to displaced-index-offset)
  nil)

;;; adjust-array

;;; adjustable-array-p

(defun cl:aref (array &rest subscripts)
  nil)

(defun upgraded-array-element-type (typespec &optional env)
  (cond
    ((subtypep typespec 'bit)			'bit)
    ((subtypep typespec 'character)		'character)
    ((subtypep typespec '(unsigned-byte 8))	'(unsigned-byte 8))
    (t						t)))

(defun simple-vector-p (object)
  (and (vectorp object) (eq (aref object 0) 'simple-vector)))

(defun svref (vector index)
  (aref vector (1+ index)))

(defun cl:vector (&rest objects)
  (let ((vector (make-vector (1+ (length objects)) nil))
	(i 0))
    (aset vector 0 'simple-vector)
    (dolist (obj objects vector)
      (aset vector (incf i) obj))))

(defun cl:vectorp (object)
  (and (vectorp object)
       (or (simple-vector-p object)
	   (cl:subtypep 'vector (aref object 0)))))

(defun bit (array &rest subscripts)
  nil)

(defun sbit (array &rest subscripts)
  nil)

(defun bit-vector-p (object)
  nil)

(defun simple-bit-vector-p (object)
  (bool-vector-p object))
