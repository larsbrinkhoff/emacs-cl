(defun* cl:make-string (size &key initial-element element-type)
  (make-string size (or initial-element 0)))

(defun cl:string (x)
  (cond
    ((stringp x) x)
    ((symbolp x) (symbol-name x))
    ((characterp x) (concat (list x)))
    (t (error))))

(defun char (string index)
  (aref string index))
