;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 14, Conses.

(IN-PACKAGE "EMACS-CL")

;;; System Class LIST
;;; System Class NULL
;;; System Class CONS
;;; Type ATOM

(mapc (lambda (to from) (fset to (symbol-function from)))
      '(CONS CONSP ATOM)
      '(cons consp atom))

(defun RPLACA (cons object)
  (setcar cons object)
  cons)

(defun RPLACD (cons object)
  (setcdr cons object)
  cons)

(fset 'CAR (symbol-function 'car-safe))

(DEFSETF CAR (cons) (car)
  `(PROGN
     (RPLACA ,cons ,car)
     ,car))

(fset 'CDR (symbol-function 'cdr-safe))

(DEFSETF CDR (cons) (cdr)
  `(PROGN
     (RPLACD ,cons ,cdr)
     ,cdr))

(defun build-cxr (string index)
  (case (aref string index)
    (65		`(CAR ,(build-cxr string (1+ index))))
    (68		`(CDR ,(build-cxr string (1+ index))))
    (t		'cons)))

(macrolet ((def (sym)
	     (let ((name (symbol-name sym)))
	       `(progn
		 (defun ,sym (cons)
		   ,(build-cxr name 1))
		 (defsetf ,sym (cons) (obj)
		   (list ',(if (eq (aref name 1) 65) 'setcar 'setcdr)
			 ,(build-cxr name 2) obj))))))
  (def CAAR) (def CADR) (def CDAR) (def CDDR)
  (def CAAAR) (def CAADR) (def CADAR) (def CADDR)
  (def CDAAR) (def CDADR) (def CDDAR) (def CDDDR)
  (def CAAAAR) (def CAAADR) (def CAADAR) (def CAADDR)
  (def CADAAR) (def CADADR) (def CADDAR) (def CADDDR)
  (def CDAAAR) (def CDAADR) (def CDADAR) (def CDADDR)
  (def CDDAAR) (def CDDADR) (def CDDDAR) (def CDDDDR))

(defun COPY-TREE (tree)
  (if (CONSP tree)
      (CONS (COPY-TREE (CAR tree)) (COPY-TREE (CDR tree)))
      tree))

(defun* SUBLIS (alist tree &key (key #'IDENTITY) test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test #'EQL))
  (let ((pair (ASSOC tree alist :key key :test test)))
    (cond
      (pair		(CDR pair))
      ((ATOM tree)	tree)
      (t		(CONS
			 (SUBLIS alist (CAR tree) :key key :test test)
			 (SUBLIS alist (CDR tree) :key key :test test))))))

(defun* NSUBLIS (alist tree &key (key #'IDENTITY) test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test #'EQL))
  (let ((pair (ASSOC tree alist :key key :test test)))
    (cond
      (pair		(CDR pair))
      ((ATOM tree)	tree)
      (t
       (progn
	 (RPLACA tree (NSUBLIS alist (CAR tree) :key key :test test))
	 (RPLACD tree (NSUBLIS alist (CDR tree) :key key :test test)))))))

(defun* SUBST (new old tree &key (key #'IDENTITY) test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test #'EQL))
  (cond
    ((FUNCALL test old (FUNCALL key tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (CONS (SUBST new old (CAR tree) :key key :test test)
	   (SUBST new old (CAR tree) :key key :test test)))))

(defun* SUBST-IF (new predicate tree &key (key #'IDENTITY))
  (cond
    ((FUNCALL predicate (FUNCALL key tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (CONS (SUBST-IF new predicate (CAR tree) :key key)
	   (SUBST-IF new predicate (CAR tree) :key key)))))

(defun* SUBST-IF-NOT (new predicate tree &key (key #'IDENTITY))
  (cond
    ((not (FUNCALL predicate (FUNCALL key tree)))
     new)
    ((ATOM tree)
     tree)
    (t
     (CONS (SUBST-IF new predicate (CAR tree) :key key)
	   (SUBST-IF new predicate (CAR tree) :key key)))))

(defun* NSUBST (new old tree &key (key #'IDENTITY) test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test #'EQL))
  (cond
    ((FUNCALL test old (FUNCALL key tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (RPLACA tree (SUBST new old (CAR tree) :key key :test test))
     (RPLACD tree (SUBST new old (CDR tree) :key key :test test)))))

(defun* NSUBST-IF (new predicate tree &key (key #'IDENTITY))
  (cond
    ((FUNCALL predicate (FUNCALL key tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (RPLACA tree (NSUBST-IF new predicate (CAR tree) :key key))
     (RPLACD tree (NSUBST-IF new predicate (CDR tree) :key key)))))

(defun* NSUBST-IF-NOT (new predicate tree &key (key #'IDENTITY))
  (cond
    ((not (FUNCALL predicate (FUNCALL key tree)))
     new)
    ((ATOM tree)
     tree)
    (t
     (RPLACA tree (NSUBST-IF new predicate (CAR tree) :key key))
     (RPLACD tree (NSUBST-IF new predicate (CDR tree) :key key)))))

(defun* TREE-EQUAL (tree1 tree2 &key test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test #'EQL))
  (cond
    ((and (ATOM tree1) (ATOM tree2))
     (FUNCALL test tree1 tree2))
    ((and (CONSP tree1) (CONSP tree2))
     (and (TREE-EQUAL (CAR tree1) (CAR tree2) :test test)
	  (TREE-EQUAL (CDR tree1) (CDR tree2) :test test)))))

(fset 'COPY-LIST (symbol-function 'copy-list))

(fset 'LIST (symbol-function 'list))

(fset 'LIST* (symbol-function 'list*))

(fset 'LIST-LENGTH (symbol-function 'list-length))

(fset 'LISTP (symbol-function 'listp))

(defun* MAKE-LIST (size &key initial-element)
  (make-list size initial-element))

(cl:defmacro PUSH (object place)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    (with-gensyms (obj)
      `(LET* ((,obj ,object)
	      ,@(MAPCAR #'list temps values)
	      (,(first variables) (CONS ,obj ,getter)))
	 ,setter))))

(cl:defmacro POP (place)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    (with-gensyms (car get)
      `(LET* (,@(MAPCAR #'list temps values)
	      (,get ,getter)
	      (,car (CAR ,get))
	      (,(first variables) (CDR ,get)))
	 ,setter
	 ,car))))

(fset 'FIRST (symbol-function 'car-safe))

(DEFSETF FIRST (list) (new)
  `(PROGN
     (RPLACA ,list ,new)
     ,new))

(defun SECOND (list)
  (CADR list))

(DEFSETF SECOND (list) (new)
  `(PROGN
     (RPLACA (CDR ,list) ,new)
     ,new))

(defun THIRD (list)
  (CADDR list))

(DEFSETF THIRD (list) (new)
  `(PROGN
     (RPLACA (CDDR ,list) ,new)
     ,new))

(defun FOURTH (list)
  (CADDDR list))

(DEFSETF FOURTH (list) (new)
  `(PROGN
     (RPLACA (CDDDR ,list) ,new)
     ,new))

(defun FIFTH (list)
  (CAR (CDDDDR list)))

(DEFSETF FIFTH (list) (new)
  `(PROGN
     (RPLACA (CDDDDR ,list) ,new)
     ,new))

(defun SIXTH (list)
  (CADR (CDDDDR list)))

(DEFSETF SIXTH (list) (new)
  `(PROGN
     (RPLACA (CDR (CDDDDR ,list)) ,new)
     ,new))

(defun SEVENTH (list)
  (CADDR (CDDDDR list)))

(DEFSETF SEVENTH (list) (new)
  `(PROGN
     (RPLACA (CDDR (CDDDDR ,list)) ,new)
     ,new))

(defun EIGHTH (list)
  (CADDDR (CDDDDR list)))

(DEFSETF EIGHTH (list) (new)
  `(PROGN
     (RPLACA (CDDDR (CDDDDR ,list)) ,new)
     ,new))

(defun NINTH (list)
  (CAR (CDDDDR (CDDDDR list))))

(DEFSETF NINTH (list) (new)
  `(PROGN
     (RPLACA (CDDDDR (CDDDDR ,list)) ,new)
     ,new))

(defun TENTH (list)
  (CADR (CDDDDR (CDDDDR list))))

(DEFSETF TENTH (list) (new)
  `(PROGN
     (RPLACA (CDR (CDDDDR (CDDDDR ,list))) ,new)
     ,new))

(fset 'NTH (symbol-function 'nth))

(DEFSETF NTH (n list) (new)
  (with-gensyms (cons)
    `(LET ((,cons (NTHCDR n list)))
       (WHEN ,cons
	 (RPLACA ,cons ,new))
       ,new)))

(defun ENDP (object)
  (cond
    ((null object)	'T)
    ((consp object)	nil)
    (t			(type-error object 'LIST))))

(fset 'NULL (symbol-function 'null))

(fset 'NCONC (symbol-function 'nconc))

(fset 'APPEND (symbol-function 'append))

(defun REVAPPEND (list tail)
  (nconc (reverse list) tail))

(defun NRECONC (list tail)
  (nconc (nreverse list) tail))

(defun* BUTLAST (list &optional (n 1))
  (NBUTLAST (COPY-LIST list) n))

(defun* NBUTLAST (list &optional (n 1))
  (setcdr (LAST list (1+ n)) nil)
  list)

(defun* LAST (list &optional (n 1))
  (do ((l list (cdr l))
       (r list)
       (i 0 (+ i 1)))
      ((atom l) r)
    (if (>= i n) (pop r))))

(defun LDIFF (list object)
  (unless (listp list)
    (type-error list 'LIST))
  (let ((result nil))
    (catch 'TAILP
      (while (consp list)
	(when (EQL object list)
	  (throw 'TAILP nil))
	(push (car list) result)
	(setq list (cdr list))))
    (nreverse result)))

(defun TAILP (object list)
  (unless (listp list)
    (type-error list 'LIST))
  (catch 'TAILP
    (while (consp list)
      (when (EQL object list)
	(throw 'TAILP T))
      (setq list (cdr list)))
    (EQL object list)))

(fset 'NTHCDR (symbol-function 'nthcdr))

(fset 'REST (symbol-function 'cdr-safe))

(DEFSETF REST (cons) (cdr)
  `(setcdr ,cons ,cdr))

(defun* MEMBER (object list &key (key #'IDENTITY) test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test #'EQL))
  (do ((list list (cdr list)))
      ((null list) nil)
    (when (FUNCALL test object (FUNCALL key (car list)))
      (return-from MEMBER list))))

(defun* MEMBER-IF (predicate list &key (key #'IDENTITY))
  (do ((list list (cdr list)))
      ((null list) nil)
    (when (FUNCALL predicate (FUNCALL key (car list)))
      (return-from MEMBER list))))

(defun* MEMBER-IF-NOT (predicate list &key (key #'IDENTITY))
  (do ((list list (cdr list)))
      ((null list) nil)
    (unless (FUNCALL predicate (FUNCALL key (car list)))
      (return-from MEMBER list))))

(defun MAPC (fn &rest lists)
  (let ((result (car lists)))
    (while (notany (cl:function ENDP) lists)
      (APPLY fn (mapcar (cl:function car) lists))
      (setq lists (mapcar (cl:function cdr) lists)))
    result))

(defun MAPCAR (fn &rest lists)
  (let ((result nil))
    (while (notany (cl:function ENDP) lists)
      (push (APPLY fn (mapcar (cl:function car) lists)) result)
      (setq lists (mapcar 'cdr lists)))
    (nreverse result)))

(defun MAPCAN (fn &rest lists)
  (apply (cl:function nconc)
	 (apply (cl:function MAPCAR) fn lists)))

(defun MAPL (fn &rest lists)
  (let ((result (car lists)))
    (while (notany (cl:function ENDP) lists)
      (APPLY fn lists)
      (setq lists (mapcar (cl:function cdr) lists)))
    result))

(defun MAPLIST (fn &rest lists)
  (let ((result nil))
    (while (notany (cl:function ENDP) lists)
      (push (APPLY fn lists) result)
      (setq lists (mapcar 'cdr lists)))
    (nreverse result)))

(defun MAPCON (fn &rest lists)
  (apply (cl:function nconc)
	 (apply (cl:function MAPLIST) fn lists)))

(defun ACONS (key datum alist)
  (CONS (CONS key datum) alist))

(defun* ASSOC (item alist &key (key #'IDENTITY) test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test #'EQL))
  (dolist (pair alist)
    (when (and pair (FUNCALL test item (FUNCALL key (car pair))))
      (return-from ASSOC pair))))

(defun* ASSOC-IF (predicate alist &key (key #'IDENTITY))
  (dolist (pair alist)
    (when (and pair (FUNCALL predicate (FUNCALL key (car pair))))
      (return-from ASSOC-IF pair))))

(defun* ASSOC-IF-NOT (predicate alist &key (key #'IDENTITY))
  (dolist (pair alist)
    (when (and pair (not (FUNCALL predicate (FUNCALL key (car pair)))))
      (return-from ASSOC-IF pair))))

(defun COPY-ALIST (alist)
  (mapcar (lambda (pair) (CONS (CAR pair) (CDR pair)))))

(defun PAIRLIS (keys data &optional alist)
  (NCONC (MAPCAR #'CONS keys data) alist))

(defun* RASSOC (item alist &key (key #'IDENTITY) test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test #'EQL))
  (dolist (pair alist)
    (when (and pair (FUNCALL test item (FUNCALL key (cdr pair))))
      (return-from ASSOC pair))))

(defun* RASSOC-IF (predicate alist &key (key #'IDENTITY))
  (dolist (pair alist)
    (when (and pair (FUNCALL predicate (FUNCALL key (cdr pair))))
      (return-from ASSOC-IF pair))))

(defun* RASSOC-IF-NOT (predicate alist &key (key #'IDENTITY))
  (dolist (pair alist)
    (when (and pair (not (FUNCALL predicate (FUNCALL key (cdr pair)))))
      (return-from ASSOC-IF pair))))

(defun* GET-PROPERTIES (plist indicators)
  (do ((plist plist (cddr plist)))
      ((null plist)
       (VALUES nil nil nil))
    (when (memq (car plist) indicators)
      (return-from GET-PROPERTIES
	(VALUES (car plist) (cadr plist) plist)))))

(defun* GETF (plist indicator &optional default)
  (do ((plist plist (cddr plist)))
      ((null plist)
       default)
    (when (eq (car plist) indicator)
      (return-from GETF (cadr plist)))))

;;; TODO: Can't use defsetf here, use DEFINE-SETF-EXPANDER instead.
(defsetf GETF (plist indicator &optional default) (value)
  `(MULTIPLE-VALUE-BIND (ind val tail)
       (GET-PROPERTIES ,plist '(,indicator))
     (if (null tail)
	 (progn (setf ,plist (LIST* ,indicator ,value ,plist) ,value))
	 (setf (SECOND tail) ,value))))

;;; TODO: REMF

;;; TODO: INTERSECTION, NINTERSECTION

(defun* ADJOIN (object list &key test test-not)
  (if (MEMBER object list :test test :test-not test-not)
      list
      (cons object list)))

(cl:defmacro PUSHNEW (object place &key test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test `(FUNCTION EQL)))
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    (with-gensyms (obj)
      `(LET* ((,obj ,object)
	      ,@(MAPCAR #'list temps values)
	      (,(first variables) (ADJOIN ,obj ,getter :test ,test)))
	 ,setter))))

(defun* SET-DIFFERENCE (list1 list2 &key (key #'IDENTITY) test test-not)
  (when (and test test-not)
    (error))
  (when test-not
    (setq test (COMPLEMENT test-not)))
  (unless test
    (setq test #'EQL))
  (let ((result nil))
    (dolist (x list1)
      (unless (MEMBER x list2 :key key :test test :test-not test-not)
	(push x result)))
    result))

;;; TODO: NSET-DIFFERENCE

;;; TODO: SET-EXCLUSIVE-OR, NSET-EXCLUSIVE-OR

;;; TODO: SUBSETP

;;; TODO: UNION, NUNION
