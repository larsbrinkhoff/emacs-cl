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

(defun* SUBLIS (alist tree &key KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (let ((pair (ASSOC tree alist (kw KEY) KEY (kw TEST) TEST)))
    (cond
      (pair		(CDR pair))
      ((ATOM tree)	tree)
      (t		(CONS
			 (SUBLIS alist (CAR tree) (kw KEY) KEY (kw TEST) TEST)
			 (SUBLIS alist (CDR tree) (kw KEY) KEY (kw TEST) TEST))))))

(defun* NSUBLIS (alist tree &key KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (let ((pair (ASSOC tree alist (kw KEY) KEY (kw TEST) TEST)))
    (cond
      (pair		(CDR pair))
      ((ATOM tree)	tree)
      (t
       (progn
	 (RPLACA tree (NSUBLIS alist (CAR tree) (kw KEY) KEY (kw TEST) TEST))
	 (RPLACD tree (NSUBLIS alist (CDR tree) (kw KEY) KEY (kw TEST) TEST)))))))

(defun* SUBST (new old tree &key KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (cond
    ((FUNCALL TEST old (FUNCALL KEY tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (CONS (SUBST new old (CAR tree) (kw KEY) KEY (kw TEST) TEST)
	   (SUBST new old (CAR tree) (kw KEY) KEY (kw TEST) TEST)))))

(defun* SUBST-IF (new predicate tree &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond
    ((FUNCALL predicate (FUNCALL KEY tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (CONS (SUBST-IF new predicate (CAR tree) (kw KEY) KEY)
	   (SUBST-IF new predicate (CAR tree) (kw KEY) KEY)))))

(defun* SUBST-IF-NOT (new predicate tree &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond
    ((not (FUNCALL predicate (FUNCALL KEY tree)))
     new)
    ((ATOM tree)
     tree)
    (t
     (CONS (SUBST-IF new predicate (CAR tree) (kw KEY) KEY)
	   (SUBST-IF new predicate (CAR tree) (kw KEY) KEY)))))

(defun* NSUBST (new old tree &key KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (cond
    ((FUNCALL TEST old (FUNCALL KEY tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (RPLACA tree (SUBST new old (CAR tree) (kw KEY) KEY (kw TEST) TEST))
     (RPLACD tree (SUBST new old (CDR tree) (kw KEY) KEY (kw TEST) TEST)))))

(defun* NSUBST-IF (new predicate tree &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond
    ((FUNCALL predicate (FUNCALL KEY tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (RPLACA tree (NSUBST-IF new predicate (CAR tree) (kw KEY) KEY))
     (RPLACD tree (NSUBST-IF new predicate (CDR tree) (kw KEY) KEY)))))

(defun* NSUBST-IF-NOT (new predicate tree &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond
    ((not (FUNCALL predicate (FUNCALL KEY tree)))
     new)
    ((ATOM tree)
     tree)
    (t
     (RPLACA tree (NSUBST-IF new predicate (CAR tree) (kw KEY) KEY))
     (RPLACD tree (NSUBST-IF new predicate (CDR tree) (kw KEY) KEY)))))

(defun* TREE-EQUAL (tree1 tree2 &key TEST TEST-NOT)
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (cond
    ((and (ATOM tree1) (ATOM tree2))
     (FUNCALL TEST tree1 tree2))
    ((and (CONSP tree1) (CONSP tree2))
     (and (TREE-EQUAL (CAR tree1) (CAR tree2) (kw TEST) TEST)
	  (TREE-EQUAL (CDR tree1) (CDR tree2) (kw TEST) TEST)))))

(fset 'COPY-LIST (symbol-function 'copy-list))

(fset 'LIST (symbol-function 'list))

(fset 'LIST* (symbol-function 'list*))

(fset 'LIST-LENGTH (symbol-function 'list-length))

(fset 'LISTP (symbol-function 'listp))

(cl:defun MAKE-LIST (size &key INITIAL-ELEMENT)
  (make-list size INITIAL-ELEMENT))

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

(cl:defun MEMBER (object list &key KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (do ((list list (cdr list)))
      ((null list) nil)
    (when (FUNCALL TEST object (FUNCALL KEY (car list)))
      (return-from MEMBER list))))

(cl:defun MEMBER-IF (predicate list &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (do ((list list (cdr list)))
      ((null list) nil)
    (when (FUNCALL predicate (FUNCALL KEY (car list)))
      (return-from MEMBER list))))

(cl:defun MEMBER-IF-NOT (predicate list &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (do ((list list (cdr list)))
      ((null list) nil)
    (unless (FUNCALL predicate (FUNCALL KEY (car list)))
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

(cl:defun ASSOC (item alist &key KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (dolist (pair alist)
    (when (and pair (FUNCALL TEST item (FUNCALL KEY (car pair))))
      (return-from ASSOC pair))))

(cl:defun ASSOC-IF (predicate alist &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (dolist (pair alist)
    (when (and pair (FUNCALL predicate (FUNCALL KEY (car pair))))
      (return-from ASSOC-IF pair))))

(cl:defun ASSOC-IF-NOT (predicate alist &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (dolist (pair alist)
    (when (and pair (not (FUNCALL predicate (FUNCALL KEY (car pair)))))
      (return-from ASSOC-IF-NOT pair))))

(defun COPY-ALIST (alist)
  (mapcar (lambda (pair) (CONS (CAR pair) (CDR pair)))))

(defun PAIRLIS (keys data &optional alist)
  (NCONC (MAPCAR #'CONS keys data) alist))

(cl:defun RASSOC (item alist &key KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (dolist (pair alist)
    (when (and pair (FUNCALL TEST item (FUNCALL KEY (cdr pair))))
      (return-from ASSOC pair))))

(cl:defun RASSOC-IF (predicate alist &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (dolist (pair alist)
    (when (and pair (FUNCALL predicate (FUNCALL KEY (cdr pair))))
      (return-from ASSOC-IF pair))))

(cl:defun RASSOC-IF-NOT (predicate alist &key KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (dolist (pair alist)
    (when (and pair (not (FUNCALL predicate (FUNCALL KEY (cdr pair)))))
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

(DEFINE-SETF-EXPANDER GETF (plist indicator &optional default)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION plist nil) ;TODO: env
    (with-gensyms (itemp dtemp obj)
      (let (ilist)
	(unless (null default)
	  (push dtemp temps)
	  (push default values))
	(if (CONSTANTP indicator)
	    (setq itemp (eval-with-env indicator nil) ;TODO: env
		  ilist `(QUOTE (,itemp)))
	    (setq temps (cons itemp temps)
		  values (cons indicator values)
		  ilist `(LIST ,itemp)))
	(VALUES temps
		values
		(list obj)
		`(MULTIPLE-VALUE-BIND (ind val tail)
		     (GET-PROPERTIES ,getter ,ilist)
		   (IF (NULL tail)
		       (MULTIPLE-VALUE-BIND ,variables 
			   (LIST* ,itemp ,obj ,getter)
			 ,setter)
		       (SETF (SECOND tail) ,obj)))
		`(GETF ,getter ,itemp))))))

(defun delete-property (plist indicator)
  (cond
    ((null plist)			nil)
    ((eq (car plist) indicator)		(cddr plist))
    (t					(RPLACD (cdr plist)
						(delete-property (cddr plist)
								 indicator))
					plist)))

(cl:defmacro REMF (place indicator) ;TODO: &environment
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place nil)
    `(LET* (,@(MAPCAR #'list temps values)
	    (,(first variables) (delete-property ,getter ,indicator)))
       ,setter)))

(cl:defun INTERSECTION (list1 list2 &rest keys)
  (let ((result nil))
    (dolist (x list1 result)
      (when (apply #'MEMBER x list2 keys)
	(push x result)))))

(fset 'NINTERSECTION (symbol-function 'INTERSECTION))

(cl:defun ADJOIN (object list &rest keys)
  (if (apply #'MEMBER object list keys)
      list
      (cons object list)))

(cl:defmacro PUSHNEW (object place &rest keys)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    (with-gensyms (obj)
      `(LET* ((,obj ,object)
	      ,@(MAPCAR #'list temps values)
	      (,(first variables) (ADJOIN ,obj ,getter ,@keys)))
	 ,setter))))

(cl:defun SET-DIFFERENCE (list1 list2 &rest keys)
  (let ((result nil))
    (dolist (x list1 result)
      (unless (apply #'MEMBER x list2 keys)
	(push x result)))))

(fset 'NSET-DIFFERENCE (symbol-function 'SET-DIFFERENCE))

(cl:defun SET-EXCLUSIVE-OR (list1 list2 &rest keys)
  (let ((result nil))
    (dolist (x list1)
      (unless (apply #'MEMBER x list2 keys)
	(push x result)))
    (dolist (x list2 result)
      (unless (apply #'MEMBER x list1 keys)
	(push x result)))))

(fset 'NSET-EXCLUSIVE-OR (symbol-function 'SET-EXCLUSIVE-OR))

(cl:defun SUBSETP (list1 list2 &rest keys)
  (EVERY (lambda (x) (apply #'MEMBER x list2 keys))
	 list1))

(cl:defun UNION (list1 list2 &rest keys)
  (let ((result nil))
    (dolist (x list1)
      (setq result (apply #'ADJOIN x result keys)))
    (dolist (x list2 result)
      (setq result (apply #'ADJOIN x result keys)))))

(fset 'NUNION (symbol-function 'UNION))
