;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 7, Objects.

;;; TODO: Standard Generic Function FUNCTION-KEYWORDS
;;; TODO: Function ENSURE-GENERIC-FUNCTION
;;; TODO: Standard Generic Function ALLOCATE-INSTANCE
;;; TODO: Standard Generic Function REINITIALIZE-INSTANCE
;;; TODO: Standard Generic Function SHARED-INITIALIZE
;;; TODO: Standard Generic Function UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
;;; TODO: Standard Generic Function UPDATE-INSTANCE-FOR-REDEFINED-CLASS
;;; TODO: Standard Generic Function CHANGE-CLASS
;;; TODO: Function SLOT-BOUNDP
;;; TODO: Function SLOT-EXISTS-P
;;; TODO: Function SLOT-MAKUNBOUND
;;; TODO: Standard Generic Function SLOT-MISSING
;;; TODO: Standard Generic Function SLOT-UNBOUND
;;; TODO: Function SLOT-VALUE
;;; TODO: Standard Generic Function METHOD-QUALIFIERS
;;; TODO: Standard Generic Function NO-APPLICABLE-METHOD
;;; TODO: Standard Generic Function NO-NEXT-METHOD
;;; TODO: Standard Generic Function REMOVE-METHOD
;;; TODO: Standard Generic Function MAKE-INSTANCE
;;; TODO: Standard Generic Function MAKE-INSTANCES-OBSOLETE
;;; TODO: Standard Generic Function MAKE-LOAD-FORM
;;; TODO: Function MAKE-LOAD-FORM-SAVING-SLOTS
;;; TODO: Macro WITH-ACCESSORS
;;; TODO: Macro WITH-SLOTS
;;; TODO: Macro DEFCLASS
;;; TODO: Macro DEFGENERIC
;;; TODO: Macro DEFMETHOD
;;; TODO: Accessor FIND-CLASS
;;; TODO: Local Function NEXT-METHOD-P
;;; TODO: Local Macro CALL-METHOD,
;;; TODO:             MAKE-METHOD
;;; TODO: Local Function CALL-NEXT-METHOD
;;; TODO: Standard Generic Function COMPUTE-APPLICABLE-METHODS
;;; TODO: Macro DEFINE-METHOD-COMBINATION
;;; TODO: Standard Generic Function FIND-METHOD
;;; TODO: Standard Generic Function ADD-METHOD
;;; TODO: Standard Generic Function INITIALIZE-INSTANCE
;;; TODO: Standard Generic Function CLASS-NAME
;;; TODO: Standard Generic Function (SETF CLASS-NAME)
;;; TODO: Function CLASS-OF

;;; UNBOUND-SLOT and UNBOUND-SLOT-INSTANCE defined in cl-conditions.el.
