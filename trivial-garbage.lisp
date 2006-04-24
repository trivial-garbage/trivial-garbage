;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; trivial-garbage.lisp --- Trivial Garbage!
;;;
;;; This software is placed in the public domain by Luis Oliveira
;;; <loliveira@common-lisp.net> and is provided with absolutely no
;;; warranty.

(defpackage #:trivial-garbage
  (:use #:cl)
  (:nicknames #:tg)
  (:export #:gc
           #:make-weak-pointer
           #:weak-pointer-value
           #:weak-pointer-p
           #:weak-key-hash-table-p
           #:weak-value-hash-table-p
           #:make-weak-key-hash-table
           #:make-weak-value-hash-table
           #:finalize
           #:cancel-finalization))

(in-package #:trivial-garbage)

;;;; GC

(defun gc (&key full verbose)
  "Initiates a garbage collection."
  (declare (ignorable verbose full))
  #+:cmu (ext:gc :verbose verbose :full full)
  #+:sbcl (sb-ext:gc :full full)
  #+:allegro (excl:gc (not (null full)))
  #+:clisp (ext:gc)
  #+:openmcl (ccl:gc)
  #+:corman (ccl:gc (if full 3 0))
  #+:lispworks (hcl:mark-and-sweep (if full 3 0))) 

;;;; Weak Pointers

#+:openmcl
(defvar *weak-pointers* (make-hash-table :test 'eq :weak :value)
  "Weak value hash-table mapping between pseudo weak pointers and its values.")

#+(or :allegro :openmcl :lispworks)
(defstruct (weak-pointer (:constructor %make-weak-pointer))
  #-:openmcl pointer)

(defun make-weak-pointer (object)
  "Creates a new weak pointer which points to OBJECT. For
portability reasons, OBJECT most not be NIL."
  (assert (not (null object)))
  #+:sbcl (sb-ext:make-weak-pointer object)
  #+:cmu (ext:make-weak-pointer object)
  #+:clisp (ext:make-weak-pointer object)
  #+:allegro
  (let ((wv (excl:weak-vector 1)))
    (setf (svref wv 0) object)
    (%make-weak-pointer :pointer wv))
  #+:openmcl
  (let ((wp (%make-weak-pointer)))
    (setf (gethash wp *weak-pointers*) object)
    wp)
  #+:corman (ccl:make-weak-pointer object)
  #+:lispworks
  (let ((array (make-array 1)))
    (hcl:set-array-weak array t)
    (setf (svref array 0) object)
    (%make-weak-pointer :pointer array)))

#-(or :allegro :openmcl :lispworks)
(defun weak-pointer-p (object)
  "Returns true if OBJECT is a weak pointer and NIL otherwise."
  #+:sbcl (sb-ext:weak-pointer-p object)
  #+:cmu (ext:weak-pointer-p object)
  #+:clisp (ext:weak-pointer-p object)
  #+:corman (ccl:weak-pointer-p object))

(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, returns its value. Otherwise, returns NIL."
  #+:sbcl (prog1 (sb-ext:weak-pointer-value weak-pointer))
  #+:cmu (prog1 (ext:weak-pointer-value weak-pointer))
  #+:clisp (prog1 (ext:weak-pointer-value weak-pointer))
  #+:allegro (svref (weak-pointer-pointer weak-pointer) 0)
  #+:openmcl (prog1 (gethash weak-pointer *weak-pointers*))
  #+:corman (ccl:weak-pointer-obj weak-pointer)
  #+:lispworks (svref (weak-pointer-pointer weak-pointer) 0))

;;;; Weak Hash-tables

(defun weak-key-hash-table-p (ht)
  "Returns true if HT is an hash-table with weak keys, NIL otherwise."
  #+(or :sbcl :corman) (declare (ignore ht))
  #+:allegro (excl:hash-table-weak-keys ht)
  #+:clisp (eq (ext:hash-table-weak-p ht) :key)
  #+:cmu (lisp::hash-table-weak-p ht)
  #+:openmcl (eq (ccl::hash-table-weak-p ht) :key)
  #+:lispworks (eql (system::hash-table-weak-kind ht) :key)
  #+(or :sbcl :corman)
  (error "Your lisp does not support weak key hash-tables."))

(defun make-weak-key-hash-table (&rest args)
  "Creates an hash-table with weak keys. Accepts the same
arguments as CL:MAKE-HASH-TABLE except :TEST which is forced to
be 'EQ." 
  #+(or :sbcl :corman) (declare (ignore args))
  #-(or :sbcl :corman)
  (progn 
    (assert (let ((test (getf args :test)))
              (or (null test) (eq test 'eq))))
    (remf args :test)
    (apply #'make-hash-table :test 'eq
           #+:allegro :weak-keys #+:allegro t
           #+:clisp :weak #+:clisp :key
           #+:cmu :weak-p #+:cmu t
           #+:openmcl :weak #+:openmcl :key
           #+:lispworks :weak-kind #+:lispworks :key
           args))
  #+(or :sbcl :corman)
  (error "Your lisp does not support weak key hash-tables."))

(defun weak-value-hash-table-p (ht)
  "Returns true if HT is an hash-table with weak values, NIL otherwise."
  #+(or :sbcl :cmu :corman) (declare (ignore ht))
  #+:allegro (eq (excl:hash-table-values ht) :weak)
  #+:clisp (eq (ext:hash-table-weak-p ht) :value)
  #+:openmcl (eq (ccl::hash-table-weak-p ht) :value)
  #+:lispworks (eq (system::hash-table-weak-kind ht) :value)
  #+(or :sbcl :cmu :corman)
  (error "Your lisp does not support weak value hash-tables."))

(defun make-weak-value-hash-table (&rest args)
  "Creates an hash-table with weak values. Accepts the same
arguments as CL:MAKE-HASH-TABLE."
  #+(or :sbcl :cmu :corman) (declare (ignore args))
  #-(or :sbcl :cmu :corman)
  (apply #'make-hash-table
         #+:allegro :values #+:allegro :weak
         #+:clisp :weak #+:clisp :value
         #+:openmcl :weak #+:openmcl :value
         #+:lispworks :weak-kind #+:lispworks :value
         args)
  #+(or :sbcl :cmu :corman)
  (error "Your lisp does not support weak value hash-tables."))

;;;; Finalizers

;;; The fact that SBCL/CMUCL throw away the object *before* running
;;; the finalizer is somewhat unfortunate...

#+(or :allegro :clisp :lispworks :openmcl)
(defvar *finalizers*
  (make-hash-table :test 'eq
                   #+:allegro :weak-keys #+:allegro t
                   #+(or :clisp :openmcl) :weak
                   #+:lispworks :weak-kind
                   #+(or :clisp :openmcl :lispworks) :key)
  "Weak hashtable that holds registered finalizers.")

#+:corman
(defvar *finalizers* '()
  "Weak alist that holds registered finalizers.")

#+:lispworks
(progn
  (hcl:add-special-free-action 'free-action)
  (defun free-action (object)
    (let ((finalizers (gethash object *finalizers*)))
      (unless (null finalizers)
        (mapc #'funcall finalizers)))))

(defun finalize (object function)
  "Pushes a new FUNCTION to the OBJECT's list of
finalizers. FUNCTION should take no arguments. Returns OBJECT.

For portability reasons, FUNCTION should not attempt to look at
OBJECT by closing over it because, in some lisps, OBJECT will
already have been garbage collected and is therefore not
accessible when FUNCTION is invoked."
  #+:cmu (ext:finalize object function)
  #+:sbcl (sb-ext:finalize object function)
  #+:allegro
  (progn
    (push (excl:schedule-finalization
           object (lambda (obj) (declare (ignore obj)) (funcall function)))
          (gethash object *finalizers*))
    object)
  #+:clisp
  (progn
    (push function (gethash object *finalizers*))
    (ext:finalize object
                  (lambda (obj)
                    (mapc #'funcall (gethash obj *finalizers*))))
    object)
  #+:openmcl
  (progn
    (ccl:terminate-when-unreachable
     object (lambda (obj) (declare (ignore obj)) (funcall function)))
    ;; store number of finalizers
    (if (gethash object *finalizers*)
        (incf (gethash object *finalizers*))
        (setf (gethash object *finalizers*) 1))
    object)
  #+:corman
  (flet ((get-finalizers (obj)
           (assoc obj *finalizers* :test #'eq :key #'ccl:weak-pointer-obj)))
    (let ((pair (get-finalizers object)))
      (if (null pair)
          (push (list (ccl:make-weak-pointer object) function) *finalizers*)
          (push function (cdr pair))))
    (ccl:register-finalization
     object (lambda (obj) (mapc #'funcall (cdr (get-finalizers obj)))))
    object)
  #+:lispworks
  (progn
    (push function (gethash object *finalizers*))
    (hcl:flag-special-free-action object)
    object))

(defun cancel-finalization (object)
  "Cancels all of OBJECT's finalizers, if any."
  #+:cmu (ext:cancel-finalization object)
  #+:sbcl (sb-ext:cancel-finalization object)
  #+:allegro
  (progn
    (mapc #'excl:unschedule-finalization
          (gethash object *finalizers*))
    (remhash object *finalizers*))
  #+:clisp (remhash object *finalizers*)
  #+:openmcl
  (let ((count (gethash object *finalizers*)))
    (unless (null count)
      (dotimes (i count)
        (ccl:cancel-terminate-when-unreachable object))))
  #+:corman (setq *finalizers*
                  (delete object *finalizers*
                          :test #'eq :key #'ccl:weak-pointer-obj))
  #+:lispworks
  (progn
    (remhash object *finalizers*)
    (hcl:flag-not-special-free-action object)))
