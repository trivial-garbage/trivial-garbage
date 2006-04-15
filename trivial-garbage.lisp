;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; trivial-garbage.lisp --- Trivial Garbage!
;;;
;;; Copyright (C) 2006, Luis Oliveira  <loliveira(@)common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

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
           #:cancel-finalizations))

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

#+(or :allegro :openmcl :lispworks)
(defstruct %weak-pointer
  value)

(defun make-weak-pointer (object)
  "Creates a new weak pointer which points to OBJECT. For
portability reasons, OBJECT most not be NIL."
  (assert (not (null object)))
  #+:sbcl (sb-ext:make-weak-pointer object)
  #+:cmu (ext:make-weak-pointer object)
  #+:clisp (ext:make-weak-pointer object)
  #+:allegro
  (let ((wp (make-%weak-pointer :value (excl:weak-vector 1))))
    (setf (svref (%weak-pointer-value wp) 0) object)
    wp)
  #+:openmcl
  (let ((wp (make-%weak-pointer         ; so silly
             :value (make-hash-table :test 'eq :weak :key :size 1))))
    (setf (gethash t (%weak-pointer-value wp)) object)
    wp)
  #+:corman (ccl:make-weak-pointer object)
  #+:lispworks
  (let ((wp (make-%weak-pointer :value (make-array 1))))
    (hcl:set-array-weak (%weak-pointer-value wp) t)
    (setf (svref (%weak-pointer-value wp) 0) object)
    wp))

(defun weak-pointer-p (object)
  "Returns true if OBJECT is a weak pointer and NIL otherwise."
  #+:sbcl (sb-ext:weak-pointer-p object)
  #+:cmu (ext:weak-pointer-p object)
  #+:clisp (ext:weak-pointer-p object)
  #+:allegro (%weak-pointer-p object)
  #+:openmcl (%weak-pointer-p object)
  #+:corman (ccl:weak-pointer-p object)
  #+:lispworks (%weak-pointer-p object))

(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, returns its value. Otherwise, returns NIL."
  #+:sbcl (prog1 (sb-ext:weak-pointer-value weak-pointer))
  #+:cmu (prog1 (ext:weak-pointer-value weak-pointer))
  #+:clisp (prog1 (ext:weak-pointer-value weak-pointer))
  #+:allegro (svref (%weak-pointer-value weak-pointer) 0)
  #+:openmcl (prog1 (gethash t (%weak-pointer-value weak-pointer)))
  #+:corman (ccl:weak-pointer-obj weak-pointer)
  #+:lispworks (svref (%weak-pointer-value weak-pointer) 0))

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
arguments as CL:MAKE-HASH-TABLE."
  #+(or :sbcl :corman) (declare (ignore args))
  #-(or :sbcl :corman)
  (progn
    (assert (eq (getf args :test) 'eq))
    (apply #'make-hash-table
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
  #+:clisp (eql (ext:hash-table-weak-p ht) :value)
  #+:openmcl (eql (ccl::hash-table-weak-p ht) :value)
  #+:lispworks (eql (system::hash-table-weak-kind ht) :value)
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
;;; the finalizer is unfortunate... It'd be nice to figure out a way
;;; to work around that limitation.

#+(or :allegro :clisp :lispworks :corman :openmcl)
(defvar *finalizations* (make-weak-key-hash-table :test 'eq)
  "Weak hashtable that holds registered finalizations.")

#+:lispworks
(progn
  (hcl:add-special-free-action 'free-action)
  (defun free-action (object)
    (let ((finalizations (gethash object *finalizations*)))
      (unless (null finalizations)
        (mapc #'funcall finalizations)))))

(defun finalize (object function)
  "Pushes a new FUNCTION to the OBJECT's list of
finalizations. FUNCTION should take no arguments. Returns OBJECT.

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
          (gethash object *finalizations*))
    object)
  #+:clisp
  (progn
    (push function (gethash object *finalizations*))
    (ext:finalize object
                  (lambda (obj)
                    (mapc #'funcall (gethash obj *finalizations*))))
    object)
  #+:openmcl
  (progn
    (ccl:terminate-when-unreachable
     object (lambda (obj) (declare (ignore obj)) (funcall function)))
    ;; store number of finalizations
    (if (gethash object *finalizations*)
        (incf (gethash object *finalizations*))
        (setf (gethash object *finalizations*) 1))
    object)
  #+:corman
  (progn
    (push function (gethash object *finalizations*))
    (ccl:register-finalization
     object (lambda (obj)
              (mapc #'funcall (gethash obj *finalizations*))))
    object)
  #+:lispworks
  (progn
    (push function (gethash object *finalizations*))
    (hcl:flag-special-free-action object)
    object))

(defun cancel-finalizations (object)
  "Cancels all of OBJECT's finalizations, if any."
  #+:cmu (ext:cancel-finalization object)
  #+:sbcl (sb-ext:cancel-finalization object)
  #+:allegro
  (progn
    (mapc #'excl:unschedule-finalization
          (gethash object *finalizations*))
    (remhash object *finalizations*))
  #+:clisp (remhash object *finalizations*)
  #+:openmcl
  (let ((count (gethash object *finalizations*)))
    (unless (null count)
      (dotimes (i count)
        (ccl:cancel-terminate-when-unreachable object))))
  #+:corman (remhash object *finalizations*)
  #+:lispworks
  (progn
    (remhash object *finalizations*)
    (hcl:flag-not-special-free-action object)))
