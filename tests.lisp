;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tests.lisp --- trivial-garbage tests.
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

(defpackage #:trivial-garbage-tests
  (:use #:cl #:trivial-garbage #:regression-test))

(in-package #:trivial-garbage-tests)

;;;; Weak Pointers

(deftest pointers.1
    (weak-pointer-p (make-weak-pointer 42))
  t)

(deftest pointers.2
    (weak-pointer-value (make-weak-pointer 42))
  42)

;;;; Weak Hashtables

#+(or :sbcl :corman)
(pushnew 'hashtables.1 rt::*expected-failures*)

(deftest hashtables.1
    (let ((ht (make-weak-key-hash-table :test 'eq)))
      (values (hash-table-p ht)
              (weak-key-hash-table-p ht)))
  t t)

#+(or :sbcl :cmu :corman)
(pushnew 'hashtables.2 rt::*expected-failures*)

(deftest hashtables.2
    (let ((ht (make-weak-value-hash-table)))
      (values (hash-table-p ht)
              (weak-value-hash-table-p ht)))
  t t)

;;;; Finalizers
;;;
;;; These tests are, of course, not very reliable. And they way they're
;;; written doesn't help either. :-/

(defparameter *finalized?* nil)

(defun setup-finalizers (count &optional remove)
  (setq *finalized?* (make-list count))
  (let ((obj (copy-seq "xpto")))
    (dotimes (i count)
      (let ((i i))
        (finalize obj
                  (lambda ()
                    ;;(assert (null *finalized?*))
                    (setf (nth i *finalized?*) t)))))
    (when remove
      (cancel-finalizations obj)))
  (gc :full t))

(defun do-it-to-it (setup-function &rest args)
  (apply setup-function args)
  (gc :full t))

;;; CLISP crashes hard on this one. See:
;;; http://article.gmane.org/gmane.lisp.clisp.general/11028
#-:clisp
(deftest finalizers.1
    (progn
      (do-it-to-it #'setup-finalizers 1)
      (gc :full t)
      (car *finalized?*))
  t)

(deftest finalizers.2
    (progn
      (do-it-to-it #'setup-finalizers 1 t)
      (gc :full t)
      (car *finalized?*))
  nil)

#-:clisp
(deftest finalizers.3
    (progn
      (do-it-to-it #'setup-finalizers 3)
      (gc :full t)
      *finalized?*)
  (t t t))

(deftest finalizers.4
    (progn
      (do-it-to-it #'setup-finalizers 3 t)
      (gc :full t)
      *finalized?*)
  (nil nil nil))