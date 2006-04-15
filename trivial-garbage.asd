;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; trivial-garbage.asd --- ASDF system definition for trivial-garbage.
;;;
;;; Copyright (C) 2006, Luis Oliveira  <loliveira@common-lisp.net>
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

#-(or :cmu :sbcl :allegro :clisp :openmcl :corman :lispworks)
(error "Sorry, your Lisp is not supported by trivial-garbage.")

(defpackage #:trivial-garbage-system
  (:use #:cl #:asdf))
(in-package #:trivial-garbage-system)

(defsystem trivial-garbage
  :description "Portable finalizers, weak hash-tables and weak pointers."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :version "0.9"
  :licence "MIT"
  :components ((:file "trivial-garbage")))

(defmethod perform ((op test-op) (sys (eql (find-system :trivial-garbage))))
  (operate 'load-op :trivial-garbage-tests)
  (operate 'test-op :trivial-garbage-tests))

(defsystem trivial-garbage-tests
  :description "Unit tests for TRIVIAL-GARBAGE."
  :depends-on (trivial-garbage rt)
  :components ((:file "tests")))

(defmethod perform ((op test-op)
                    (sys (eql (find-system :trivial-garbage-tests))))
  (funcall (find-symbol (symbol-name '#:do-tests) '#:regression-test)))

;; vim: ft=lisp et
