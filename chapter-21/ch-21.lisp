;;;; chapter 21
(defpackage #:adi
  (:use :common-lisp))

(defpackage #:vincent
  (:use :common-lisp))

(in-package #:adi)

(defun hello ()
  (print "Hello from my package."))

(defun foo () (format t "This is Adi's foo"))

(defun baz () (format t "This is Vincent's foo"))

;;; Export and use-package

(in-package adi)
(defclass adi-class () (slot1 slot2 slot3))
(defvar *published-symbols* '(adi-class slot1 slot2 slot3))

(in-package vincent)
(import 'adi:adi-class)
(make-instance 'adi-class)

;;; A package programmatically using macro
;;; The hello and add-numbers functions are defined in the :adi package, and can use now  without
;;; The need for a prefix like `adi-package'

(defmacro define-my-package (package-name &rest exported-symbols)
  `(defpackage ,package-name
     (:use :common-lisp)
     (:export ,@exported-symbols)))

(define-my-package :adi
  :hello
  :add-numbers)

(in-package :adi)

(defun hello ()
  "Meh."
  (format t "Hello from Package-Adi~%"))

(defun add-numbers (a b)
  "Meh."
  (+ a b))



