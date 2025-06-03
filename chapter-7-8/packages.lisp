(in-package :cl-user)

(defpackage :com.gigamonkeys.macro-utilities
  (:use :common-lisp)
  (:export
   :with-gensyms
   :when
   :unless
   :once-only
   :mac-define-binary-class
   :mac-define-tagged-binary-class
   :mac-define-generic-binary-class
   :mac-define-binary-type))

