;;; syntax and semantics

;;; funcall and apply and lambda form

(defun test (x z) 
  (let ((z (* x 2))) (print z)) 
  z)

(defun foo (x y z) (+ x y z))

((lambda (x y z) (+ x y z)) 2 3 5)

;;test-form
(quote (+ 1 2))

;;x ; the symbol X
;() ; the empty list
;(1 2 3); a list of three numbers
;("foo" "bar") ; a list of two strings
;(x y z); a list of three symbols
;(x 1 "foo") ; a list of a symbol, a number, and a string
;(+ (* 2 3) 4) ; a list of a symbol, a list, and a number.

(defun hello-world ()
  (format t "hello, world"))

(* (+ 1 2) (- 3 4))

(dolist (x foo)
  (print x))

(defun print-list (list)
  (dolist (i list)
    (format t "item: ~a~%" i)))

(defun foo ()
(dotimes (i 5)
  (format t "~d. hello~%" i)))


