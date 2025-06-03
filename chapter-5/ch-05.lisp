;;; Functions

(defun foo (a &optional (b 10)) (list a b))

(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))


(format t "hello, world")
(format t "hello, ~a" name)
(format t "x: ~d y: ~d" x y)
(+)
(+ 1)
(+ 1 2)
(+ 1 2 3)

(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

(defun foo (x &optional y &key z) (list x y z))

(defun plot (fn min max step)
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
    (format t "~%")))

(funcall #'(lambda (x y) (+ x y)) 2 3)

((lambda (x y) (+ x y)) 2 3)

(function (lambda () 42))
