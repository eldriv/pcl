;;; Variables

(defun foo (x)
  (format t "Parameter: ~a~%" x)
  (let ((x 2))
    (format t "Outer LET: ~a~%" x)
    (let ((x 3))
      (format t "Inner LET: ~a~%" x))
    (format t "Outer LET: ~a~%" x))
  (format t "Parameter: ~a~%" x))

;nested
(let ((x 10))
  (let ((y (+ x 10)))
    (list x y)))

;;; Lexical Variables

(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))

(defvar *count* 0
"Count of widgets made so far.")
(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

(defvar *x* 10)
(defun foo () (format t "X: ~d~%" *x*))

(defun bar ()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))

(defun foo ()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*))
