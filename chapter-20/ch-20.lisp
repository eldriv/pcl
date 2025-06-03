;;; Chapter20
;;; Special Operators

#|
Controlling Evaluation
Three operators that provides basic control
-Quote : prevents evaluation altogether and allows you to get at s-expressions as data
-If : provides the fundamental boolean choice operation from which all other conditional execution constructs can be built
-Progn : provides the ability to sequence a number of forms.

LET and LET* are examples of special operators that manipulate the lexical environment since they can introduce new lexical bindings for variables.

*LEXICAL ENVIRONMENT*
- Here references to the established entity can occur only within certain program portions that are lexically (that is, textually) contained within the establishing construct. Typically the construct will have a part designated the body, and the scope of all entities established will be (or include) the body. 

|#

;;quote prevents code from being evaluated
(quote (+ 1 2))
;;if - most fundamental boolean choice operation
(when (< 4 5)
  (print "hello"))
(unless (> 4 5)
  (print "hello"))


;;progn - runs multiple statements sequentially
(progn 
  (print "hello1")
  (print "hello2")
  (print "hello3"))

;;let, let*, setq
(let ((x 0)
      (y 1))
  (print (x + y)))

(let* ((x 1)
       (y (* x 2)))
  (print '(x y)))

;;setf
(let ((x 0))
  (setf x (incf x))
  (print x))

;;exical Scope

;;;LABELS AND FLET
;;;ormat
;;;LET Inside FLET the function defined cannot do calls to itself, while it is possible with LABELS
;;;LET doesn't allow recursion while LABELS  does.
(flet (function-definition*)
  body-form*)

;;example:
(defun example-flet ()
  (flet ((square (x)(* x x)))
    (print (square 4))))  ; prints 16

;and like this:

(labels (function-definition*)
  body-form*)


;;;example:
(defun example-labels ()
  (labels ((factorial (n)
             (if (zerop n)
                 1
                 (* n (factorial (1- n))))))
    (print (factorial 5))))  ; prints 120

(labels ((fac (n acc)
	   (if (<= n 1)
	       acc
	       (fac (1- n) (* acc n)))))
  (print (fac 5 1)))

(defun factorial (n acc)
  (labels ((fac (n acc)
             (if (<= n 1)
                 acc
                 (fac (1- n) (* acc n)))))
    (fac n acc)))




;;;symbol-macrolet - like regular macros except they can't take arguments and are
;;;referred to as a plain symbol

(defun symbol-macrolet-try ()
  (let ((a-list (list 1 2 3 4 5)))
    (symbol-macrolet ((firstlist (first a-list)))(setf firstlist 6)
      (print a-list))))


;;;another way
(defparameter *things* '(1 2 3 4 5))
(define-symbol-macro *another-thing* (first *things*))

(defun symbol-macro-try ()
  (setf *another-thing* 1)
  (print *things*))

;;;scope and extent

;;;scope
;;; - a and b are only accessible within the scope of this function, nothing
;;;can escape from and b
(defun add (a b)
  (a + b))

;;extents
;;; it means you can access that labels or symbols, especially when dealing
;;;with call stacks, within call stacks variables may not be accessible than
;;;labels.

;;;tagbody and blocks unless you are really creating higher-level constructs that are not in standard that are rarely used in everyday code

#|
The name is a symbol, and the forms are Lisp forms. The forms are evaluated in order, and
the value of the last form is returned as the value of the BLOCK unless a RETURN-FROM is used to
return from the block early
|#
;;block template
(block name
  form*)

;;;block example is used to group a multiple form in order, If x is greater
;;;than 1, the return-from statement
;;;is used to exit the block and return the result of the multiplication. If
;;;not, the last form in block will
;;;execute
;;;example of block and return-from
(defun multiplication (x)
  (block y
    (format t "Executing form 1~%")
    (if (> x 1)
        (return-from y (* x 2)))
    (format t "Executing form 3~%")
    nil))

;;;flow example using block without return-from
;;----------------------------------

(defun multi (x)
  (format t "Entering multiplication~%")
  (block a
    (format t " Entering block a ~%")
    (let ((result1 (* x 2)))
      (format t "   Result: ~a~%" result1)
      (add #'(lambda () result1)))
    (format t " Leaving block a~%" ))
  (format t "Leaving multiplication~%"))

(defun add (fn)
  (format t "Entering addition~%")
  (block b
    (format t " Entering block b ~%")
    (let ((result2 (+ (funcall fn) 12)))
      (format t "   Result: ~a~%" result2)
      (subt #'(lambda () result2)))
    (format t "  Leaving block b~%"))
  (format t "  Leaving addition~%"))

(defun subt (fn)
  (format t "Entering subtraction~%")
  (block c
    (format t " Entering block c ~%")
    (let ((result3 (- (funcall fn) 5)))
      (format t "   Result: ~a~%" result3))
    (format t "Leaving block c~%"))
  (format t " Leaving subtraction~%"))


;--------------------------

;;;;tagbody -- TAGBODY and GO have a similar relationship to each other as BLOCK and RETURN-FROM: a
;;;;TAGBODY form defines a context in which names are defined that can be used by GO
;;;;tagbody template
(tagbody
 tag-or-compound-form*)

;;;example of tagbody and go
(tagbody
top
(print 'hello)
   (go top))

;;;let's have this example

(defun foo ()
  (format t "Entering foo ~%")
  (tagbody
     (format t " Entering TAGBODY ~%")
     (bar #'(lambda () (go exit-foo)))
   exit-foo
     (format t "Leaving foo ~%")))

(defun bar (fn)
  (format t " Entering bar~%")
  (list 3 2)
  (baz fn)
  (format t " Leaving bar~%"))

(defun baz (fn)
  (format t " Entering baz~%")
  (funcall fn)
  (format t " Leaving baz~%"))

;;;another example for simple-minded like me :)
;;;it demonstrates how a non-local control flow using tags and go statements in Common Lisp.

(defun non-local-example ()
  (tagbody
     (format t "Entering tagbody~%")
     (format t "Executing code before the jump~%")
     (go jump-tag) 
     ;; (format t "This code is unreachable~%") because any code after the
   ;;go statement won't be executed due to the go jump-tag
   jump-tag
     (format t "Jumped to jump-tag~%")
     (format t "Exiting tagbody~%")))


;;; Catch and Throw without defparamater


(defun main-function ()
  (format t " Start of the program~%")
  (let ((result (function-two)))
    (append '(adi) result)))

(defun function-two ()
  (catch 'one
    (format t "  Before the throw, body of catch inside function-two ~%")
    (throw 'one
      (let ((result2 (remove-if-not #'oddp '(1 2 3 4 5 6 7 8 9 10 11))))
	result2))))

;;it won't be printed because the throw operator is called. So
;;it's skipped

;;;unwind-protect

(unwind-protect protected-form
  cleanup-form*)

;;;You’ll occasionally use UNWIND-PROTECT directly. More often you’ll use it as the basis for
;;;WITH- style macros, similar to WITH-OPEN-FILE, that evaluate any number of body forms in a
;;;context where they have access to some resource that needs to be cleaned up after they’re
;;;done, regardless of whether they return normally or bail via a restart or other nonlocal exit. For
;;;example, if you were writing a database library that defined functions open-connection and
;;;close-connection, you might write a macro like this:

(defmacro with-database-connection ((var &rest open-args) &body body)
  `(let ((,var (open-connection ,@open-args)))
     (unwind-protect (progn ,@body)
       (close-connection ,var))))

;;;which lets you write code like this:

(with-database-connection (conn :host "foo" :user "scott" :password "tiger")
  (do-stuff conn)
  (do-more-stuff conn))


;;;and not have to worry about closing the database connection, since the UNWIND-PROTECT will
;;;make sure it gets closed no matter what happens in the body of the with-database-connection
;;;form.

;;;GETHASH—is the ability for a single form to return multiple values.



;;;multiple-value-bind

(defun get-user-info ()
  (values "Adi Villareal" 22 "michael.adrian.villareal@adamson.edu.ph"))

;;;(multiplie-values-bind (name age email) (get-user-info)
(format t "Name: ~a, Age: ~a, Email: a~" name age email)

;;;unwind-protect
;;;template of unwind-protect
(unwind-protect protected-form
cleanup-form*)

#|
A special operator you’ll need to understand in order to write certain kinds of macros is
EVAL-WHEN. For some reason, Lisp books often treat EVAL-WHEN as a wizards-only topic. But the
only prerequisite to understanding EVAL-WHEN is an understanding of how the two functions
LOAD and COMPILE-FILE interact. 
I’ve touched briefly on the relation between LOAD and COMPILE-FILE in previous chapters,
but it’s worth reviewing again here. The job of LOAD is to load a file and evaluate all the top-level
forms it contains. The job of COMPILE-FILE is to compile a source file into a FASL file, which
can then be loaded with LOAD such that (load "foo.lisp") and (load "foo.fasl") are essen-
tially equivalent.
Because LOAD evaluates each form before reading the next, the side effects of evaluating
forms earlier in the file can affect how forms later in the form are read and evaluated. For
instance, evaluating an IN-PACKAGE form changes the value of *PACKAGE*, which will affect the
way subsequent forms are read.12 Similarly, a DEFMACRO form early in a file can define a macro
that can then be used by code later in the file.
|#

