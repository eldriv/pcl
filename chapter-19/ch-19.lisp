;;; Chapter 19
;;; Beyond Exception Handling: Conditions and Restarts

#|
* Agenda
  * Introduction to error Handling
  * How do other programming languages handle error
  *Introduction to the CL condition systems
  *[Time permitting] restarts

Error handling -
*doesn't necessarily mean bug in systems, Its external factors are lack of data,abrupt network connection, pre-conditions not met

*could cause cascade of failures if not handled properly
*as the saying goes, getting an error may not be a bug but not handling an error is mostly certain one.

*solutions to handling errors
-return in an error code and some message
-exception

*exceptions
-feature available in most programming languages
-common way to handle errors
   -when error arises one would "throw" an exception object with some information.
   -to handle errors, one would "catch" the exception object and handle it.
   -two part division: handler code is typically separated from code that signals the actual error
-how things are thrown/caught and how to clean up as you unwind the stack is typically implementation dependent.

*Introduction to CL condition system
   *Performs a three-way separation
     -code that signals the condition
     -code that handles the condition
     -code that actually restarts execution after condition is handled.

What are conditions?

*class of objects
*instance data carries details about what lead to condition being signalled.
*DEFINE-CONDITION macro
   *works like defclass
   *difference from DEFCLASS
         -unlike classes defined with DEFCLASS, default superclass is CONDITION not STANDARD-OBJECT
         -MUST specify a :reader or :accessor option for any slot define for a condition
                    -cannot use SLOT-VALUE
         -MUST use MAKE-CONDITION not MAKE-INSTANCE
                    -must specify arguments for condition construction with :initarg
                    -no further customization can be made with an INITIALIZE-INSTANCE equivalent
   *SPECIFIC for error handling
         -should definey your condition as subclasses of ERROR (itself subclass of CONDITION)
|#

;;; Example of error conditiom to call the define the invalid-argument-error from check-argument function
(define-condition invalid-argument-error(error)
  ((text :initarg :text :reader text)))

;;; Condition handlers

(defun add (a b)
  (if (not (integerp a))
      (error 'invalid-argument-error :text "a must be an interger")
      (if (not (integerp b))
	  (let ((err-obj (make-condition 'invalid-argument-error :text "b must be an integer")))
	    (error err-obj))
	  (+ a b))))

;checking if the value is integer, if not it will throw an error with a message
(defun check-argument (a)
  (unless (integerp a)
    (error 'invalid-argument-error :text "argument must be an integer")))

;;; For failed arguments handling an error using handler-case
(defun add (a b)
  (let ((failed-arguments nil))
    (handler-case (check-argument a)
      (invalid-argument-error ()
	(progn
	  (format t "~A not an integer" a)
	  (setf failed-arguments 't))))
    (handler-case (check-argument b)
      (invalid-argument-error ()
	(progn
	  (format t "~A not an integer" b)
	  (setf failed-arguments 't))))
    (unless failed-arguments
      (+ a b))))

;;; Restarts - you can think of a restart  like you are providing a way for the computation to be executed, that pauses the computation almost. allowing to change or manipulating a particular values to be successful.

(defun add (a b)
  (restart-case (check-argument a)
    (provide-default-value ()
      (format t "provide new for a: ")
      (setf a (parse-integer (read-line)))))
  (restart-case (check-argument b)
    (provide-default-value ()
      (format t "provide new for b: ")
      (setf b (parse-integer (read-line)))))
  (+ a b))

(define-condition invalid-argument-error (error)
  ((text :initarg :text :reader invalid-argument-text)))

(defun test-math (n)
  (if (< n 0)
      (error 'invalid-argument-error :text "Input must be a non-negative integer.")
      (let ((result 1))
        (loop for i from 1 to n do
              (setq result (* result i)))
        result)))

(defun use-abs-value (condition)
  (declare (ignore condition))
  (invoke-restart 'compute-abs-value))

(defun compute-abs-value (n)
  (test-math (abs n)))

(defun test ()
  (handler-bind ((invalid-argument-error
                   (lambda (c)
                     (declare (ignore c))
                     (format t "Caught invalid-argument-error, handling it without unwinding the stack.~%"))))
    (format t "The factorial is ~A~%" (test-math -16))))

(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
      (make-instance 'log-entry ...)
      (error 'malformed-log-entry-error :text text)))

(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
	  for entry = (handler-case (parse-log-entry text)
			(malformed-log-entry-error () nil))
	  when entry collect it)))

(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
	  for entry = (restart-case (parse-log-entry text)
			(skip-log-entry () nil))
	  when entry collect it)))

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
		   #'(lambda (c)
		       (invoke-restart 'skip-log-entry))))
    (dolist (log (find-all-logs))
      (analyze-log log))))
