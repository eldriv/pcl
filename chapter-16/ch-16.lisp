;;; Object Reorientation: Generic Functions

#|
Chapter 16:
Object Reorientation : Generic Functions

-all 00 languages follow the same methodology spawn by Simula
 -behavior associated with classes through methods
    -also called "instance methods", "member functions", etc.
    -all methods belong to a particular class
    -model of method invocation is called "message passing"
- ORIGINAL LISP 00
  -SEND
     -method used in early lisp object systems to do message passing to particular objects
     -example: (send object 'foo)
     -Design side effects
       - could'nt be used like a normal function
       - had to wrap message send/receuve's with lambda to use in higher order functions
- GENERIC FUNCTION
  -fixed issues with normal message passing objects
  -ended up becoming heart of Common lisp's object system
  -What is a generic function?
      - abstract operation but with no implementation
      - similar to what we call interface method (although generic functions are not tied to a specific object).
|#  

;; simple example of a class
(defclass point ()
  ((x :initarg :x :initform nil :reader point-x)
   (y :initarg :y initform nil  :reader point-y)))

;; simple example of generic function
(defgeneric draw (shape)
  (:documentation "draw the given shape on the screen."))

;; simple example (compliments of PCL)
;; defmethod is an implentation of defgeneric when I define a defmethod inside defgeneric that's the same thing defining a method on that generic globally where a way to put a couple of methods in one spot
;; the importance of documentation, generally when I am designing my generic functions is to understand what the argument what really mean.
;; NOTE: methods only can specialize on classes not types
(defgeneric withdraw(account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than amount."))

;;defmethod ~~ define method that implement the withdraw generic function
(defmethod withdraw ((account bank-account) amount)
  (when (<(balance account) amount)
    (error "Account overdrawn."))
  (decf (balance amount) amount))

;;common lisps type system is much more flexible than the class system
(def method withdraw ((account checking-account) amount)
  (let ((overdraft (-amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

;; ordering example (primary method combination)

(def method withdraw :before ((account checking-account) amount)
  (let ((overdraft (-amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

(def method withdraw :after ((account checking-account) amount)
  (let ((overdraft (-amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

(def method withdraw :around ((account checking-account) amount)
  (let ((overdraft (-amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

#|
what is the used of EQL specialization??

The eql function checks for both identity and equality of numbers, characters, and some other types, but it does not perform type conversion or coercion. This means that eql returns true if its arguments are the same object
(as checked by eq), or if they are numbers or characters of the same type and value. For example, (eql 3 3) is true because both are integers with the same value, but (eql 3 3.0) is false because, although they represent
the same numeric value, they are of different types (integer vs. float) 

the idea of EQL in the book is that they are looking at the type that instance  are being passed in, if matched it will invoked(executing a method).

plusp --- is a predicate function. Predicates in Lisp are functions that return a boolean value (t for true, nil for false) based on the evaluation of their argument. The plusp function specifically checks if a given number is
greater than zero. If the number is greater than zero, plusp returns t otherwise, it returns nil.

overdraft -- plusp overdraft is used to determine if the overdraft amount is positive. If overdraft is positive, it means the withdrawal amount exceeds the account balance, indicating an overdraft situation.

This code is a common pattern in financial software to handle overdrafts by transferring the deficit to an o.draft account.
|#

(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))
    (call-next-method)))

;;example evaluation to know the ordering, 'cause I'm slow-learner 
;; in :around method there is a some semantics should be considered when dealing with around methods

(defparameter *a-obj* nil)
(defparameter *b-obj* nil)

(defclass a(b)
  ())
(defclass b()
  ())

(defgeneric some-method (obj))

(defmethod some-method ((obj a))
  (print "class a method call"))

(defmethod some-method :before ((obj a))
  (print "class a before method call"))

(defmethod some-method :around ((obj a))
  (print "class a around method call")
  (call-next-method))

(defmethod some-method :after ((obj a))
  (print "class a after method call")
  (call-next-method))

;----------------------------------------------------
(defmethod some-method ((obj b)) 
  (print "class b method call"))

(defmethod some-method :before ((obj b))
  (print "class b before method call"))

(defmethod some-method :around ((obj b))
  (print "class b around method call")
  (call-next-method))

(defmethod some-method :after ((obj b))
  (print "class b after method call"))

;;; Method combination using progn and identify what's the use of a keyword method combo and most-specific-last evaluation

(defgeneric some-progn-method (obj)
  (:documentation "Shows off progn method combination.")
  (:method-combination progn :most-specific-last))

(defmethod some-progn-method progn ((obj a))
  (print "class a progn method combination called")
  5)

(defmethod some-progn-method progn ((obj b))
  (print "class b progn method combination called")
  6)

;;;ADDITIONALS

;; another example of a class
;; accessor
(defclass person ()
  ((name :initarg :name :accessor name)
   (age :initarg :age :accessor age)))

;; usage
(let ((p (make-instance 'person :name "Adi" :age 23)))
  (setf (name p) "Fred")
  (format nil "~a: ~a" (name p) (age p)))

;;; initform
(defclass person ()
  ((name :initarg :name  :initform "Adi" :accessor name)
   (age  :initarg :age   :initform 16   :accessor age)))

;; usage
(let ((p (make-instance 'person :name "Michael")))
  (format nil "~a: ~a" (name p) (age p)))


;;; type and documentation
(defclass person ()
  ((name :initarg :name  :initform "Adi" :accessor name :type string  :documentation "Stores a persons name")
   (age  :initarg :age   :initform 16    :accessor age  :type integer :documentation "Stores a persons age")))

;;; usage
(let ((p (make-instance 'person :name "Michael")))
  (format nil "~a: ~a " (name p) (age p)))

;;; added data, and allocation
(defclass person ()
  ((name :initarg :name  :initform "Adi" :accessor name :allocation :instance :type string  :documentation "Stores a persons name")
   (age  :initarg :age   :initform 16    :accessor age  :allocation :instance :type integer :documentation "Stores a persons age")
   (species :initarg :species :initform "Human" :accessor species :allocation :class )))

;;usage
(let ((p (make-instance 'person :name "145"))
      (p2 (make-instance 'person :name "Adi" :age 45)))
  (setf (species p2) "Not-human")
  
  (let ((p3 (make-instance 'person :name "Michael" :age 34)))
    (format nil "~a: ~a {~a}" (name p3) (age p3) (species p3))))

(defvar ppl (make-instance 'person :name "Ads" :age 22))

> (defvar p1 (make-instance 'person :name "me" ))
> (name p1)
> (lisper p1) --> nil
> (setf (lisper p1) "Adi")
> (lisper p1) --> "Adi"

;;; GENERIC FUNCTIONS BASICS
(defgeneric test-g (x y))

(defmethod test-g ((x number) (y number))
  (+ x y))

;; RESUlT
> (test-g 2 3)
5

(defmethod test-g ((x string) (y string))
  (format nil "~a~a" x y))

;; RESUlT
> (test-g "foo" "bar")
"foobar"

(defmethod test-g ((x number) (y string))
  (format nil "~a~a" x y))

;;RESUlT
>(test-g 1 "foo")
"1foo"
