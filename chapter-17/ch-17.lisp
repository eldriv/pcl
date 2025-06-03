;; Object Reorientation: Classes

;;; Format
(defclass name (direct-superclass-name*)
  (slot-specifier*))

;;; Slot specifiers

(defclass bank-account ()
  (customer-name
   balance))

(defparameter *account* (make-instance 'bank-account))
(setf (slot-value *account* 'customer-name) "John Doe")
(setf (slot-value *account* 'balance) 1000)

(defparameter *account*
  (make-instance 'bank-account :customer-name "Michael Villareal" :balance 5000))

(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))

(defmethod initialize-instance :after ((account bank-account)
                                       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
          (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

(defclass bank-account ()
  ((customer-name :initarg :customer-name :accessor customer-name)
   (balance :initarg :balance :accessor balance)
   (opening-bonus-percentage :initarg :opening-bonus-percentage :accessor opening-bonus-percentage)
   (account-type :accessor account-type)))

(defun format-account-details (account)
  (format t "Customer: ~a~%Balance: $~,,'0D~%Opening Bonus Percentage: ~a%~%Type: ~a~%"
          (customer-name account)
          (balance account)
          (opening-bonus-percentage account)
          (account-type account)))

(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))

