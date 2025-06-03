(in-package :com.gigamonkeys.macro-utilities)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (string n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro unless (condition &rest body)
`(if (not ,condition) (progn ,@body)))

;;; Another version that generates the same code as define binary class 2 when it's fully expanded
(defmacro mac-define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
	 (declare (ignorable ,streamvar))
	 (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	   ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))


(defmacro mac-define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',name 'slots) ',(mapcar #'first slots))
	 (setf (get ',name 'superclasses) ',superclasses))
       
       (defclass ,name ,superclasses
	 ,(mapcar #'slot->defclass-slot slots))

       ,read-method
       
       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
	 (declare (ignorable ,streamvar))
	 (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	   ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))


(defmacro mac-define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
	 (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
	   (let ((,objectvar
		   (make-instance
		    ,@(or (cdr (assoc :dispatch options))
			  (error "Must supply :dispatch form."))
		    ,@(mapcan #'slot->keyword-arg slots))))
	     (read-object ,objectvar ,streamvar)
	     ,objectvar))))))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))


(defmacro mac-define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
	 `(progn
	    (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
	      (read-value ',derived-from ,stream ,@derived-args))
	    (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
	      (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
	  ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
	     `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
		,@body))
	  ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
	     `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
		,@body)))))))


;;looping

;;(defun loop-example ()
 ; (dotimes (x 20) (format t "~d," x)))

;;(defun calendar (a)
 ; (dotimes (x a)
  ;  (if (zerop (mod x 6))
;	(format t "~%")
;	(format t "~3d "(+ 1 (* 7 (truncate x 6)))))))

;;(defun calendar2 (a)
;;  (dotimes (x a)
  ;  (format t "~3d " (+ 1 x))
    ;(when (zerop (mod (+ 1 x) 6))
   ;   (format t "~%"))))

;; macros

;; (defmacro do-primes ((var start end) &body body)
;;   (with-gensyms (ending-value-name)
;;     `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
;; 	  (,ending-value-name ,end))
;; 	 ((> ,var ,ending-value-name))
;;        ,@body)))

;;(defmacro test (condition then-branch &optional else-branch)
;;`(if,condition,then-branch,else-branch))


;;(if a
  ;;   (do-x)
;;   (if b
;;       (do-y)
;;     (do-z)))

;; (defmacro do-primes-a ((var start end) &body body)
;;   (append '(do)
;; 	  (list (list (list var
;; 			    (list 'next-prime start)
;; 			    (list 'next-prime (list '1+ var)))))
;; 	  (list (list (list '> var end)))
;; 	  body))


;; (defmacro do-primes ((var start end) &body body)
;; `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;; ((> ,var ,end))
;; ,@body))

;; (do-primes var-and-range &rest body)
;; (do-primes (var start end) &body body)

;; ;;define-binary-class macros
;; (defmacro define-binary-class (name slots)
;;   (with-gensyms (typevar objectvar streamvar)
;;     `(progn
;;        (defclass ,name ()
;; 	 ,(mapcar #'slot->defclass-slot slots))

       
;;        (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
;; 	 (let ((,objectvar (make-instance ',name)))
;; 	   (with-slots ,(mapcar #'first slots) ,objectvar
;; 	     ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
;; 	   ,objectvar))
       
;;        (defmethod write-value ((,typevar (eql ',name)) ,streamvar ,objectvar &key)
;; 	 (with-slots ,(mapcar #'first slots) ,objectvar
;; 	   ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

;; ;;minor changes to binary-class macro
;; (defmacro define-binary-class1 (name (&rest superclasses) slots)
;;   (with-gensyms (objectvar streamvar)
;;     `(progn
;;        (eval-when (:compile-toplevel :load-toplevel :execute)
;; 	 (setf (get ',name 'slots) ',(mapcar #'first slots))
;; 	 (setf (get ',name 'superclasses) ',superclasses))
       
;;        (defclass ,name ,superclasses
;; 	 ,(mapcar #'slot->defclass-slot slots))
       
;;        (defmethod read-object progn ((,objectvar ,name) ,streamvar)
;; 	 (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
;; 	   ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots)))
       
;;        (defmethod write-object progn ((,objectvar ,name) ,streamvar)
;; 	 (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
;; 	   ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))
