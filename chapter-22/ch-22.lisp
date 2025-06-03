;;; LOOP FOR BLACK BELTS

;;; Looping Over Collections and Packages

(loop :for i in (list 10 20 30 40) collect i)
→ (10 20 30 40)

(loop :for i in (list 10 20 30 40) by #'cddr collect i)
→ (10 30)

(loop :for x on (list 10 20 30) collect x)
→ ((10 20 30) (20 30) (30))

(loop :for x on (list 10 20 30 40) by #'cddr collect x)
→ ((10 20 30 40) (30 40))

(loop :for x across "abcd" collect x)
→ (#\a #\b #\c #\d)

;;Equals-then Iteration

(loop repeat 5
      :for y = 1 then (+ x y)
      for x = 0 then y
      collect y)
→ (1 1 2 4 8)

(loop repeat 5
      :for x = 0 then y
      and y = 1 then (+ x y)
      collect y)
→ (1 1 2 3 5)

;;; Destructuring Variables

? (loop :for (a b) in '((1 2) (3 4) (5 6))
	do (format t "a: ~a; b: ~a~%" a b))

→ a: 1; b: 2
  a: 3; b: 4
  a: 5; b: 6
NIL

;;; Recall that ncocn destructive version of APPEND
(loop :for i :upto 3 nconc (list i i))

;;; Unconditional exec
(block outer
  (loop :for i :from 0 return 100) ; 100 returned from LOOP
  (print "This will print")
  200) → 200
to this:
(block outer
  (loop :for i :from 0 do (return-from outer 100)) ; 100 returned from BLOCK
  (print "This won't print")
  200) → 100

;;; Conditional exec

(loop :for i :from 1 to 10 :do (when (evenp i) (print i)))

(loop :for i :from 1 to 10 :when (evenp i) sum i) → 30

(if (loop for n in numbers never (oddp n)) (print "All numbers even."))

;;; A there is clause is used to test whether the test form is ever true. As soon as the test form returns a non-NIL value, the loop is terminated, returning that value. If the loop runs to completion, the thereis clause provides a default return value of NIL.

(loop :for char :across "abc123" thereis (digit-char-p char)) → 1

(loop :for char :xacross "abcdef" thereis (digit-char-p char)) → NIL

;;; All in one using push on what I learned on chapter 12

(defun a-loops ()
  (let ((results '()))
    (push (loop :for i :in '(10 20 30 40) collect i) results)
    (push (loop :for x :on '(10 20 30) collect x) results)
    (push (loop :for x :on '(10 20 30 40) by #'cddr collect x) results)
    (push (loop :for x :across "abcd" collect x) results)
    
    (push (loop :repeat 5 :for y = 1 :then (+ x y) :for x = 0 :then y :collect y) results)
    (push (loop :repeat 5 :for x = 0 :then y and y = 1 :then (+ x y) :collect y) results)

;;; This line 'reverse' function used to push the format to the front of the list 
 
(format t "Looping Over Collections and Packages and Equal-then-iteration: ~%~{~a~%~}" (reverse results))))

;;; Repeat loop random into 100 of 10000 then collect

(loop repeat 30
       :for x = 0 then y
       and  y = 1 then (+ x y)
       collect y)

;;; Using counting,summing, maximizing, minimizing and finally

(defparameter *random* (loop repeat 100 collect (random 10000)))

(loop for num in *random*
      :counting (evenp num) into evens
      :counting (oddp num) into odds
      :summing num into total
      :maximizing num into max
      :minimizing num into min
      :finally (return (list min max total evens odds)))


;;; Special variable

(defparameter *numbers* '(1 2 3 4 5 6 7 8 9 10))

(defparameter *pairs* '((a 1) (b 2) (c 3) (d 4) (e 5)))

(defparameter *triples* '((a 1 1) (b 2 2) (c 3 3) (d 4 4) (e 5 5)))

;;; Test
(loop :for i :in *numbers* :if (oddp i) :sum i)
;;; Will result nil

(loop :for i :in *numbers* :if (oddp i) :sum i :into a)

;;; Return

(loop :for i :in *numbers* :if (oddp i) :sum i :into a :finally (return a))

;;; Sum even and odd

(loop :for i :in *numbers*
      :if (oddp i) :sum i :into odds
	:else :sum i :into evens
      :finally (return (list odds evens)))


(loop :for i :being :the :elements :of #(1 2 3) :collect i)

;;; loop in hash

(defparameter *hashmap* (make-hash-table))

(setf (gethash 'key1 *hashmap*) 'value1)

(setf (gethash 'key2 *hashmap*) 'value2)

(setf (gethash 'key3 *hashmap*) 'value3)

(loop :for i :being :the :hashkey :of *hashmap* :collect i)

(loop :for (x y) :in *pairs* :collect (list x (* y y)))

(loop :for (x y z) :in *pairs* :collect (list x (* y y) z))
