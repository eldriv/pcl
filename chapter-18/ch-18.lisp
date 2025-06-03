;; chapter 18
;; Few format recipes

(loop for cons on list
      do (format t "~a" (car cons))
      when (cdr cons) do (format t ", "))

(format t "~{~a~^, ~}" list)

;; FORMAT RECIPES

CL-USER> (format t "~d" 1000000)

1000000
NIL
CL-USER> (format t "~:d" 1000000)

1,000,000
NIL

CL-USER> (format t "~@d" 1000000)

+1000000
NIL

CL-USER> (format t "~:@d" 1000000)

+1,000,000
NIL

(format nil "The value is: ~a" 10)
→ "The value is: 10"
(format nil "The value is: ~a" "foo")
→ "The value is: foo"
(format nil "The value is: ~a" (list 1 2 3))
→ "The value is: (1 2 3)"


(format nil "~d" 1000000) → "1000000"
As I mentioned previously, with a colon modifier it adds commas.
(format nil "~:d" 1000000) → "1,000,000"
And with an at-sign modifier, it always prints a sign.
(format nil "~@d" 1000000) → "+1000000"
And the two modifiers can be combined.
(format nil "~:@d" 1000000) → "+1,000,000"

(format nil "~12d" 1000000)
→"1000000"
(format nil "~12,'0d" 1000000)
→ "000001000000"


(format nil "~:d" 100000000)
→ "100,000,000"
(format nil "~,,'.,4:d" 100000000) → "1.0000.0000"

(format nil "~x" 1000000) → "f4240"
(format nil "~o" 1000000) → "3641100"
(format nil "~b" 1000000) → "11110100001001000000"

;;float
(format nil "~f" pi)
→ "3.141592653589793d0"
(format nil "~,4f" pi) → "3.1416"
(format nil "~e" pi)
→ "3.141592653589793d+0"
(format nil "~,4e" pi) → "3.1416d+0"

;;english language directives

(format nil "~r file~:p" 1) → "one file"
(format nil "~r file~:p" 10) → "ten files"
(format nil "~r file~:p" 0) → "zero files"


(format nil "~r famil~:@p" 1) → "one family"
(format nil "~r famil~:@p" 10) → "ten families"
(format nil "~r famil~:@p" 0) → "zero families"

(format nil "file~p" 1) → "file"
(format nil "file~p" 10) → "files"
(format nil "file~p" 0) → "files"

(format nil "~(~a~)" "tHe Quick BROWN foX") → "the quick brown fox"
(format nil "~@(~a~)" "tHe Quick BROWN foX") → "The quick brown fox"
(format nil "~:(~a~)" "tHe Quick BROWN foX") → "The Quick Brown Fox"
(format nil "~:@(~a~)" "tHe Quick BROWN foX") → "THE QUICK BROWN FOX"

(format nil *list-etc*)
→ "NONE."
(format nil *list-etc* 'a)
→ "A."
(format nil *list-etc* 'a 'b)
→ "A and B."
(format nil *list-etc* 'a 'b 'c)
→ "A, B and C."
(format nil *list-etc* 'a 'b 'c 'd)
→ "A, B, C, etc."
(format nil *list-etc* 'a 'b 'c 'd 'e) → "A, B, C, etc."


(format nil "~@[x = ~a ~]~@[y = ~a~]" 10 20)
→ "x = 10 y = 20"
(format nil "~@[x = ~a ~]~@[y = ~a~]" 10 nil) → "x = 10 "
(format nil "~@[x = ~a ~]~@[y = ~a~]" nil 20) → "y = 20"
(format nil "~@[x = ~a ~]~@[y = ~a~]" nil nil) → ""

;iteration
(format nil "~{~a, ~}" (list 1 2 3)) → "1, 2, 3, "


(defparameter *list-etc*
"~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].")
