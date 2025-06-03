;;; Numbers, string and chars

#|
Character Comparison Functions
Numeric Analog                  Case-Sensitive                  Case-Insensitive
=                                   CHAR=                           CHAR-EQUAL
/=                                  CHAR/=                        CHAR-NOT-EQUAL
<                                   CHAR<                           CHAR-LESSP
>                                   CHAR>                         CHAR-GREATERP
<=                                  CHAR<=                       CHAR-NOT-GREATERP
>=                                  CHAR>=                        CHAR-NOT-LESSP
|#

;;; Character Comparison

(defun compare-chars (char1 char2)
  (cond
    ((char> char1 char2) :greater-than)
    ((char< char1 char2) :less-than)
    ((char= char1 char2) :equal-to)))

(defun size-of-char (char)
  (cond
    ((upper-case-p char) :big)
    ((lower-case-p char) :small)
    (t                   :no-size)))

(defun change-size-of-char (char wanted-size)
  (let ((x (char-upcase char))
        (y (char-downcase char)))
    (cond
      ((eq wanted-size :big) x)
      ((eq wanted-size :small) y))))

(defun type-of-char (char)
  (cond
    ((alpha-char-p char) :alpha)
    ((digit-char-p char) :numeric)
    ((char= char #\Space) :space)
    ((char= char #\Newline) :newline)
    (t :unknown)))
