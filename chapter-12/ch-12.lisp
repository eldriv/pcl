;;; Chapter 12 lisp processing

(defparameter *cons* (cons 1 2))
(defparameter *list* (list 1 2 3 4))
(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))
(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(defparameter *list* (list 4 3 2 1))
#|
LAST      Returns the last cons cell in a list. With an integer, argument returns the
          last n cons cells.
BUTLAST   Returns a copy of the list, excluding the last cons cell. With an integer
          argument, excludes the last n cells.
NBUTLAST  The recycling version of BUTLAST; may modify and return the argument
          list but has no reliable side effects.
LDIFF     Returns a copy of a list up to a given cons cell.
TAILP     Returns true if a given object is a cons cell that’s part of the structure of
          a list.
LIST      *Builds a list to hold all but the last of its arguments and then makes the
          last argument the CDR of the last cell in the list. In other words, a cross
          between LIST and APPEND.
MAKE-LIST Builds an n item list. The initial elements of the list are NIL or the value
          specified with the :initial-element keyword argument.
REVAPPEND Combination of REVERSE and APPEND; reverses first argument as with
          REVERSE and then appends the second argument.
NRECONC   Recycling version of REVAPPEND; reverses first argument as if by NREVERSE
          and then appends the second argument. No reliable side effects.
CONSP     Predicate to test whether an object is a cons cell.
ATOMP     Predicate to test whether an object is not a cons cell.
LISTP     Predicate to test whether an object is either a cons cell or NIL.
NULL      Predicate to test whether an object is NIL. Functionally equivalent to NOT
          but stylistically preferable when testing for an empty list as opposed to
          boolean false.LASTReturns the last cons cell in a list. With an integer, argument returns the
          last n cons cells.
#|
