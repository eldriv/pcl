;;; a variable where all generated output will store here.

(defvar *html-output* *standard-output*)

;; (defun emit-html (html)
;;   "An interpreter for the literal HTML language."
;;   (write-sequence html *html-output*))

;; (defmacro html (html)
;;   "A compiler for the literal HTML language."
;;   `(write-sequence ,html *html-output*))


;;; PUBLIC API

(defmacro html (&whole whole &body body)
  (declare (ignore body))
  `(if *pretty*
     (macrolet ((html (&body body) (codegen-html (sexp->ops body) t)))
       (let ((*html-pretty-printer* (get-pretty-printer))) ,whole))
     (macrolet ((html (&body body) (codegen-html (sexp->ops body) nil)))
       ,whole)))

;;; Tests whether a given object is self-evaluating for FOO’s purposes.
(defun self-evaluating-p (form)
  (and (atom form) (if (symbolp form) (keywordp form) t)))

	;;;tests whether a given object matches either of these syntaxes
(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (or (funcall test (car form))
	   (and (consp (car form)) (funcall test (caar form))))))


(defun parse-explicit-attributes-sexp (sexp)
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attributes-sexp (sexp)
  (loop with tag = (first sexp)
	for rest on (rest sexp) by #'cddr
	while (and (keywordp (first rest)) (second rest))
	when (second rest)
	  collect (first rest) into attributes and
	collect (second rest)into attributes
	end
	finally (return (values tag attributes rest))))

;;; That takes a form and parses it into three elements, the tag, the attributes plist, and the body list, returning them as multiple values

(defun parse-cons-form (sexp)
  (if (consp (first sexp))
      (parse-explicit-attributes-sexp sexp)
      (parse-implicit-attributes-sexp sexp)))

;;; STRING ESCAPING

;;; Accepts a single character and returns a string containing a character reference entity for that character

(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))



(defparameter *element-escapes* "<>&")
(defparameter *attribute-escapes* "<>&\"'")
(defvar *escapes* *element-escapes*)

(defun escape (in to-escape)
  (flet ((needs-escape-p (char) (find char to-escape)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
	    for pos = (position-if #'needs-escape-p in :start start)
	    do (write-sequence in out :start start :end pos)
	    when pos do (write-sequence (escape-char (char in pos)) out)
	      while pos))))

;;; INDENTING-PRINTER

(defclass indenting-printer ()
  ((out
    :accessor out :initarg :out)
   (beginning-of-line-p
    :accessor beginning-of-line-p :initform t)
   (indentation
    :accessor indentation :initform 0)
   (indenting-p
    :accessor indenting-p :initform t)))

;;;The last two functions in the indenting-printer API are emit-newline and emit-freshline, which are both used to emit a newline character, similar to the ~% and ~& FORMAT directives. That is, the only difference is that emit-newline always emits a newline, while emit-freshline does so only if beginning-of-line-p is false.

(defun emit-newline (ip)
  (write-char #\Newline (out ip))
  (setf (beginning-of-line-p ip) t))

(defun emit-freshline (ip)
  (unless (beginning-of-line-p ip) (emit-newline ip)))

;;;The helper indent-if-necessary checks beginning-of-line-p and indenting-p to deter- mine whether it needs to emit indentation and, if they’re both true, emits as many spaces as indicated by the value of indentation.

(defun indent-if-necessary (ip)
  (when (and (beginning-of-line-p ip) (indenting-p ip))
    (loop repeat (indentation ip) do (write-char #\Space (out ip)))
    (setf (beginning-of-line-p ip) nil)))

;;; To emit a string that’s known not to contain any newlines.

(defun emit/no-newlines (ip string &key (start 0) end)
  (indent-if-necessary ip)
  (write-sequence string (out ip) :start start :end end)
  (unless (zerop (- (or end (length string)) start))
    (setf (beginning-of-line-p ip) nil)))

;;; Emit function that takes the printer and a string, keeping track of when it emits a newline so it can reset the beginning of line p slot

(defun emit (ip string)
  (loop for start = 0 then (1+ pos)
	for pos = (position #\Newline string :start start)
	do (emit/no-newlines ip string :start start :end pos)
	when pos do (emit-newline ip)
	  while pos))


;;; HTML-PROCESSOR-INTERFACE
;;;BACKEND INTERFACE 8 GENERIC FUNCTIONS
;;;NOTE: define the abstract operations that are used by the FOO language processors and won’t always be implemented in terms of calls to the indenting-printer functions.

(defgeneric raw-string (processor string &optional newlines-p))
(defgeneric newline (processor))
(defgeneric freshline (processor))
(defgeneric indent (processor))
(defgeneric unindent (processor))
(defgeneric toggle-indenting (processor))
(defgeneric embed-value (processor value))
(defgeneric embed-code (processor code))

;;; HTML-PRETTY-PRINTER

(defclass html-pretty-printer ()
  ((printer
    :accessor printer :initarg :printer)
   (tab-width
    :accessor tab-width :initarg :tab-width :initform 2)))

;;;invoked with strings that don’t contain newlines, so the default behavior is to use emit/no-newlines unless the caller specifies a non-NIL newlines-p argument.
(defmethod raw-string ((pp html-pretty-printer) string &optional newlines-p)
  (if newlines-p
      (emit (printer pp) string)
      (emit/no-newlines (printer pp) string)))

(defvar *pretty* t)
	;;;—one to hold an instance of indenting-printer and one to hold the tab width—the number of spaces you want to increase the indentation for each level of nesting of HTML elements
(defvar *html-pretty-printer* nil)


(defmethod newline ((pp html-pretty-printer))
  (emit-newline (printer pp)))

(defmethod freshline ((pp html-pretty-printer))
  (when *pretty* (emit-freshline (printer pp))))

(defmethod indent ((pp html-pretty-printer))
  (when *pretty*
    (incf (indentation (printer pp)) (tab-width pp))))

(defmethod unindent ((pp html-pretty-printer))
  (when *pretty*
    (decf (indentation (printer pp)) (tab-width pp))))

(defmethod toggle-indenting ((pp html-pretty-printer))
  (when *pretty*
    (with-slots (indenting-p) (printer pp)
      (setf indenting-p (not indenting-p)))))

;;;used only by the FOO COMPILER to generate code that will emit the value of CL expression while embed code is used to embed a bit of code to be run and its result discarded.

(defmethod embed-value ((pp html-pretty-printer) value)
  (error "Can't embed values when interpreting. Value: ~s" value))
(defmethod embed-code ((pp html-pretty-printer) code)
  (error "Can't embed code when interpreting. Code: ~s" code))

;;;basic evaluation rule

(defparameter *empty-elements*
  '(:area :base :br :col :hr :img :input :link :meta :param))

(defparameter *preserve-whitespace-elements* '(:pre :script :style))

(defun preserve-whitespace-p (tag) (find tag *preserve-whitespace-elements*))

(defparameter *xhtml* nil)

(defun empty-element-p (tag) (find tag *empty-elements*))

(defparameter *block-elements*
  '(:body :colgroup :dl :fieldset :form :head :html :map :noscript :object
    :ol :optgroup :pre :script :select :style :table :tbody :tfoot :thead
    :tr :ul))

(defparameter *paragraph-elements*
  '(:area :base :blockquote :br :button :caption :col :dd :div :dt :h1
    :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
    :td :textarea :th :title))

(defparameter *inline-elements*
  '(:a :abbr :acronym :address :b :bdo :big :cite :code :del :dfn :em
    :i :img :ins :kbd :label :legend :q :samp :small :span :strong :sub
    :sup :tt :var))

(defun block-element-p (tag) (find tag *block-elements*))

(defun paragraph-element-p (tag) (find tag *paragraph-elements*))

(defun sexp-html-p (form)
  (or (self-evaluating-p form) (cons-form-p form)))

(defun emit-attributes (processor attributes)
  (loop for (k v) on attributes by #'cddr do
    (raw-string processor (format nil " ~(~a~)='" k))
    (let ((*escapes* *attribute-escapes*))
      (process processor (if (eql v t) (string-downcase k) v)))
    (raw-string processor "'")))

(defun emit-open-tag (processor tag body-p attributes)
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor))
  (raw-string processor (format nil "<~(~a~)" tag))
  (emit-attributes processor attributes)
  (raw-string processor (if (and *xhtml* (not body-p)) "/>" ">")))


(defun emit-close-tag (processor tag body-p)
  (unless (and (or *xhtml* (empty-element-p tag)) (not body-p))
    (raw-string processor (format nil "</~(~a~)>" tag)))
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor)))

(defun emit-element-body (processor tag body)
  (when (block-element-p tag)
    (freshline processor)
    (indent processor))
  (when (preserve-whitespace-p tag) (toggle-indenting processor))
  (dolist (item body) (process processor item))
  (when (preserve-whitespace-p tag) (toggle-indenting processor))
  (when (block-element-p tag)
    (unindent processor)
    (freshline processor)))

(defun process-cons-sexp-html (processor form)
  (when (string= *escapes* *attribute-escapes*)
    (error "Can't use cons forms in attributes: ~a" form))
  (multiple-value-bind (tag attributes body) (parse-cons-form form)
    (emit-open-tag
     processor tag body attributes)
    (emit-element-body processor tag body)
    (emit-close-tag
     processor tag body)))

(defun process-sexp-html (processor form)
  (if (self-evaluating-p form)
      (raw-string processor (escape (princ-to-string form) *escapes*) t)
      (process-cons-sexp-html processor form)))

;;; HTML PROCESSOR

(defun process (processor form)
  (cond
    ((special-form-p form) (process-special-form processor form))
    ((macro-form-p form)(process processor (expand-macro-form form)))
    ((sexp-html-p form) (process-sexp-html processor form))
    ((consp form)       (embed-code processor form))
    (t                  (embed-value processor form))))


;;; Helper for PUBLIC API

(defun get-pretty-printer ()
  (or *html-pretty-printer*
      (make-instance
       'html-pretty-printer
       :printer (make-instance 'indenting-printer :out *html-output*))))

;;; PUBLIC API

(defun emit-html (sexp) (process (get-pretty-printer) sexp))


(defmacro with-html-output ((stream &key (pretty *pretty*)) &body body)
  `(let* ((*html-output* ,stream)
	  (*pretty* ,pretty))
     ,@body))

#|
COMPILER
it will show here how to implement a macro that compiles FOO expressions into Common Lisp so we can embed HTML generation code directly into our Lisp Programs.
Chapter 31 - The Compiler, Last Chapter.
In FOO, the compiler is a Common Lisp macro that translates FOO into Common lisp so it can be embedded in a Common lisp Program.
When The compiler is a Common Lisp Macro, you also have the(defun special-form-p advantage that it's easy for the language understood by the compiler to contain embedded Common Lisp -- The compiler just has to recognize it and embed it in the right place in the generated code. The Foo compiiler will take advantage of this capability

The other significant difference between the compiler and the interpreter is that the
compiler can embed Lisp forms in the code it generates. To take advantage of that, you need to
modify the process function so it calls the embed-code and embed-value functions when asked
to process an expression that’s not a FOO form.
|#

;;; OPS BUFFER

;;;two functions that slightly abstract the vector you’ll use to save ops in the first two phases of compilation.
(defun make-op-buffer () (make-array 10 :adjustable t :fill-pointer 0))
(defun push-op (op ops-buffer) (vector-push-extend op ops-buffer))

;;; BACK-END IMPLEMENTATION OF COMPILER

(defclass html-compiler ()
  ((ops :accessor ops :initform (make-op-buffer))))

(defmethod raw-string ((compiler html-compiler) string &optional newlines-p)
  (push-op `(:raw-string ,string ,newlines-p) (ops compiler)))

(defmethod newline ((compiler html-compiler))
  (push-op '(:newline) (ops compiler)))

(defmethod freshline ((compiler html-compiler))
  (push-op '(:freshline) (ops compiler)))

(defmethod indent ((compiler html-compiler))
  (push-op `(:indent) (ops compiler)))

(defmethod unindent ((compiler html-compiler))
  (push-op `(:unindent) (ops compiler)))

(defmethod toggle-indenting ((compiler html-compiler))
  (push-op `(:toggle-indenting) (ops compiler)))

(defmethod embed-value ((compiler html-compiler) value)
  (push-op `(:embed-value ,value ,*escapes*) (ops compiler)))

(defmethod embed-code ((compiler html-compiler) code)
  (push-op `(:embed-code ,code) (ops compiler)))

;;;first phase of the compilier, sexp->ops
(defun sexp->ops (body)
  (loop with compiler = (make-instance 'html-compiler)
	for form in body do (process compiler form)
	finally (return (ops compiler))))

;;; Next phase of compiler: takes a vector of ops and returns a new vector containing the optimized version.

(defun optimize-static-output (ops)
  (let ((new-ops (make-op-buffer)))
    (with-output-to-string (buf)
      (flet ((add-op (op)
	       (compile-buffer buf new-ops)
	       (push-op op new-ops)))
	(loop for op across ops do
	  (ecase (first op)
	    (:raw-string (write-sequence (second op) buf))
	    ((:newline :embed-value :embed-code) (add-op op))
	    ((:indent :unindent :freshline :toggle-indenting)
	     (when *pretty* (add-op op)))))
	(compile-buffer buf new-ops)))
    new-ops))

;; ;;;The last step is to translate the ops into the corresponding Common Lisp code. This phase
;; also pays attention to the value of *pretty*. When *pretty* is true, it generates code that invokes
;; the backend generic functions on *html-pretty-printer*, which will be bound to an instance
;; of html-pretty-printer. When *pretty* is NIL, it generates code that writes directly to
;; *html-output*, the stream to which the pretty printer would send its output.

(defun compile-buffer (buf ops)
  (loop with str = (get-output-stream-string buf)
	for start = 0 then (1+ pos)
	for pos = (position #\Newline str :start start)
	when (< start (length str))
	  do (push-op `(:raw-string ,(subseq str start pos) nil) ops)
	when pos do (push-op '(:newline) ops)
	  while pos))

(defun codegen-html (ops pretty)
  (let ((*pretty* pretty))
(defun codegen-html (ops pretty)
  (let ((*pretty* pretty))
    `(progn ,@(generate-code (optimize-static-output ops)) nil)))

    `(progn ,@(generate-code (optimize-static-output ops)) nil)))

(defun generate-code (ops)
  (loop for op across ops collect (apply #'op->code op)))

(defgeneric op->code (op &rest operands))

(defmethod op->code ((op (eql :raw-string)) &rest operands)
  (destructuring-bind (string check-for-newlines) operands
    (if *pretty*
	`(raw-string *html-pretty-printer* ,string ,check-for-newlines)
	`(write-sequence ,string *html-output*))))

(defmethod op->code ((op (eql :newline)) &rest operands)
  (if *pretty*
      `(newline *html-pretty-printer*)
      `(write-char #\Newline *html-output*)))    

(defmethod op->code ((op (eql :freshline)) &rest operands)
  (if *pretty*
      `(freshline *html-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :indent)) &rest operands)
  (if *pretty*
      `(indent *html-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :unindent)) &rest operands)
  (if *pretty*
      `(unindent *html-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :toggle-indenting)) &rest operands)
  (if *pretty*
      `(toggle-indenting *html-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :embed-value)) &rest operands)
  (destructuring-bind (value escapes) operands
    (if *pretty*
	(if escapes
	    `(raw-string *html-pretty-printer* (escape (princ-to-string ,value) ,escapes) t)
	    `(raw-string *html-pretty-printer* (princ-to-string ,value) t))
	(if escapes
	    `(write-sequence (escape (princ-to-string ,value) ,escapes) *html-output*)
	    `(princ ,value *html-output*)))))

(defmethod op->code ((op (eql :embed-code)) &rest operands)
  (first operands))

;;;FOO SPECIAL OPERATORS

(defun special-form-p (form)
  (and (consp form) (symbolp (car form)) (get (car form) 'html-special-operator)))


;;;macro for special operators
(defmacro define-html-special-operator (name (processor &rest other-parameters)
					&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',name 'html-special-operator)
	   (lambda (,processor ,@other-parameters) ,@body))))

(defun process-special-form (processor form)
  (apply (get (car form) 'html-special-operator) processor (rest form)))


;;; Special Forms

(define-html-special-operator :attribute (processor &rest body)
  (let ((*escapes* *attribute-escapes*))
    (loop for exp in body do (process processor exp))))

(define-html-special-operator :print (processor form)
  (cond
    ((self-evaluating-p form)
     (warn "Redundant :print of self-evaluating form ~s" form)
     (process-sexp-html processor form))
    (t
     (embed-value processor form))))

(define-html-special-operator :format (processor &rest args)
  (if (every #'self-evaluating-p args)
      (process-sexp-html processor (apply #'format nil args))
      (embed-value processor `(format nil ,@args))))

(define-html-special-operator :newline (processor)
  (newline processor))

(define-html-special-operator :progn (processor &rest body)
  (loop for exp in body do (process processor exp)))

;;; Foo Macros

(defun generate-macro-with-attributes (name attribute-args args body)
  (with-gensyms (attributes form-body)
    (if (symbolp attribute-args) (setf attribute-args `(&rest ,attribute-args)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'html-macro-wants-attributes) t)
       (setf (get ',name 'html-macro)
	     (lambda (,attributes ,form-body)
	       (destructuring-bind (,@attribute-args) ,attributes
		 (destructuring-bind (,@args) ,form-body
		   ,@body)))))))
(defun generate-macro-no-attributes (name args body)
  (with-gensyms (form-body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'html-macro-wants-attributes) nil)
       (setf (get ',name 'html-macro)
	     (lambda (,form-body)
	       (destructuring-bind (,@args) ,form-body ,@body))))))
  
(defmacro define-html-macro (name (&rest args) &body body)
  (multiple-value-bind (attribute-var args)
      (parse-html-macro-lambda-list args)
    (if attribute-var
	(generate-macro-with-attributes name attribute-var args body)
	(generate-macro-no-attributes name args body))))

(defun generate-macro-with-attributes (name attribute-args args body)
  (with-gensyms (attributes form-body)
    (if (symbolp attribute-args) (setf attribute-args `(&rest ,attribute-args)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'html-macro-wants-attributes) t)
       (setf (get ',name 'html-macro)
	     (lambda (,attributes ,form-body)
	       (destructuring-bind (,@attribute-args) ,attributes
		 (destructuring-bind (,@args) ,form-body
		   ,@body)))))))

(defun parse-html-macro-lambda-list (args)
  "Parse a lambda list that can include the &attributes lambda-list-keyword."
  (let ((attr-cons (member '&attributes args)))
    (values 
     (cadr attr-cons)
     (nconc (ldiff args attr-cons) (cddr attr-cons)))))

(defun expand-macro-form (form)
  (if (or (consp (first form))
          (get (first form) 'html-macro-wants-attributes))
      (multiple-value-bind (tag attributes body) (parse-cons-form form)
	(funcall (get tag 'html-macro) attributes body))
      (destructuring-bind (tag &body body) form
	(funcall (get tag 'html-macro) body))))

;;; PUBLIC API

(defmacro in-html-style (syntax)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (case syntax
      (:html (setf *xhtml* nil))
      (:xhtml (setf *xhtml* t)))))
