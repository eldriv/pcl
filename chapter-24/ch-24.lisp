(defpackage :com.gigamonkeys.binary-data
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
  (:export
   :define-binary-class
   :define-tagged-binary-class
   :define-binary-type
   :read-value
   :write-value
   :*in-progress-objects*
   :parent-of-type
   :current-binary-object
   :+null+))

(defconstant +null+ (code-char 0))

(defvar *in-progress-objects* nil)


(define-binary-class generic-frame ()
  ((id (iso-8859-1-string :length 3))
   (size u3)
   (data (raw-bytes :bytes size))))

(define-tagged-binary-class id3-frame ()
  ((id
    (iso-8859-1-string :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))


(define-binary-type iso-8859-1-string (length)
  (:reader (in)
	   (let ((string (make-string length)))
	     (dotimes (i length)
	       (setf (char string i) (code-char (read-byte in))))
	     string))
  (:writer (out string)
	   (dotimes (i length)
	     (write-byte (char-code (char string i)) out))))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))

(defun current-binary-object () (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

(defun read-u2 (in)
  (+ (* (read-byte in) 256) (read-byte in)))

(defun read-u2 (in)
  "Reads a 16-bit unsigned integer from the binary stream IN."
  (+ (* (read-byte in) 256) (read-byte in)))

(defun read-u2-from-file (file-path)
  "Reads 16-bit unsigned integers from a binary file."
  (with-open-file (in file-path :element-type '(unsigned-byte 8))
    (loop :for i :from 1 to (file-length in) :by 10
          :collect (read-u2 in))))

(defun print-u2-list (u2-list)
  "Prints a list of 16-bit unsigned integers."
  (format t "List of 16-bit unsigned integers: ")
  (loop :for u2 :in u2-list :do (format t "~a " u2))
  (format t "~%"))

(defun show-unsigned ()
  "Main function to demonstrate the usage of read-u2."
  (let* ((file-path  "/home/michael-adrian-villareal/snap/firefox/common/.mozilla/firefox/wcrcgxfd.default-1709706975681/SiteSecurityServiceState.bin")
         (u2-list (read-u2-from-file file-path)))
    (print-u2-list u2-list)))

(defvar *num-bits* 0)
(setf (ldb (byte 8 0) *num*) 128)
(setf (ldb (byte 8 8) *num-bits*) 255)

(defun read-u2 (in)
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    u2))

(defun write-u2 (out value)
  (write-byte (ldb (byte 8 8) value) out)
  (write-byte (ldb (byte 8 0) value) out))

(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte in))
	  until (char= char +null+) do (write-char char s))))

(defun write-null-terminated-ascii (string out)
  (loop for char across string
	do (write-byte (char-code char) out))
  (write-byte (char-code +null+) out))

(defclass id3-tag ()
  ((identifier ;; This slot represents the identifier of the ID3 tag.
    :initarg :identifier
    :accessor identifier)
   (major-version :initarg :major-version :accessor major-version) ;;This slot represents the major version of the ID3 tag.
   (revision ;;  This slot represents the revision of the ID3 tag.
    :initarg :revision
    :accessor revision)
   (flags ;;  This slot represents any flags associated with the ID3 tag.
    :initarg :flags
    :accessor flags)
   (size ;;  This slot represents the size of the ID3 tag.
    :initarg :size
    :accessor size)
   (frames ;; This slot represents the frames (metadata entries) contained within the ID3 tag.
    :initarg :frames
    :accessor frames)))

(defun read-id3-tag (in)
  (let ((tag (make-instance 'id3-tag)))
    (with-slots (identifier major-version revision flags size frames) tag
      (setf identifier
	    (read-iso-8859-1-string in :length 3))
      (setf major-version (read-u1 in))
      (setf revision
	    (read-u1 in))
      (setf flags
	    (read-u1 in))
      (setf size
	    (read-id3-encoded-size in))
      (setf frames
	    (read-id3-frames in :tag-size size)))
    tag))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x) (if (listp x) x (list x)))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (get ',name 'slots) ',(mapcar #'first slots))
  (setf (get ',name 'superclasses) ',superclasses))

;;;3 helper functions for accessing the information
(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
	nconc (direct-slots super)
	nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-sloto
	  name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))


#|
Alright, let's break this down for simplicity: Strings in Binary Files

    Binary Files: These are files that store data in a format that computers can understand directly, using 0s and 1s.
    Textual Strings: These are just sequences of characters, like letters, numbers, and symbols.

    Decoding and Encoding: When we read or write strings in a binary file, we can't do it directly. We have to convert them into a format that the computer can understand, and vice versa.

    Character Code and Encoding:
    Character Code: It's like a map that tells us which number represents which character. For example, in ASCII, the number 65 represents the letter 'A'.
    Character Encoding: It tells us how these numbers are stored as bytes in a file. For simple codes like ASCII, each character is just one byte.

    Types of Encoding:
    Single-Byte Encoding: Each character is represented by one byte. Simple and easy.
    Double-Byte Encoding: Characters are represented by two bytes, which might need extra information to know their order.
    Variable-Width Encoding: Different characters can take different numbers of bytes. This can save space but is more complicated.

    UTF-8 Encoding: It's a type of variable-width encoding, often used for storing Unicode characters (which includes characters from many different languages). It's     efficient because it uses fewer bytes for common characters but c    an use more bytes for less common ones.

    Practical Example:
    If your text only uses characters that fit in the ASCII range (0-127), UTF-8 encoding will look just like ASCII encoding, using one byte per character.
    But if your text includes characters outside the ASCII range, UTF-8 might use more bytes per character, especially for less common characters.

So, basically, when dealing with strings in binary files, we need to know how characters are mapped to numbers and how those numbers are stored as bytes, which can vary depending on the encoding used.
|#