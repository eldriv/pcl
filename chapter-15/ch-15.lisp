;; ch15 API, A Portable Pathname Library - cl
;; Pre- requisites : Understanding implementation-specif(component-present-p 'some-value)
;; read-time conditinalization
;; #+ reader reads next expression normally while #- skips the following expression if the following reader expression is true

(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))

;;; naive use of listing file with deirectory and wild pathnames

(print (directory (make-pathname :name :wild :type :wild :defaults "/home/michael-adrian-villareal/src/pcl/")))

;;; some helper functions to help simplify list-directories
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;;; to check whether a pathname is already in directory form
(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))))

;;; converts any pathname to a directory form pathname
(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name
	 nil
	 :type
	 nil
	 :defaults pathname)
	pathname)))

;;; that takes a pathname in either directory or file form and returns a proper wildcard for the given imple-
;;; mentation using read-time conditionalization to make a pathname with a :wild type component
;;; in all implementations except for CLISP and NIL in CLISP.

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

;; Portable implementation

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	       (name-and-type (pathname (first (last directory)))))
	  (make-pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
	 (cond
	   ((directory-pathname-p name)
	    (when (and directories (funcall test name))
	      (funcall fn name))
	    (dolist (x (list-directory name)) (walk x)))
	   ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))



