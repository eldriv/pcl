;;; Simple Database

(defvar *db* nil)

(defun load-db (filename)
(with-open-file (in filename)
(with-standard-io-syntax
(setf *db* (read in)))))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;;; Add record
(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{ ~a:~10t~a~%~}~%" cd)))

;;; While our add-record function works fine for adding records, it’s a bit Lispy for the casual user.And if they want to add a bunch of records, it’s not very convenient. So youmay want to write
;;; A function to prompt the user for information about a set of CDs.

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;;; You can combine your existing make-cd function with prompt-read to build a function that makes a new CD record from data it gets by prompting for each value in turn

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

;;; You can finish the “add a bunch of CDs” interface by wrapping prompt-for-cd in a function that loops until the user is done. You can use the simple form of the LOOP macro

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
	(if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;;; Saving

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;;; Select
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd)
      (equal (getf cd :artist) artist)))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title
	   (equal (getf cd :title) title) t)
       (if artist
	   (equal (getf cd :artist) artist) t)
       (if rating
	   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;;; Update

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title
		   (setf (getf row :title) title))
	       (if artist
		   (setf (getf row :artist) artist))
	       (if rating
		   (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db*)))
