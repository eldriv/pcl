;;; I/O

(let ((in (open "/home/michael-adrian-villareal/test-for-14.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
	  while line do (format t "~a~%" line))
    (close in))) 

(defparameter *s* (open"/home/michael-adrian-villareal/test-for-14.txt"))

;;; Closing file
(let((stream (open "/home/michael-adrian-villareal/test-for-14.txt")))
  (close stream))


;;; I can examine these individual components of a pathname with the functions given below:

;; to examine the 1st and 2nd dir
(pathname-directory (pathname "/home/michael-adrian-villareal/test-for-14.txt"))


;; to test the specific file
(pathname-name(pathname "/home/michael-adrian-villareal/test-for-14.txt"))

;; to know what type file

(pathname-type(pathname "/home/michael-adrian-villareal/test-for-14.txt"))

;;; arbitrary pathname using make-pathname : relative

(make-pathname
 :directory '(:relative "backups")
 :defaults #P"/home/michael-adrian-villareal/test-for-14.txt")

(make-pathname :type "txt" :defaults input-file)`

