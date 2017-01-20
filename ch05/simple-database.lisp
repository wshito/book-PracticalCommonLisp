(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil "The database instance")

(defun add-record (cd) (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

; Three different ways to impement!
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun dump-db ()
  (mapcar (lambda (x) (format t "~{~a:~10t~a~%~}~%" x)) *db*))

(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

; P.50 Improving the User Interaction
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)    ; prints the prompt
  (force-output *query-io*)            ; flush the buffer
  (read-line *query-io*))              ; reads a line and return with a newline

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p (prompt-read "Ripped [y/n]: "))))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

; p.52 Saving and Loading the Database
(defun save-db (filename)
  (with-open-file (out filename               ; out is bound to the file stream
		       :direction :output     ; writing
		       :if-exists :supersede) ; overwrite
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)   ; input stream is the default
    (with-standard-io-syntax
      (setf *db* (read in)))))    ; 'read' corresponds to 'print'
