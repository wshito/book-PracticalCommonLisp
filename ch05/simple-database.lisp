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

; (defun dump-db ()
;   (mapcar (lambda (x) (format t "~{~a:~10t~a~%~}~%" x)) *db*))

; (defun dump-db ()
;   (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

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

; Querying the Database p.54
(defun select-by-artist (artist)
  (remove-if-not (lambda (cd) (equal (getf cd :artist) artist))
		 *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

; test select function
(select (lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))

(defun artist-selector (artist)
  (lambda (cd) (equal (getf cd :artist) artist)))

; test
(select (artist-selector "Dixie Chicks"))

(defun where (&key title artist rating (ripped nil ripped-p))
  (lambda (cd)
    (and
     (if title    (equal (getf cd :title)  title)  t)
     (if artist   (equal (getf cd :artist) artist) t)
     (if rating   (equal (getf cd :rating) rating) t)
     (if ripped-p (equal (getf cd :ripped) ripped) t))))

; test
(select (where :artist "Dixie Chicks" :rating 8))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 (lambda (row)
		  (when (funcall selector-fn row)
		    (if title    (setf (getf row :title)  title))
		    (if artist   (setf (getf row :artist) artist))
		    (if rating   (setf (getf row :rating) rating))
		    (if ripped-p (setf (getf row :ripped) ripped)))
		  row)
	 *db*)))
; test
					; (update (where :artist "Dixie Chicks") :rating 11)

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
