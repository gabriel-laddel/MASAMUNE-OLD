(in-package mm)

;;; books
;;; ============================================================================
;;; introduction to algorithms
;;; algorithm design
;;; art of computer programming
;;; hackers delight
;;; 
;;; websites 
;;; ============================================================================
;;; 'smart tool' haskell guy
;;;
;;; data structures
;;; ============================================================================
;;; - finger trees
;;; - red black trees
;;; - AVL trees

(defun start-programming-practice (habit)
  (record-event habit (event :started))
  (stumpwm::emacs)
  ;; XXX 2014-12-23T14:16:06+00:00 Gabriel Laddel
  ;; always throws
  (ignore-errors 
   (with-live-swank-connection 
       (swank::eval-in-emacs
	'(progn (find-file "~/quicklisp/local-projects/masamune/systems/programming-practice.lisp")
	  (delete-other-windows)))))
  (stumpwm::run-with-timer
   (* 20 60) nil 
   (lambda () 
     (with-live-swank-connection
	 (stumpwm::message-no-timeout "Time is almost up")
       (loop for i from 10 downto 0
	     finally (progn (record-event habit (event :finished))
			    (stumpwm::message-no-timeout "finished")
			    (mmg::run-or-focus-dashboard)))))))

(defun visualize-programming-practice (habit sheet)
  (declare (ignore habit))
  (format sheet "Problems remaining on project euler~%Algorithm problems completed today~%Avg. completion time per problem"))

(defun programming-practice-install ()
  (push (i 'habit
	   :name "Programming Practice"
	   :initialization-function 'start-programming-practice
	   :visualization-function 'visualize-programming-practice
	   :occurrence :daily)
	*habits*))

;;; Project Euler
;;; ============================================================================



(fiasco:define-test-package #:euler
  (:use #:mm #:fiasco))

(in-package #:euler)

;; (deftest 1 ()
;;   ( ))

;; (let* ((solution '(loop for n from 1 below 1000 when (or (= 0 (mod n 5)) (= 0 (mod n 3))) sum n))
;;        (answer ))
;;   (car (mm::split " " (rp (format nil "echo -n ~a  | md5sum" (eval solution))))))

(defun download-problems ()
  (unless (probe-file (qlpp "/project-euler/")) 
    (mkdir (qlpp "/project-euler/")))
  (download-url "http://kmkeen.com/local-euler/project_euler.zip" 
		(qlpp "project-euler/project-euler-extra-files.zip"))
  (download-url "http://kmkeen.com/local-euler/project_euler.txt" 
		(qlpp "project-euler/problems.txt")))

(defun parse-problems-textfile (&optional (output-file "/tmp/euler-problems.lisp"))
  (let* ((problem-strings (mm::->> "/project-euler/problems.txt"
				   (mm::qlpp)
				   (mm::slurp-file)
				   (mm::split "Problem \\d*\\n=========[=]*")
				   (rest)))
	 (*print-level* nil) 
	 (*print-length* nil))    
    (with-open-file (stream output-file :direction :output
					:if-exists :supersede
					:if-does-not-exist :create)
      (loop for problem-string in problem-strings
	    for i = 1 then (1+ i)
	    do (format stream "~%~%~S"
		       (let* ((lpos (mm::ssearch "Visible links" problem-string t))
			      (apos (mm::ssearch "Answer: " problem-string t))
			      (links (when lpos 
				       (mm::->> (subseq problem-string lpos apos)
						(mm::split "\\n")
						(rest)
						(mapcar (lambda (k) (drop 3 (mm::trim-dwim k))))
						(remove-if #'null)))))
			 (destructuring-bind (prompt answer)
			     (mapcar 'mm::trim-dwim (mm::split "Answer: " problem-string))
			   (list :problem i :prompt prompt :answer answer :links links))))))))

;; (defun synthesize-clim-friendly-problems ()
;;   )

;; (rp (format nil "echo -n '~s' | md5sum"))


