(in-package #:mm)

;;; Programming Practice 
;;; ============================================================================
;;;
;;; The implementation is going to change significantly with the passage of time
;;; but the goal will remain the same - to supply daily practice on any
;;; programming subject that the student may have had trouble with in addition
;;; to challenging exercises he may not be wholly prepared for.
;;; 
;;; The rational behind the latter is to leave the student with a hunger for
;;; more information.
;;; 
;;; books
;;; =====
;;; introduction to algorithms
;;; algorithm design
;;; art of computer programming
;;; hackers delight
;;; Algorithms, Robert Sedgewick
;;; The Algorithm Design Manual, Steven S Skiena
;;; 
;;; websites 
;;; ========
;;; http://www.gibiansky.com/
;;; pvk.ca
;;; http://blog.sigfpe.com/
;;; http://www.coranac.com/
;;;
;;; data structures
;;; ===============
;;; - finger trees
;;; - red black trees
;;; - AVL trees
;;; 
;;; Project Euler
;;; ============================================================================
;;; 
;;; TODO 2015-01-07T01:32:44+00:00 Gabriel Laddel
;;; * check for new problems periodically.

(defvar *euler-dir* #P"~/.masamune/habits/project-euler/")
(defvar *euler-problem-id* nil
  "names a problem # used to identify the focused problem")

(unless (probe-file *euler-dir*) (mkdir *euler-dir*))
(unless (probe-file (merge-pathnames *euler-dir* "solutions.lisp"))
  (create-project-euler-scaffolding))

(defun sorted-euler-problems ()
  (sort (filter (lambda (o) (with-slots (prompt links solution problem-number) o
			 (not (every 'null (list prompt links solution problem-number)))))
		(manardb::retrieve-all-instances 'euler-problem))
	(lambda (p1 p2) (< (problem-number p1) (problem-number p2)))))

(defun euler-problem (problem-number)
  (find-if (lambda (k) (= problem-number (problem-number K))) (sorted-euler-problems)))

(defun next-logical-unsolved-problem ()
  (loop for p in (sorted-euler-problems) unless (solved? p) return p))

(defun link-type (link-string)
  (cond ((ssearch "problem=" link-string) 'euler-problem-link)
	((ssearch "about=" link-string)   'about)
	((ssearch "http://" link-string) 'url)
	((equal "txt" (pathname-type link-string)) 'txt)
	((string= "CabriJava.class" link-string) nil)
	(t 'image)))

(defclass euler-problem (pgraphic)
  ((prompt             :accessor prompt
		       :initarg :prompt             :initform nil)
   (links              :accessor links
		       :initarg :links              :initform nil)
   (solution           :accessor solution
		       :initarg :solution           :initform nil)
   (problem-number     :accessor problem-number
		       :initarg :problem-number     :initform nil))
  (:metaclass manardb::mm-metaclass))

(defmethod solution? ((euler-problem euler-problem) submission)
  (string= (solution euler-problem)
	   (car (split " " (rp (format nil "echo -n '~s' | md5sum" submission))))))

(defmethod solved? ((euler-problem))
  (let* ((fiasco:*PRINT-TEST-RUN-PROGRESS* nil)
	 (test-name (format-symbol "euler::problem-~d" (problem-number euler-problem))))
    (fiasco:without-debugging (funcall test-name))))

(defmethod render ((euler-problem euler-problem) stream)
  (labels ((render-link (link-string stream) 
	     (case (link-type link-string)
	       (about (format stream "~A" link-string))
	       (euler-problem-link (format stream "~A" link-string))
	       (image (merge-pathnames *euler-dir* link-string))
	       (url (multiple-value-bind (x y)
			(clim::stream-cursor-position stream)
		      (mmg::draw-url link-string x y stream)))
	       (txt (format stream "text file: ~A" link-string))
	       (t (format stream "included link: ~A" link-string)))))
    (window-clear stream)
    (with-slots (prompt links problem-number) euler-problem
      (format stream "Problem Number~A~%~%~A" problem-number prompt)
      (loop for link-string in links
	    do (clim::with-room-for-graphics (stream)
		 (render-link link-string stream))))))

(defmethod problem-test-skeleton ((euler-problem euler-problem))
  `(deftest ,(format-symbol "problem-~d" problem-number)
       (is (solution? (euler-problem ,problem-number) nil))))

(defun create-project-euler-scaffolding ()
  "creates all files and problems for project euler. Should be run once when the habit is first downloaded"
  (let* ((tmp-dir "/tmp/")
	 (*print-case* :downcase))
    (mkdir tmp-dir)
    (labels ((download-project-euler-files ()
	       (mkdir "/tmp/project-euler/")
	       (download-url "http://kmkeen.com/local-euler/project_euler.zip" 
			     "/tmp/project-euler/project-euler-extra-files.zip")
	       (download-url "http://kmkeen.com/local-euler/project_euler.txt" 
			     "/tmp/project-euler/problems.txt"))

	     (create-problems-from-textfile ()
	       (let* ((problem-strings (mm::->> (merge-pathnames tmp-dir "problems.txt")
						(mm::qlpp)
						(mm::slurp-file)
						(mm::split "Problem \\d*\\n=========[=]*")
						(rest)))
		      (*print-level* nil) 
		      (*print-length* nil))    
		 (with-open-file (stream "/tmp/euler-problems.lisp" :direction :output
								    :if-exists :supersede
								    :if-does-not-exist :create)
		   (loop for problem-string in problem-strings
			 for i = 1 then (1+ i)
			 do (let* ((lpos (mm::ssearch "Visible links" problem-string t))
				   (apos (mm::ssearch "Answer: " problem-string t))
				   (links (when lpos 
					    (mm::->> (subseq problem-string lpos apos)
						     (mm::split "\\n")
						     (rest)
						     (mapcar (lambda (k) (drop 3 (mm::trim-dwim k))))
						     (remove-if #'null)))))
			      (destructuring-bind (prompt answer)
				  (mapcar 'mm::trim-dwim (mm::split "Answer: " problem-string))
				(make-instance 'euler-problem
					       :problem-number i
					       :prompt prompt
					       :solution answer 
					       :links links
					       :x 0 :y 0)))))))

	     (f (stream sexp) (write sexp :stream stream) (terpri stream) (terpri stream))))

    (downloads-project-euler-files)
    (create-problems-from-textfile)
    ;; all the problems are in manardb at this point
    (with-open-file (s (merge-pathnames euler-dir "solutions.lisp")
		       :direction :output
		       :if-does-not-exist :create)
      (f s '(fiasco:define-test-package #:euler (:use #:mm #:fiasco)))
      (f s '(in-package #:euler))
      (loop for problem in (sorted-euler-problems)
	    do (with-slots (problem-number) problem
		 (f s (problem-test-skeleton problem)))))))

;;; Quiz GUI modifications 
;;; ============================================================================

(in-package #:mmg)

(define-command-table project-euler-commands)

(define-quiz-command (com-next-euler-problem
		      :name "Next Problem" :keystroke (#\f :control)) ()
  (incf *euler-problem-id*)
  (aif (euler-problem *euler-problem-id*)
       (render it *standard-output*)
       (format "No more problems")))

(define-quiz-command (com-previous-euler-problem
		      :name "Previous Problem" :keystroke (#\f :control)) ()
  (decf *euler-problem-id*)
  (aif (euler-problem *euler-problem-id*)
       (render it *standard-output*)
       (format "No more problems")))

(define-quiz-command (com-nth-euler-problem
		      :name "Nth problem" :keystroke (#\n :control)) ()
  (let* ((input (accept 'integer :prompt "problem number"))
	 (eproblem (euler-problem *euler-problem-id*)))
    (setf *euler-problem-id* )
    (if eproblem (render it *standard-output*)
	(format *query-io* "failed"))))

;;; Habit
;;; ============================================================================

(in-package #:mm) 

(setf *euler-problem-id* (latest-unsolved-problem))

(defun start-project-euler ()
  (if (stumpwm::something-fullscreen?)
      (progn (mmg::run-or-focus-quiz)
	     (render (euler-problem *euler-problem-id*)
	     	     (mmg::find-pane-named *quiz* 'mmg::display-pane))
	     (stumpwm::hsplit)
	     (stumpwm::fnext)
	     (stumpwm::emacs)
	     (ignore-errors 
	      (swank::eval-in-emacs
	       '(progn 
		 (window-configuration-to-register :programming-practice)
		 (find-file "~/.masamune/habits/project-euler/solutions.lisp")
		 (delete-other-windows)
		 nil))))
      (message "The dashboard should be fullscreen before running this program")))

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
  (format sheet "Problems remaining on project euler~%Algorithm problems completed today~%Avg. completion time per problem")
  (let* ((x 600) (y 200) (r 100)
	 (percentage-complete .32)
	 (percentage-incomplete (- 1 percentage-complete))) 
    (clim:draw-circle* sheet x y r :ink clim:+blue+
    				   :start-angle (* percentage-complete (* 2 pi))
    				   :end-angle (* 2 pi))

    (clim:draw-circle* sheet x y r :ink clim:+grey+ 
				   :start-angle (* 2 pi)
				   :end-angle (* percentage-complete (* 2 pi)))))

(defun programming-practice-install ()
  (push (i 'habit
	   :name "Programming Practice"
	   :initialization-function 'start-programming-practice
	   :visualization-function 'visualize-programming-practice
	   :occurrence :daily)
	*habits*))

