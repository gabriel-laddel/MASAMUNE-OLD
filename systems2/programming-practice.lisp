(in-package #:mm)

;;; "The majority of programmers have literally no idea how to practice, since
;;; nobody teaches it. So they simply don't do it. The first step towards
;;; becoming good at practicing is knowing a thing or two about practice
;;; itself. Practicing for anything is generally best done via drills: short,
;;; high-intensity exercises designed to yield the highest return on the time
;;; you invest." -- Steve Yegge


;;; books
;;; ============================================================================
;;; introduction to algorithms
;;; Art of computer programming
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
  (climacs:edit-file "~/lisp/projecteuler/solutions.lisp")
  (stumpwm::run-with-timer (* 20 60) nil 
			   (lambda () (stumpwm::message-no-timeout "Time is almost up")
			     (loop for i from 15 downto 0
				   finally (progn (record-event habit (event :finished))
						  (stumpwm::message-no-timeout "finished")
						  (bt:destroy-thread (mm:thread-by-name "Climacs"))
						  (mmg::run-or-focus-dashboard))))))

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
