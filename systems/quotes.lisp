;;; 'Quotes' is really closer to 'interesting reading' 
;;; ============================================================================

(in-package #:mm)

(defun visualize-quotes (habit sheet)
  (format sheet "TODO, implement me!"))

(defun start-quotes (habit)
  (record-event habit (event :started))
  (climacs:edit-file "~/documents/quotes.txt")
  (stumpwm::run-with-timer (* 5 60) nil 
			   (lambda () (stumpwm::message-no-timeout "Time is almost up")
			     (loop for i from 15 downto 0
				   finally (progn (stumpwm::message-no-timeout "finished")
						  (bt:destroy-thread (mm:thread-by-name "Climacs"))
						  (mmg::run-or-focus-dashboard)))
			     (record-event habit (event :finished)))))

(defun quotes-install ()
  (push (i 'habit
	   :name "Quotes"
	   :initialization-function 'start-quotes
	   :visualization-function 'visualize-quotes
	   :occurrence :daily)
	*habits*))
