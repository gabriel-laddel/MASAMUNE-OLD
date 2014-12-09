(in-package #:mm)

(defun finish-mathematics ()
  (stumpwm::message-no-timeout "Time is almost up")
  (loop for i from 15 downto 0
	finally (progn (record-event (mmg::habit-by-name "mathematics practice")
				     (event :finished))
		       (stumpwm::message-no-timeout "finished")
		       (awhen (mm:thread-by-name "Climacs") (bt:destroy-thread it))
		       (mmg::run-or-focus-dashboard))))

(defun start-mathematics (habit)
  (record-event habit (event :started))
  (climacs:edit-file (ppath "/mathematics-scratch.lisp"))
  (stumpwm::run-with-timer (* 60 60) nil #'finish-mathematics))

(defun visualize-mathematics (habit sheet)
  (let* ((y 1084) (x 1460))
    (format sheet "~%    problems solved: ~d~%    tables memorized: ~d~%    definitions memorized: ~d~%    session length: ~d minutes~%"
	    (random 10) (random 10) (random 200) (random 200))))

(defun mathematics-practice-install ()
  (push (i 'habit
	   :name "Mathematics Practice"
	   :visualization-function 'visualize-mathematics
	   :initialization-function 'start-mathematics
	   :occurrence :daily)
	*habits*))
