(in-package #:mm)

(defun start-observations (habit)
  (let* ((swank::*emacs-connection* (car swank::*connections*)))
    (record-event habit (event :started))
    (stumpwm::emacs)
    ;; NOTE 2014-12-19T05:53:43+00:00 Gabriel Laddel
    ;; this will throw because of swank internals I'm not aware of
    (ignore-errors (swank::eval-in-emacs
		    '(progn (find-file "~/documents/observations.txt") 
		      (delete-other-windows)) t))
    (stumpwm::run-with-timer
     (* 5 60) nil 
     (lambda ()
       (with-live-swank-connection
	 (stumpwm::message-no-timeout "Time is almost up")
	 (loop for i from 10 downto 0
	       finally (progn (stumpwm::message-no-timeout "finished")
			      (handler-case (swank::eval-in-emacs
					     '(progn (kill-buffer "observations.txt") nil)
					     t)
				(error nil))
			      (mmg::run-or-focus-dashboard)))
	 (record-event habit (event :finished)))))))

(defun observations-install ()
  (push (i 'habit
	   :name "Review Observations"
	   :initialization-function 'start-observations
	   :occurrence :daily)
	*habits*))
