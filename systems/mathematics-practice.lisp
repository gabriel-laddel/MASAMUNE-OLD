(in-package #:mm)

(defun start-mathematics-practice (habit)
  (record-event habit (event :started))
  (stumpwm::emacs)
  ;; XXX 2014-12-23T14:16:06+00:00 Gabriel Laddel
  ;; always throws
  (ignore-errors 
   (with-live-swank-connection 
       (swank::eval-in-emacs
	'(progn (delete-other-windows)
	  (find-file "~/quicklisp/local-projects/masamune/systems/mathematics-practice.lisp")
	  (find-file "~/quicklisp/local-projects/masamune/mathematics-scratch.lisp")
	  nil))))
  (stumpwm::run-with-timer
   (* 60 60) nil 
   (lambda () 
     (with-live-swank-connection
	 (stumpwm::message-no-timeout "Time is almost up")
       (loop for i from 10 downto 0
	     finally (progn (record-event habit (event :finished))
			    (stumpwm::message-no-timeout "finished")
			    (mmg::run-or-focus-dashboard)))))))

(defun visualize-mathematics (habit sheet)
  (let* ((y 1084) (x 1460))
    (format sheet "~%    problems solved: ~d~%    tables memorized: ~d~%    definitions memorized: ~d~%    session length: ~d minutes~%"
	    (random 10) (random 10) (random 200) (random 200))))

(defun mathematics-practice-install ()
  (push (i 'habit
	   :name "Mathematics Practice"
	   :visualization-function 'visualize-mathematics
	   :initialization-function 'start-mathematics-practice
	   :occurrence :daily)
	*habits*))
