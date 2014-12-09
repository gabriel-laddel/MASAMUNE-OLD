(in-package #:mm)

(defun visualize-communications (habit sheet)
  (let* ((y 1084) (x 1460))
    (format sheet " ~%    communications word count: ~d~%    emails: ~d~%    session length: ~d minutes"
	    (random 200) (random 200) (random 200))))

(defun start-communications (habit)
  (record-event habit (event :started))
  (mmb::open-uri "http://mail.google.com")
  (mmb::open-uri "http://facebook.com")
  (record-event habit (event :finished)))

(defun communications-install ()
  (push (i 'habit
	   :name "Communications"
	   :description "~check mail"
	   :initialization-function 'start-communications
	   :visualization-function 'visualize-communications
	   :occurrence :daily)
	*habits*))
