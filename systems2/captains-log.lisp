(in-package #:mm)
 
(defvar *captains-log-start-time* nil)
(defvar *captains-log-length* 20 "minutes")

(c captains-log () (start-time end-time title body word-count vocabulary-words))

(defmethod start-timestamp ((captains-log captains-log))
  (universal-to-timestamp (start-time captains-log)))

(defmethod vocabulary-words-used ((log captains-log))
  (loop for word in (vocabulary-words log)
	when (search word (body log) :test #'string=) collect word))

(defmethod occurs-on-p ((log captains-log) (timestamp local-time:timestamp))
  (= (timestamp-day timestamp) (timestamp-day (zero-timestamp (start-timestamp log)))))

(defun zero-timestamp (timestamp)
  (adjust-timestamp timestamp (set :hour 0) (set :minute 0) (set :sec 0) (set :nsec 0)))

(defmethod occurence-percentage ((log captains-log) (timestamp local-time:timestamp))
  "given LOG occurs on TIMESTAMP, what time of the day does it occur?"
  (float (* 100 (/ (timestamp-hour (universal-to-timestamp (start-time log))) 24))))

(defun make-empty-file (pathname)
  (with-open-file (stream pathname :direction :output)))

(defun new-captains-log (habit)
  (declare (ignore habit))
  (stumpwm::eval-command "init-captains-log"))

(defun init-captains-log% (title) 
  (let* ((safe-title (mm::regex-replace-all "/" (mm::regex-replace-all " " (string-downcase title) "-") ""))
	 (log-temporary-pathname (format nil "/tmp/~A.txt" safe-title))
	 (log-pathname (format nil "~~/.masamune/captains-logs/~A.lisp" safe-title)))
    (if (probe-file log-pathname)
	(loop for i from 0 to 3
	      do (progn (sleep 1) (stumpwm::message-no-timeout "file already exists, rename please")) 
	      finally (stumpwm::eval-command "init-captains-log"))
	(progn (mm::make-empty-file log-temporary-pathname)
	       (setf mm::*captains-log-start-time* (get-universal-time))
	       (let* ((climacs-gui:*with-scrollbars* nil)) (climacs::edit-file log-temporary-pathname))
	       (stumpwm::run-with-timer (* mm::*captains-log-length* 60) nil
					(lambda () (captains-log-cleanup log-pathname log-temporary-pathname title)))))
    (stumpwm::pop-top-map)))

(defun captains-log-cleanup (log-pathname log-temporary-pathname title &optional vocabulary-words)
  (stumpwm::message-no-timeout "save the buffer you're working in, time is almost up")
  (record-event (mmg::habit-by-name "captains log") (event :finished))
  (loop for i from 15 downto 0
	do (when (< i 10) (stumpwm::message "~d seconds remaining to save" i) (sleep 1))
	finally (progn (stumpwm::message-no-timeout "finished")
		       (bt:destroy-thread (mm:thread-by-name "Climacs"))
		       (let* ((body (with-open-file (stream log-temporary-pathname :direction :input)
				      (slurp-stream stream))))
			 (with-open-file (stream log-pathname :direction :output 
							      :if-does-not-exist :create)
			   (write (list :start-time *captains-log-start-time* 
					:end-time (get-universal-time)
					:title title
					:body body
					:word-count (length (split " " body))
					:vocabulary-words vocabulary-words) 
				  :stream stream))))))

(in-package #:mmg)

(defun captains-log-file-obj (pathname)
  (apply #'make-instance (cons 'mm::captains-log (car (mm::read-file pathname)))))

(defun logs-by-time (l l1)
  (< (mm::start-time l) (mm::start-time l1)))

(defparameter *time-sorted-captains-logs*
  (sort (mapcar #'captains-log-file-obj (ls-clean "~/.masamune/captains-logs/")) #'logs-by-time))

(defparameter summary-focused-day (mm::start-timestamp (nth 10 *time-sorted-captains-logs*)))
(defparameter *focused-captains-log-info* (list :vocabulary-words-used '("what")
						:vocabulary-words-missed '("what")
						:session-length 20 
						:word-count 200)
  "plist with the keys :vocabulary-words-used :vocabulary-words-missed :session-length :word-count")

(defparameter plot-ink +blue+)

(define-presentation-type captains-log ())

(defun days-and-logs (start-timestamp n-days)
  "returns tuples for N-DAYS from START-TIMESTAMP, inclusively as a tuple with
the correspondingly logs"
  (loop for i from 0 to n-days
	for tstamp = (adjust-timestamp start-timestamp (offset :day i))
	for o = (filter (lambda (log) (when (= (day-of (mm::start-timestamp log)) (day-of tstamp))
				   log))
			*time-sorted-captains-logs*)
	collect (list tstamp o)))

(defun total-days-and-logs ()
  (let* ((start-time (mm::start-timestamp (car *time-sorted-captains-logs*)))
	 (end-time (mm::start-timestamp (llast *time-sorted-captains-logs*))))
    (days-and-logs start-time (- (day-of end-time) (day-of start-time)))))
(defun visualize-captains-log (habit sheet)
  ;; XXX 2014-11-09T05:34:38-08:00 Gabriel Laddel
  ;; 
  ;; the dashboard doesn't render correctly on startup. for whatever
  ;; reason it thinks that it has about half the rendering space alotted
  ;; and will draw the graphics as such. `(step (run-dashboard))' is
  ;; highly recommended when debugging this.
  ;;
  ;; additionally, there appears to be some sort of threading issue so
  ;; that one cannot normally interact with the dashboard to say, send it
  ;; a key event, also, an issue with SBCL/stumpwm timers see
  ;; https://github.com/stumpwm/stumpwm/issues/166 for SBCL description
  ;; and try the stumpwm timers to witness it yourself.
  (declare (ignore habit))
  "when two logs were written on the same day, the summary displays only the one
with the highest word count, and the detail view displays both"
  (let* ((y-detail-start (- (/ vph 4) 50))
	 (y-detail-end (* 2 (/ vph 3)))
	 (detail-height (- y-detail-end y-detail-start))
	 (y-summary-start (+ 40 y-detail-end))
	 (y-summary-end (- (floor vph) 27))
	 (summary-height (- y-summary-end y-summary-start))
	 (max-word-count (apply #'max (mapcar #'mm::word-count *time-sorted-captains-logs*)))
	 ;; detail view
	 (number-of-detail-days 21)
	 (detail-day-width (floor (/ vpw 20)))
	 (datapoint-width  (/ (floor vpw) (length (total-days-and-logs))))
	 (focused-timestamps (loop for i from 0 to (- number-of-detail-days 1)
				   for d = summary-focused-day then (timestamp+ d 1 :day)
				   collect d))
	 (detail-start-pos (position (day-of summary-focused-day)
				     (mapcar (compose #'day-of #'car) (total-days-and-logs)) :test #'=))
	 (x-offset 3))

    ;; Focused Log Metrics
    (let* ((*print-pretty* nil))
      (format sheet "~{~%    word count: ~d~%    vocabulary words used: ~a, missed ~a~%    session-length: ~d minutes ... (TODO: avgs.)~}" 
	      (loop for k in '(:word-count :vocabulary-words-used :vocabulary-words-missed :session-length)
		    collect (getf *focused-captains-log-info* k))))

    ;; Detail
    (loop for (day log-list) in (take 21 (drop detail-start-pos (total-days-and-logs)))
	  for i = 0 then (1+ i)
	  do (let* ((timestring (format-timestring nil day :format '(:month "/" :day)))
		    (log (car (sort log-list (lambda (log1 log2) (> (mm::word-count log1) (mm::word-count log2))))))
		    (r (when log (/ (mm::word-count log) max-word-count)))
		    (x (+ x-offset (* detail-day-width i))) 
		    (x-1 (+ detail-day-width x)))
	       (when log
		 (with-output-as-presentation (sheet log 'captains-log) 
		   (draw-rectangle* sheet x y-detail-end x-1 (+ y-detail-start (- detail-height (* detail-height r)))
				    :ink plot-ink :filled nil)))
	       (draw-text* sheet timestring (+ x (/ detail-day-width 2)) (+ 15 y-detail-end)
			   :align-x :center :align-y :center)))
    ;; Summary
    (loop for (day log-list) in (total-days-and-logs)
	  for i = 0 then (1+ i) 
	  do (progn (when (member (mm::zero-timestamp day) (mapcar #'mm::zero-timestamp focused-timestamps) :test #'timestamp=)
		      (draw-rectangle* sheet (* i datapoint-width) (+ 2 y-summary-end) 
				       (+ datapoint-width (* i datapoint-width)) 
				       y-summary-start :ink +dark-grey+))
		    (when log-list 
		      (let* ((log (car (sort log-list (lambda (log1 log2) (> (mm::word-count log1) (mm::word-count log2))))))
			     (x   (* i datapoint-width))
			     (r   (/ (mm::word-count log) max-word-count))
			     (h   (* summary-height r))
			     (y   (- y-summary-end h)))
			(draw-line* sheet x y-summary-end x y)))))))

(defun new-captains-log (habit)
  (mm::record-event habit  (mm::event :started))
  (mmg::init-captains-log))

(define-dashboard-command (com-move-selection-forwards
			   :name t :keystroke (#\f :control)) ()
  (setf summary-focused-day (timestamp+ summary-focused-day 1 :day)))

(define-dashboard-command (com-move-selection-back 
			   :name t :keystroke (#\b :control)) ()
  (setf summary-focused-day (timestamp- summary-focused-day 1 :day)))

;; (define-dashboard-command (com-set-selection :name t :keystroke) ()
;;   (let* ((max-selection) (current-selection)
;; 	 (input-number (accept 'number :key (format nil "Hacking ~d" current-selection max-selection))))
;;     (setf summary-focused-day (timestamp-  input-number :day))))

(define-dashboard-command com-focus-captains-log
    ((captains-log 'captains-log :gesture :select))
  (with-slots (mm::word-count
	       mm::vocabulary-words
	       mm::start-time
	       mm::end-time
	       mm::body) 
      captains-log
    (let* ((vocabulary-words-used (loop for word in mm::vocabulary-words
					when (search word mm::body :test #'string=)
					  collect word))
	   (vocabulary-words-missed (remove-if (lambda (w) (member w vocabulary-words-used :test #'string=))
					       vocabulary-words-used)))
      (setf *focused-captains-log-info* (list :vocabulary-words-used vocabulary-words-used
					      :vocabulary-words-missed vocabulary-words-missed
					      :session-length (/ mm::start-time mm::end-time 60))))))

(in-package #:mmg)

(define-dashboard-command (com-init-captains-log :name t) ()
  "This function exists because there are bugs in the naive stumpwm input
functionality that can cause a deadlock"
  (mm::init-captains-log% (accept 'string :prompt "Log title")))

(in-package #:mm)

(defun captains-log-install ()
  (push (i 'habit
	   :name "Captains Log"
	   :initialization-function 'new-captains-log
	   :visualization-function 'mmg::visualize-captains-log
	   :occurrence :daily)
	*habits*))
