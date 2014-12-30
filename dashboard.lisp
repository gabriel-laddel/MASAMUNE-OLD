(in-package #:mm)

(defparameter test-system-info 
  (car (read-file "~/.masamune/trash-files/test-system-info")))

(defun parse-sloccount-output (sloccount-output)
  (let* ((s1 (subseq sloccount-output 
		     (search "(dominant language first)" sloccount-output) 
		     (search "Total Physical" sloccount-output))))
    (loop for line in (rest (split "\\n" s1))
	  collect (mapcar (lambda (s) (regex-replace ":" s ""))
			  (take 2 (remove "" (split " " line) :test #'string=))))))

(defun local-lisp-system-stats ()
  (loop for proj in (filter #'cl-fad:directory-pathname-p (ls (qlpp)))
	collect (list proj (parse-sloccount-output (rp (format nil "cd ~a && sloccount ." proj))))))

(defun external-lisp-system-stats ()
  (loop for proj in (filter #'cl-fad:directory-pathname-p (ls "/root/quicklisp/dists/quicklisp/software"))
	collect (list proj (parse-sloccount-output (rp (format nil "cd ~a && sloccount ." proj))))))

(defun algol-system-stats ()
  "Credit for data goes to David A. Wheeler's sloccount"
  (let* ((distfiles (ls "/usr/portage/distfiles")))
    (format nil "there are currently ~d algol systems installed" (length distfiles))))

(defun calculate-operating-system-stats ()
  (list :local-systems (local-lisp-system-stats)
	:external-systems (external-lisp-system-stats)
	:algol-systems (algol-system-stats)))

(defun systems-stats-string ()
  (labels ((f (s) (apply #'+ (mapcar (lambda (o) (parse-integer o :radix 10))
				     (flatten (mapcar (lambda (l) (mapcar #'second (second l))) 
						      s)))) ))
    (let* ((total-local-lisp-system-loc (f (getf test-system-info :local-systems)))
	   (total-external-lisp-system-loc (f (getf test-system-info :external-systems)))
	   (external-lisp-system-count (length (mapcar #'car (getf test-system-info :external-systems))))
	   (local-lisp-system-count (length (mapcar #'car (getf test-system-info :local-systems))))
	   (s (make-string-output-stream)))
      (format s "in total ~:d known lines of code running on the operating system, across ~:d lisp systems~%"
	      (+ total-external-lisp-system-loc total-local-lisp-system-loc)
	      (+ external-lisp-system-count local-lisp-system-count))
      (format s "~:d local lisp systems with a total of ~:d lines of code~%"
	      local-lisp-system-count total-local-lisp-system-loc)
      (format s "~:d external lisp systems with a total of ~:d lines of code~%" 
	      external-lisp-system-count total-external-lisp-system-loc)
      (format s "also, ~a" (getf test-system-info :algol-systems))
      (get-output-stream-string s))))

(in-package #:mmg)

(defvar *focused-habit* nil)
(defvar *dashboard* nil)
(defparameter lpw (float (* mm::screen-width .25)) "[l]eft [p]ane [w]idth")
(defparameter lph (float (/ (* mm::screen-height 6/7) 3)) "[l]eft [p]ane [h]eight")
(defparameter vpw (float(* mm::screen-width .75)) "[v]isualization [p]ane [w]idth")
(defparameter vph (float (* mm::screen-height 6/7)) "[v]isualization [p]ane [h]eight")

(define-application-frame dashboard ()
  ((habit-pane) (calendar-pane) (visualization-pane) (interaction-pane))
  (:pointer-documentation t)
  (:menu-bar nil)
  (:panes (interaction-pane :interactor)
	  ;; XXX 2014-11-08T00:26:47-08:00 Gabriel Laddel
	  ;; if scrollbars are not used on these panes they'll get resized if anything
	  ;; grows larger than it's current boundries jto make room for its graphics.
	  (habit-pane :application 
		      :scroll-bars :vertical
		      :display-function 'render-habits)
	  (system-pane :application
		       :scroll-bars :vertical
		       :display-function 'render-systems)
	  (calendar-pane :application 
			 :scroll-bars :vertical
			 :display-function 'render-calendar)
	  (visualization-pane :application
			      :scroll-bars nil
			      :display-function 'render-visualization))
  (:layouts (default (vertically () 
		       (6/7 (horizontally () 
			      (1/4 (vertically () habit-pane system-pane calendar-pane))
			      (3/4 visualization-pane)))
		       (1/7 interaction-pane)))
	    (fullscreen (horizontally () 
			  (1/4 (vertically () habit-pane system-pane calendar-pane))
			  (3/4 visualization-pane)))))

(define-presentation-type habit ())

;;; commands

(define-dashboard-command (com-quit :name "Quit") ()
  (frame-exit *application-frame*))

(define-dashboard-command (com-init-habit :name t)
    ((habit 'habit))
  (aif (mm::initialization-function habit) (funcall it habit)
       (format (interaction-pane) "it doth not exist!")))

(define-dashboard-command (com-init-focused-habit :name t :keystroke (#\i :control)) ()
  (if *focused-habit* (funcall (mm::initialization-function *focused-habit*) *focused-habit*)
      (format (interaction-pane) "no habit currently focused!")))

(define-dashboard-command (com-describe-habit)
    ((habit 'habit :gesture :select))
  (format (interaction-pane) "~a" (mm::description habit)))

(define-dashboard-command (com-focus :name "Focus Habit") 
    ((habit 'habit :gesture :select))
  (setf *focused-habit* habit)
  (render-visualization *dashboard* (find-pane-named *dashboard* 'visualization-pane)))

(define-dashboard-command (com-next-habit :name t :keystroke (#\n :control)) ()
  (labels ((p () (position *focused-habit* mm::*habits* :test #'eq)))
    (setf *focused-habit*
	  (if (or (null *focused-habit*) (= (- (length mm::*habits*) 1) (p)))
	      (car mm::*habits*)
	      (nth (1+ (p)) mm::*habits*)))))

(define-dashboard-command (com-previous-habit :name t :keystroke (#\p :control)) ()
  (labels ((p () (position *focused-habit* mm::*habits* :test #'eq)))
    (setf *focused-habit*
	  (if (or (null *focused-habit*) (= 0 (p)))
	      (llast mm::*habits*)
	      (nth (- (p) 1) mm::*habits*)))))

(define-dashboard-command (com-display-overview :name t :keystroke (#\o :control)) ()
  (setf *focused-habit* nil)
  (render-overview *dashboard* (find-pane-named *dashboard* 'visualization-pane)))

(defun render-overview (frame pane)
  (declare (ignore frame))
  (let* ((*print-pretty* nil))
    (format pane "~%    TODO, time spent in programs for day, month, year")
    (format pane "~%    total TODO, XXX, FIXME, NOTE LoC added, removed, ")
    (format pane "~%    systems under supervision, visualization of sleep time, high scores for some things")
    (format pane "~%    timeline detailing time spent at various things (habits etc.) as of now")
    (format pane "~%    Concepts learned in past day, week, month, all time high & global stats~%")
    (format pane "    sleep & day breakdown timeline~%")
    (format pane "    how much am I contributing to masamune? that is, problems definitions contributed, concepts I run etc.~%")
    (format pane "    avg. week, month etc. for all metrics & agenda items from org-mode and calendar")
    (format pane "    # of concepts, problems etc. added")
    (format pane "    Agenda ~{~%    TODO: ~a~}" (mapcar #'mm::title mm::*agenda*))
    (format pane "    # of exceptions per day~%~%")
    (format pane "    Current System Information~%~%")
    (format pane "~{~%~a~}" (split "\\n" (mm::systems-stats-string)))
    (draw-rectangle* pane 0 (- vph 180) vpw (+ (- vph 96) 50) :ink +PeachPuff4+)))

(defun render-habits (frame pane)
  (declare (ignore frame))
  (destructuring-bind (x1 y1 x2 y2) 
      (coerce (slot-value (sheet-region pane) 'clim-internals::coordinates) 'list)
    (let* ((center-x (/ (- x2 x1) 2))
	   (center-y (/ (- y2 y1) 2))
	   (y-spacing 20))
      (labels ((draw-habit-text (h y) (draw-text* pane (mm::name h) center-x (* y-spacing y) 
						  :ink (if (eq *focused-habit* h) +blue+ +black+)
						  :align-x :center
						  :align-y :center
						  :text-size 20)))
	(if mm::*habits*
	    (loop for habit in mm::*habits*
		  for y = 1 then (1+ y)
		  for habit-finished = (not (mm::occurs-now? habit))
		  for strikethrough-y = (* y-spacing y)
		  for x  = (- center-x (* 3 (length (mm:name habit))))
		  for x1 = (+ center-x (* 3 (length (mm:name habit))))
		  do (with-output-as-presentation (pane habit 'habit)
		       (if habit-finished
			   (progn (draw-habit-text habit y)
				  (draw-line* pane x strikethrough-y x1 strikethrough-y)) 
			   (draw-habit-text habit y))))
	    (draw-text* pane "No Habits Installed" center-x center-y
			:align-x :center :align-y :center))))))

(defun render-systems (frame pane)
  ;; TODO 2014-11-13T19:43:18-08:00 Gabriel Laddel
  ;; - issue tracking system that integrates with github, org mode style thing.
  ;; - # of docstrings, functions, marcos and variables
  (declare (ignore frame))
  (if mm::*system-information*
      (format pane "~%~{~:@{   System: ~@(~a~), ~@:{~%    ~@(~a~) LoC:  ~d ~}~%   #TODOs: #NOTEs: #XXXs: #FIXMEs:~%~%~}~}" mm::*system-information*)
      (destructuring-bind (x1 y1 x2 y2) 
	  (coerce (slot-value (sheet-region pane) 'clim-internals::coordinates) 'list)
	(let* ((center-x (/ (- x2 x1) 2)) (center-y (/ (- y2 y1) 2)))
	  (draw-text* pane "Masamune doesn't know about any systems. Add one?" center-x center-y
		      :ink +black+ :align-x :center :align-y :center :text-size 20)))))

;;; calendar

(defvar *day-of-week-string*
  (make-array 7 :initial-contents (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))

(defvar *month-lengths*
  #(31 28 31 30 31 30 31 31 30 31 30 31))

(defun month-length (month year)
  (declare (special *month-lengths*))
  (cond ((/= month 2) (aref *month-lengths* (- month 1)))
	((null (zerop (mod year 4))) 28)
	((null (zerop (mod year 400))) 29) (t 28)))

(defun calendar-month (month year &key (stream *standard-output*))
  (declare (special *day-of-week-string*))
  (labels ((today? (date) t))
    (let* ((days-in-month (month-length month year)))
      (multiple-value-bind (n1 n2 n3 n4 n5 n6 start-day)
	  (decode-universal-time (encode-universal-time 0 0 0 1 month year))
	(setq start-day (mod (+ start-day 1) 7))
	(formatting-table (stream :move-cursor t)
	  (formatting-row (stream)
	    (dotimes (d 7)
	      (formatting-cell (stream :align-x :center)
		(write-string (aref *day-of-week-string* (mod d 7)) stream))))
	  (do ((date 1)
	       (first-week t nil))
	      ((> date days-in-month))
	    (formatting-row (stream)
	      (dotimes (d 7)
		(formatting-cell (stream :align-x :right)
		  (when (and (<= date days-in-month)
			     (or (not first-week) (>= d start-day)))
		    (format stream "~d" date)
		    (incf date)))))))))))
  
(defun render-calendar (frame pane)
  (declare (ignore frame))  
  (calendar-month 6 12 :stream pane))

(defun render-visualization (frame pane)
  (declare (ignore frame))
  (if (and *focused-habit* (mm::visualization-function *focused-habit*))
      (funcall (mm::visualization-function *focused-habit*) *focused-habit* pane)
      (render-overview *dashboard* (find-pane-named *dashboard* 'visualization-pane))))

(defun run-dashboard ()
  (when mm::*habits* (setq *focused-habit* (or *focused-habit* (car mm::*habits*))))
  (setf *dashboard* (make-application-frame 'dashboard))
  (run-frame-top-level *dashboard* :name "Dashboard"))

(defun run-or-focus-dashboard ()
  (aif (stumpwm::window-by-name "dashboard")
       (stumpwm::select-window (stumpwm::window-name it))
       (bt:make-thread (lambda () (run-dashboard)) :name "dashboard")))
 
(defun habit-by-name (name)
  "case insensitive"
  (some (lambda (h) (when (string= (string-downcase name) (string-downcase  (mm::name h))) h))
	mm::*habits*))
