(in-package #:mm)

(defun available-programs ()
  (read-from-string
   (flexi-streams:octets-to-string (http "https://raw.githubusercontent.com/gabriel-laddel/masamune/master/systems/systems.lisp"))))

(defun install-program (name)
  "name is case insensitive"
  (awhen (some (lambda (pl) (when (string= (string-downcase (getf pl :name)) (string-downcase name)) pl))
	       (available-programs))
    (let* ((filename (getf it :file))
	   (program-location (format nil "https://raw.githubusercontent.com/gabriel-laddel/masamune/master/systems/~a" filename))
	   (program-string (http program-location))
	   ;; TODO 2014-10-28T18:10:54-07:00 Gabriel Laddel
	   ;; load on startup
	   (installed-filename (format nil "~~/.masamune/installed-programs/~a" filename)))
      (with-open-file (s installed-filename
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	(write program-string :stream s))
      (compile-file installed-filename))))

(in-package #:mmg)

(defvar *repository* nil)

(define-application-frame repository ()
  ((repository-pane) (interaction-pane))
  (:pointer-documentation t)
  (:menu-bar t)
  (:panes (repository-pane :application
			   :scroll-bars t
			   :display-function 'render-repository)
	  (interaction-pane :interactor))
  (:layouts (default (vertically ()
		       (7/8 repository-pane)
		       (1/8 interaction-pane)))
	    (fullscreen repository-pane)))

(defun installed-p (program-pl)
  (probe-file (format nil "~~/.masamune/installed-programs/~a" (getf program-pl :file))))

(defun render-repository (frame pane)
  (declare (ignore frame))
  (let* ((*print-pretty* nil))
    (if (mm::available-programs)
	(loop for program-pl in (mm::available-programs)
	      do (if (installed-p program-pl) (format pane "INSTALLED: ~s~%" program-pl)
		     (format pane "~s~%" program-pl)))
	(format pane "No programs available for download"))))

(defun run-repository ()
  (setf *repository* (make-application-frame 'repository))
  (run-frame-top-level *repository* :name "repository"))

(defun run-or-focus-repository ()
  (bt:make-thread (lambda () (aif (some (lambda (w) (when (scan "repository" (stumpwm::window-name w)) (stumpwm::window-name w)))
				   (stumpwm::all-windows))
			     (stumpwm::select-window it)
			     (run-repository)))
		  :name "repository"))
