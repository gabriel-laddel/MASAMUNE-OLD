(in-package #:common-lisp-user)

(format t "please be patient and don't click on anything quite yet.") 

(sb-ext:restrict-compiler-policy 'debug 3)

(ql:quickload 'cl-fad)
(ql:quickload 'alexandria)

(defun cat (&rest objs)
  (apply #'concatenate 'string
	 (mapcar (lambda (o) (if (stringp o) o (write-to-string o))) objs)))

(defun rp (shell-string &optional (output-stream :string))
  (uiop:run-program shell-string :output output-stream))

(defun rp-in-dir (commands dir &optional (output-stream :string))
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command) output-stream)))

(defun interpose (o l)
  (loop for i in l append (list i o)))

(defun lg (message)  (format t (format nil "~%~a" message)))
(defun k (j)  (rp (format nil "emerge ~a" j) *standard-output*))

(defun write-dotfiles ()
  (macrolet ((f (file form) `(with-open-file (stream ,file
						     :direction :output
						     :if-exists :supersede
						     :if-does-not-exist :create)
			       (format stream "~A" ,form))))
    (f "~/.swank.lisp" ";;; -*- Mode: Lisp -*-~%(in-package #:cl)~%(setf swank::globally-redirect-io t)")  
    (f "~/.emacs" "(load \"~/quicklisp/local-projects/masamune/init.el\")") ;; XXX
    (f "~/.stumpwmrc" (format nil ";;; -*- Mode: Lisp -*-~%(in-package #:stumpwm)~%(redirect-all-output \"~~/.masamune/stumpwm-debug-output\")~%(ql:quickload 'swank)~%(swank:create-server :port 4005 :dont-close t)~%~S~%~S"
			      '(setq
				*input-window-gravity* :center
				*message-window-gravity* :center
				*normal-border-width* 0
				*window-border-style* :none
				*transient-border-width* 0
				*top-level-error-action* :break
				*mouse-focus-policy* :sloppy
				*startup-message* "Please wait for Masamune to start - this might take a minute")
			      '(run-or-raise "emacs --debug-init" (quote (:class "Emacs")))))))

(write-dotfiles)
(lg "wrote dotfiles")

(cerror "my mouse and keyboard work as demonstrated by pressing this restart"
	"If the mouse and keyboard don't work you're in undocumented territory, see the bottom of http://www.funtoo.org/X_Window_System for more information. If you could report this as a bug on http://github.com/gabriel-laddel/masamune and include as much information about the box in question you're comfortable sharing it would be greatly appreciated.")

(stumpwm::delete-window 
 (car (remove-if-not (lambda (w) (search "emacs" (window-name w))) 
		     (stumpwm::all-windows))))
(stumpwm::emacs)
