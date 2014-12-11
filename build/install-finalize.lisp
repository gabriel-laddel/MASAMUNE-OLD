(in-package #:common-lisp-user)

(sb-ext:restrict-compiler-policy 'debug 3)

(defvar *masamune-pathnames*
  '("~/.masamune/emacs-desktop-state/"
    "~/.masamune/pclos-datastore/"
    "~/screenshots/"
    "~/algol/"
    "~/lisp/"))

(define-condition installation-choice ()
  ((query :accessor query :initarg :query))
  (:report (lambda (condition stream) (format stream (query condition))))
  (:documentation "the query slot is passed to `format' directly but currently receives no arguments"))

(defun rp-in-dir (commands dir &optional (output-stream :string))
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command) output-stream)))

(defun rp (shell-string &optional (output-stream :string))
  (uiop:run-program shell-string :output output-stream))

(defun log (message)  (format t (format nil "~%~a" message)))
(defun k (j)  (rp (format nil "emerge ~a" j) *standard-output*))

(defun install-network-manager ()
  (rp "emerge networkmanager" *standard-output*)
  (rp "rc-update add NetworkManager")
  (rp "rc"))

(defun download-hyperspec ()
  (rp-in-dir
   '("curl ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz > /tmp/HyperSpec-7-0.tar.gz"
     "tar xzf /tmp/HyperSpec-7-0.tar.gz")
   "/tmp/")
  (rename-file "/tmp/HyperSpec" "~/lisp/HyperSpec"))

(defun write-dotfiles ()
  (macrolet ((f (file form) `(with-open-file (stream ,file
						     :direction :output
						     :if-exists :supersede
						     :if-does-not-exist :create)
			       (format stream "~A" ,form))))
    (f "~/.swank.lisp" "(setf swank::globally-redirect-io t)")  
    (f "~/.emacs" "(load \"~/quicklisp/local-projects/masamune-os/init.el\")") ;; XXX
    (f "~/.sbclrc" (concatenate 'string
				"#" "-quicklisp"
				;; ^ paredit doens't understand reader macros in strings
				"(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))"))
    (f "~/.stumpwmrc" (format nil ";;; -*- Mode: Lisp -*-~%(in-package #:stumpwm)~%(ql:quickload 'swank)~%(swank:create-server :port 4005 :dont-close t)~a"
			      '(setq *input-window-gravity* :center
				*message-window-gravity* :center
				*normal-border-width* 0
				*window-border-style* :none
				*transient-border-width* 0
				*top-level-error-action* :break)))))

(defun install-maxima () (k "maxima"))

(defun install-the-gimp ()
  (dolist (k '("app-doc/gimp-help"
	       "media-gfx/gimp-arrow-brushes"
	       "media-gfx/gimp"))
    (k k)))

(defun install-misc-x-extensions ()
  (dolist (k '("x11-apps/xrandr"
	       "x11-misc/xcalib" 
	       "x11-apps/xdpyinfo"))
    (k k)))

(defun install-misc-mm-required ()
  "masamune requires these for various reasons"
  (dolist (k '("media-gfx/imagemagick"
	       "app-text/sloccount"
	       "sys-apps/lshw"
	       "net-wireless/aircrack-ng"
	       "net-analyzer/nmap"
	       "sys-process/htop"))
    (k k)))

(defmacro force-installation-choice (query restart-function-name)
  `(restart-case (error (make-condition 'installation-choice :query ,query)) ;; so this is sorta
     (lazy () :report "Schedule it to install in a background process. (unimplemented, will install anyway as per \"go\")"
	      :interactive ,restart-function-name)
     (nope () :report "I don't need it. (unimplemented, will install anyway as per \"go\")"
	      :interactive ,restart-function-name)
     (todo () :report "Add this install as a TODO on my dashboard. (unimplemented, will install anyway as per \"go\")"
	      :interactive ,restart-function-name)
     (go () :report "Go ahead and install it in the current thread."
	    :interactive ,restart-function-name)))

(defun mouse-focus-policy-selection ()
  (restart-case (error (make-condition 'installation-choice 
				       :query "The mouse focus policy decides how the mouse affects input focus. Possible values are :ignore, :sloppy, and :click. :ignore means stumpwm ignores the mouse. :sloppy means input focus follows the mouse; the window that the mouse is in gets the focus. :click means input focus is transfered to the window you click on.")) ;
    (ignore () :ignore)
    (sloppy () :sloppy)
    (click () :click)))

(loop for k in *masamune-pathnames* unless (probe-file k)
      do (rp (format nil "mkdir -p ~a" k) *standard-output*))
(log "created masamune pathnames")

(download-hyperspec)
(log "downloaded hyperspec")

(write-dotfiles)
(log "wrote dotfiles")

(rp "cd ~/algol && git clone https://github.com/gabriel-laddel/inxi.git")
(log "clone'd inxi into ~/algol/inxi/. See `mm::machine-information' if you're curious as to why this is being installed.")

(install-misc-mm-required)
(log "install misc required libraries")

(install-misc-x-extensions)
(log "installed misc X extensions")

(install-network-manager)
(log "installed network manager")

;; (install-conkeror)
;; (log "installed conkeror")

(cerror "my mouse and keyboard work as demonstrated by pressing this restart"
	"If the mouse and keyboard don't work you're in undocumented territory, see the bottom of http://www.funtoo.org/X_Window_System for more information. If you could report this as a bug on http://github.com/gabriel-laddel/masamune and include as much information about the box in question you're comfortable sharing it would be greatly appreciated. [Note: If input doesn't work you want to boot into crippled mode (the linux console)]")

(force-installation-choice "How should Maxima (a common lisp computer algebra system) be installed?" install-maxima)
;; (force-installation-choice "How should the GIMP (an open source image manipulation system) be installed?" install-the-gimp)

(with-open-file (stream "~/quicklisp/local-projects/masamune/lisp-customizations.lisp"
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)
  (format stream "~%~s" `(setq *input-window-gravity* :center
			       *message-window-gravity* :center
			       *normal-border-width* 0
			       *window-border-style* :none
			       *transient-border-width* 0
			       *top-level-error-action* :break
			       *mouse-focus-policy* ,(mouse-focus-policy-selection))))

;; (defun build-emacs-speaks ()
;;   ;; TODO 2014-12-08T20:04:28+00:00 Gabriel Laddel
;;   ;; something is wrong with `dtk-initialize' in one of the emacspeak.el
;;   ;; files. it has a bunch of variables like `dtk-program' that should probably
;;   ;; be set, but arn't for whatever reason.
;;   "error here: http://www.cs.vassar.edu/~priestdo/emacspeak/2007/msg00642.html appears to be a problem with dtk-initialize"
;;   (rp "emerge app-accessibility/emacspeak")
;;   (rp "emerge app-accessibility/emacspeak-ss")
;;   (rp "emerge app-accessibility/flite")
;;   (rp "emerge app-accessibility/eflite")
;;   ;; XXX the emacspeak install-guide insists you need these, but portage doesn't
;;   ;; treat them as dependencies?
;;   (rp "emerge dev-lang/tk")
;;   (rp "emerge tcl")
;;   (with-open-file (stream "~/.bashrc" :direction :output 
;; 				      :if-exists :append
;; 				      :if-does-not-exist :create)
;;     (format stream "~%export DTK_PROGRAM=$(which eflite)"))
;;   ;; "export DTC_TCL=$(which tcl)
;;   "(let ((sound-synth-program (shell-command-to-string \"$(which eflite)\")))
;;     (setenv \"DTK_PROGRAM\" sound-synth-program))
;;    (load \"/usr/share/emacs/site-lisp/emacspeak/lisp/emacspeak-setup.el\")")
 
