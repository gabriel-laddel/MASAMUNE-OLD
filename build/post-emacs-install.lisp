;;; TODO
;;; 
;;; - all packages installed via portage should have their build logged to the
;;; screen and also a build logfile on startup
;;;
;;; - start with binaries, build sources in the background.
;;;
;;; - output from emacs, stumpwm, SBCL builds needs to all be logged and validated or portage should simply be removed.
;;;
;;; - (scripted) different wifi setups
;;; 
;;; - check that each portage package downlaods correctly and let user debug
;;;   any failures that may occur, which implies a rather specific ordering
;;;   to the install process.
;;;
;;; - REPL -> condition system as the condition system allows for several options.
;;;
;;; the correct way to make this whole thing work is to get emacs, stumpwm running
;;; and only then go forwards and install all the other goodies. use ~/.stumpwmrc
;;; as a bootstrap script and when it finishes building, clear itself out.

(sb-ext:restrict-compiler-policy 'debug 3)

(format t "congrats! booted!")

;; (defun rp (shell-string &optional (output-stream :string))
;;   (uiop:run-program shell-string :output output-stream))

;; (defun rp-in-dir (commands dir &optional (output-stream :string))
;;   (dolist (shell-command commands)
;;     (rp (format nil "cd ~A && ~A" dir shell-command) output-stream)))

;; (defun emerge-and-log (emerge-string)
;;   "be careful of format escapes!"
;;   (let* ((logfile))
;;     (uiop:run-program (format "emerge ~a" emerge-string) :output logfile)))

;; (defun init-logging ()
;;   (setf build-start (get-universal-time))
;;   (rp (format nil "echo '(:MESSAGE \"Build init\" :TIME ~d)' > ~a" build-start (build-log-pathname)))
;;   (let* ((command-string (format nil "exec xterm -e tail -f ~a" (build-log-pathname))))
;;     (stumpwm::run-commands command-string)))

;; (defun log-format (format-string &rest args)
;;   (with-open-file (stream (build-log-pathname)
;; 			  :direction :output
;; 			  :if-exists :append
;; 			  :if-does-not-exist :create)
;;     (write (list :message (apply #'format (cons nil (cons format-string args)))
;; 		 :time (get-universal-time))
;; 	   :stream stream)
;;     (terpri stream)))

;; (defun build-log-pathname () (format nil "/tmp/masamune-build-log-~d" build-start))

;; (defun directory-pathname-p (pathname)
;;   (string= "/" (subseq pathname (- (length pathname) 1) (length pathname))))

;; (defun qlpp (string)
;;   "[q]uicklisp [l]ocal [p]rojects [p]athname"
;;   (format nil "~~/quicklisp/local-projects/masamune-os/"))

;; (defun downolad-hyperspec ()
;;   (rp-in-dir
;;    '("curl ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz > /tmp/HyperSpec-7-0.tar.gz"
;;      "tar xzf /tmp/HyperSpec-7-0.tar.gz")
;;    "/tmp/")
;;   (rename-file "/tmp/HyperSpec" "~/lisp/HyperSpec"))

;; (defun configure-video-card ()
;;   "TODO automate video card choice"
;;   (format t "~%Use the guide http://www.funtoo.org/Video to determine what video card you are running and supply the identifier string for the VIDEO_CARDS key value pair. (be warned this string is not validated because this /should/ be automated) the string should be enclosed in double quotes, e.g. \"intel\":"))
;;   (with-open-file (stream "/etc/make.conf"
;; 			  :if-exists :append
;; 			  :if-does-not-exist :create
;; 			  :direction :output)
;;     (format stream "~%VIDEO_CARDS=~s" (read *query-io*)))

;; (defun start-interactive-install ()
;;   (labels ((mouse-focus-policy () ;; should really go in lisp-customizations.lisp
;; 	     (let* ((input (read *query-io*)))
;; 	       (if (member input '(:ignore :sloppy :click) :test #'eq)
;; 		   mouse-focus-policy
;; 		   (request-mouse-focus-policy)))))
;;     (format t "The mouse focus policy decides how the mouse affects input focus. Possible values are :ignore, :sloppy, and :click. :ignore means stumpwm ignores the mouse. :sloppy means input focus follows the mouse; the window that the mouse is in gets the focus. :click means input focus is transfered to the window you click on.")
;;     (format t "Which mouse focus policy would you prefer when X starts?~%")
;;     (with-open-file ("~/.stumpwmrc" :direction :output
;; 				    :if-exists :append
;; 				    :if-does-not-exist :create)
;;       (format stream
;; 	      "(setq *input-window-gravity* :center
;;        *message-window-gravity* :center
;;        *normal-border-width* 0
;;        *window-border-style* :none
;;        *transient-border-width* 0
;;        *top-level-error-action* :break
;;        *mouse-focus-policy* ~a)" (mouse-focus-policy))))
  
;;   (build-x)
;;   (dolist (pathname '("~/.masamune/emacs-desktop-state/"
;; 		      "~/.masamune/pclos-datastore/"
;; 		      "~/Pictures/screenshots/"
;; 		      "~/algol/"
;; 		      "~/lisp/"
;; 		      "~/quicklisp/local-projects/"))
;;     (when (and (not (probe-file pathname)) (directory-pathname-p pathname)
;; 	       (rp (format nil "mkdir -p ~a" pathname)))))  
;;   (rp-in-dir '("git clone https://github.com/gabriel-laddel/inxi.git")
;; 	     "~/algol/")
;;   (download-hyperspec)
;;   (macrolet ((f (file form) `(with-open-file (stream ,file
;; 						     :direction :output
;; 						     :if-exists :supersede
;; 						     :if-does-not-exist :create)
;; 			       (format stream "~A" ,form))))
;;     (f "~/.swank.lisp" "(setf swank::globally-redirect-io t)")  
;;     (f "~/.emacs" "(load \"~/quicklisp/local-projects/masamune-os/init.el\")") ;; XXX
;;     (f "~/.sbclrc" (concatenate "#" "-quicklisp"
;; 				;; ^ paredit doens't understand reader macros in strings
;; 				"(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\"
;;                                        (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))" 
;; 				'string)))
;;   (build-sbcl))

;; (labels ((undocumented-territory ()
;; 	   (format t "you're in undocumented territory, see the bottom of http://www.funtoo.org/X_Window_System for more information. If you could report this as a bug on http://github.com/gabriel-laddel/masamune and include as much information about the box in question that you're comfortable sharing it would be greatly appreciated.")))
;;   (if (probe-file "/proc/config.gz")
;;       (format t "You're in undocumented territory, and ")
;;       (if (string= "CONFIG_INPUT_EVDEV=y")
;; 	  (emerge-and-log "xf86-input-evdev")
;; 	  (undocumented-territory))
;;       (rp "zcat /proc/config.gz | grep EVDEV")
;;       (undocumented-territory)))

;;; build SBCL, Emacs, Stumpwm, Maxima
;;; ============================================================================

;; (defun build-maxima ()
;;   ;; XXX 2014-11-11T09:20:41-08:00 Gabriel Laddel
;;   ;; maxima needs to be converted to a quicklisp friendly format. until that
;;   ;; point, building via emerge is acceptable. It won't have swank, but
;;   ;; should work nonetheless.
;;   (rp "emerge maxima"))

;; (defvar portage-packages
;;   '(:required ("dev-util/ctags")
;;     :gimp ("app-doc/gimp-help"
;; 	   "media-gfx/gimp-arrow-brushes"
;; 	   "media-gfx/gimp")
;;     :x11 ("x11-apps/xrandr"
;; 	  ;; xrandr is needed for multiple screens and stumpwm. The other two
;; 	  ;; are included for the eventual removal of X.
;; 	  "x11-misc/xcalib" 
;; 	  "x11-apps/xdpyinfo")
;;     :misc ("media-gfx/imagemagick"
;; 	   "app-text/sloccount"
;; 	   "sys-apps/lshw"
;; 	   "net-wireless/aircrack-ng"
;; 	   "net-analyzer/nmap"
;; 	   "sys-process/htop"))
;;   "portage packages that don't belong anywhere else. I've tried to install
;;   packages via emerge only when required so there will be calls to emerge in
;;   functions like `build-lispkit'")

;;; Emacs Speak
;;; ============================================================================

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

;;; Networking
;;; ============================================================================

;; (defun install-wpa-supplicant ()
;;   "http://www.funtoo.org/Funtoo_Linux_Networking"
;;   (with-open-file (stream "/etc/portage/package.use"
;; 			  :direction :output
;; 			  :if-exists :append
;; 			  :if-does-not-exist :create)
;;     (format stream "~&net-wireless/wpa_supplicant qt4"))
;;   (rp "emerge wpa_supplicant")
;;   (rp "rc-update add dhcpcd default") ;; ensure we can use ethernet
;;   (rp "rc-update add wpa_supplicant"))


;; (define-condition failed-emerge (error) 
;;       ((shell-program :initarg :shell-program :accessor shell-program)
;;        (shell-program-output :initarg :shell-program-output :accessor shell-program-output)) 
;;       (:report (lambda (condition stream)
;; 		 (format stream "Attempted to run the shell program ~s which failed with the output~%~%~a"
;; 			 (shell-program condition)
;; 			 (shell-program-output condition)))))
 
