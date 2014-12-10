(in-package #:common-lisp-user)

(sb-ext:restrict-compiler-policy 'debug 3)

(defun rp (shell-string &optional (output-stream :string))
  (uiop:run-program shell-string :output output-stream))

(defun rp-in-dir (commands dir &optional (output-stream :string))
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command) output-stream)))

(defun build-emacs-and-x ()
  (rp (concatenate 'string
		   "curl http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz"
		   "> /tmp/emacs-24.4.tar.xz"
		   " && cd ~/quicklisp/local-projects/"
		   " && tar xf /tmp/emacs-24.4.tar.xz"))
  (dolist (s '("virtual/jpeg" "media-libs/tiff" "media-libs/giflib"
	       "x11-libs/libXpm" "xorg-x11" "-1N xinit"))    
    (rp (format nil "emerge ~a" s) *standard-output*))
  (rp "echo 'x11-apps/xinit -minimal >> /etc/portage/package.use'")
  (rp-in-dir '("./configure" "make" "make install")
	     "~/quicklisp/local-projects/emacs-24.4/"
	     *standard-output*))

(rp "curl http://beta.quicklisp.org/quicklisp.lisp > /tmp/quicklisp.lisp")
(load "/tmp/quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:quickload 'cl-ppcre)
(ql:quickload 'swank)

(let* ((desktop-profile-line (some (lambda (s) (when (cl-ppcre:scan "desktop" s) s))
				   (cl-ppcre:split "\\n" (rp "eselect profile list"))))
       (desktop-profile-id (read-from-string (car (cl-ppcre:all-matches-as-strings "\\d" desktop-profile-line)))))
  (rp (format nil "eselect profile set-flavor ~d" desktop-profile-id)))

(build-emacs-and-x)

(defun build-stumpwm ()
  "XXX this particular SBCL rc file is required for to stumpwm build to work as it
makes use of quicklisp to load cl-ppcre etc."
  (with-open-file (stream "~/.sbclrc" :direction :output
				      :if-exists :supersede
				      :if-does-not-exist :create)
    (format stream "~s" '(load "~/quicklisp/setup.lisp")))
  (ql:quickload 'stumpwm)
  (let* ((stumpwm-version (rp "ls ~/quicklisp/dists/quicklisp/software/ | grep stumpwm"))
	 (stumpwm-version (subseq stumpwm-version 0 (- (length stumpwm-version) 1)))
	 (stumpwm-location (format nil "~~/quicklisp/dists/quicklisp/software/~a" stumpwm-version)))
    (rp-in-dir '("autoconf" "./configure" "make" "make install") stumpwm-location *standard-output*))
  (with-open-file (s "~/.xinitrc"
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    "/usr/local/bin/stumpwm"))

(build-stumpwm)

(with-open-file (stream "~/.sbclrc"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
  (concatenate 'string
	       "#" "-quicklisp"
	       ;; xxx paredit doens't understand reader macros in strings
	       "(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\"
                                       (user-homedir-pathname))))
		 (when (probe-file quicklisp-init)
		   (load quicklisp-init))"))

(rp "cp ~/quicklisp/local-projects/masamune/build/temporary-dot-emacs.el ~/.emacs")

(with-open-file (stream "~/.stumpwmrc"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
  (format stream
	  "(in-package :stumpwm)~%(ql:quickload 'swank)~%(mode-line)
(swank:create-server :port 4005 :style swank:*communication-style* :dont-close t)~%(emacs)"))

(format t "Matching drivers up with chips appears to be a /very/ difficult problem. So difficult in fact that it isn't yet solved. You might find this hard to believe, but well - believe it. There doesn't exist a program today that will get a list of all the hardware you have and check this against a canonical lookup table that Intel/AMD/opensource ventors co-develop (or whatever - someone aggregates in some automated fashion) to map drivers to chips and then offer you the option of installing XYZ drivers for each chip. Can't be done.~%~%Morons.~%~%Anyways, you want to install video drivers so that X will be able to start. Follow the guide here: http://www.funtoo.org/Video (make sure to update /etc/make.conf with the correct chipset identifier!)~%~%The guide sucks and if you find that your particular setup isn't adequately documented try running \"emerge -s driver\" (which will return a list of all drivers) glance through it (emacs will be installed at this point so \"emacs\" and run in an async shell command with \"M-&\" to scroll around and see what looks \"close enough\"). When you find a candidate run \"emerge <name of candidate>\" in a shell. When this finishes run \"startx\" to continue the install process - if it fails, try again with a different setup).~%~%I wish you the best on this irritating journey.")
(quit)
