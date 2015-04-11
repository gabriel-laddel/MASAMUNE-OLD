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
    (rp-in-dir '("autoconf" "./configure" "make" "make install") stumpwm-location *standard-output*)))

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
	  "(in-package :stumpwm)~%(ql:quickload 'swank)
(swank:create-server :port 4005 :style swank:*communication-style* :dont-close t)~%(emacs)"))

(format t 
"
Afaik there doesn't exist a program today that will get a list of all the
hardware you have and check this against a canonical lookup table that
Intel/AMD/opensource vendors co-develop (or someone aggregates in some automated
fashion) to map drivers to chips and then offer you the option of installing XYZ
drivers for each chip.

Morons. 

Anyways, you want to install video drivers so that X will be able to
start. Follow the guides,

http://www.funtoo.org/X_Window_System
http://www.funtoo.org/Video 

and add the correct chipset identifier to /etc/make.conf. The guides suck and 
if you find that your particular setup isn't adequately documented try 

\"emerge -s driver\" 

at the shell to get a list of all drivers.

Emacs is installed at this point, and using it to glance through the output will
probably be useful (as opposed to using whatever key combo is available at the
console shell). If you're unfamilar with Emacs, use M-x shell (alt-x, \"shell\"
followed by RET).  You can scroll backwards with M-v and forwards with C-v
(control-v).

You can install your driver of choice with 

\"emerge <selection name>\"

When the driver finished installing, quit Emacs, 

\"echo 'stumpwm' >> .xinitrc\"
\"startx\"

The build process will continue after starting X, and will take ~the better part
of a day, with periodic input from you.

[Note: why didn't I do this in a script? It turns out that installing the drivers
will clear the X init file.]

This message is located at the bottom of the file:

\"~/quicklisp/local-projects/masamune/build/cripple-mode-install.lisp\"

")
(quit)
