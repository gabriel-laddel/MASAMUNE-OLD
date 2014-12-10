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

(defun build-stumpwm ()
  (labels ((stumpwm-dir (format nil "~~/quicklisp/dists/quicklisp/software/~a"
			 (rp "ls ~/quicklisp/dists/quicklisp/software/ | grep stumpwm"))))
    (rp-in-dir '("autoconf" "./configure" "make" "make install") (stumpwm-dir))
    (with-open-file (s "~/.xinitrc" :if-does-not-exist :create
    				    :if-exists :supersede)
      "/usr/local/bin/stumpwm")))

(defun configure-video-card ()
  "TODO automate video card choice"
  (format t "~%Use the guide http://www.funtoo.org/Video to determine what video card you are running and supply the identifier string for the VIDEO_CARDS key value pair. (be warned this string is not validated because this /should/ be automated) the string should be enclosed in double quotes, e.g. \"intel\":"))
  (with-open-file (stream "/etc/make.conf"
			  :if-exists :append
			  :if-does-not-exist :create
			  :direction :output)
    (format stream "~%VIDEO_CARDS=~s" (read *query-io*)))

(rp "curl http://beta.quicklisp.org/quicklisp.lisp > /tmp/quicklisp.lisp")
(load "/tmp/quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:quickload 'cl-ppcre)
(ql:quickload 'swank)
(let* ((desktop-profile-line (some (lambda (s) (when (cl-ppcre:scan "desktop" s) s))
				   (cl-ppcre:split "\\n" (rp "eselect profile list"))))
       (desktop-profile-id (read-from-string (car (cl-ppcre:all-matches-as-strings "\\d" desktop-profile-line)))))
  (rp (format nil "eselect profile set-flavor ~d" desktop-profile-id)))
(configure-video-card)
(build-emacs-and-x)
(build-stumpwm)
(with-open-file (stream "~/.sbclrc"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
  (concatenate 'string
	       "#" "-quicklisp"
	       ;; xxx paredit doens't understand reader macros in strings
	       "'(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\"
                                       (user-homedir-pathname))))
		 (when (probe-file quicklisp-init)
		   (load quicklisp-init))
		 (ql:quickload 'swank) "))
(with-open-file ("~/.stumpwmrc" :direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
  (format stream
	  "(in-package :stumpwm)~%(ql:quickload 'swank)~%(mode-line)~%(emacs)
(swank:create-server :port 4005 :style swank:*communication-style* :dont-close t)"))
(rp "curl https://raw.githubusercontent.com/gabriel-laddel/masamune/master/build/temporary-dot-emacs.el > ~/.emacs")
(format t "run \"emacs\" to continue the install process")
(quit)

