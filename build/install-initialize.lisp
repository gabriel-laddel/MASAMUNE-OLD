(in-package #:common-lisp-user)

(sb-ext:restrict-compiler-policy 'debug 3)

(defun rp (shell-string &optional (output-stream :string))
  (uiop:run-program shell-string :output output-stream))

(defun rp-in-dir (commands dir &optional (output-stream :string))
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command) output-stream)))

(defun cat (&rest objs)
  (apply #'concatenate 'string
	 (mapcar (lambda (o) (if (stringp o) o (write-to-string o))) objs)))

(defun build-x-and-emacs ()
  (rp (cat "curl http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz"
	   " > /tmp/emacs-24.4.tar.xz"
	   " && cd ~/quicklisp/local-projects/"
	   " && tar xf /tmp/emacs-24.4.tar.xz"))
  (dolist (s '("virtual/jpeg" "media-libs/tiff" "media-libs/giflib"
	       "x11-libs/libXpm" "xorg-x11"))    
    (rp (format nil "emerge ~a" s) *standard-output*))
  (rp-in-dir '("./configure" "make" "make install")
	     "~/quicklisp/local-projects/emacs-24.4/"
	     *standard-output*))

(let* ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (progn 
	(rp "curl http://beta.quicklisp.org/quicklisp.lisp > /tmp/quicklisp.lisp")
	(load "/tmp/quicklisp.lisp")
	(quicklisp-quickstart:install))))

(ql:quickload 'swank)
(ql:quickload 'cl-ppcre)

(build-x-and-emacs)

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
(rp "emerge stumpwm" *standard-output*)

(with-open-file (stream "~/.stumpwmrc"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
  (format stream
	  "(in-package :stumpwm)~%(ql:quickload 'swank)
(swank:create-server :port 4005 :style swank:*communication-style* :dont-close t)~%(emacs)"))

(with-open-file (stream "~/.xinitrc"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
  "startx")

(format *standard-output* 
	"Almost there. Run 'startx' at the shell, which will boot X and kick off the remainder of the install process (this will require input from you to ensure that the mouse and keyboard work). ")
  
(quit)
