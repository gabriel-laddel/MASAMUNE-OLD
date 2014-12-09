(in-package #:common-lisp-user)

(sb-ext:restrict-compiler-policy 'debug 3)

(defun rp (shell-string)
  (uiop:run-program shell-string :output :string))

(defun rp-in-dir (commands dir)
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command))))

(defun build-emacs ()
  (rp (concatenate 'string
		   "curl http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz"
		   "> /tmp/emacs-24.4.tar.xz"
		   " && cd ~/quicklisp/local-projects/"
		   " && tar xf /tmp/emacs-24.4.tar.xz"))
  (dolist (s '("virtual/jpeg" "media-libs/tiff" "media-libs/giflib"))
    (rp (format nil "emerge ~a" s)))
  (rp-in-dir '("./configure" "make" "make install") "~/quicklisp/local-projects/"))

(rp "curl http://beta.quicklisp.org/quicklisp.lisp > /tmp/quicklisp.lisp")
(load "/tmp/quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:quickload 'cl-ppcre)
(ql:quickload 'swank)
(let* ((desktop-profile-line (some (lambda (s) (when (cl-ppcre:scan "desktop" s) s))
				   (cl-ppcre:split "\\n" (rp "eselect profile list"))))
       (desktop-profile-id (read (car (cl-ppcre:all-matches-as-strings "\\d" desktop-profile-line)))))
  (rp (format "eselect profile set-flavor ~d" desktop-profile-id)))
(build-emacs)
(with-open-file (stream "~/.sbclrc"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
  (concatenate "#" "-quicklisp"
	       ;; xxx paredit doens't understand reader macros in strings
	       "'(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\"
                                       (user-homedir-pathname))))
		 (when (probe-file quicklisp-init)
		   (load quicklisp-init))
		 (ql:quickload 'swank)
		 (swank:create-server :port 4005 :style swank:*communication-style* :dont-close t))" 
	       'string))
(rp "curl https://raw.githubusercontent.com/gabriel-laddel/masamune/master/build/temporary-dot-emacs.el > ~/.emacs")
(format t "Start emacs to continue the install process")
(quit)
