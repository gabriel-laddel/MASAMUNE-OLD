(in-package #:common-lisp-user)

;;; TODO
;;; - build emacs by hand
;;; - build stumpwm, git clone https://github.com/stumpwm/stumpwm.git
;;; - echo ~/quicklisp/.../stumpwm/stumpwm >> ~/.xinitrc
;;; - emerge xterm ahead of starting stumpwm
;;; - change startup message in intermediate ~/.stumpwmrc
;;; - void function string-matches-p in ~/.tempemacs
;;; - ~/.../stumpwm > ~/.masamune/stumpwm-output
;;; - (redirect-all-output "~/.masamune/stumpwm-debug-output")

;; /usr/local/bin/stumpwm > ~/.masamune/stumpwm-output
;; export EDITOR=emacsclient

;;; emerge this crud

;; app-text/enchant
;; app-text/ghostscript-gpl
;; media-gfx/imagemagick
;; sys-apps/lshw
;; net-wireless/aircrack-ng
;; app-text/sloccount
;; app-misc/mc
;; net-analyzer/nmap
;; sys-process/htop
;; net-analyzer/netcat
;; x11-apps/xrandr
;; x11-misc/xcalib 
;; x11-apps/xdpyinfo
;; xterm

;; 12.3 Debugging StumpWM

;; Variable: *debug-level*

;;     Set this variable to a number > 0 to turn on debugging. The greater the number the more debugging output. 

;; Variable: *debug-stream*

;;     This is the stream debugging output is sent to. It defaults to *error-output*. It may be more convenient for you to pipe debugging output directly to a file. 

;; Function: redirect-all-output file

;;     Elect to redirect all output to the specified file. For instance, if you want everything to go to ~/.stumpwm.d/debug-output.txt you would do:
     	

;;     (redirect-all-output (data-dir-file "debug-output" "txt"))


(sb-ext:restrict-compiler-policy 'debug 3)

;;; NOTE 2015-05-24T04:37:57+00:00 Gabriel Laddel
;;; we load swank now so the system is on disk for install-finalize.lisp
(ql:quickload 'swank)
(ql:quickload 'cl-ppcre)

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
	   " && tar xf /tmp/emacs-24.4.tar.xz")
      *standard-output*)
  (dolist (s '("virtual/jpeg" "media-libs/tiff" "media-libs/giflib"
	       "x11-libs/libXpm" "x11-base/xorg-x11"))    
    (rp (format nil "emerge ~a" s) *standard-output*))
  (rp-in-dir '("./configure" "make" "make install")
	     "~/quicklisp/local-projects/emacs-24.4/"
	     *standard-output*))

(defun download-hyperspec ()
  (rp-in-dir
   '("curl ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz > /tmp/HyperSpec-7-0.tar.gz"
     "tar xzf /tmp/HyperSpec-7-0.tar.gz")
   "/tmp/"
   *standard-output*)
  (rename-file "/tmp/HyperSpec" "~/lisp/HyperSpec"))

(loop for k in '("~/.masamune/pclos-datastore/"
		 "~/screenshots/"
		 "~/algol/"
		 "~/lisp/") 
      unless (probe-file k)
      do (rp (format nil "mkdir -p ~a" k) *standard-output*))
(lg "wrote masamune pathnames")
(rp "cd ~/algol/ && git clone https://github.com/gabriel-laddel/inxi.git" *standard-output*)
(lg "clone'd inxi into ~~/algol/inxi/. See `mm::machine-information' if you're curious as to why this is being installed.")
(download-hyperspec)
(lg "downloaded hyperspec")

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
	  ";;; -*- Mode: Lisp -*-
(in-package :stumpwm)
(redirect-all-output \"~~/.masamune/stumpwm-debug-output\")
(ql:quickload 'swank)
(swank:create-server :port 4005 :style swank:*communication-style* :dont-close t)
(run-or-raise \"emacs --debug-init\" '(:class \"Emacs\"))"))

(with-open-file (stream "~/.xinitrc"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
  "/root/quicklisp/local-projects/stumpwm/stumpwm > ~/.masamune/stumpwm-output")

(format *standard-output* 
	"Almost there. Run 'startx' at the shell, which will boot X and kick off the remainder of the install process (this will require input from you to ensure that the mouse and keyboard work). ")
  
(quit)
