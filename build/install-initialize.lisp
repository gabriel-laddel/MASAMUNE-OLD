(in-package #:common-lisp-user)

;;; XXX this code is very order-dependent

;; virtual/jpeg
;; media-libs/tiff
;; media-libs/giflib
;; x11-libs/libXpm
;; x11-base/xorg-x11
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

;;; then build emacs with
;; ./configure 
;; make 
;; make install

;;; then build stumpwm with
;;; sbcl --eval (progn (ql:quickload '(clx cl-ppcre)))

(sb-ext:restrict-compiler-policy 'debug 3)

;;; NOTE 2015-05-24T04:37:57+00:00 Gabriel Laddel
;;; we load swank and clx now so the systems are on disk for
;;; install-finalize.lisp and installing stumpwm
(ql:quickload '(cl-ppcre swank clx))

(defun rp (shell-string &optional (output-stream :string))
  (uiop:run-program shell-string :output output-stream))

(defun rp-in-dir (commands dir &optional (output-stream :string))
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command) output-stream)))

(defun cat (&rest objs)
  (apply #'concatenate 'string
	 (mapcar (lambda (o) (if (stringp o) o (write-to-string o))) objs)))

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
(rp "cd ~/algol/ && git clone https://github.com/gabriel-laddel/inxi.git" *standard-output*)
(download-hyperspec)
(rp "cp ~/quicklisp/local-projects/masamune/build/temporary-dot-emacs.el ~/.emacs")

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
  (format stream
	  "/root/quicklisp/local-projects/stumpwm/stumpwm > ~~/.masamune/stumpwm-output~%export EDITOR=emacsclient"))

(rp (cat "curl http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz"
	 " > /tmp/emacs-24.4.tar.xz"
	 " && cd ~/quicklisp/local-projects/"
	 " && tar xf /tmp/emacs-24.4.tar.xz")
    *standard-output*)

(rp-in-dir '("git clone https://github.com/stumpwm/stumpwm.git")
	   "~/quicklisp/local-projects/"
	   *standard-output*)

(format *standard-output* 
	"Almost there. Run 'startx' at the shell, which will boot X and kick off the remainder of the install process (this will require input from you to ensure that the mouse and keyboard work). ")
  
(quit)
