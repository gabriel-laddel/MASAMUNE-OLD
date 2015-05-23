(in-package #:common-lisp-user)

;;; XXX 2015-05-23T13:07:31+00:00 Gabriel Laddel
;;; this script *will overwrite* your ~/.emacs, ~/.stumpwmrc and ~/.swank.lisp
;;; files. If these are in anyway valuable please back them up.
;;;
;;; The code in your ~/.emacs can move to
;;; ~/quicklisp/local-projects/masamune/emacs-customizations.el
;;; 
;;; the code from your ~/.stumpwmrc,
;;; ~/quicklisp/local-projects/masamune/lisp-customization.lisp
;;;
;;; ~/.swank.lisp ? IDK, do what you feel appropriate. 
;;; 
;;; I'm assuming you've got SBCL, quicklisp, stumpwm, and X installed already
;;; and that X boots into stumpwm by default. I also assume that quicklisp will
;;; be loaded into SBCL by defualt.
;;;
;;; using this file to install Masamune amounts to
;;; cd
;;; sbcl --load ~/quicklisp/local-projects/masamune/build/trinque.lisp

(ql:quickload '(cl-fad alexandria swank cl-ppcre))

(defvar masamune-pathnames
  '("~/.masamune/pclos-datastore/"
    "~/screenshots/"
    "~/algol/"
    "~/lisp/"))

(defun cat (&rest objs)
  (apply #'concatenate 'string
	 (mapcar (lambda (o) (if (stringp o) o (write-to-string o))) objs)))

(defun rp (program-string &optional (output-stream :string) (ignore-error-status nil))
  "shorthand, returns shell program output as string"
  (run-program program-string :output output-stream :ignore-error-status ignore-error-status))

(defun rp-in-dir (commands dir &optional (output-stream :string) (ignore-error-status nil))
  (if (listp  commands)
      (dolist (shell-command commands)
	(rp (format nil "cd ~A && ~A" dir shell-command) output-stream ignore-error-status))
      (rp (format nil "cd ~A && ~A" dir commands) output-stream ignore-error-status)))

(defun lg (message)  (format t (format nil "~%~a" message)))
(defun k (j)  (rp (format nil "emerge ~a" j) *standard-output*))

(defun install-misc-x-extensions ()
  (dolist (s '("x11-apps/xrandr"
	       "x11-misc/xcalib" 
	       "x11-apps/xdpyinfo"))
    (k s)))

(defun install-misc-mm-required ()
  "masamune requires these for various reasons"
  (dolist (s '("media-gfx/imagemagick"
	       "app-text/sloccount"
	       "sys-apps/lshw"
	       "net-wireless/aircrack-ng"
	       "app-text/sloccount"	       
	       "app-misc/mc" ;; see #bitcoin-assets !s midnight-commander
	       "net-analyzer/nmap"
	       "sys-process/htop"
	       "netcat")) ;; useful for printing a bunch of output to the screen
    (k s)))

(defun download-hyperspec ()
  (rp-in-dir
   '("curl ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz > /tmp/HyperSpec-7-0.tar.gz"
     "tar xzf /tmp/HyperSpec-7-0.tar.gz")
   "/tmp/")
  (rename-file "/tmp/HyperSpec" "~/lisp/HyperSpec"))

(defun build-emacs ()
  (rp (cat "curl http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz"
	   " > /tmp/emacs-24.4.tar.xz"
	   " && cd ~/quicklisp/local-projects/"
	   " && tar xf /tmp/emacs-24.4.tar.xz"))
  (dolist (s '("virtual/jpeg" "media-libs/tiff" "media-libs/giflib" "x11-libs/libXpm"))    
    (rp (format nil "emerge ~a" s) *standard-output*))
  (rp-in-dir '("./configure" "make" "make install")
	     "~/quicklisp/local-projects/emacs-24.4/"
	     *standard-output*))

(defun write-dotfiles ()
  (macrolet ((f (file form) `(with-open-file (stream ,file
						     :direction :output
						     :if-exists :supersede
						     :if-does-not-exist :create)
			       (format stream "~A" ,form))))
    (f "~/.swank.lisp" ";;; -*- Mode: Lisp -*-~%(in-package #:cl)~%(setf swank::globally-redirect-io t)")  
    (f "~/.emacs" "(load \"~/quicklisp/local-projects/masamune/init.el\")") ;; XXX
    (f "~/.stumpwmrc" (format nil ";;; -*- Mode: Lisp -*-~%(in-package #:stumpwm)~%(ql:quickload 'swank)~%(swank:create-server :port 4005 :dont-close t)~%~S~%~S"
			      '(setq
				*input-window-gravity* :center
				*message-window-gravity* :center
				*normal-border-width* 0
				*window-border-style* :none
				*transient-border-width* 0
				*top-level-error-action* :break
				*startup-message* "Please wait for Masamune to start - this might take a minute")
			      '(run-or-raise "emacs --debug-init" '(:class "Emacs"))))))

(defun build-conkeror ()
  "XXX this takes ages to install."
  (with-open-file (stream "/etc/portage/package.use"
			  :if-exists :append
			  :if-does-not-exist :create
			  :direction :output)
    ;; NOTE 2015-04-13T04:41:44+00:00 Gabriel Laddel
    ;; conkeror won't install without these.
    (format stream "~%~A~%~A~%~A"
	    "=x11-libs/cairo-1.12.18-r1 X"
	    ">=media-libs/libpng-1.6.15 apng"
	    ">=x11-libs/gdk-pixbuf-2.31.1 X"))
  (rp "emerge conkeror" *standard-output*)
  (dolist (install-dir '("~/algol/xulrunner/" "~/algol/conkeror/"))
    (when (probe-file install-dir) (cl-fad:delete-directory-and-files install-dir)))
  (with-open-file (stream "/tmp/conkeror-install-log.lisp" 
			  :if-exists :append
			  :if-does-not-exist :create
			  :direction :output)
    (format stream "~S~%"
	    (labels ((j (strings) (apply #'cat (butlast (interpose " && " strings))))
		     (fn (&rest args) (apply (alexandria:curry 'format nil) args)))
	      (let* ((xulrunner-ftp-uri "http://ftp.mozilla.org/pub/mozilla.org/xulrunner/releases/33.1/runtimes/xulrunner-33.1.en-US.linux-x86_64.tar.bz2")
		     (xulrunner-tmp-location "/tmp/xulrunner-33.1.en-US.linux-x86_64.tar.bz2"))
		(list :start-time (get-universal-time)
		      :download-xulrunner (rp (fn "curl ~a > ~a" xulrunner-ftp-uri xulrunner-tmp-location))
		      :extract-xulrunner (rp (fn "mv ~a ~~/algol/ && cd ~~/algol/ && tar xvjf xulrunner-33.1.en-US.linux-x86_64.tar.bz2" xulrunner-tmp-location))		      
		      :clone-conkeror (rp (j '("mkdir ~/algol/conkeror/"  
					       "cd ~/algol/conkeror/" 
					       "git init"
					       "git remote add origin git://repo.or.cz/conkeror.git"  
					       "git fetch origin 48d3ef4369f267faf42451a580b1ac6bcb6a5a18:refs/remotes/origin/master"
					       "git reset --hard FETCH_HEAD && make"))) ;; `make' is neccecary to edit conkeror text fields from Emacs.
		      :write-conkerorrc (cl-fad:copy-file (ppath "/browser/default-conkerorrc.js") "~/.conkerorrc" :overwrite t)
		      :copy-help-files (loop for path in (remove-if-not (lambda (pathname) (let* ((namestring (namestring pathname)))
											(string= "html" (subseq namestring (- (length namestring) 4)))))
									(cl-fad:list-directory "/root/quicklisp/local-projects/masamune/browser/help/"))
					     for path-filename = (llast (cl-ppcre::split "/" path))
					     do (cl-fad:copy-file path (merge-pathnames #P"/root/algol/conkeror/help/" path-filename) :overwrite t))
		      :end-time (get-universal-time))))))
  (format t "~%~%Conkeror install finished. Build log available at /tmp/conkeror-install-log.lisp~%~%"))

(loop for k in masamune-pathnames unless (probe-file k)
      do (rp (format nil "mkdir -p ~a" k) *standard-output*))
(lg "created masamune pathnames")
(install-misc-x-extensions)
(lg "installed misc x extensions")
(install-misc-mm-required)
(lg "installed misc mm required")
(k "app-text/enchant")
(lg "installed spellchecking program 'echant'")
(k "app-text/ghostscript-gpl")
(lg "installed ghostscript")
(k "xterm")
(lg "installed xterm")
(download-hyperspec)
(lg "downloaded hyperspec")
(rp "cd ~/algol && git clone https://github.com/gabriel-laddel/inxi.git")
(lg "clone'd inxi into ~~/algol/inxi/. See `mm::machine-information' if you're curious as to why this is being installed.")
(build-emacs)
(lg "built emacs")
(build-conkeror)
(lg "built conkeror")
(write-dotfiles)
(lg "wrote dotfiles")
(lg
"Maxima is the last system that needs to be installed. I don't know what strings
need to be added to /etc/portage/package.use so you're going to have to emerge
it manually. The easiest way is to \"emerge app-emacs/imaxima\" and then just
add whatever it asks for.

This is known not to work anymore due to whatever, idiots. See:

https://forums.gentoo.org/viewtopic-t-459152-start-0.html

I have a working imaxima/maxima/emacs setup on my machine and will figure out
why it works as soon as I've another machine to hack on.

The changelogs for indicate that there are only a few things that could
have been changed.

http://gentoobrowse.randomdan.homeip.net/package/app-emacs/imaxima
http://gentoobrowse.randomdan.homeip.net/package/sci-mathematics/maxima

An error report on the matter indicates that one might be able to just
change the version and move on.

https://bugs.gentoo.org/show_bug.cgi?id=448242

Sorry for the mess!")
