(in-package #:common-lisp-user)

(format t "please be patient and don't click on anything quite yet.") 

(sb-ext:restrict-compiler-policy 'debug 3)

(ql:quickload 'cl-fad)
(ql:quickload 'alexandria)

(defun cat (&rest objs)
  (apply #'concatenate 'string
	 (mapcar (lambda (o) (if (stringp o) o (write-to-string o))) objs)))

(defun rp (shell-string &optional (output-stream :string))
  (uiop:run-program shell-string :output output-stream))

(defun rp-in-dir (commands dir &optional (output-stream :string))
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command) output-stream)))

(defun interpose (o l)
  (loop for i in l append (list i o)))

(defun lg (message)  (format t (format nil "~%~a" message)))
(defun k (j)  (rp (format nil "emerge ~a" j) *standard-output*))

(defun install-conkeror ()
  "XXX this takes ages to install."
  ;; chmod +x xulrunner
  ;; 
  ;; (with-open-file (stream "/etc/portage/package.use"
  ;; 			  :if-exists :append
  ;; 			  :if-does-not-exist :create
  ;; 			  :direction :output)
  ;;   ;; NOTE 2015-04-13T04:41:44+00:00 Gabriel Laddel
  ;;   ;; conkeror won't install without these.
  ;;   (format stream "~%~A~%~A~%~A"
  ;; 	    "=x11-libs/cairo-1.12.18-r1 X"
  ;; 	    ">=media-libs/libpng-1.6.15 apng"
  ;; 	    ">=x11-libs/gdk-pixbuf-2.31.1 X"))
  ;; (rp "emerge conkeror" *standard-output*)
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
		      :download-xulrunner
		      (rp (fn "curl ~a > ~a" xulrunner-ftp-uri xulrunner-tmp-location))
		      :extract-xulrunner
		      (rp (fn "mv ~a ~~/algol/ && cd ~~/algol/ && tar xvjf xulrunner-33.1.en-US.linux-x86_64.tar.bz2" xulrunner-tmp-location))
		      :clone-conkeror
		      (rp (j
			   '("mkdir ~/algol/conkeror/"  
			     "cd ~/algol/conkeror/" 
			     "git init"
			     "git remote add origin git://repo.or.cz/conkeror.git"  
			     "git fetch origin 48d3ef4369f267faf42451a580b1ac6bcb6a5a18:refs/remotes/origin/master"
			     "git reset --hard FETCH_HEAD && make"))) ;; `make' is neccecary to edit conkeror text fields from Emacs.
		      :write-conkerorrc-file (cl-fad:copy-file "~/quicklisp/local-projects/masamune/browser/default-conkerorrc.js"
							       "~/.conkerorrc" :overwrite t)
		      :copy-help-files (loop for path in (remove-if-not (lambda (pathname) (let* ((namestring (namestring pathname)))
											(string= "html" (subseq namestring (- (length namestring) 4)))))
									(cl-fad:list-directory "/root/quicklisp/local-projects/masamune/browser/help/"))
					     for path-filename = (llast (cl-ppcre::split "/" path))
					     do (cl-fad:copy-file path (merge-pathnames #P"/root/algol/conkeror/help/" path-filename) :overwrite t))
		      :end-time (get-universal-time))))))
  (format t "~%~%Conkeror install finished. Build log available at /tmp/conkeror-install-log.lisp~%~%"))

(defun write-dotfiles ()
  (macrolet ((f (file form) `(with-open-file (stream ,file
						     :direction :output
						     :if-exists :supersede
						     :if-does-not-exist :create)
			       (format stream "~A" ,form))))
    (f "~/.swank.lisp" ";;; -*- Mode: Lisp -*-~%(in-package #:cl)~%(setf swank::globally-redirect-io t)")  
    (f "~/.emacs" "(load \"~/quicklisp/local-projects/masamune/init.el\")") ;; XXX
    (f "~/.stumpwmrc" (format nil ";;; -*- Mode: Lisp -*-~%(in-package #:stumpwm)~%(redirect-all-output \"~~/.masamune/stumpwm-debug-output\")~%(ql:quickload 'swank)~%(swank:create-server :port 4005 :dont-close t)~%~S~%~S"
			      '(setq
				*input-window-gravity* :center
				*message-window-gravity* :center
				*normal-border-width* 0
				*window-border-style* :none
				*transient-border-width* 0
				*top-level-error-action* :break
				*mouse-focus-policy* :sloppy
				*startup-message* "Please wait for Masamune to start - this might take a minute")
			      '(run-or-raise "emacs --debug-init" (:class "Emacs"))))))

(write-dotfiles)
(lg "wrote dotfiles")

;; (install-conkeror)
;; (lg "installed conkeror")

(cerror "my mouse and keyboard work as demonstrated by pressing this restart"
	"If the mouse and keyboard don't work you're in undocumented territory, see the bottom of http://www.funtoo.org/X_Window_System for more information. If you could report this as a bug on http://github.com/gabriel-laddel/masamune and include as much information about the box in question you're comfortable sharing it would be greatly appreciated.")

(lg "we've not yet installed conkeror, you might want to install it by hand")

;; (stumpwm::delete-window 
;;  (car (remove-if-not (lambda (w) (search "emacs" (window-name w))) 
;; 		     (stumpwm::all-windows))))
;; (stumpwm::emacs)
