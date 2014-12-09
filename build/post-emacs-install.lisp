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

(defun rp (shell-string)
  (uiop:run-program shell-string :output :string))

(defun rp-in-dir (commands dir)
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command))))

;; (defun emerge-and-log (emerge-string)
;;   "be careful of format escapes!"
;;   (let* ((logfile))
;;     (uiop:run-program (format "emerge ~a" emerge-string) :output logfile)))
