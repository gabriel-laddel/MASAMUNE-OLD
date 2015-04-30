;;; Save state
;;; ============================================================================
;;;
;;; many thanks to www.gentei.org/~yuuji/ who made this much easier
;;;
;;; Emacs, Stumpwm and Conkeror are all capable of saving and restoring a
;;; previous state. CLIM applictions are generally structured such that state
;;; can be saved in the appliction object itself via `define-appliction-frame'.
;;; 
;;; desktop.el is traditionally used to save state for Emacs, Stumpwm comes with
;;; its own set of related primitives and Conkeror has the ability to, but I'm
;;; not aware of anyone who has made a 'mode' for it.
;;;
;;; In my own hacking sessions Emacs generally manages 100s of buffers, Conkeror
;;; has 10-30 (at which point it really starts to slow down) and one or two
;;; CLIM windows, possibly a xterm that ncurses some music. Saving state for all
;;; but Xterm is the goal.
;;;
;;; TODO
;;; ============================================================================
;;; - docview
;;; - browser
;;; - global winner mode
;;; - Save mark rings.
;;; - REPL history?
;;; 
;;; We load state into emacs via the `after-init-hook'
;;;
;;; Unlike desktop.el etc. we ignore major and minor modes. Emacs is being cut
;;; down to a shim for hacking lisp and nothing more.
;;;
;;; the required elisp:
;;
;; (defun send-emacs-state ()
;;   (slime-eval-async `(cl::setf (cl::getf mm::state :emacs) (quote ,(current-window-configuration-printable)))))
;;
;; (run-with-idle-timer .1 t #'send-emacs-state)

(in-package #:mm)

(defvar state
  '(:mode-line nil
    :emacs nil
    ;; restore emacs state via `restore-window-configuration'
    :browser nil
    :clim nil
    :stumpwm nil)
  "the :browser's buffer order is significant - `car' is the focused buffer")

(defvar storage-dir #P"~/.masamune/desktops/")

(unless (probe-file storage-dir) (mkdir storage-dir))

(defvar desktop-lookup ()
  "strings cannot be used as register identifiers for emacs, hence a lookup")

(defun save-desktop (&optional name)
  (assert (or (not name) (keywordp name)))  
  (let* ((id (get-universal-time)))
    (when (member name (vals desktop-lookup))
      (let* ((key (getf (reverse desktop-lookup) name)))
	(delete-if (lambda (o) (or (equal name o) (equal key o))) desktop-lookup)))
    (stumpwm::dump-desktop-to-file (cat storage-dir id))
    (swank::eval-in-emacs (list 'window-configuration-to-register id) t)
    (when name (setf (getf desktop-lookup id) name))
    (or name id)))

(defun restore-desktop (id-or-name)
  (let* ((id-or-name (or (getf (reverse desktop-lookup) id-or-name) id-or-name)))
    (stumpwm::restore-desktop (stumpwm::read-dump-from-file (cat storage-dir id-or-name)))
    (swank::eval-in-emacs (list 'jump-to-register id-or-name) t)
    id-or-name))

(defun desktops ()
  (mapcar (lambda (p) (let* ((id (read-from-string (pathname-name p))))
		   (or (getf desktop-lookup id) id)))
	  (ls storage-dir)))

;; (defun restore-state (&optional (state state))
;;   (let* ((swank::*emacs-connection* (car swank::*connections*)))
;;     (with-getfs (:mode-line :emacs :browser :clim :stumpwm) state
;;       (if mode-line (stumpwm::mode-line-on) (stumpwm::mode-line-off))
;;       (when emacs (swank::eval-in-emacs (list 'restore-window-configuration emacs) t))
;;       (when browser (format t "~&Failed to restore browser state: ~A" browser))
;;       (when clim (format t "~&Failed to restore CLIM state: ~A" clim))
;;       (stumpwm::restore-desktop stumpwm))))

;; (setf (getf state :stumpwm) (stumpwm::dump-desktop))
