(in-package #:parenscript)

(define-expression-operator inline-js (inline-js-string)
  `(ps-js:escape ,inline-js-string))

(in-package #:stumpwm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keyboard layout

(defun keyboard-layout ()
  (some (lambda (s) (when (string= "variant" (mm:take 7 s))
		 (mm::llast (mm::split #\space s))))
	(mm::split #\Newline (mm:run-program "setxkbmap -query" :output :string))))

(defcommand rotate-keyboard-layout () ()
  "toggle through various keyboard configurations"
  (let* ((layout (keyboard-layout)))
    (cond ((null layout)
	   (mm:run-program "setxkbmap us -variant colemak -option ctrl:nocaps")
	   (message-no-timeout "The current keyboard layout is COLEMAK. The keybinding to rotate the keyboard layout is C-t M-k, which maps to C-f M-n on the default QWERTY keyboard"))
	  ((string= "colemak" layout) 
	   (mm:run-program "setxkbmap us -variant dvorak -option ctrl:nocaps")
	   (message-no-timeout "The current keyboard layout is DVORAK. The keybinding to rotate the keyboard layout is C-t M-k, which maps to C-k M-v on the default QWERTY keyboard"))
	  ((string= "dvorak" layout)
	   (mm:run-program "setxkbmap us -option ctrl:nocaps")
	   (message-no-timeout "The current keyboard layout is QWERTY. The keybinding to rotate the keyboard layout is C-t M-k.")))))

(define-key STUMPWM::*GROUP-ROOT-MAP* (kbd "M-k") "rotate-keyboard-layout")

;;; networking
;;; ============================================================================

(defcommand network () () ""
  (run-commands "exec xterm -e nmtui"))

;;; audio
;;; ============================================================================

(defcommand increase-volume () ()
  "Increase the sound volume"
  (run-shell-command "amixer sset PCM 5+ unmute"))

(defcommand decrease-volume () ()
  "Decrease the sound volume"
  (run-shell-command "amixer sset PCM 5- unmute"))

;;; TODO 2014-11-10T08:03:40-08:00 Gabriel Laddel
;;; toggle

(defcommand mute () ()
  "Set the sound to mute"
  (run-shell-command "amixer sset PCM mute"))

;;; this is wrong - I need some way to bind the keys without the C-t prefix
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "increase-volume")
;; (define-key *group-root-map* (kbd "XF86AudioLowerVolume") "decrease-volume")
;; (define-key *group-root-map* (kbd "XF86AudioMute") "mute-volume")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; from a bunch of people's .stumpfiles

;; copy for uber mode
;; https://github.com/stumpwm/stumpwm/wiki/HandlingTheGimp

;;; TODO 2014-11-10T11:49:44-08:00 Gabriel Laddel
;;; review (ppath "../stumpwm-contrib/")

(defcommand echo-colors-brief () ()
  "Output a brief list of currently defined colors."
  (echo-string (current-screen) (eval "
BOLD ^B^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n
NONE ^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n")))

(defcommand echo-date () ()
  "Display date highlighting the most important parts"
  (message "^1*~a" (format-expand *time-format-string-alist*
                                  "^B%l^b:%M:%S %p ^B%a^b %Y-%m-^B%e^b")))

;;; TODO 2014-11-10T05:34:55-08:00 Gabriel Laddel
;;; stumpwm ships with a bunch of goodies and they should all be included

;;; hot modeline

;; (load-module "cpu")
;; (load-module "disk")
;; ;; (load-module "mpd")
;; (load-module "mem")
;; (load-module "net")
;; (load-module "battery-portable")
;; (load-module "notifications")
;; (define-key *root-map* (kbd "N") '*notifications-map*)

;; ;; (defun top-programs)
;; (setf *time-modeline-string* "%a %m-%e ^4*^B%l:%M^b^n %p") ; zero-pad day
;; (setf *screen-mode-line-format*
;;       (list
;;        "[^B%n^b] " ; group num
;;        '(:eval (color-ping (read-ml-file ".ml-wifi")))
;;        "%B " ; battery
;;        ;; "%g" ;groups
;;        ;; "^B%w^b" ; window list
;;        ;; voicemail, sms, email
;;        '(:eval (read-ml-file ".ml-email"))
;;        ;; quotes
;;        '(:eval (read-ml-file ".ml-quotes"))
;;        ;; notifications
;;        " %Z"
;;        ;; FIXME add weather forecast
;;        ;; TODO add google reader unread
;;        ;; TODO add linphone status/incoming calls
;;        ;; TODO add irc alert
;;        ;; TOOD add current todo (from emacs/org, clocked in item)
;;        ;; " DRP: " '(:eval (read-ml-file ".ml-dropbox"))
;;        "^>" ; right align
;;        ;; pomodoro
;;        '(:eval (read-ml-file ".ml-pomodoro-msg")) " "
;;        '(:eval (read-ml-file ".ml-pomodoro-time")) " "
;;        '(:eval (read-ml-file ".ml-weather")) "°F " ;; "°F "
;;        ;; ;; volume
;;        ;; '(:eval (read-file ".mode-line-volume")) " "
;;        "%f "
;;        "%c" ; cpu
;;        ;; '(:eval (read-ml-file ".ml-sensors")) " "
;;        ;; "%M" ; mem
;;        "NET: %l" ; net
;;        ;; "%D" ; disk
;;        ;; "Media: " '(:eval (read-ml-file ".ml-media")) " "
;;        ;; "Home: " '(:eval (read-ml-file ".ml-home")) " "
;;        "Trash: " '(:eval (read-ml-file ".ml-trash")) " "
;;        ;; "^> %M %c" ;; I like %c but not working last time I tries it's cpu.lisp
;;        ;; "»»»"
;;        "%d" ;; crappy default date
;;        ;; '(:eval (string-right-trim '(#\Newline) (run-shell-command
;;        ;; "date +'%a %m-%d ^4*^B%l:%M^b^n %p'|tr -d '\\n'"
;;        ;; uses date command so time can be bold
;;        ;; "date +'%a %m-%d ^4*^B%l:%M^b^n %p'" t)))
;;        ))

;; (defcommand uaml () ()
;;   ""
;;   (update-all-mode-lines))

;; (dolist (head
;; 	 (list (first (screen-heads (current-screen)))) ; first
;; 	 ;; (screen-heads (current-screen)) ; all
;; 	 )
;;   (enable-mode-line (current-screen) head
;; 		    t *screen-mode-line-format*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc

(defun maybe-kill-shell-window ()
  (anaphora:awhen (some (lambda (w) (when (and (not (search "emacs" (window-name w)))
					  (search "lambda" (window-name w)))
				 w)) (all-windows))
    (pull-window anaphora:it)
    (delete-window anaphora:it)))

(defun window-by-name (name)
  "case insensitive"
  (some (lambda (w) (when (string= (string-downcase name) (string-downcase (window-name w))) w))
	(all-windows)))

(defun emacs-window ()
  (car (remove-if-not (lambda (w) (search "emacs" (window-name w))) (all-windows))))

(defun browser-window ()
  ;; TODO 2014-10-28T02:40:32-07:00 Gabriel Laddel
  ;; implement me!
  (car (remove-if-not (lambda (w) (search "conkeror" (window-name w))) (all-windows))))

(defun select-emacs ()
  (select-window (window-name (emacs-window))))

(defun emacs-selected? ()
  (search "emacs" (window-name (current-window))))

(defun emacs-fullscreen? ()
  (and (emacs-selected?) (not (listp (car (tile-group-frame-tree (current-group)))))))

(defun something-fullscreen? ()
  (let* ((o (car (tile-group-frame-tree (current-group)))))
    (not (listp o))))

(defun fullscreen-emacs ()
  (loop while (not (emacs-fullscreen?))
     do (cond ((something-fullscreen?) (pull-hidden-next))
	      ((emacs-selected?) (fnext))
	      (t (remove-split)))))

(defun select-nil-frame ()
  "Selects initial frame if a nil frames does not exist"
  (let ((initial-window (current-window))
	(initial-loop t))
    (loop while (or (not (null (current-window)))
		    (and (eq initial-window (current-window)) (not initial-loop)))
          do (progn (fnext) (setq initial-loop nil)))))

(defcommand dump-group-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (dump-group (current-group)) file))

(defcommand dump-screen-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of all groups of the current screen to the named file"
  (dump-to-file (dump-screen (current-screen)) file))

(defcommand dump-desktop-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of all groups of all screens to the named file"
  (dump-to-file (dump-desktop) file))

(defcommand restore-from-file (file) ((:rest "Restore From File: "))
  "Restores screen, groups, or frames from named file, depending on file's contents."
  (let ((dump (read-dump-from-file file)))
    (typecase dump
      (gdump
       (restore-group (current-group) dump))
      (sdump
       (restore-screen (current-screen) dump))
      (ddump
       (restore-desktop dump))
      (t
       (message "Don't know how to restore ~a" dump)))))
