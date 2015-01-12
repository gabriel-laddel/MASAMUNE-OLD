;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'cl)
(require 'package)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/"))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal packages

(require 'auth-source)
(require 'dired-x)
(require 'doc-view)
(require 'easymenu)
(require 'eldoc)
(require 'ibuffer)
(require 'info)
(require 'json)
(require 'org)
(require 'org-agenda)
(require 'rcirc)
(require 'sh-script)
(require 'simple)
(require 'timer)
(require 'winner)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; external packages
;;; 
;;; the emacs lisp repos have gone offline twice during this project. as such, 
;;; I'm storing everything in the masamune repository instead.

(loop for i in '(ac-helm anaphora auto-complete auto-highlight-symbol autopair
			 bookmark+ cl-format dash dash-functional desktop dired+
			 dired-details dired-details+ dired-filter
			 dired-hack-utils dired-rainbow elisp-slime-nav
			 git-commit-mode git-link git-rebase
			 find-file-in-repository flx flx-ido helm
			 helm-dictionary helm-projectile helm-projectile-all
			 helm-themes highlight-symbol ibuffer-vc imaxima
			 latex.el list-processes+ magit magit-gh-pulls
			 magit-gitflow magit-topgit paredit pcache
			 persistent-soft popup pretty-lambdada
			 rainbow-delimiters redshank request s shadchen tco
			 thingatpt+.el uuid vkill websocket wgrep)
      do (add-to-list 'load-path
		      (expand-file-name (concatenate 'string "~/quicklisp/local-projects/masamune/third-party-elisp/" (symbol-name i)))))

;; (require 'ace-jump-mode)
(require 'anaphora)
(require 'auto-complete)
(require 'auto-highlight-symbol)
(require 'autopair)
(require 'bookmark+)
(require 'cl-format)
(require 'cl-lib)
(require 'dash)
(require 'desktop)
(require 'dired+)
(require 'dired-details+)
(require 'dired-filter)
(require 'dired-rainbow)
(require 'elisp-slime-nav) 
(require 'find-file-in-repository)
(require 'flx-ido)
(require 'helm)
(require 'highlight-symbol)
(require 'ibuffer-vc)
(require 'list-processes+)
(require 'magit)
(require 'paredit)
(require 'pcache)
(require 'persistent-soft)
(require 'popup)
(require 'pretty-lambdada)
(require 'rainbow-delimiters)
(require 'rcirc)
(require 'redshank)
(require 'request)
(require 's)
(require 'shadchen)
(require 'skeleton)
(require 'tco)
(require 'uuid)
(require 'vkill)
(require 'websocket)
(require 'wgrep)
(require 'helm-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup slime

(cl-defun cat (&rest args) 
  (apply #'concatenate 'string (mapcar (lambda (x) (if (stringp x) x (p1 x))) args)))

;; (defun latest-swank ()
;;   (let* ((ql-dir-name "~/quicklisp/dists/quicklisp/software/")
;; 	 (perhaps (remove-if-not (lambda (s) (string-match-p "slime" s)) 
;; 				 (directory-files ql-dir-name))))
;;     (concatenate 'string ql-dir-name 
;; 		 (car (sort perhaps (lambda (s1 s2) (< (subseq (- (length s1) 2) (length s1))
;; 						       (subseq (- (length s1) 2) (length s1))))))
;; 		 "/")))

(defun latest-swank () "~/quicklisp/dists/quicklisp/software/slime-2.11")

(add-to-list 'load-path (latest-swank))  
(add-to-list 'load-path (cat (latest-swank) "/contrib"))

(defun ls-clean (dir)
  (remove-if (lambda (s) (or (s-prefix? "." s) (s-prefix? "#" s)
			     (s-suffix? "#" s) (s-suffix? "~" s)))
	     (directory-files dir)))

(let* ((dir "~/quicklisp/local-projects/masamune/third-party-elisp/"))
  (dolist (path (mapcar (lambda (s) (cat dir  s "/")) (ls-clean dir)))
    (add-to-list 'load-path path)))
(setq slime-backend (expand-file-name (cat (latest-swank) "swank-loader.lisp"))
      slime-path (latest-swank)
      slime-contribs '(slime-editing-commands
		       slime-fancy
		       slime-fancy-inspector
		       slime-fontifying-fu
		       slime-fuzzy
		       slime-indentation
		       slime-media
		       slime-package-fu
		       slime-references
		       slime-repl
		       slime-sbcl-exts
		       slime-scratch
		       slime-sprof
		       slime-xref-browser
		       slime-asdf)
      slime-use-autodoc-mode         t
      slime-autodoc-use-multiline-p  t
      slime-enable-evaluate-in-emacs t
      slime-protocol-version 'ignore
      ;; XXX 2014-10-22T20:21:26-07:00 Gabriel Laddel
      inferior-lisp-program (if (search "Ubuntu" (shell-command-to-string "lsb_release -a"))
				(car (split-string (subseq (shell-command-to-string "whereis sbcl") 5)  " " t nil))
			      (split-string (subseq (shell-command-to-string "whereis sbcl") 5) " " t))
      ;; TODO 2014-10-26T00:40:32-07:00 Gabriel Laddel
      ;; configure
      inferior-lisp-buffer           "*slime-repl sbcl*"
      slime-complete-symbol*-fancy t
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      ;; TODO 2014-10-22T20:20:56-07:00 Gabriel Laddel
      ;; this should be a configurable variable.
      common-lisp-hyperspec-root (cat "file://" (expand-file-name "~/lisp/HyperSpec/")))

(require 'slime-autoloads)
(require 'slime)
(slime-setup)

(load "~/quicklisp/local-projects/masamune/util.el")

(defvar mm:*global-state* nil "A plist of all masamune program state in Emacs.")

(defvar *hack* nil "place to put the odd value when hacking")
 
(defun mm:gstate (k &optional v)
  (if v (setf (getf mm:*global-state* k) v)
    (getf mm:*global-state* k)))

(defun init-gstate! ()
  (loop with pl = '((:lisp-port             9100)
		    (:lisp-connection       nil)
		    (:localhost-port        8001)
		    (:emacs-port            8105)
		    (:chrome-connection     nil)
		    (:server                nil)
		    (:tabs                  nil)
		    (:parenscript-callbacks ())
		    (:dir                "~/.masamune/")
		    (:current-lesson-times  nil)
		    (:emacs-focused?        t)
		    (:local-systems-dir     "file:///home/francis/quicklisp/local-projects/")
		    (:lessons               nil)
		    (:systems               nil)
		    (:slime-input-validator nil)
		    (:slime-ouput-validator nil))
	for (k v) in pl
	do (mm:gstate k v)))

(defun* configure-window-layout ()
  (interactive)
  ;; TODO 2014-10-22T20:05:17-07:00 Gabriel Laddel
  ;; revisit!
  (if (member "conkeror" (mapcar #'process-name (process-list)))
      (slime-eval-async '(stumpwm::configure-stumpwm-window-layouts))
    (run-at-time "30 seconds" nil #'configure-window-layout)))

(defun* start-masamune! (&optional (slime-host 127.0.0.1) (slime-port 4005))
  (interactive)
  (slime-connect slime-host slime-port)
  (init-gstate!)
  ;; TODO 2014-10-22T20:05:57-07:00 Gabriel Laddel
  ;; revisit!
  (slime-eval-async `(mm:start! ,(mm:gstate :lisp-port))))

;;; TODO 2014-10-22T20:09:10-07:00 Gabriel Laddel
;;; implement some sort of system management that connects to org for the time being.
;;; 
;; (dolist (elm '("~/quicklisp/local-projects/masamune"))
;;   (add-to-list 'org-agenda-files (cat elm "/system.org")))

(load-file "~/quicklisp/local-projects/masamune/editing.el")	   ; Editing utilities
(load-file "~/quicklisp/local-projects/masamune/masamune.el")	   ; application code
(load-file "~/quicklisp/local-projects/masamune/clhs.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sanity setup

(put 'dired-find-alternate-file 'disabled nil)
(setq inhibit-startup-message t      
      slime-eval-in-emacs t
      desktop-restore-frames t      
      next-line-add-newlines nil
      default-justification 'left
      ido-use-faces nil
      enable-recursive-minibuffers t
      calendar-week-start-day 1		; mon
      doc-view-continuous t
      column-number-mode t
      package-load-list '(all)
      ring-bell-function (lambda () (message "*beep*"))
      default-fill-column 80
      org-startup-folded nil
      desktop-load-locked-desktop t
      bookmark-version-control t
      max-lisp-eval-depth 40000
      max-specpdl-size '100000
      ;; common-lisp-hyperspec-root ?
      highlight-symbol-idle-delay 0.8
      show-paren-delay 0
      tab-always-indent 'complete)
(elisp-slime-nav-mode 1)
(blink-cursor-mode -1)
(global-visual-line-mode 1); Proper line wrapping
;; (desktop-save-mode 1) TODO fix this
(condition-case nil (display-battery-mode 1) (error nil))
(global-auto-revert-mode t)
(ido-mode t)
(ido-everywhere 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(fringe-mode 0)
(winner-mode t)
(recentf-mode 1) 
(savehist-mode 1)
(flx-ido-mode 1)
(global-auto-complete-mode 0)
(global-pretty-lambda-mode)
(global-rainbow-delimiters-mode t)
(auto-compression-mode t)
(random t) ;; Seed the random-number generator

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'file-as-sexps #'read-sexps-from)
(defalias 'slurp-sexps #'read-sexps-from)
(defalias 'p1 'prin1-to-string)
(defun browse-url (url) (mm:open-uri url t))

(defun gui-off ()
  (interactive)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

(defun gui-on ()
  (interactive)
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (scroll-bar-mode 1))

(defun toggle-gui ()
  (interactive)
  (if menu-bar-mode (gui-off) (gui-on)))

(defun true-fullscreen? ()
  (equal 'fullboth (frame-parameter nil 'fullscreen)))

(defun toggle-fullscreen (&optional f)
  (interactive)
  ;; I've not the slightest idea what `old-fullscreen' is doing here.
  (set-frame-parameter nil 'fullscreen
		       (if (true-fullscreen?)
			   (progn (fringe-mode 0) (when (boundp 'old-fullscreen) old-fullscreen))
			 (progn (fringe-mode 0) 
				(setq old-fullscreen (true-fullscreen?)) 
				'fullboth))))

;;; TODO 2014-10-20T00:27:45-07:00 Gabriel Laddel
;;; fix...

(defun alert (s &optional no-timeout)
  (slime-eval-async `(swank:eval-and-grab-output
		      ,(cat (if no-timeout "(message-no-timeout \"" "(message \"") s "\" )"))
    (lambda (_)) "stumpwm"))

(defun message-me-in (time)
  (interactive "sTime: ")
  (run-at-time time nil #'alert (read-from-minibuffer "Message: ")))

(defun message-me-in-persistant-message (time)
  (interactive "sTime: ")
  (run-at-time time nil #'alert (read-from-minibuffer "Message: ") t))

(defun current-theme ()
  "Symbol name of the current theme"
  (car (some #'custom-theme-enabled-p (custom-available-themes))))

(defun load-theme-disable-current (theme)
  (disable-theme (current-theme))
  (load-theme theme)
  (set-font-size 70))

(defun rotate-snazzy-theme ()
  (interactive)
  (llet ((snazzy-themes '(leuven
			  twilight-bright
			  solarized-dark
			  cyberpunk
			  tsdh-light
			  grandshell
			  colorsarenice-dark
			  dakrone
			  gruvbox
			  ir-black
			  light-blue
			  subatomic-enhanced
			  hemisu-light
			  molokai))
	 (i (-find-index (lambda (s) (equal s (current-theme))) snazzy-themes))
	 (new-i (cond ((null i) 0)
		      ((equal (length snazzy-themes) (1+ i)) 0)
		      (t (1+ i))))
	 (old-theme (when i (nth i snazzy-themes)))
	 (new-theme (nth new-i snazzy-themes)))
    (disable-theme (current-theme))
    (load-theme new-theme))
  (set-font-size 78))

(defun set-font-size (n)
  (interactive "nSize: ")
  (set-face-attribute 'default nil :height n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscj

(defadvice split-window-right (after align-normally last)
  (llet ((window-heights))
    (walk-window-tree (lambda (w) (push (window-width) window-heights)))
    (when (and (= 3 (length window-heights)) (every (curry #'= (car window-heights)) window-heights))
      (balance-windows))))

(ad-activate 'split-window-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(ad-activate 'magit-status)

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun magit-goto-next-sibiling-section-like-sldb ()
  (interactive)
  (magit-section-hideshow (lambda (s) (magit-section-set-hidden s t)))
  (magit-goto-next-sibling-section)
  (magit-section-hideshow (lambda (s) (magit-section-set-hidden s nil))))

(defun magit-goto-previous-sibiling-section-like-sldb ()
  (interactive)
  (magit-section-hideshow (lambda (s) (magit-section-set-hidden s t)))
  (magit-goto-previous-sibling-section)
  (magit-section-hideshow (lambda (s) (magit-section-set-hidden s nil))))

(define-key magit-status-mode-map (kbd "M-n") 
  'magit-goto-next-sibiling-section-like-sldb)
(define-key magit-status-mode-map (kbd "M-p") 
  'magit-goto-previous-sibiling-section-like-sldb)
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))
     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; ignore whitespace

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm mode

;; (helm-mode 1)
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; (setq helm-quick-update                     t ; do not display invisible candidates
;;       helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;;; download!
;; (when (executable-find "ack-grep")
;;   (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
;;         helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;; (setq helm-locate-command "locate %s -e -A --regex %s")
;; (global-set-key (kbd "C-c h o") 'helm-occur)
;; (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
;; (global-set-key (kbd "C-c h x") 'helm-register)
;; (global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)
;; (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
;; (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maxima
;;; 
;;; NOTE 2014-10-27T16:20:48-07:00 Gabriel Laddel
;;; I've included this code in the third-party-elisp, but it also ships with
;;; maxima when you bulid from source.

(add-to-list 'load-path (expand-file-name "~/quicklisp/local-projects/masamune/third-party-elisp/imaxima/"))
;; (add-to-list 'Info-directory-list "/usr/local/share/info/")
(autoload 'imaxima "imaxima" "Image support for Maxima." t)
(autoload 'imath-mode "imath" "Interactive Math minor mode." t)

(require 'imaxima)
(require 'imath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mozrepl

;;; TODO 2014-10-23T13:15:45-07:00 Gabriel Laddel
;;; patch.

;; (add-to-list 'load-path (expand-file-name "~/lisp/mozrepl/"))
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; (add-hook 'javascript-mode-hook 'javascript-custom-setup)
;; (defun javascript-custom-setup () (moz-minor-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired

(defun mm:open-uri (uri &optional focus-browser)
  (slime-eval-async `(mmb::open-uri ,uri ,focus-browser)))

(defun dired-browser-find-file ()
  (interactive)
  (mm:open-uri (cat "file://" (dired-get-file-for-visit)) t))

(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-mode-hook 'dired-omit-mode)
(setq dired-recursive-deletes 'always)

(defun* mm:emacs-finalize-boot-callback () (start-conkeror-and-mozrepl))

(defun* finalize-boot ()
  (interactive)
  (slime-eval-async '(cl:progn (cl:unless (cl:find-package (quote masamune))
					  (ql:quickload (quote (masamune))))
			       (setf mm::*swank-connection-hack* *standard-output*))
    (lambda (x) (mm:emacs-finalize-boot-callback))))

(add-hook 'slime-connected-hook 'finalize-boot)

(loop with p = (ppath "/third-party-elisp/themes/") 
      for dir in (ls-clean p)
      do (add-to-list 'custom-theme-load-path (cat p dir)))

(defun slime-port ()
  (when (file-exists-p "~/.xsession-errors")
    (let* ((ss ";; Swank started at port: ")
	   (k (llast (filter (lambda (s) (search ss s)) (s-split "\n" (slurp "~/.xsession-errors")))))
	   (k (subseq k (length ss) (- (length k) 1))))
      (car (read-from-string k)))))

(defun fnil (&rest args)
  "accepts any number of arguments and returns nil"
  nil)

(defun masamune-state ()
  (list :emacs (let ((out))
		 (walk-windows (lambda (window) (push (buffer-file-name (window-buffer window)) out)))
		 out)
	:browser nil))

(defun* mm:write-state-loop ()
  "TODO run in idle mode instead"
  `(funcall #'mm::save-masamune-state ,(masamune-state))
  (slime-eval-async `(mm::save-masamune-state (quote ,(masamune-state))) #'fnil)
  (run-at-time "30 seconds" nil #'mm:write-state-loop))

(unless (slime-connected-p) (slime-connect "127.0.0.1" (or (slime-port) 4005)))
(enable-masamune-keybindings)
(init-gstate!)
;; (mm:write-state-loop)
(when (file-exists-p "~/quicklisp/local-projects/masamune/emacs-customizations.el")
  (load "~/quicklisp/local-projects/masamune/emacs-customizations.el"))
(server-start) ;; for emacsclient to connect to 
