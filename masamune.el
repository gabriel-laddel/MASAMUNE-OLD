;;; -*- lexical-binding: t -*-

(defvar alphabet '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(defun* change-hardware-mac-address ()
  (interactive)
  ;; TODO 2014-08-22T07:38:07-07:00 Gabriel Laddel
  ;; this needs some work: http://en.wikipedia.org/wiki/MAC_address
  (save-window-excursion
    (async-shell-command (cat "ifconfig eth0 down && macchanger -m "
			      (if (zerop (random* 2))
				  (nth (random* 26) alphabet)
				(random* 10))

			      (if (zerop (random* 2))
				  (nth (random* 26) alphabet)
				(random* 10))

			      ":"

			      (if (zerop (random* 2))
				  (nth (random* 26) alphabet)
				(random* 10))

			      (if (zerop (random* 2))
				  (nth (random* 26) alphabet)
				(random* 10))

			      ":66:4e:16:88"
			      " eth0 && ifconfig eth0 up"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc

(defun save-desktop ()
  (interactive)
  (desktop-save "~/.masamune/emacs-desktop-state/"))

(defun load-desktop ()
  (interactive)
  (desktop-read "~/.masamune/emacs-desktop-state/"))

(defmacro* swma (body &optional (cont '(lambda (_))) (package "stumpwm"))
  "[S]tump[W][M] [A]ccess - evaluate code on stumpwm proc"
  `(slime-eval-async (quote (swank:eval-and-grab-output ,(prin1-to-string body)))
     ,cont ,package))

(defmacro ps:ps (sexp &optional cont)
  (slime-eval-async `(swank:eval-and-grab-output ,(format "(ps %s)" (p1 sexp)))
    (or cont (lambda (x) (message "PS> %S" (read (cadr x)))))
    "parenscript"))

(defmacro* ps (body &optional (cont '(lambda (_))) (package "stumpwm"))
  "[P]aren[S]cript - evaluate arbitrary parenscript"
  `(slime-eval-async (quote (swank:eval-and-grab-output ,(prin1-to-string body)))
     ,cont ,package))

(defun mm:phrase (s)
  (interactive "sPhrase: ")
  (save-window-excursion
    (llet (deactivate-mark)
      (unless (member "phrases.txt" (buffer-name-list))
	(find-file "~/.ag/phrases.txt"))
      (with-current-buffer "phrases.txt"
	(goto-char (point-max))
	(insert (cat s " "))
	(save-buffer)))
    (kill-buffer* "phrases.txt")))

(defun message-me-in (time)
  (interactive "sTime: ")
  (run-at-time time nil #'alert (read-from-minibuffer "Message: ")))

(defun message-me-in-persistant-message (time)
  (interactive "sTime: ")
  (run-at-time time nil #'alert (read-from-minibuffer "Message: ") t))

(defun alert (message &optional no-timeout)
  (if no-timeout (slime-eval-async `(stumpwm::message-no-timeout ,message))
    (slime-eval-async `(stumpwm::message ,message))))

(defun start-or-focus-masamune-irc ()
  (interactive)
  (if (rcirc-process-list)
      ;; TODO 2014-11-01T02:25:40-07:00 Gabriel Laddel
      ;; don't assume freenode is the only channel
      (progn (rcirc-join-channels (car (rcirc-process-list)) '("#masamune"))
	     (pop-to-buffer "#masamune@irc.freenode.net"))
    (progn (setq old-rcirc-server-alist rcirc-server-alist)
	   (setq rcirc-server-alist '(("irc.freenode.net" :channels
				       ("#masamune"))))
	   (rcirc nil)
	   (setq rcirc-server-alist old-rcirc-server-alist)
	   (run-at-time "1 seconds" nil (lambda () (pop-to-buffer "#masamune@irc.freenode.net"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous

(defun* mm:dashboard ()
  (interactive)
  (slime-eval-async '(mmg::run-or-focus-dashboard)))

(defun* mm:kgraph ()
  (interactive)
  (slime-eval-async '(mmg::run-or-focus-kgraph)))

(defun* mm:repository ()
  (interactive)
  (slime-eval-async '(mmg::run-or-focus-repository)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; external resources
;;; 
;;; TODO 2014-11-05T13:41:06-08:00 Gabriel Laddel
;;; all of this needs to be implemented.

(defun mm:define (s)
  ;; Record the vocab word
  (interactive "sWord: ")
  ;; (save-window-excursion
  ;;   (llet (deactivate-mark)
  ;;     (unless (member "vocab.txt" (buffer-name-list))
  ;; 	(find-file "~/.ag/vocab.txt"))
  ;;     (with-current-buffer "vocab.txt"
  ;; 	(goto-char (point-max))
  ;; 	(insert (cat s " "))
  ;; 	(save-buffer)))
  ;;   (kill-buffer* "vocab.txt"))
  ;; (mm:open-url (cat "https://www.google.com/search?q=define+" (s-replace " " "+" s)) t)
  (alert "implement vocabulary goodness!"))

(defun* goog-define ()
  (interactive)
  (llet ((r (region-no-properties))
  	 (input (if r (read-string (cat "Define (default " r "): ")) 
  		  (read-string "Define: ")))
  	 (input (cond ((and (empty? input) (not r)) (message "No input, not googling."))
  		      ((and r (empty? input)) r)
  		      (t input))))
    (slime-eval-async `(mb::open-url ,(cl-format nil "https://www.google.com/search?q=define+~a" (s-replace " " "+" input))))))

(defun* google ()
  (interactive)
  (alert "connect to CL google code"))

(defun* wikipedia-lookup ()
  ;; TODO 2014-09-11T15:08:53-07:00 Gabriel Laddel
  ;; use google aor ddg to correct for spelling errors
  (interactive)
  ;; (let* ((region (region-no-properties))
  ;; 	 (article-title-query (read-string (aif region (cat "Wikipedia: (default " it " ): ") "Wikipedia: ")))
  ;; 	 (article-title-query (if (empty? article-title-query) region article-title-query)))
  ;;   (slime-eval-async `(mm::search-wikipedia-article-titles ,article-title-query) 
  ;;     #'mm:maybe-wikipedia-autocomplete))
  (alert "implement wikipedia goodness!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lazy.

(defun ppath (string)
  "[p]roject [path]name"
  (cl-format nil "~~/quicklisp/local-projects/masamune-os/~a"
	     (if (string= "/" (subseq string 0 1)) (subseq string 1) string)))

(defun start-conkeror-and-mozrepl ()
  (interactive)
  (save-window-excursion
    (async-shell-command "xulrunner ~/algol/conkeror/application.ini"))
  (run-at-time "3 seconds" nil (lambda () (with-current-buffer "*slime-repl sbcl*"
				       (end-of-buffer)
				       (insert "(setq MASAMUNE::*SWANK-CONNECTION-HACK* *standard-output*)")
				       (slime-repl-return)
				       (end-of-buffer)
				       (insert "(mmb::start-ps-repl)")
				       (slime-repl-return)))))

