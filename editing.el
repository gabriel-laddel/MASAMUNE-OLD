;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Lisp

(defun pretty-curry-compose ()
  "Compact functional combination display"
  (mapc (lambda (pair)
          (let ((regexp (car pair))
                (symbol (cdr pair)))
            (font-lock-add-keywords 'emacs-lisp-mode
				    `((,regexp
				       (0 (progn (compose-region (match-beginning 1) (match-end 1)
								 ,symbol)
						 nil)))))))
        '(("(\\(compose\\)[ \t\n\r]" . ?\∘)
          ("(\\(curry\\)[ \t\n\r]"   . ?\»)
          ("(\\(rcurry\\)[ \t\n\r]"  . ?\«))))

(defface highlight-symbol-face-0
  '((((class color) (background dark))
     (:background "dark red"))
    (((class color) (background light))
     (:background "cyan")))
  "Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)

(defun highlight-symbol-temp-highlight ()
  "Highlight the current symbol until a command is executed."
  (when highlight-symbol-mode
    (let ((symbol (highlight-symbol-get-symbol)))
      (unless (or (equal symbol highlight-symbol)
                  (highlight-symbol-symbol-highlighted-p symbol))
        (highlight-symbol-mode-remove-temp)
        (when symbol
          (setq highlight-symbol symbol)
          (highlight-symbol-add-symbol-with-face symbol 'highlight-symbol-face-0)
          (font-lock-fontify-buffer))))))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("\\<\\(FIXME\\)"          1 font-lock-warning-face prepend)
			  ("\\<\\(TODO\\)"           1 font-lock-warning-face prepend)
			  ("\\<\\(XXX\\)"            1 font-lock-warning-face prepend)
			  ("\\<\\(NOTE\\)"           1 font-lock-warning-face prepend)
			  ("(\\(compose\\)[ \t\n\r]" 1 font-lock-keyword-face)
                          ("(\\(curry\\)[ \t\n\r]"   1 font-lock-keyword-face)
                          ("(\\(rcurry\\)[ \t\n\r]"  1 font-lock-keyword-face)
			  ("\\(?:a\\(?:\\(?:nd\\|ssert\\)
			\\)\\|clambda\\|def\\(?:\\(?:macro\\|un\\)\\*\\)\\|\\(?:l\\(?:let\\|oop\\)\\|not\\|or\\)
			\\)" . font-lock-keyword-face)))


(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'completion-styles 'initials t)
(add-to-list 'emacs-lisp-mode-hook 'pretty-curry-compose)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp (SBCL)

(font-lock-add-keywords 'lisp-mode
			'(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
			  ("\\<\\(TODO\\)"  1 font-lock-warning-face prepend)
			  ("\\<\\(XXX\\)"   1 font-lock-warning-face prepend)
			  ("\\<\\(NOTE\\)"  1 font-lock-warning-face prepend)
			  ("\\<\\(?:\\(?:a\\(?:if\\|nd\\|when\\)\\|it\\(?:er\\)?\\|l\\(?:et\\+\\|let\\)\\|not\\|or\\) \\)\\>" 
			   . font-lock-keyword-face)))

(add-hook 'slime-repl-mode-hook 'highlight-symbol-mode)
(add-hook 'slime-repl-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'highlight-symbol-nav-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'highlight-symbol-mode)

(defun make-buffer-package-current ()
  (interactive)
  (let ((to-insert (cl-format nil "(in-package ~a)" (slime-current-package))))
    (with-current-buffer "*slime-repl sbcl*"
      (end-of-buffer)
      (end-of-line)
      (insert to-insert)
      (slime-repl-return))))

(defun mm-tests ()
  (interactive) 
  (with-current-buffer "*slime-repl sbcl*"
    (end-of-buffer)
    (insert "(masamune-tests::test-masamune)")
    (slime-repl-return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shared

(defun indent-pp-sexp-at-point ()
  (interactive)
  (save-excursion (slime-beginning-of-defun) (indent-pp-sexp)))

(defun tlf ()
  (acond ((comment-block-on-line-p)   'comment-block)
	 ((thing-at-point 'defun t) (first (read-from-whole-string it)))))

(defun mm:cut-string ()
  (interactive)
  (insert "\"  \"")
  (backward-char)
  (backward-char))

(defun mm:nest-call ()
  "moves 'up' one level and inserts a wrapping sexp"
  (interactive)
  (paredit-backward-up)
  (paredit-wrap-round))

(defun comment-sexp-forward-dwim ()
  (interactive)
  ;; TODO 2014-07-11T12:38:37-07:00 Gabriel Laddel
  ;; doesn't uncomment correctly.
  (if (paredit-comment-on-line-p)
      (save-excursion (call-interactively #'set-mark-command)
      		      (loop while (paredit-comment-on-line-p)
      			    do (next-line)
      			    finally (paredit-comment-dwim)))
    (progn (mark-sexp)
  	   (paredit-comment-dwim))))

(defun new-shell ()
  (interactive)
  (when (member "*shell*" (buffer-name-list))
    (with-current-buffer "*shell*"
      (rename-uniquely)))
  (shell))

(defun insert-lisp-comment (x)
  "FIXME     -  mark potential problematic code that requires special attention and
             or review.
NOTE      -  document inner workings of code and indicate potential pitfalls.
TODO      -  indicate planned enhancements.
XXX       -  warn other programmers of problematic or misguiding code. 
(h)eading -  denote section"
  (interactive "s(t)ODO, (f)IXME, (n)OTE, (x)XX, or (h)eading?")
  (if (equal x "h")
      (progn (save-excursion (paredit-comment-dwim)
			     (newline)
			     (paredit-comment-dwim)
			     (->> (loop for i from 0 to (- 79 (current-column)) collect "=")
			       (apply #'cat)
			       (insert)))
             (previous-line)
	     (loop repeat 4 do (forward-char) finally (insert " "))) 
    (let ((heading nil))
      (cond ((equal x "x") (setq heading "XXX"))
      	    ((equal x "t") (setq heading "TODO"))
      	    ((equal x "f") (setq heading "FIXME"))
      	    ((equal x "n") (setq heading "NOTE"))	  
      	    (t (message "TODO, implement restarts for interactive and, read-single-char.")))
      (progn (paredit-comment-dwim)
	     (insert heading " " (iso-now) " Gabriel Laddel" "\n")
	     (paredit-comment-dwim)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; desktops

(defvar *dumped-desktops* ())

(defun avaliable-desktops ()
  (sort (mapcar (lambda (s) (read (s-replace "desktop-dump" "" s)))
		(filter (lambda (file-name) (s-matches? "desktop-dump" file-name)) (ls-clean "/tmp")))
	#'<))

(defun dump-current-desktop ()
  (interactive)
  (eval `(swma (dump-group-to-file ,(cat "/tmp/desktop-dump" (length *dumped-desktops*)))))
  (window-configuration-to-register (make-symbol (cat ":desktop-dump" (length *dumped-desktops*))))
  (push (list :stumpwm-group-dump (cat "/tmp/desktop-dump" (length *dumped-desktops*))
	      :emacs-register (make-symbol (cat ":desktop-dump" (length *dumped-desktops*))))
	*dumped-desktops*))

(defun restore-desktop (&optional desktop-name)
  (interactive)
  (if (avaliable-desktops)
      (llet ((n (or desktop-name (read-string (cat "Select from avaliable desktops: " (apply #'cat (-interpose " " (avaliable-desktops))) ": ")))))
	(eval `(swma (restore-from-file ,(cat "/tmp/desktop-dump" n))))
	;; (jump-to-register (make-symbol (cat ":desktop-dump" n)))
	)
    (message "There are no desktops avaliable")))

(defun restore-last-desktop ()
  (interactive)
  (assert (avaliable-desktops) "No desktop to restore")
  (restore-desktop (llast (avaliable-desktops))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refactoring tools

(defvar *rename-point* nil 
  "When finished renaming code, jump back to previoius point")

(defadvice highlight-symbol-query-replace
    (before set-previous-rename-point first)
  (setf *rename-point* (point)))

(defadvice highlight-symbol-query-replace
    (after set-previous-rename-point last)
  (goto-char *rename-point*))

(ad-activate 'highlight-symbol-query-replace)

(defun* next-hl-sym-maybe-slime-next-note ()
  (interactive)
  (if (equal 'slime-repl-mode major-mode) (slime-repl-forward-input)
    (if (slime-find-next-note) (slime-next-note)
      (highlight-symbol-next))))

(defun* previous-hl-sym-maybe-slime-previous-note ()
  (interactive)
  (if (equal 'slime-repl-mode major-mode) (slime-repl-backward-input)
    (if (slime-find-next-note) (slime-previous-note)
      (highlight-symbol-prev))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ad Hoc Lisp system documentation

(defun* mm:document-buffer (&optional (buffer (current-buffer)))
  ;; FIXME 2014-08-12T21:58:58-07:00 Gabriel Laddel
  ;; failes on:
  ;; (progn (find-file "/home/francis/lisp/sbcl/src/code/filesys.lisp") (mm:document-buffer))
  (interactive)
  (mm:document-files (y-or-n-p "allow `other' sexps?") 
		     (with-current-buffer buffer buffer-file-name)))

(defun* mm:documentation-information-for-file (file)
  (save-window-excursion
    (find-file file)
    (mapcar (lambda (l) (list :line (second l)
			 :file buffer-file-name
			 :type (acase (caar l)
				 (defvar       'var) 
				 (defparameter 'var)
				 (setq         'var)
				 (defun        'function)
				 (cl-defun     'function)
				 (defun*       'function)
				 (defmacro     'macro)
				 (cl-defmacro  'macro)
				 (defmacro*    'macro)
				 (defstruct    'defstruct)
				 (defmethod    'defmethod)
				 (defgeneric   'defgeneric)
				 (defclass     'defclass)
				 (t 'other))
			 :name (typecase (second (car l))
				 (string (second (car l)))
				 (symbol (second (car l)))
				 (cons ""))))
	    (buffer-sexps nil t))))

(defun* mm:document-files (spurious-sexps? &rest files)
  ;; helm swoop seems to have much of the functionality that I desire.
  ;; * This will fail on CL files with reader macros that elisp doesn't recognize
  ;; (window-configuration-to-register :prior-dox)
  ;; TODO 2014-07-11T12:39:30-07:00 Gabriel Laddel
  ;; 
  ;; * Currently cannot sort by lines
  ;; * Optionally ignore forms like `require', `provide' `define-key' etc?  
  ;; * should have probably built a thing that slurps all the info out of a single file, then merge it in
  ;;   while looping through files.
  ;; * when I scroll over a name, go to it's source, n/p to navigate lines, type to search, and quit with
  ;; * Font lock faces to highlight
  ;; `defclass'
  ;; `defgeneric' + method
  ;; * I should be able to sort / filter by symbols that have been exported
  ;; * incemental search
  ;; * Detect macros introduced by the user. There are probably other tools to integrate with for this
  ;; * tabulated-list-mode?
  ;; * inline docstrings? (can tabulated list mode do multiline formats? What does apropos do?)
  (interactive)
  (window-configuration-to-register :prior-to-documentation)
  (dump-current-desktop)
  (assert (loop with ext = ()
		for file in (remove (lambda (s) (equal (llast (s-split "/" s)) "package.lisp")) files)
		for file-extension = (llast (s-split "." file))
		do (progn (unless (file-exists-p file) (error "file %S does not exist" file))
			  (unless (or (and (equal ext file-extension)) 
				      (setq ext file-extension))
			    (return)))
		finally (return t))
	  "The file paths you passed do not have the same extension")
  (llet ((data (loop for file in files append (mm:documentation-information-for-file file)))
	 (sorted-data (sort-pls '(:name :type :file :line)
				(align-pls (if spurious-sexps? data
					     (remove-if (lambda (pl) (equal 'other (mm:getf pl :type))) data))))))
    (imtx sorted-data
	  '((:name "Name" :width 35 :sort-function t)
	    (:name "Type" :width 10 :sort-function t)
	    (:name "File" :width 25 :sort-function t)
	    (:name "Line" :width 25 :sort-function t))
	  'lisp-system-documentation-mode
	  'lisp-system-display-function)))

(define-minor-mode lisp-system-documentation-mode
  "" nil nil
  (loop with kmap = (make-sparse-keymap)
	for (keycode function-or-code)
	in (-partition 2 '("n" 'lisp-system-documentation-mode-next-line
			   "p" 'lisp-system-documentation-mode-previous-line
			   "q" 'lisp-system-documentation-mode-quit))
	do (define-key kmap (kbd keycode) (cadr function-or-code))
	finally (return kmap)))

(defun* lisp-system-documentation-mode-quit ()
  (interactive)
  (kill-buffer* "*masamune*")
  (restore-last-desktop)
  (jump-to-register :lisp-system-documentation))

(defun* lisp-system-documentation-mode-show-source-for-id ()
  (interactive)
  (llet ((pl (tabulated-list-get-id)))
    (call-interactively #'other-window)
    (current-buffer)
    (find-file (mm:getf pl :file))
    (goto-line (mm:getf pl :line))
    ;; (slime-beginning-of-defun)
    (slime-highlight-sexp)
    ;; (recenter-top-bottom)
    ;; (recenter-top-bottom)
    ;; (scroll-down-command)
    (pop-to-buffer "*masamune*")))

(defun* lisp-system-documentation-mode-next-line ()
  (interactive)
  (next-line)
  (lisp-system-documentation-mode-show-source-for-id))

(defun* lisp-system-documentation-mode-previous-line ()
  (interactive)
  (previous-line)
  (lisp-system-documentation-mode-show-source-for-id))

(defun slime-find-file-at-point ()
  (interactive)
  (let* ((pathname (thing-at-point 'sexp t)))
    (if (file-exists-p pathname)
	(find-file pathname)
	(message "could not resolve pathname"))))

(defun* lisp-system-display-function ()
  (interactive)
  (dump-current-desktop)
  (window-configuration-to-register :lisp-system-documentation)
  (mm:position-frames :emacs :fullscreen)
  (pop-to-buffer "*masamune*")
  (delete-other-windows)
  (split-window-horizontally)
  (window-resize (some (lambda (b) (when (equal "*masamune*" (buffer-name (window-buffer b))) b))
		       (window-list)) 4 t)
  (lisp-system-documentation-mode-show-source-for-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings

(defun dired-copy-filename-to-kill-ring ()
  (interactive)
  (kill-new (dired-file-name-at-point)))

(define-key dired-mode-map (kbd "M-n") 'dired-copy-filename-to-kill-ring)

(defmacro mm:define-key (key-string fn)
  `(progn (define-key lisp-mode-map       ,(kbd key-string) ,fn)
	  (define-key slime-repl-mode-map ,(kbd key-string) ,fn)
	  (define-key slime-mode-map      ,(kbd key-string) ,fn)
	  (define-key emacs-lisp-mode-map ,(kbd key-string) ,fn)))

(defun jump-to-compilation-buffer ()
  (interactive)
  (pop-to-buffer "*slime-compilation*"))

(defun enable-masamune-keybindings ()
  ;; other
  (define-key text-mode-map (kbd "C-c SPC") 'ace-jump-mode)
  (global-set-key (kbd "M-o v")  'mm:open)  
  (mm:define-key "M-o t"   'mm-tests)
  (mm:define-key "M-o g"   'google)
  (mm:define-key "M-o b"   'mm:document-buffer)
  (mm:define-key "M-o s"   'redshank-slot-spec-skeleton)
  (mm:define-key "M-o q"   'query-replace)
  (mm:define-key "M-o m"   'remember)
  (mm:define-key "M-o h"   'hyperspec-lookup)
  (mm:define-key "M-o n"   'mm:define)
  (mm:define-key "M-o M-n" 'goog-define)
  (mm:define-key "M-o v"   'mm:open)
  (mm:define-key "M-o w"   'wikipedia-lookup)
  (mm:define-key "M-o d"   'mm:dashboard)
  (mm:define-key "M-o k"   'mm:kgraph)
  (mm:define-key "M-o r"   'highlight-symbol-query-replace)
  (mm:define-key "C-M-;"   'comment-sexp-forward-dwim)
  (mm:define-key "C-M-k"   'kill-sexp)
  (mm:define-key "M-o f"     'slime-find-file-at-point)
  (mm:define-key "C-c C-q" 'mm:cut-string)
  (mm:define-key "C-c a"   'redshank-align-slot-specs-in-form)
  (mm:define-key "M-RET"   'mm:nest-call)
  (mm:define-key "M-o c"   'redshank-defclass-skeleton)
  (mm:define-key "M-n"     'highlight-symbol-next)
  (mm:define-key "M-p"     'highlight-symbol-prev)
  (mm:define-key "C-c n"   'create-new-buffer)
  (mm:define-key "C-c C-n" 'split-to-new-buffer)

  (define-key lisp-mode-map       (kbd "M-o p") 'make-buffer-package-current)
  (define-key compilation-mode-map   (kbd "C-c SPC")   'ace-jump-mode)
  (define-key lisp-mode-map (kbd "M-o C-c") 'jump-to-compilation-buffer)
  (define-key slime-repl-mode-map (kbd "M-o C-c") 'jump-to-compilation-buffer)
  (define-key slime-mode-map (kbd "M-o C-c") 'jump-to-compilation-buffer)

  (global-set-key (kbd "C-x 5")   'mm:dashboard)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-c 2")   'find-function)
  (global-set-key (kbd "C-c 3")   'highlight-symbol-nav-mode)
  (global-set-key (kbd "C-c 4")   'highlight-symbol-at-point)
  (global-set-key (kbd "C-c 5")   'highlight-symbol-query-replace)
  (global-set-key (kbd "C-c C-2") 'slime-edit-definition)
  (global-set-key (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (global-set-key (kbd "C-c C-a") (clambda () (interactive) 
					   (llet ((old (buffer-name (current-buffer))))
					     (pop-to-buffer "*Messages*")
					     (end-of-buffer)
					     (pop-to-buffer old))))

  (define-key org-mode-map        (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key org-agenda-mode-map (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key org-mode-map        (kbd "C-c SPC") 'ace-jump-mode)
  ;; org
  (define-key org-mode-map        (kbd "M-o n") 'mm:define)
  (define-key org-mode-map        (kbd "M-o M-n") 'goog-define)
  (define-key org-mode-map        (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key org-mode-map        (kbd "C-c C-e") 'slime-interactive-eval)
  ;; org agenda
  (define-key org-agenda-mode-map (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key org-agenda-mode-map (kbd "C-c C-e") 'slime-interactive-eval)
  ;; dired
  (define-key dired-mode-map      (kbd "C-c C-e") 'slime-interactive-eval)
  (define-key dired-mode-map      (kbd "C-M-o")   'find-name-dired)
  (define-key dired-mode-map      (kbd "e")       'dired-browser-find-file)
  ;; elisp
  (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
  (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
  ;; slime
  (define-key slime-mode-map      (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key slime-repl-mode-map (kbd "C-c C-d") 'slime-describe-symbol)
  (define-key slime-repl-mode-map (kbd "C-c C-e") 'slime-interactive-eval)
  (define-key slime-repl-mode-map (kbd "C-c C-z") 'slime-clear-presentations)
  ;; english editing
  (define-key text-mode-map (kbd "M-o d") 'mm:dashboard)
  (define-key text-mode-map (kbd "C-c n") 'create-new-buffer)
  (define-key text-mode-map (kbd "M-o n") 'mm:define)
  (define-key text-mode-map (kbd "M-o M-n") 'goog-define)
  (define-key text-mode-map (kbd "M-o w") 'wikipedia-lookup)
  (define-key text-mode-map (kbd "M-o m") 'remember)   
  ;; org agenda mode
  (define-key org-agenda-mode-map (kbd "g") 'mm:dashboard)
  ;; org-mode
  (define-key org-mode-map (kbd "C-M-<return>") (clambda () (interactive) (org-html-export-to-html)))
  ;; rcirc
  (define-key rcirc-mode-map (kbd "M-o d") 'mm:dashboard)
  (define-key rcirc-mode-map (kbd "M-o m") 'remember)
  (define-key rcirc-mode-map (kbd "C-c n") 'create-new-buffer)
  (define-key rcirc-mode-map (kbd "C-c C-n") 'split-to-new-buffer)
  ;; from my dot emacs
  (global-unset-key (kbd "C-x C-c"))
  (global-set-key (kbd "C-c C-m")   'execute-extended-command)
  (global-set-key (kbd "C-x C-m")   'execute-extended-command)
  (global-set-key (kbd "C-c C-o")   'rgrep)
  ;; (global-set-key (kbd "C-c SPC")   'ace-jump-mode)
  (global-set-key (kbd "C-x 7")     'revert-buffer)
  (global-set-key (kbd "C-x 8")     'kill-buffer-and-window)
  (global-set-key (kbd "C-x <tab>") 'other-window)
  (global-set-key (kbd "C-x C-u")   'magit-status)
  ;; (global-set-key (kbd "C-x SPC")   'ace-jump-mode-pop-mark)
  (global-set-key (kbd "S-<down>")  'windmove-down)
  (global-set-key (kbd "S-<left>")  'windmove-left)
  (global-set-key (kbd "S-<right>") 'windmove-right)
  (global-set-key (kbd "S-<up>")    'windmove-up)
  (global-set-key [f10]             'insert-lisp-comment)
  (global-set-key [f8]              'fill-paragraph)

  (define-key emacs-lisp-mode-map (kbd "C-M-r")     'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-e")   'slime-interactive-eval)

  ;; (define-key org-agenda-mode-map (kbd "C-SPC")     'ace-jump-mode)
  ;; (define-key org-agenda-mode-map (kbd "C-c SPC")   'ace-jump-mode)
  ;; (define-key org-agenda-mode-map (kbd "C-c SPC")   'ace-jump-mode)
  (define-key org-agenda-mode-map (kbd "S-<down>")  'windmove-down)
  (define-key org-agenda-mode-map (kbd "S-<left>")  'windmove-left)
  (define-key org-agenda-mode-map (kbd "S-<right>") 'windmove-right)
  (define-key org-agenda-mode-map (kbd "S-<up>")    'windmove-up)

  ;; (define-key org-mode-map        (kbd "C-c SPC")   'ace-jump-mode)
  (define-key org-mode-map        (kbd "S-<down>")  'windmove-down)
  (define-key org-mode-map        (kbd "S-<left>")  'windmove-left)
  (define-key org-mode-map        (kbd "S-<right>") 'windmove-right)
  (define-key org-mode-map        (kbd "S-<up>")    'windmove-up)

  (define-key dired-mode-map (kbd "C-o")   'dired-display-file)
  (define-key dired-mode-map (kbd "M-b")   'backward-word)
  ;; doc view mode
  (define-key doc-view-mode-map (kbd "i") 'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "o") 'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "r") 'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "a") 'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "s") 'doc-view-previous-page)
  (define-key doc-view-mode-map (kbd "t") 'doc-view-next-page))

(define-key slime-repl-mode-map (kbd "M-p") 'slime-repl-previous-input)
(define-key slime-repl-mode-map (kbd "M-n") 'slime-repl-next-input)
