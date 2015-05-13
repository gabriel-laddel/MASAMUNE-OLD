(require 'cl)
(require 'cl-lib)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun cat (&rest args) 
  (apply #'concatenate 'string (mapcar (lambda (x) (if (stringp x) x (prin1-to-string x))) args)))

(defun latest-swank ()
  (let* ((ql-dir-name "~/quicklisp/dists/quicklisp/software/")
	 (perhaps (remove-if-not (lambda (s) (string-match-p "slime" s)) 
				 (directory-files ql-dir-name)))
	 (local-project-dir "~/quicklisp/local-projects/slime/"))
    (if (file-exists-p local-project-dir)
	local-project-dir
      (concatenate 'string ql-dir-name 
		   (car (sort perhaps (lambda (s1 s2) (< (subseq (- (length s1) 2) (length s1))
						    (subseq (- (length s1) 2) (length s1))))))
		   "/"))))

(add-to-list 'load-path (latest-swank))
(add-to-list 'load-path (cat (latest-swank) "/contrib"))

(require 'slime-autoloads)
(require 'slime)

(setq slime-backend (expand-file-name (cat (latest-swank) "swank-loader.lisp"))
      slime-path (latest-swank)
      slime-contribs '(slime-editing-commands
		       slime-fontifying-fu
		       slime-fuzzy
		       slime-fancy
		       slime-asdf
		       slime-indentation
		       slime-package-fu
		       slime-references
		       slime-repl
		       slime-xref-browser)
      slime-use-autodoc-mode         t
      slime-autodoc-use-multiline-p  t
      slime-enable-evaluate-in-emacs t
      slime-protocol-version 'ignore
      inferior-lisp-program (shell-command-to-string "which sbcl")
      inferior-lisp-buffer           "*slime-repl sbcl*"
      slime-complete-symbol*-fancy t
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root (expand-file-name "~/lisp/HyperSpec/"))

(slime-setup)

(defun start-interactive-install ()
  (if (cl-member "*slime-repl sbcl*" (mapcar #'buffer-name (buffer-list)) :test 'equal)
      (progn (with-current-buffer "*slime-repl sbcl*"
	       (end-of-buffer)
	       (insert "(load \"~/quicklisp/local-projects/masamune/build/install-finalize.lisp\")")
	       (slime-repl-return "*slime-repl sbcl*")
	       (delete-other-windows))
	     (switch-to-buffer))
    (run-at-time "1 seconds" nil #'start-interactive-install)))

(add-hook 'window-setup-hook #'(lambda () (slime-connect "127.0.0.1" 4005) 
				 (start-interactive-install)))
