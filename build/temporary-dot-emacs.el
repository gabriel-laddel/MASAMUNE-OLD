(require 'cl)
(require 'cl-lib)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun cat (&rest args) 
  (apply #'concatenate 'string (mapcar (lambda (x) (if (stringp x) x (prin1-to-string x))) args)))

(defun latest-swank ()
  (let* ((ql-dir-name "~/quicklisp/dists/quicklisp/software/")
	 (perhaps (remove-if-not (lambda (s) (string-match-p "slime" s)) 
				 (directory-files ql-dir-name))))
    (concatenate 'string ql-dir-name 
		 (car (sort perhaps (lambda (s1 s2) (< (subseq (- (length s1) 2) (length s1))
						  (subseq (- (length s1) 2) (length s1))))))
		 "/")))

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
      ;; TODO 2014-10-22T20:20:56-07:00 Gabriel Laddel
      ;; this should be a configurable variable.
      common-lisp-hyperspec-root (expand-file-name "~/lisp/HyperSpec/"))

(slime-setup)

(defun start-interactive-install ()
  (slime-eval-async 
      '(cl::load "~/quicklisp/local-projects/masamune/build/install-finalize.lisp")
    (lambda (&rest args) nil)))

(add-hook 'slime-connected-hook 'start-interactive-install)
(add-hook 'window-setup-hook #'(lambda () (slime-connect "127.0.0.1" 4005)))
