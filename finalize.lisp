(in-package #:mm)

(unless (every #'probe-file '("~/.conkerorrc" "~/.mozrepl-conkeror.js"))
  (install-conkeror))

(awhen (probe-file (ppath "lisp-customizations.lisp")) (load it))

;; (calculate-system-information)
(populate-agenda-items)
(mm::start-conkeror)
(swank::eval-in-emacs 
 '(with-current-buffer "*slime-repl sbcl*"
   (end-of-buffer)
   (insert "(setf mm::*swank-connection-hack* *standard-output*)")
   (slime-repl-return)) t)

(swank::eval-in-emacs
 '(progn (loop for i in '("/root/quicklisp/local-projects/masamune/mathematics-scratch.lisp" 
			  "/root/quicklisp/local-projects/masamune/util.el" 
			  "/root/documents/writing/funding.org"
			  "/root/quicklisp/local-projects/masamune/browser/conkeror.lisp"
			  "/root/quicklisp/local-projects/masamune/masamune-gui.lisp" 
			  "/root/documents/books/notes/john-von-neumann-and-norber-weiner-from-mathematics-to-the-technologies-of-life-and-death.org" 
			  "/root/documents/writing/programming/nixos.org" 
			  "/root/quicklisp/local-projects/masamune/systems/summarize-logs.lisp" 
			  "/root/quicklisp/local-projects/masamune/systems/captains-log.lisp" 
			  "/root/quicklisp/local-projects/masamune/browser/anaphora.paren"
			  "/root/quicklisp/local-projects/masamune/browser/custom-commands.js"
			  "/root/quicklisp/local-projects/masamune/browser/default-conkerorrc.js"
			  "/root/quicklisp/local-projects/masamune/browser/documentation.lisp"
			  "/root/quicklisp/local-projects/masamune/browser/js-to-ps.lisp"
			  "/root/quicklisp/local-projects/masamune/build-bootable-hard-drive.lisp"
			  "/root/quicklisp/local-projects/masamune/captain-mcclim.lisp" 
			  "/root/quicklisp/local-projects/masamune/celluar-automata.png"
			  "/root/quicklisp/local-projects/masamune/classes.lisp" 
			  "/root/quicklisp/local-projects/masamune/clhs.el"
			  "/root/quicklisp/local-projects/masamune/culture.lisp" 
			  "/root/quicklisp/local-projects/masamune/dashboard.lisp"
			  "/root/quicklisp/local-projects/masamune/default-data.lisp"
			  "/root/quicklisp/local-projects/masamune/editing.el"
			  "/root/quicklisp/local-projects/masamune/emacs-customizations.el" 
			  "/root/quicklisp/local-projects/masamune/finishing-touches.css" 
			  "/root/quicklisp/local-projects/masamune/init.el" 
			  "/root/quicklisp/local-projects/masamune/init.lisp"
			  "/root/quicklisp/local-projects/masamune/lisp-customizations.lisp" 
			  "/root/quicklisp/local-projects/masamune/lisp-system.lisp" 
			  "/root/quicklisp/local-projects/masamune/masamune.asd"
			  "/root/quicklisp/local-projects/masamune/masamune.el"
			  "/root/quicklisp/local-projects/masamune/monkey-patches.lisp"
			  "/root/quicklisp/local-projects/masamune/packages.lisp"
			  "/root/quicklisp/local-projects/masamune/parenscript-mode.el"
			  "/root/quicklisp/local-projects/masamune/readme.org" 
			  "/root/quicklisp/local-projects/masamune/repository.lisp" 
			  "/root/quicklisp/local-projects/masamune/save-state.lisp" 
			  "/root/quicklisp/local-projects/masamune/splash.gif" 
			  "/root/quicklisp/local-projects/masamune/system-apropos.lisp" 
			  "/root/quicklisp/local-projects/masamune/system.html" 
			  "/root/quicklisp/local-projects/masamune/system.org" 
			  "/root/quicklisp/local-projects/masamune/util.paren" 
			  "/root/quicklisp/local-projects/masamune/systems/communications.lisp"
			  "/root/quicklisp/local-projects/masamune/systems/exercise.lisp"
			  "/root/quicklisp/local-projects/masamune/systems/hebrew.lisp"
			  "/root/quicklisp/local-projects/masamune/systems/mathematics-practice.lisp" 
			  "/root/quicklisp/local-projects/masamune/systems/memory-practice.lisp" 
			  "/root/quicklisp/local-projects/masamune/systems/non-von-neumann-computing.txt"
			  "/root/quicklisp/local-projects/masamune/systems/observations.lisp" "/root/quicklisp/local-projects/masamune/systems/programming-practice.lisp")
	       do (find-file i)) nil)
 t)
