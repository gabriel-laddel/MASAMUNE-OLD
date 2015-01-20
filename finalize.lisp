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
