;;; Save state
;;; ============================================================================
;;;
;;; Emacs, Stumpwm and Conkeror are all capable of saving and restoring a
;;; previous state. McCLIM applictions are generally structured such that state
;;; can be saved in the appliction object itself via `define-appliction-frame'.
;;; 
;;; desktop.el is traditionally used to save state for Emacs, Stumpwm comes with
;;; its own set of related primitives and Conkeror has the ability to, but I'm
;;; not aware of anyone who has made a 'mode' for it.
;;;
;;; In my own hacking sessions Emacs generally manages 100s of buffers, Conkeror
;;; has 10-30 (at which point it really starts to slow down) and one or two
;;; CLIM windows, possibly a xterm dedicated to ncurses for music.
;;;
;;; - docview

(in-package #:mm)

(in-package #:stumpwm)

;; (dump-desktop-to-file "/tmp/window-placement-rules")
;; (restore-desktop (read-dump-from-file "/tmp/window-placement-rules"))
