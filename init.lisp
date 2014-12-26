(in-package #:mm)

(defvar *hack* nil "Occasionally I need somewhere to put an intermediate value.")
(defvar *project-location* #P"~/quicklisp/local-projects/masamune/")
(defparameter *swank-connection-hack* nil
  "for some scoping reasons mcclim applications won't have access to
swank::*emacs-connection* unless referenced through this variable")
(defvar *habits* nil)
(defparameter *systems* '("masamune")
  "list of strings naming dirs in ~/quicklisp/local-projects/. Masamune finds
   and tracks lisp systems in these dirs.")
(defvar *nodes* nil)
(defvar *focused-node* nil)
(defvar *agenda* nil "~org-mode style agenda items")
(defvar *system-information* nil)

;;; PCLOS initalization
;;; ============================================================================

(in-package #:manardb)

(use-mmap-dir #P"~/.masamune/pclos-datastore/")
(open-all-mmaps)

(DEFCLASS force-manardb-init ()
  ((test-slot :ACCESSOR test-slot :INITARG :test-slot :INITFORM nil))
  (:metaclass manardb::mm-metaclass))

(make-instance 'force-manardb-init :test-slot t)

(in-package #:stumpwm)

(setq *input-window-gravity* :center
      *message-window-gravity* :center
      *normal-border-width* 0      
      *window-border-style* :none
      *transient-border-width* 0
      *top-level-error-action* :break)


(setf STUMPWM:*MODE-LINE-BORDER-WIDTH* 0
      STUMPWM:*MODE-LINE-BACKGROUND-COLOR* "white"
      STUMPWM:*MODE-LINE-FOREGROUND-COLOR* "black"
      STUMPWM:*MODE-LINE-PAD-X* 0
      STUMPWM:*MODE-LINE-PAD-Y* 0
      STUMPWM:*MODE-LINE-POSITION* :bottom)

(mode-line)
