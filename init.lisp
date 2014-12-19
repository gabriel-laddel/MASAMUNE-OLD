(in-package #:mm)

(defvar *hack* nil "Occasionally I need somewhere to put an intermediate value.")
(defvar *project-location* #P"~/quicklisp/local-projects/masamune/")

;;; PCLOS initalization
;;; ============================================================================

(in-package #:manardb)

(use-mmap-dir #P"~/.masamune/pclos-datastore/")
(open-all-mmaps)

(DEFCLASS force-manardb-init ()
  ((test-slot :ACCESSOR test-slot :INITARG :test-slot :INITFORM nil))
  (:metaclass manardb::mm-metaclass))

(make-instance 'force-manardb-init :test-slot t)
