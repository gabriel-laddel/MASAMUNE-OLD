(in-package #:masamune)

(defvar *habits* nil)
(defparameter *swank-connection-hack* nil
  "for some scoping reasons mcclim applications won't have access to
swank::*emacs-connection* unless referenced through this variable")
(defparameter *systems* '("masamune")
  "list of strings naming dirs in ~/quicklisp/local-projects/. Masamune finds
   and tracks lisp systems in these dirs.")
(defvar *nodes* nil)
(defvar *focused-node* nil)
(defvar *agenda* nil "~org-mode style agenda items")
(defvar *system-information* nil)

(defmacro c (name (&rest superclasses) (&rest slots) &rest args)
  `(defclass ,name ,superclasses 
     ,(loop for slot-name in slots
	    collect (list slot-name :accessor slot-name 
				    :initarg (intern (format nil "~a" slot-name) 'keyword) 
				    :initform nil))
     ,@args))

;;; TODO 2014-12-03T23:54:40-08:00 Gabriel Laddel
;;; rename. vertex? concept? meme?
(c node () (name parents description program))

(defun make-node (name &optional (parents) program)
   (push (i 'node :name name
		  :parents (mapcar #'node-by-name parents)
		  :program program)
	 *nodes*))

(defun node-by-name (name)
   (aif (some (lambda (n) (when (string= (string-downcase name) (string-downcase (name n))) n)) *nodes*)
	it (error "no nodes with that name exist")))

(defmethod node-focusedp ((node node)) (eq node *focused-node*))

(defmethod children ((node node))
  (filter (lambda (n) (member node (parents n) :test #'eq)) *nodes*))

(mmg::define-presentation-type node ())
