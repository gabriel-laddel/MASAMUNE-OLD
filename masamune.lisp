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

(defmacro c (name (&rest superclasses) (&rest slots))
  `(defclass ,name ,superclasses 
     ,(loop for slot-name in slots
	    collect (list slot-name :accessor slot-name :initarg (intern (format nil "~a" slot-name) 'keyword) :initform nil))))

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

;;; habits

(c habit () (name description occurrence initialization-function visualization-function))

(defmethod print-object ((habit habit) stream)
  (with-slots (name description) habit
    (format stream "#< habit ~a , ~a >" name description)))

(defmethod occurs-now? ((habit habit))
  "habits should specialize this method as they see fit"
  ;; TODO 2014-11-21T16:26:04-08:00 Gabriel Laddel
  ;; tie into the sleep cycle, currently approximating it.
  (or (null (events habit))
      (let* ((last-finished-event (llast (filter (lambda (l) (eq :finished (getf l :event))) (events habit))))
	     (day (* 60 60 24))
	     (week (* day 7))
	     (time (getf last-finished-event :time))
	     (now (get-universal-time)))
	(if last-finished-event 
	    (ecase (occurrence habit)
	      (:daily (<= time (- now (* .7 day))))
	      (:weekly (<= time (- now week))))
	    t))))

(defun event (event-name &rest misc-information)
  (list :event event-name :time (get-universal-time) :misc-information misc-information))

(defun habit-events-pathname (habit) 
  (format nil "~~/.masamune/data/~a" (regex-replace-all " "  (string-downcase (name habit)) "-")))

(defmethod events ((habit habit))
  (when (probe-file (habit-events-pathname habit))
    (read-file (habit-events-pathname habit))))

(defmethod record-event ((habit habit) event)
  (with-open-file (stream (habit-events-pathname habit)
			  :if-exists :append
			  :if-does-not-exist :create
			  :direction :output)
    (write event :stream stream)))

;; (defun daily-habit (habit time)
;;    "returns generic boolean if habit is not yet done"
;;    (some (lambda (l) (and (eq :finished (getf l :event)) (< (- time (* 24 60 60)) (getf l :time))))
;; 	 (events habit)))

;;; problems

;; (c problem-set (event-store) () (problems urls parent-nodes))
(c problem (event-store) (prompt answers hint correctness-test start-time))

(defun submission (state problem-start &optional submission)
   "STATE is one of #{ :abort, :correct, :incorrect, :hint, :surrender}
PROBLEM-START, is the slot by the same name on the problem that has been
submitted, specifically, it's unix time."
  (list :state state :time (- (get-universal-time) problem-start) :submission submission))

;; (defmethod start (problem)
;;   (format t "~%~s~%" (prompt problem))
;;   (setf (start-time problem) (get-universal-time))
;;   (let+ ((submission (read-line)))
;;     (cond ((equal (string-downcase submission) "surrender")
;;            (push (submission :surrender (start-time problem) submission) 
;;                  (submissions problem))
;;            (format t "~%Lame. The answer is ~S~%"  (answers problem))
;; 	   (start problem))

;;           ((or (funcall (correctness-test problem) submission (answers problem))
;; 	       (member submission (answers problem) :test #'equal))
;;            (push (submission :correct (start-time problem) submission) 
;;                  (submissions problem))
;;            (format t "~%~a~%" "Correct!")
;;            t)

;;           ((equal submission "hint")
;;            (push (submission :hint (start-time problem)) 
;;                  (submissions problem))
;;            (format t "~%~a~%" (hint problem)))

;;           ((member submission '("quit" "abort" "exit") :test #'equal)
;;            (push (submission :abort (start-time problem)) (submissions problem))
;;            (format t "~%See you again soon.~%"))

;;           (t (format t "~%~a is incorrect. Try again.~%" submission)
;;              (push (submission :incorrect (start-time problem) submission) 
;;                    (submissions problem))
;;              (start problem)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; systems

(defun parse-sloccount-output (sloccount-output)
  (let* ((s1 (subseq sloccount-output 
		     (search "(dominant language first)" sloccount-output) 
		     (search "Total Physical" sloccount-output))))
    (loop for line in (rest (split "\\n" s1))
	  collect (mapcar (lambda (s) (regex-replace ":" s ""))
			  (take 2 (remove "" (split " " line) :test #'string=))))))

(defun calculate-system-information ()
  ;; TODO 2014-11-21T03:56:20-08:00 Gabriel Laddel
  ;; this doesn't work on the masamune repository today
  (loop for system-name in *systems*
	collect (cons system-name
		      (->> system-name
			   (format nil "sloccount ~~/quicklisp/local-projects/~a")
			   (rp)
			   (parse-sloccount-output)))))

;;; Agenda
;;; ============================================================================

(c agenda-item () (due-date title description start-function))

(defun make-agenda-item (title &optional description due-date start-function)
  (make-instance 'agenda-item
		 :title title
		 :due-date due-date
		 :description description
		 :start-function start-function))

(defun populate-agenda-items ()
  (setf *agenda* (l (make-agenda-item "Review Norm's paper on Riemann's curvature tensor")
		    (make-agenda-item "Masamune save state")
		    (make-agenda-item "review CLOCC codebase "
				      "http://clocc.sourceforge.net/")
		    (make-agenda-item "allegro common lisp trial")
		    (make-agenda-item "Historical MPEX data")
		    (make-agenda-item "respond to zach maril"))))
