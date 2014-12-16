;;; usocket guides
;;; 
;;; http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp
;;;
;;; TODO
;;; - package exports should be a symbol
;;; - notify user when symbols they've attempted to export were not found
;;; - unify with asciilifeform's new internet description
;;; - add MOCL information to the trivial sockets page (does it work?):
;;;   http://common-lisp.net/project/usocket/implementation-comparison.shtml
;;;   & "status for currently targeted backends"
;;; http://common-lisp.net/project/usocket/
;;; - fix usocket tests, convert to stefil
;;; 
;;; open questions / unimlemented:
;;; 
;;; - security
;;;   - PGP
;;; - allow / disallow reader macros?
;;;
;;; usage
;;; =====
;;; call the function `interactively-network-library' which will walk you
;;; through the process of making your library network ready. keep in mind the
;;; following:
;;;
;;; - exported functions should not return multiple values.
;;;
;;; - symbols you're exporting must name a function or a macro.
;;;
;;; so, what does this have over SLIME'ing around in a restricted envionment?
;;; Well. I don't actually know that it is any better of an idea, honestly. But
;;; I'm not aware of anyone doing either of the previous, and I'm more than open
;;; to, after securing this (or finding out how unfeasible it is to secure)
;;;
;;; as it stands it has the benefit of being written, being more enjoyable to
;;; program for as both a client and a server. So, there is that I guess.
;;;
;;; so what is this then? we have objects tied to WoT keys? how exactly does
;;; that work when you've got 10,000 boxen?
;;;
;;; however it ends up working I think that this model is a pretty good starting
;;; point.
;;;
;;; playing with the setf'ing etc. should be fine so long as you have some
;;; sort of versioning system for PCLOS (ie, a good backup system)
;;;
;;; so lets enumerate a few implementations and see where that gets us:
;;; 
;;; - business
;;; - publicly available webservice
;;; - private service
;;;
;;; http://www.contravex.com/2014/11/28/breaking-a-bitcoin-brainwallet/
;;; http://trilema.com/2013/steganography-or-the-simple-yet-strong-brain-wallet/
;;;
;;; I was thinking about possibly using boxes as a dead drop and from time to
;;; time giving up access to them? this is obviously incorrect. the correct
;;; way to go about these things is to pass messages back and forth. But what
;;; if I want your message to be able to change some state on my box?
;;;
;;; what all of this is based on of course is how much do you trust the other
;;; guy? the correct way to go about this is to have a scale of how much you'll
;;; allow someone else to do based on how much you value the data in question.
;;;
;;; things like facebook are not going to exist in the future, really, and the
;;; only people who will be dealing with "the masses" are cattle ranchers.
;;; we'll leave it up to them to solve I suppose.
;;;
;;; performance
;;; ============================================================================
;;; - what is the maximum number of sockets you can open on a linux box? is this
;;;   tied to threads etc. does it make sense to keep sockets open?
;;;

(in-package #:mm)

(defun export-defun-or-macro (defun-sexp)
  "currently dependent on *socket being named thusly"
  (destructuring-bind (name lambda-list maybe-docstring) 
      (take 3 (rest defun-sexp))
    (if (member '&rest lambda-list)
	(error "rest args are currently not handled")
	(let* ((body `((write (list (quote ,name) 
				    ,@(remove-if (lambda (s) (member s '(&rest &optional &keys)))
						 lambda-list)) :stream (usocket:socket-stream *socket*))
		       (finish-output (usocket:socket-stream *socket*))
		       (read (usocket:socket-stream *socket*)))))
	  `(defun ,name ,lambda-list
	     ,@(if (stringp maybe-docstring)
		   (cons maybe-docstring body)
		   body))))))

(defun generate-client-library
    (output-dir input-pathname client-library-name host port
     symbols-to-export &optional skip-on-error)
  "INPUT-PATHNAME names the dir containing the project you're exporting from"
  (let* ((client-library-pathname (merge-pathnames output-dir "client-library.lisp")))
    (with-open-file (s client-library-pathname
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (labels ((h (control-string &rest args)
		 (format-symbol *package* control-string args))
	       (valid-sexps (file) (loop for sexp in (read-file file)
					 when (and (member (cadr sexp) symbols-to-export) 
						   (member (car sexp) '(defmacro defun)))
					   collect sexp))
	       (k (l) (format s "~{~s~%~%~}" l)))

	(let* ((*print-case* :downcase)
	       (skip-on-error skip-on-error)
	       (sexps-to-export (loop for file in (filter (lambda (p) (string= "lisp" (pathname-type p)))
							  (recursive-contents input-pathname))
				      for sexps = (if skip-on-error
						      (handler-case (valid-sexps file)
							(error () nil))
						      (restart-case (valid-sexps file)
							(skip () :report "don't read this file")
							(skip-all () :report "skip any file which throws an error"
							  (setf skip-on-error t)
							  nil)))
				      when sexps append sexps))
	       (detected-functions-or-macros (mapcar #'second sexps-to-export))
	       (undetected-symbols (set-difference symbols-to-export detected-functions-or-macros)))
	  (restart-case (if undetected-symbols
			    (error "The symbols ~s were not detected as functions or macros and thus cannot be exported"
				   undetected-symbols))
	    (continue-anyways () :report "disregard the missing symbols"))

	  (k `((in-package ,client-library-name)
	     
	       (defparameter *socket* nil)
	       (defparameter *host* ,host)
	       (defparameter *port* ,port)

	       (defun connect ()
	  	 (setf *socket*(usocket:socket-connect *host* *port* 
						       :element-type 'character
						       :protocol :stream)))

	       (defun disconnect ()
		 (write '(disconnect) :stream (usocket:socket-stream *socket*))
		 (finish-output (usocket:socket-stream *socket*))
	  	 (usocket:socket-close *socket*))))

	  (format s "~{~s~%~%~}" (mapcar #'export-defun-or-macro sexps-to-export)))))
    (format t "~%created client library at ~a" client-library-pathname)))

(defun generate-client-asd-file (output-dir system-name)
  (labels ((f (sym) (format-symbol nil sym)))
    (let* ((*print-case* :downcase)
	   (asd-pathname (merge-pathnames output-dir 
					  (format nil "~a.asd" system-name))))
      (with-open-file (stream asd-pathname
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(format stream "~s~%~%~s" '(in-package #:asdf)
		`(defsystem ,(f system-name)
		   :serial t
		   :depends-on (#:masamune)
		   :components ((:file "packages.lisp") (:file "client-library")))))
      (format t "created client library .asd file, ~a" asd-pathname))))

(defun generate-client-package
    (output-dir package-name package-nicknames package-exports)
  ;; TODO 2014-12-14T04:47:15+00:00 Gabriel Laddel
  ;; * derive `package-name' from package being 'exported'
  ;; * remove regex hack
  (assert (and (symbolp package-name)
	       (listp package-nicknames)
	       (listp package-exports)))
  (labels ((f (sym) (format-symbol nil sym)))
    (let* ((*print-case* :downcase)
	   (package-sexp-string (format nil "~s"
					`(defpackage ,(f package-name)
					   (:nicknames ,@(mapcar #'f package-nicknames))
					   (:export ,@package-exports)
					   (:use #:masamune))))
	   (output-pathname (merge-pathnames output-dir "packages.lisp")))
      (with-open-file (stream output-pathname 
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(format stream (regex-replace-all "\\|" package-sexp-string "")))
      (format t "created client library package ~a" output-pathname))))

(defun generate-server
    ;; TODO 2014-12-15T05:06:06+00:00 Gabriel Laddel
    ;; - fully qualify exported symbols
    ;; - see slime-indentation for pretty printing, also, trivial indent
    (input-library-path host port symbols-to-export &optional (output-dir "/tmp/"))
  (labels ((defsystem-sexp-p (l) (when (member (car l) '(asdf/defsystem:defsystem
							 asdf:defsystem
							 defsystem)) l)))
    (let* ((server-codebase-asd-file (some (lambda (p) (when (string= "asd" (pathname-type p)) p)) 
					   (ls input-library-path)))
	   (*print-case* :downcase)) 
      (with-open-file (s (if output-dir
			     (merge-pathnames output-dir "rpc-server.lisp")
			     (merge-pathnames input-library-path "rpc-server.lisp"))
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	(format s "~a" 
		(cat (format nil "(in-package #:mm)~%~%~{~s~%~%~}"
			     `((defparameter *host* ,host)
			       (defparameter *port* ,port)
			       (defparameter *exported-symbols*
				 (quote ,(append '(help %describe disconnect) symbols-to-export)))))
		     (slurp-file (qlpp "/masamune/rpc/rpc-server-template.lisp"))))
	;; update .asd file
	(with-open-file (s server-codebase-asd-file
			   :direction :input
			   :if-exists :supersede)
	  (with-open-file (stream (merge-pathnames output-dir "server.asd")
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create) 
	    (format stream "~{~s~%~%~}" 
		    (let* ((asd-file-contents (->> (ls input-library-path)
						   (some (lambda (p) (when (string= "asd" (pathname-type p)) p)))
						   (read-file)))
			   (defsystem-sexp (some #'defsystem-sexp-p asd-file-contents))
			   (defsystem-sexp-pos (position defsystem-sexp asd-file-contents :test #'eq))
			   (asd-file-leading (take defsystem-sexp-pos asd-file-contents))
			   (asd-file-trailing (drop (1+ defsystem-sexp-pos) asd-file-contents))
			   (components (getf defsystem-sexp :components)) 
			   (rpc-file '(:file "rpc-server")))
		      (unless (member :masamune (getf defsystem-sexp :depends-on))
			(push :masamune (getf defsystem-sexp :depends-on)))
		      (unless (member rpc-file (getf defsystem-sexp :components) :test #'equal)
			(setf (getf defsystem-sexp :components)
			      (append components (list rpc-file))))
		      (if (= 0 defsystem-sexp-pos) (list defsystem-sexp) 
			  (append asd-file-leading (cons defsystem-sexp asd-file-trailing)))))))))))
