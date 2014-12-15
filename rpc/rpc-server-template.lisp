(defparameter *socket* (usocket:socket-listen *host* *port* :element-type 'character))
(defparameter *client-connection* nil)

(defun help ()
  (let* ((s (make-string-output-stream)))
    (format s "the functions available to you are:~%")
    (loop for sym in *exported-symbols* 
	  do (if (fboundp sym) (format s "~%~s (fbound)" sym)
		 (format s "~%~s" sym)))
    (format s "~%~%if you'd like more information about a symbol (eg, docstring,
lambda-list and type information) try calling `%describe' on it.")
    (get-output-stream-string s)))

(defmacro %describe (o)
  "returns the contents of `describe-object' as string"
  `(let* ((s (make-string-output-stream)))
     (describe-object ,o s)
     (get-output-stream-string s)))

(defun send (quoted-sexp)
  (write quoted-sexp :stream (usocket:socket-stream *client-connection*)))

(defun disconnect ()
  (usocket:socket-close *socket*)
  (setf *client-connection* nil
	*socket* (usocket:socket-listen *host* *port* :element-type 'character)))

(bt:make-thread
 (lambda () 
   (let* ((input))
     (loop (if *client-connection*
	       (progn (setf input (read *client-connection*))
		      (if (member (car input) *exported-symbols*)
			  (if (eq 'disconnect (car input))
			      (disconnect)
			      (send (eval input)))
			  (send '(error "attempted to call restricted function"))))
	       (setf *client-connection* (usocket:socket-accept socket))))))
 :name "server rpc loop")
