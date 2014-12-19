(fiasco:define-test-package #:rpc-tests
  (:use #:masamune #:fiasco))

(in-package #:mm)

(defvar *rpc-test-file* nil)

(defun rpc-function-to-export ()
  (let* ((rpc-test-file (format nil "/tmp/test-output-~a" (get-universal-time))))
    (setf *rpc-test-file* rpc-test-file)
    (with-open-file (stream rpc-test-file
			    :direction :output
			    :if-exists :error
			    :if-does-not-exist :create)
      (format stream "t"))))

(in-package #:rpc-tests)

(export-project-rpcs "127.0.0.1" 6000 '(rpc-function-to-export) 
		     'masamune-client '(:mmc)
		     "/root/quicklisp/local-projects/masamune" "/tmp/"
		     :skip-on-error t)

(deftest local-rpc ()
  (let* ((client-output-dir (format nil "/tmp/test-~a" (get-universal-time)))
	 (port (loop with p = 6000 while (mm::port-in-use-p p) 
		     do (incf p) finally (return p))))
    (rp (format nil "mkdir -p ~a" client-output-dir))
    
    (load (merge-pathnames client-output-dir "rpc-server.lisp"))
    ;; new sbcl proc
    (format nil "sbcl --load ~a --eval ~a"
	    (merge-pathnames client-output-dir "client-library.lisp")
	    '(progn (funcall 'rpc-function-to-export) (quit)))
    (is (car (mm::read-file mm::*rpc-test-file*)))
    (setf mm::*rpc-test-file* nil)))
