;;; Lisp RPC
;;; ============================================================================
;;; 
;;; the web was obsolete at its inception. there is no hypertext document format
;;; or stylesheets there are only programs. there is no cloud, there is only
;;; another's disk etc. the failure to use the terminology of computer science
;;; gives rise to plugins, extension languages, frameworks and other such
;;; definitionless monstrosities.
;;; 
;;; various parties claim that they're going to 'fix the web' and to those
;;; innocent of the implementation details it's not clear this is impossible. in
;;; practice, unless the powers that be are willing to scrap everything that the
;;; web currently is and replace it with something sane, these projects are
;;; doomed to failure through obscurity, or by succeeding only in impressing
;;; idiots. consider google's pnacl. the goal is to have a system that allows a
;;; programmer to intermix html, css and javascript with system level resources
;;; by compiling any language down to a subset of llvm bytecode. to produce
;;; anything on top of this tower of nonsense requires one to know the (moving)
;;; specifications for the 3 web languages + the language or languages being
;;; compiled down to llvm bytecode. such a system is the antithesis of
;;; comprehensibility, and thus, progress. last i checked, you couldn't so much
;;; as get error messages through pnacl and it's written in c++. yes, i know
;;; they're "working on it" and oracle is still trying to work around the idiocy
;;; that is java and africa is still trying to work around the
;;; genetic/epigenetic/cultural relationship that leads the inhabitants to
;;; destroy anything resembling order. the philosophy "if we continue to add
;;; more code order and sophistication will somehow emerge from the chaos!"
;;; produces precisely the opposite of the intended result. order and
;;; sophistication blossom from comprehensibility, not layers of obsolescent
;;; crud.
;;; 
;;; this position is diametrically opposed to that of ~every startup &
;;; corporation in silicon valley. i shall take a moment to illustrate an
;;; alternative to some of their madness. consider backend architectures, eg,
;;; code callable through an http/tcp interface. the way this is done today is
;;; by wiring (generic)functions on the backend to a url scheme invented on the
;;; fly. successful 'apis' are wrapped by hand (string hacking!) and made
;;; available as client libraries because it's more convenient to call a
;;; function (which has semantics) in one's native language than make http
;;; requests by concatenating strings (devoid of semantics).
;;; 
;;; the correct way to go about this is to use RPC. a programmer wishes to make
;;; a codebase available over the internet. he gives a list of functions he'd
;;; like to make available to another program. this program walks the ast,
;;; retrieves the documentation and argument lists of said functions, wires them
;;; to a standard calling interface over http or tcp, and exports a tree (as
;;; s-expressions) notating this information. any programmer can now walk the
;;; tree in any language they wish and transform it into a client library (the
;;; end game for any sufficiently useful or popular service). layering rate
;;; limiting, encryption, public key authentication, a standard gui, etc. on
;;; this is straightforwards.
;;;
;;; This is the best usocket guide I'm aware of.
;;; http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp

(in-package #:mm)

;;; Tests
;;; ============================================================================
;; in repl0
(defvar last-stream nil)

(defun handler (stream)
  (declare (type stream stream))
  (setq last-stream stream)
  (format t "stream:~a" stream)
  (format stream "~a" '(1 2 3 4)))

(defvar socket-server
  (usocket:socket-server "192.168.0.28" 4007 'handler nil
			 :in-new-thread t
			 :reuse-address t
			 :element-type 'character))

;; repl1 
(setf socket (usocket:socket-connect "localhost" 4007 :protocol :stream
						      :element-type 'character))

(progn (format (usocket:socket-stream socket) "testing 1 2 3...")
       (force-output (usocket:socket-stream socket)))

(read (usocket:socket-stream socket)) ;; will return (1 2 3 4)

;;; listener thread

(setf master-socket
      (usocket:socket-listen "127.0.0.1" 4007 :reuse-address t
					      :element-type 'character))

(usocket:wait-for-input master-socket)
(setf new-client (usocket:socket-accept master-socket))

(setf new-client (usocket:socket-accept master-socket))

;; (format (usocket:socket-stream new-client) "~&Hello server")
;; (format (usocket:socket-stream master-socket) "~&Hello client")

;; (setf sockets (list master-socket))
;; ;; see below for the :ready-only description
(loop
  (loop :for s :in (usocket:wait-for-input sockets :ready-only t) :do
    (if (eq s master-socket)
        ;; THEN: new connection
        (let ((new (usocket:socket-accept s)))
          ;; add the new socket to the list of sockets that we're listening to
          (setf sockets (nconc sockets `(,new)))
          (handle-client-connect new))
        ;; ELSE: data from a connected client
        (handle-client-input s))))
