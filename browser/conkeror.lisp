;;; Interface to the Conkeror web browser.
;;; =============================================================================
;;; 
;;; This file serves the following purposes:
;;; 
;;; 1. Downloading binary 33.1 of XULrunner and using it to launch Conkeror
;;;    commit 48d3ef4369f267faf42451a580b1ac6bcb6a5a18, master branch of the
;;;    repository git://repo.or.cz/conkeror.git
;;; 
;;; 2. Building Mozrepl from the sources in .../masamune-os/browser/mozrepl/.
;;; 
;;; 3. Providing abstractions for interfacing with Conkeror through CL.
;;;
;;; 4. Providing documentation on the proceeding processes.
;;; 
;;; I have a limited amount of patience for parsing semantics from
;;; ~ALGOL. Ideally we would like to have a parenscsript interface to Conkeror,
;;; accompanying documentation, inspection of js objects in a SLIME-inspector
;;; style interface etc. As there is currently no generally-accepted (or any
;;; acceptable) processes to manage software projects, it is highly reccomended
;;; you read this comment in its entirety before modifying this code. Mozilla's
;;; extension documentation is shit and should not be used unless absolutely
;;; neccecary
;;;
;;; Under no circumstances should the XULrunner or Conkeror be blindly updated
;;; to HEAD. The sources are included in the distribution to enforce a code
;;; freeze. If, for some reason it is necessary to patch them, patch the inclued
;;; sources directly. At a glance, I see many things that could simply be thrown
;;; out, eg the android nonsense, anything neccecary for the java / python
;;; bindings. that said, xulrunner alone has 98,346 files and 6,725 directories.
;;; not one is documentation.
;;;
;;; parenscript has some issues - namely that it isn't common lisp and there
;;; isn't any way to get away from the underlying JS model. now, there are two
;;; ways of dealing with this - the first, try and be common lisp. it would be
;;; awfully nice if someone were to put in the work to make it so, but I doubt
;;; it'll happen. the other option would be to take it and make it something
;;; completely different - an arc dialect or whatever. one could even go as far
;;; as to run the backwards-parenscript CL system on conkeror's js to transpile
;;; it into something sane. (eg, something emacs like - distinctly inferior to
;;; CL, but workable)
;;;
;;; another route would be to use lispkit instead of conkeror, though for the
;;; time being I'm going to dismiss this as all the necessary libraries are not
;;; available through right now.
;;; 
;;; 
;;;                                     (O)        
;;;                                      H       
;;;                                      H
;;;                                 ____hHh____
;;;                                 `---------`      
;;;                                    | | |      
;;;                                    | | |
;;;                                    | | |
;;;                                    | | |
;;;                                    | | |
;;;                                    | | |
;;;                                    | | |
;;;                                    | | |
;;;                              _____;~~~~~:____
;;;                           __'                \
;;;                          /         \          |
;;;                         |    _\\_   |         |\
;;;                         |     \\    |         | |      ___
;;;                  __    /     __     |         | |    _/   \
;;;                 /__\  |_____/__\____|_________|__\  /__\___\
;;; TODO
;;; =============================================================================
;;;
;;; - conkeror has some notion of 'chrome contexts' - where is this list located?
;;;
;;; - will mozrepl pick up on conkeror's docstrings?
;;;
;;; - rewrite the 'custom' commands in parenscript
;;;
;;; - ensure parenscript documentation is downloaded and cross referenced with
;;;   javascript documentation.
;;;
;;; - color themes & everything else from dan.
;;; 
;;; - load new html/javascript/css onto any webpage via mozrepl. could be
;;;   converted to a general tookit for making use of online content. see:
;;;   https://github.com/jlbfalcao/mozrepl_tools
;;; 
;;; - install extensions programatically
;;; 
;;; - what does firebug lite offer, and should it be included by default?
;;; 
;;; - stripped (of css) copies of the documentation for the conkeror / XULrunner
;;;   distributed with masamune.
;;;
;;; - edit text fields from Emacs
;;;
;;; - mozrepl doesn't give details on errors
;;;
;;; - what disgusting hacks can be done with __proto__?
;;;
;;; - resource://gre/modules/Preferences.jsm conkeror allows you to access the JS
;;;   that runs it, but doesn't auto-complete on it - checking the source for a
;;;   particular version should allow me to build something similar, at least, a
;;;   table with the each value. this table and all other should be indexed and
;;;   autocompletion made avalible.
;;;   
;;; - http://conkeror.org/PageModes ;; already has the stuff for clean wikipedia
;;;   about:config ; may have some useful configuration options
;;; 
;;; Glossary
;;; =============================================================================
;;; 
;;; Mozilla et. all take great pleasure in renaming concepts without adding any
;;; value, giving rise to deplorable terms such as "add-ons". Reading MozREPL
;;; sources or the Mozilla documentation requires you're aware of their jargon.
;;;
;;; Interactor - How MozREPL's refers to its REPL loop, and the idea that *gasp!*
;;;              you can monkey patch it.
;;;              
;;; Commands - MozREPL declares that anything added to it's default interface is
;;;            a 'command'
;;;
;;; Component - ?
;;; 
;;; Overlay   - ?
;;; 
;;; XULrunner - C/C++ framework used for building Mozilla applications, eg
;;;             firefox. Conkeror uses this as it's basis. You can access much
;;;             of the underlying functionality via javascript
;;;             
;;; Context   - the current js environment, eg a webpage, the repl (extension)
;;;             environment etc.
;;;                   
;;; MozRepl Pathnames and their meanings
;;; =============================================================================
;;;
;;; ./mozrepl
;;; |
;;; ├── chrome                       
;;; │       ├── overlay_browser.xul  | XML detailing a MozRepl 'overlyay' menu TODO accessed how?
;;; |       |                        | 
;;; │       ├── overlay_impl.js      | useful functions TODO how to call them? what js object are they attatched to?
;;; |       |                        | 
;;; │       ├── overlay.js           | event listener to init 'overlay'
;;; |       |                        | 
;;; │       ├── repl.js              | 
;;; │       ├── server.js            | 
;;; │       ├── ui.js                | 2fns constructor(server),toggleServer(sourceCommand)--
;;; |       |                        | 
;;; │       └── util.js              | 3fns helpUrlFor(thing),docFor(thing),--argList(fn)--
;;; |                                | 
;;; ├── chrome.manifest              | boilerplace k/v pairs specifying details about the extension
;;; |                                | 
;;; ├── components                   |
;;; |   |                            | 
;;; │   ├── CommandLine.js           | details the command line interface to mozrepl TODO, is this used by comint? apparently, one can used the command line options here to specify stuff ;line "/usr/bin/firefox -profile /path/to/profile/folder -repl -repl 7070" source: https://github.com/bard/mozrepl/wiki/Starting-the-REPL
;;; |   |                            | 
;;; │   ├── Makefile                 | generates the .xpt from the .idl and specifies the XULrunner binaries to do this
;;; |   |                            | 
;;; │   ├── MozRepl.idl              | .idl is an algol-derived "language-independent" interface description. this file describes a handful of functions avalible in server.js
;;; |   |                            | 
;;; │   ├── MozRepl.js               | boilerplate for adding an extension to mozilla
;;; |   |                            | 
;;; │   └── MozRepl.xpt              | binary of the .idl file
;;; ├── defaults                     | 
;;; │   └── preferences              |
;;; |       |                        | 
;;; │       └── mozrepl.js           | default preferences. TODO, the object `extensions.mozrepl.initUrl' is a thing here. to what javascript object is it attached? I cannot access it from ;the repl.
;;; |                                |
;;; ├── install.rdf                  |
;;; |                                |
;;; └── mozrepl@hyperstruct.net.xpi  | zip file firefox accepts as an extension. why the pathname .xpi type? because.

(in-package #:mm)

;; (defun patch-mozrepl ()
;;   "assumes the existence of ~/lisp/"
;;   (rp "cd ~/lisp && git clone https://github.com/bard/mozrepl.git")
;;   (let* ((install-file-string (with-open-file (stream "~/lisp/mozrepl/install.rdf" :direction :input)   
;; 				(slurp-stream stream)))
;; 	 (splice-location (search "<!-- toolkit -->" install-file-string))
;; 	 (string-part-one (subseq install-file-string 0 splice-location))
;; 	 (string-part-two (subseq install-file-string splice-location)))
;;     (with-open-file (stream "~/lisp/mozrepl/install.rdf" :direction :output :if-exists :supersede)
;;       (format stream 	      
;; 	      "~A~%%~%%<!-- Conkeror -->
;; 	      <em:targetApplication>
;; 	      <Description>
;; 	      <em:id>{a79fe89b-6662-4ff4-8e88-09950ad4dfde}</em:id>
;; 	      <em:minVersion>0.1</em:minVersion>
;; 	      <em:maxVersion>9.9</em:maxVersion>
;; 	      </Description>
;; 	      </em:targetApplication>~%%~%%~A"
;; 	      string-part-one
;; 	      string-part-two))
;;     ;; you must point to the extension with M-x extensions, click the wrench, and
;;     ;; instal "install add on from file" and point the the location of mozrepl
;;     ;; 
;;     ;; the extension is here on my machine ~/.mozilla/firefox/uzbd2tpd.default/extensions
;;     ;; can we put it elsewhere?
;;     ))

(defmacro newline-and-format (stream &rest strings)
  `(format ,stream ,(apply #'cat (butlast (interpose "~%" strings)))))

(defun write-conkerorrc-files ()
  "NOTE this must be run BEFORE Conkeror starts"
  (with-open-file (stream "~/.mozrepl-conkeror.js"
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (newline-and-format stream 
			"var conkeror = Cc[\"@conkeror.mozdev.org/application;1\"].getService().wrappedJSObject;"
			"this.enter(conkeror);"))
  (cl-fad:copy-file (ppath "/browser/default-conkerorrc.js") "~/.conkerorrc" :overwrite t))

(defun install-conkeror ()
  ;; XXX 2014-12-07T16:28:19+00:00 Gabriel Laddel
  ;; this is currently required to get our version of conkeor up and running. Be
  ;; forewarned it takes ages to install.
  (rp "emerge conkeror")
  (dolist (install-dir '("~/algol/xulrunner/" "~/algol/conkeror/"))
    (when (probe-file install-dir)
      (cl-fad:delete-directory-and-files install-dir)))
  (with-open-file (stream "/tmp/conkeror-install-log.lisp" 
			  :if-exists :append
			  :if-does-not-exist :create
			  :direction :output)
    (format stream "~S~%"
	    (labels ((shell-commands (strings) (apply #'cat (butlast (interpose " && " strings))))
		     (fn (&rest args) (apply (curry #'format nil) args))
		     (rp (program-string) (run-program program-string :output :string)))
	      (let* ((xulrunner-ftp-uri "http://ftp.mozilla.org/pub/mozilla.org/xulrunner/releases/33.1/runtimes/xulrunner-33.1.en-US.linux-x86_64.tar.bz2")
		     (xulrunner-tmp-location "/tmp/xulrunner-33.1.en-US.linux-x86_64.tar.bz2"))
		(list :start-time (get-universal-time)
		      :download-xulrunner (rp (fn "curl ~a > ~a" xulrunner-ftp-uri xulrunner-tmp-location))
		      :extract-xulrunner (rp (fn "mv ~a ~~/algol/ && cd ~~/algol/ && tar xvjf xulrunner-33.1.en-US.linux-x86_64.tar.bz2" xulrunner-tmp-location))
		      :clone-conkeror (rp (shell-commands
					   '("mkdir ~/algol/conkeror/"  
					     "cd ~/algol/conkeror/" 
					     "git init"
					     "git remote add origin git://repo.or.cz/conkeror.git"  
					     "git fetch origin 48d3ef4369f267faf42451a580b1ac6bcb6a5a18:refs/remotes/origin/master"
					     "git reset --hard FETCH_HEAD && make"))) ;; `make' is neccecary to edit conkeror text fields from Emacs.
		      :write-conkerorrc-files (write-conkerorrc-files)
		      :end-time (get-universal-time))))))
  (format t "~%~%Conkeror install finished~%~%"))

;;; XXX 2014-11-06T16:09:33-08:00 Gabriel Laddel
;;; for the time being, I've simply moved the .xpi from my ubuntu machine into
;;; the repository - either I'm doing something wrong, or `zip' has a bug in it
;;; that prevents conkeror from loading the extension.

;; (defun contents (dir-pathname)
;;   (loop for file in (ls dir-pathname)
;; 	nconcing (if (cl-fad:directory-pathname-p file) (contents file) (list file))))

;; (defun delete-emacs-backup-files (dir-pathname)
;;   "deletes all emacs backup from DIR-PATHNAME and its directories, recursively"
;;   (loop for dir-pathname in (labels () (contents dir-pathname))
;; 	when (let* ((name (pathname-name dir-pathname))
;; 		    (file-type (pathname-type dir-pathname)))
;; 	       (or (string= "#"  (subseq name 0 1))
;; 		   (string= ".#" (subseq name 0 2))			       
;; 		   (when file-type (string= "~" (subseq file-type  (1- (length file-type)))))
;; 		   (string= "~" (subseq name (1- (length name))))))
;; 	  do (delete-file dir-pathname)))

;; (defun zip-mozrepl ()
;;   (let* ((zip-name "~/mozrepl@hyperstruct.net.xpi"))
;;     (when (probe-file zip-name) (delete-file zip-name))
;;     (delete-emacs-backup-files "/root/quicklisp/local-projects/masamune-os/browser/mozrepl/")
;;     (zip:zip zip-name "/root/quicklisp/local-projects/masamune-os/browser/mozrepl/")))

;;; Browser Interface
;;; =============================================================================

(in-package #:mmb)

(defvar socket)

(defun start-ps-repl ()
  "currently prints javascript return values to `*standard-output*'"
  ;; TODO 2014-11-05T00:53:38-08:00 Gabriel Laddel
  ;; I am supremely unhappy with this. coroutines are needed, or return nil if
  ;; there is nothing to read.
  (setf socket (socket-connect "localhost" 4242 :protocol :stream
						:element-type 'character
						:timeout 5
						:nodelay t))
  (bt:make-thread (lambda () (loop with output-stream = MASAMUNE::*SWANK-CONNECTION-HACK*
			      while t			      
			      for line = (read-line (socket-stream socket))
			      do (progn (TERPRI output-stream)
					(princ line output-stream))))
		  :name "MozREPL print thread"))

(defmacro mps (form)
  (let* ((javascript-string (eval `(ps ,form))))
    (write-string javascript-string (socket-stream socket))
    (force-output (socket-stream socket))))

(defun open-uri (uri)
  (eval `(mps (load_url_in_new_buffer ,uri (new (interactive_context))))))

(defun open-url-in-current-buffer (url)
  (eval `(mps (load_url_in_current_buffer ,url (new (interactive_context))))))

(defun open-url-in-background (url)
  (eval `(mps (load_url_in_new_buffer_background ,url (new (interactive_context))))))

;;; Documentation scrape
;;; =============================================================================
;;; 
;;; the following links (and their sublinks) have all the information neccecary
;;; to provide fully documented web development. they need to be scraped.
;;;
;;; http://zenit.senecac.on.ca/wiki/index.php/XULRunner_Guide_Outline 
;;; https://developer.mozilla.org/en-US/docs/Components_object
;;; https://developer.mozilla.org/en-US/docs/Web
;;; https://developer.mozilla.org/en-US/docs/Web/Reference
;;; https://developer.mozilla.org/en-US/docs/WebAPI
;;; https://developer.mozilla.org/en-US/docs/Web/API
;;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference
;;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference#Value_properties
;;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar
;;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Methods_Index
;;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Index
;;; 
;;; I don't know that this is the correct think (it popped up via a 'repl.doc' call
;;; but the XUL interface is what mozilla uses to build their interfaces and there
;;; exists some documentation for it.
;;; 
;;; https://developer.mozilla.org/en-US/docs/Web/XUL/Reference

(in-package #:mm)

;; (defun hostname (url)
;;   "always ends in a forward slash"
;;   (let* ((protocol-end (nth 3 (all-matches "/" url)))
;; 	 (hostname (cat (take protocol-end url) (car (split "/" url :start protocol-end)))))
;;     (if (string= "/" (llast hostname)) hostname (cat hostname "/"))))

;; (defun hostname= (url1 url2)
;;   (string= (hostname url1) (hostname url2)))

;; (defun save-page-and-local-links (url dir-pathname &optional max-depth)
;;   "Saves the HTML, CSS and the pages linked in URL, given that they're on the
;; same website. MAX-DEPTH is to be used when downloading pages from say,
;; wikipedia. DIR-PATHNAME names the location said website is to be stored. URL
;; should be fully qualified"
;;   (assert (probe-file dir-pathname) (dir-pathname) "pathname ~s already exists!" dir-pathname)
;;   (labels ((f (s) (if (not (string= "/" (subseq s 0 1))) s (subseq s 1))))
;;     (let* ((hostname (hostname url)) (links) (current-url hostname))
;;       (hostname= hostname)
;;       (walk-tree (lambda (l) (and (listp l) (case (car l) 
;; 					 (:a (case (getf l :type)
;; 					       ;; the problem is that these need to be downloaded relative to the hostname
;; 					       ("text/css" (cat hostname (f (getf l :href))))
;; 					       ("text/html" (cat hostname (f (getf l :href))))
;; 					       (t (error "html :LINK :TYPE is not regoznired for ~s" l))))
;; 					 (:link (push links)))))
;; 		 (parse-html (http url))))))

;;; Adblock 2.6.5
;;; ============================================================================
;;; TODO 2014-11-30T07:23:02-08:00 Gabriel Laddel
;;; - automatically turn off automatic updates on adblock plus
;;; - automatically load "/home/francis/algol/conkeror/modules/extensions/adblockplus.js"
;;;
;;; the .xpi file for is included in the git repo because you can't download it
;;; through mozilla without using firefox. I'm too lazy to fake headers for this.


;;; lispkit
;;; ============================================================================

(defun build-lispkit ()
  (shell-command-in-dir '("git clone https://github.com/joachifm/cl-webkit"
			  "git clone https://github.com/crategus/cl-cffi-gtk.git"
			  "git clone https://github.com/AeroNotix/cl-xkeysym.git"
			  "git clone https://github.com/AeroNotix/lispkit.git")
			"~/quicklisp/local-projects/")
  (rp "emerge x11-libs/gdk-pixbuf")
  (rp "emerge x11-libs/cairo")
  (rp "emerge x11-libs/pango")
  (rp "emerge net-libs/webkit-gtk")
  (ql:quickload 'lispkit))
