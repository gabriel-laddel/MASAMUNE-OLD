;;; Interface to the Conkeror web browser.
;;; =============================================================================
;;; 
;;; This file serves the following purposes:
;;; 
;;; 1. Downloading binary 33.1 of XULrunner and using it to launch Conkeror
;;;    commit 48d3ef4369f267faf42451a580b1ac6bcb6a5a18, master branch of the
;;;    repository git://repo.or.cz/conkeror.git
;;; 
;;; 2. Building Mozrepl from the sources in .../masamune/browser/mozrepl/.
;;; 
;;; 3. Providing abstractions for interfacing with Conkeror through CL.
;;;
;;; 4. Providing documentation on the proceeding processes.
;;; 
;;; I have a limited amount of patience for parsing semantics from
;;; ~ALGOL. Ideally we would like to have a parenscsript interface to Conkeror,
;;; accompanying documentation, inspection of js objects in a SLIME-inspector
;;; style interface etc.
;;;
;;; It is highly reccomended you read this comment in its entirety before
;;; modifying this code. Mozilla's extension documentation is shit and should
;;; be used as a last resort.
;;;
;;; Under no circumstances should the XULrunner or Conkeror be blindly updated
;;; to HEAD. The sources are included in the distribution to enforce a code
;;; freeze. If, for some reason it is necessary to patch them, patch the inclued
;;; sources directly. At a glance, I see many things that could simply be thrown
;;; out, eg the android nonsense, anything neccecary for the java / python
;;; bindings. 
;;;
;;; parenscript has some issues - namely that it isn't common lisp and there
;;; isn't any way to get away from the underlying JS model. There are two ways
;;; of dealing with this - the first, try and be common lisp. it would be
;;; awfully nice if someone were to put in the work to make it so, but I doubt
;;; it'll happen. the other is to modify the parenscript compiler until it is
;;; but a thin (arc?) veneer over js.
;;;
;;; I lookead into using lispkit instead of Conkeror as the basis for the browser
;;; but it wasn't trivial to build.
;;; 
;;; Glossary
;;; =============================================================================
;;; 
;;; Mozilla et. all take great pleasure in renaming concepts without adding any
;;; value, giving rise to deplorable terms such as "add-ons". Reading MozREPL
;;; sources or the Mozilla documentation requires you're aware of their jargon.
;;;
;;; Interactor - MozREPL's REPL procedure.
;;;              
;;; Commands   - MozREPL declares that anything added to it's default interface 
;;;              is a 'command'
;;;
;;; Component  - ?
;;; 
;;; Overlay    - ?
;;; 
;;; XULrunner  - C/C++ framework used for building Mozilla applications, eg
;;;              firefox and the thunderbird mail client. Conkeror uses this as
;;;              it's basis. You can access much of the underlying functionality 
;;;              via javascript.
;;;             
;;; Context    - the current js environment, eg a webpage, the repl (extension)
;;;              environment etc.
;;;                   
;;; MozRepl Pathnames
;;; =============================================================================
;;;
;;; ./mozrepl
;;; |
;;; |-- chrome                       
;;; |       |-- overlay_browser.xul  | XML detailing a MozRepl 'overlay' menu
;;; |       |                        | 
;;; |       |-- overlay_impl.js      | useful functions
;;; |       |                        | 
;;; |       |-- overlay.js           | event listener to init 'overlay'
;;; |       |                        | 
;;; |       |-- repl.js              | 
;;; |       |-- server.js            | 
;;; |       |-- ui.js                | 2fns constructor(server),toggleServer(sourceCommand)--
;;; |       |                        | 
;;; |       |-- util.js              | 3fns helpUrlFor(thing),docFor(thing),--argList(fn)--
;;; |                                | 
;;; |-- chrome.manifest              | boilerplace k/v pairs specifying details about the extension
;;; |                                | 
;;; |-- components                   |
;;; |   |                            | 
;;; |   |-- CommandLine.js           | details the command line interface to mozrepl TODO, is this used by comint? apparently, one can used the command line options here to specify stuff ;line "/usr/bin/firefox -profile /path/to/profile/folder -repl -repl 7070" source: https://github.com/bard/mozrepl/wiki/Starting-the-REPL
;;; |   |                            | 
;;; |   |-- Makefile                 | generates the .xpt from the .idl and specifies the XULrunner binaries to do this
;;; |   |                            | 
;;; |   |-- MozRepl.idl              | .idl is an ALGOL "language-independent" (lol) interface description. this file describes a handful of functions avalible in server.js
;;; |   |                            | 
;;; |   |-- MozRepl.js               | boilerplate for adding an extension to mozilla
;;; |   |                            | 
;;; |   |-- MozRepl.xpt              | binary of the .idl file
;;; |                                |
;;; |-- defaults                     |
;;; |   |                            | 
;;; |   |-- preferences              |
;;; |       |                        | 
;;; |       |-- mozrepl.js           | default preferences. TODO, the object `extensions.mozrepl.initUrl' is a thing here. to what javascript object is it attached? I cannot access it from the repl.
;;; |                                |
;;; |-- install.rdf                  |
;;; |                                |
;;; |-- mozrepl@hyperstruct.net.xpi  | zip file firefox accepts as an extension. why the pathname type .xpi? because.
;;; 
;;; TODO
;;; =============================================================================
;;;
;;; - function.doc = ps repl pattern
;;;
;;; - download conkeror documentation, strip CSS
;;;
;;; - conkeror has some notion of 'chrome contexts'. What are they?
;;;
;;; - unify docstrings across conkeror and included parenscript (via monkeypatch)
;;;
;;; - ensure parenscript documentation, is downloaded, built and cross
;;;   referenced with javascript documentation.
;;;
;;; - add color themes & modes from Dan as defaults
;;;    - http://conkeror.org/PageModes
;;;    - about:config
;;; 
;;; - load new html/javascript/css onto any webpage via mozrepl. make a general 
;;;   tookit for utilizing online content. see:
;;;   https://github.com/jlbfalcao/mozrepl_tools
;;; 
;;; - install the mozrepl extension programatically (requires image processing)
;;;
;;; - install modi by default http://slayeroffice.com/tools/modi/v2.0/modi_help.html
;;;
;;; "http://slayeroffice.com/tools/modi/v2.0/modi_v2.0js"
;;;
;;; "javascript:prefFile= \";void(z=document.body.appendChild(document.createElement('script'')));void(z.language='javascript');void(z.type='text/javascript');void(z.src='...');void(z.io='modi')
;;; "
;;; http://slayeroffice.com/tools/modi/v2.0/
;;; 
;;; - what does firebug lite offer, and should it be included by default?
;;; 
;;; - stripped (of css) copies of the documentation for the conkeror / XULrunner
;;;   distributed with masamune.
;;;
;;; - fix the edit text fields from Emacs nonsense which randomly fails
;;;
;;; - detailed errors for parenscript
;;;
;;; - resource://gre/modules/Preferences.jsm conkeror allows you to access the JS
;;;   that runs it, but doesn't auto-complete on it - checking the source for a
;;;   particular version should allow me to build something similar, at least, a
;;;   table with the each value. this table and all other should be indexed and
;;;   autocompletion made avalible.
;;;   
;;; Documentation scrape
;;; =============================================================================
;;; 
;;; the following links (and their sublinks) have all the information neccecary
;;; to provide fully documented web development. they need to be scraped and
;;; organized into something like the CL HyperSpec.
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
;;; repl.doc will redirect you to these pages if possible.

(in-package #:mm)

;;; Browser Interface
;;; =============================================================================

(in-package #:mmb)

(defun ps-upcase (symbol)
  (mm::-> (mm::interleave (mm::repeat #\- (length (symbol-name symbol))) 
			  (coerce (symbol-name symbol) 'list))
	  (coerce 'string)
	  (intern)))

(defvar socket)

(defun start-ps-repl ()
  "currently prints javascript return values to `*standard-output*'"
  (let* ((get-windows-string "function getWindows() {
    var windowEnum = Cc['@mozilla.org/appshell/window-mediator;1']
        .getService(Ci.nsIWindowMediator).getEnumerator('');
    var windows = [];
    while(windowEnum.hasMoreElements())
        windows.push(windowEnum.getNext());

    return windows;
}"))
    (if (mm::port-in-use-p 4242)
	(handler-case (progn (setf socket (socket-connect "localhost" 4242 :protocol :stream
									   :element-type 'character
									   :timeout 5
									   :nodelay t))
			     (bt:make-thread (lambda () (loop with output-stream = MASAMUNE::*SWANK-CONNECTION-HACK*
							 while t			      
							 for line = (read-line (socket-stream socket))
							 do (progn (TERPRI output-stream)
								   (princ line output-stream))))
					     :name "MozREPL print thread")
			     (format (socket-stream socket) get-windows-string))
	  (error nil (stumpwm::run-with-timer 1 nil 'start-ps-repl)))
	(stumpwm::run-with-timer 1 nil 'start-ps-repl))))

(defmacro mps (form)
  (let* ((javascript-string (eval `(ps ,form))))
    (write-string javascript-string (socket-stream socket))
    (force-output (socket-stream socket))))

(defun open-uri (uri &optional focus-browser)
  (eval `(mps (load_url_in_new_buffer ,uri (new (interactive_context)))))
  (when focus-browser (stumpwm::select-browser)))

(defun open-url-in-current-buffer (url &optional focus-browser)
  (eval `(mps (load_url_in_current_buffer ,url (new (interactive_context)))))
  (when focus-browser (stumpwm::select-browser)))

(defun open-url-in-background (url &optional focus-browser)
  (eval `(mps (load_url_in_new_buffer_background ,url (new (interactive_context)))))
  (when focus-browser (stumpwm::select-browser)))

(defun ps-repl ()
  "\"quit\" quits"
  (format t "~%~%parenscript> ")
  (loop while t
	do (let* ((input (read *standard-input*)))
	     (if (string= "quit" input)
		 (return-from ps-repl)
		 (eval `(format t "~S" (ps ,input))))
	     (format t "~%~%parenscript> "))))

(defun ps-compile-to-file (pathname &optional (output-pathname (mm::alter-pathname-type pathname "js")))
  (assert (string= "paren" (pathname-type pathname)))
  (with-open-file (stream output-pathname 
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (princ (ps-compile-file pathname) stream)))

(in-package #:mm)

;;; Build REPL conkeror extension
;;; ============================================================================

(defun build-repl ()
  (let* ((dir (mm::qlpp "/masamune/browser/repl/"))
	 (ps-to-ignore '(#P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/util.paren"
			 #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/server.paren"
			 #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/repl.paren")))
    (labels ((install-dot-rdf ()
	       (with-open-file (stream (qlpp "/masamune/browser/repl/install.rdf")
				       :direction :output
				       :if-exists :supersede)
		 (format stream "<?xml version=\"1.0\"?>~%~%")
		 (xmls:write-xml '("RDF" (("xmlns" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
					  ("xmlns:em" "http://www.mozilla.org/2004/em-rdf#"))
				   ("Description" (("about" "urn:mozilla:install-manifest"))
				    (("em:id") nil "repl@l33t.net")
				    (("em:version") nil 1337)
				    (("em:type") nil 2)
				    (("em:targetApplication") nil
				     (("Description") nil
				      (("em:id") nil "{a79fe89b-6662-4ff4-8e88-09950ad4dfde}")
				      (("em:minVersion") nil 0.1)
				      (("em:maxVersion") nil 9.9)))
				    (("em:targetApplication") nil
				     (("Description") nil
				      (("em:id") nil "toolkit@mozilla.org")
				      (("em:minVersion") nil 1.9)
				      (("em:maxVersion") nil 20.*)))
				    (("em:name") nil "repl")
				    (("em:description") nil "Parenscript repl")
				    (("em:creator") nil "l33t Pete")
				    (("em:homepageUrl") nil "http://log.bitcoin-assets.com")))
				 stream
				 :indent t)))
	     (zip-repl ()
	       (let* ((other-required-files '("chrome/"
					      "chrome/content/"
					      "chrome/content/moz.el"
					      "chrome/content/overlay_browser.xul"
					      "chrome.manifest"
					      "components/"
					      "components/Makefile"
					      "components/MozRepl.xpt"
					      "components/MozRepl.idl"
					      "defaults/"
					      "defaults/preferences/"
					      "install.rdf"
					      "logo.png")))
		 (rp-in-dir (format nil "zip ~a -xi ~{~a ~}"
				    ;; NOTE 2015-01-27T09:06:20+00:00 Gabriel Laddel
				    ;; why .xpi extension? nfi, just the way it's done.
				    "repl@l33t.net.xpi"
				    (append other-required-files (mm::recursive-contents-of-type dir "js")))
			    dir))))

      (dolist (p (remove-if (lambda (p) (member p ps-to-ignore :test 'equal))
			    (recursive-contents-of-type dir "paren")))
	(mmb::ps-compile-to-file p))
      (install-dot-rdf)
      (zip-repl))))
