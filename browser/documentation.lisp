;;; Documentation
;;; ============================================================================
;;;
;;; This file will eventually be deleted or turn into a full tutorial on how to
;;; make use of mozrepl. as for now it is a collection of useful tidbits/scrach.
;;; 
;;; mozrepl allows one to hop around contexts and evaluate arbitrary code in
;;; them. a 'context' is a webpage + the repl evaluation environment plus at 
;;; least one more environment for firefox itself.
;;;
;;; See the mozrepl wikipages on github - or just read the sources
;;; 
;;; The repl starts in the Conkeror window context. eg, evaluating
;;; 
;; tabbrowser.addTab();
;; conkeror is the master object
;; -cc
;; -ci
;; -cr

;; (html-parse:parse-html (slurp-file (second (ls "~/algol/conkeror/help/"))))

(mps (@ conkeror browser_object_top))

(add_hook "buffer_dom_content_loaded_hook"
	  (lambda () (when (smember "wiki" current-url)
		  (setf (@ (chain (@ (aref (@ (aref ((@ conkeror get-windows)) 0) 
					      buffers buffer_history) 0) document)
				  (get-element-by-id "content"))
			   style margin-left)
			0)
		  (loop for dom-id in '("mw-panel" "mw-page-base" "mw-head")
			do (mps (chain (@ (chain (@ (aref (@ (aref ((@ conkeror get-windows)) 0) buffers buffer_history) 0) document)
						 (get-element-by-id dom-id)) parent-node) 
				       (remove-child (chain (@ (aref (@ (aref ((@ conkeror get-windows)) 0) buffers buffer_history) 0) document)
							    (get-element-by-id dom-id))))))))
	  t t)

(mps (add_hook "buffer-dom_content_loaded_hook"
	       (lambda () (chain repl (print "dom content loaded")))
	       t t))

(mps (chain conkeror (add_hook "buffer-dom_content_loaded_hook"
			       (lambda () (chain repl (print "dom content loaded"))))))

;; (loop for b across (@ (aref (get-windows) 0) buffers buffer_history)
;;       collect (@ b display_uri_string))

;; remove these elements 

;; modify the style

;; buffer_dom_content_loaded_hook
;; buffer_loaded_hook

;; the master object, window gets a 0 through N field named thusly that
;; correspond to tabs. 

;; (mps (chain repl (inspect (chain (@ -cc "@mozilla.org/appshell/window-mediator;1")
;; 				      (get-service (@ -ci ns-i-window-mediator))
;; 				      (get-most-recent-window "navigator:browser")
;; 				      (get-browser)))))

;; (loop for b across (@ (aref (get-windows) 0) buffers buffer_history)
;;       collect (@ b display_uri_string))

;; (mps (chain repl (inspect (aref (@ (aref ((@ conkeror get-windows)) 0) buffers buffer_history) 0))))

;; from a wikipedia page


;; and we're golden!

;; (mps (chain (@ (aref (@ (aref ((@ conkeror get-windows)) 0) buffers buffer_history) 0)
;; 	       document)
;; 	    (get-element-by-id "mw-head")))

;;; /chrome/content/

;;; overlay.js

;; (chain gwindow (add-event-listener "load" (lambda (event) (chain psrepl (init-overlay))) false))

;; (ps-inline "var psrepl = {};")

;; (chain -components (@ classes "@mozilla.org/moz/jssubscript-loader;1")
;;        (get-service (@ -components interfaces moz-i-j-s-sub-script-loader))
;;        (load-sub-script "chrome://mozrepl/content/overlay_impl.js" psrepl))

;;; overlay_impl.js

;; (js* "const Ci = Components.interfaces;
;; const Cc = Components.classes;
;; const pref = Cc['@mozilla.org/preferences-service;1'].getService(Ci.nsIPrefService).getBranch('extensions.psrepl.');")

;; (defvar server)

;; (defun init-overlay ()
;;   (let* ((server (chain (@ -cc "@hyperstruct.net/mozlab/psrepl;1")
;; 			(get-service)
;; 			(-ci ns-i-psrepl))))))

;; (defun toggle-pref (pref-name)
;;   (chain pref (set-bool-pref pref-name (not (chain pref get-bool-pref (pref-name))))))

;; (defun toggle-server (source-command)
;;   (if (@ service is-active)
;;       (chain server (stop))
;;       (chain server (start (chain pref get-int-pref "port")
;; 			   (chain pref get-bool-pref "loopbackOnly")))))

;; (defun update-menu (xul-popup)
;;   (chain document (get-element-by-id "repl-command-toggle")
;; 	 (set-attribute "label" (if (chain server (is-active)) "Stop" "Start")))
;;   (chain document (get-element-by-id "repl-command-listen-external")
;; 	 (set-attribute "label" (if (chain server (is-active)) "Stop" "Start"))
;; 	 )
;;   (chain document (get-element-by-id "repl-command-listen-external")
;; 	 (set-attribute "label" (if (chain server (is-active)) "Stop" "Start"))
;; 	 )
;;   )

;; 

;; document.title = foobarbaz

;;; will set the X window title

;; document.documentElement.innerHTML

;;; contains the html/css/js XUL interface to Conkeror.
;;;
;;; one can get online help related to any particular element via 

;; repl.doc(elm)
;; repl.doc(document.getElementById('minibuffer')))

;;; will display the type and nodename of the object and open online
;;; documentation if applicable. TODO this currently opens in a new conkeror
;;; window instead a new tab.

;; repl.enter(place) ;; will put you into a new context.
;; repl.whereAmI() ;; current context

;;; TODO apparent for Firefox 3 you'll need: repl.enter(content.wrappedJSObject)
;;; what version is conkeror running

;; repl.back() ; to return to the previous context.
;; repl.home() ; to return to the context in which the repl was started. by default, the Conkeror window.

;; var scratch = {} ; create your own context
;; repl.enter(scratch)

;; repl.enter(repl) ;;; the repl itself is a context

;;; The key functions for finding your way around

;; repl.load("file:///home/francis/quicklisp/local-projects/masamune/browser/custom-commands.js") 
;; load will allow you to specify any 
;; 
;; repl.look
;; repl.inspect
;; repl.doc
;; repl.search

;;; some other misc functions that may or may not be useful

;; repl.setenv
;; repl.getenv
;; repl.pushenv
;; repl.popenv
;; repl.print
;; repl.quit()

;;; Custom Interactors
;;; ============================================================================

;;; By default, MozRepl listens for JavaScript code, evaluates it, and prints the result of the evaluation.
;;; 
;;; An experimental feature allows you to replace this scheme, listen for any kind of input and return results in any format you want, while still operating inside the repl object and the XUL application.
;;; 
;;; As an example, try the following toy object inspector.
;;; 
;;; Create a file (e.g. /home/user/.mozrepl.js), copy and paste the content below, and set extensions.mozrepl.initUrl to the file URI (file:///home/user/.mozrepl.js).
;;; 
;; example of a custom interactor
;; 
;; 
;; defineInteractor('screenshot', {
;;     handleInput: function(repl, input) {
;;         // Given an HTTP _request_, return an array containing the verb,
;;         // the path, and protocol version, e.g. ['GET', '/foo/bar', 'HTTP/1.1']
;;         //
;;         // Careful, the implementation is as naive as it can get!

;;         function parseHTTPRequest(request) {
;;             return request
;;                 .split(/[\r\n]+/)[0] // get first line
;;                 .split(/\s+/); // split it
;;         }

;;         // Strip leading and trailing whitespace
;;         var input = input.replace(/^\s+|\s+$/g, '');

;;         var [verb, path, protocolVersion] = parseHTTPRequest(input);
;;         if(verb != 'GET')
;;             return;

;;         var browserWindow = Cc['@mozilla.org/appshell/window-mediator;1']
;;             .getService(Ci.nsIWindowMediator)
;;             .getMostRecentWindow('navigator:browser');
;;         var tabbrowser = browserWindow.getBrowser();

;;         var canvas = browserWindow.document.createElementNS('http://www.w3.org/1999/xhtml', 'canvas');
;;         var tab = tabbrowser.addTab();
;;         var browser = tabbrowser.getBrowserForTab(tab); // tab.linkedBrowser ?

;;         browser.addEventListener('load', function() {
;;             var win = browser.contentWindow;
;;             var width = win.document.width;
;;             var height = win.document.height;
;;             canvas.width = width;
;;             canvas.height = height;
;;             var ctx = canvas.getContext('2d');
;;             ctx.clearRect(0, 0, canvas.width, canvas.height);
;;             ctx.save();
;;             ctx.scale(1.0, 1.0);
;;             ctx.drawWindow(win, 0, 0, width, height, 'rgb(255,255,255)');
;;             ctx.restore();

;;             repl.onOutput('HTTP/1.1 200 OK\r\n' +
;;                           'Content-Type: image/png\r\n' +
;;                           '\r\n' +
;;                           atob(canvas
;;                                .toDataURL('image/png', '')
;;                                .split(',')[1]))

;;             tabbrowser.removeTab(tab);
;;             repl.quit();
;;         }, true);

;;         var url = decodeURIComponent(path.match(/\/screenshot\/(.*$)/)[1]); // "/screenshot/http://www.google.com" -> "http://www.google.com"
;;         browser.loadURI(url);
;;     },

;;     onStart: function(repl) {},

;;     onStop: function(repl) {},

;;     getPrompt: function(repl) {},
;; });

;;; Troubleshooting
;;; ============================================================================
(async-shell-command "lsof | grep 4242") ;;; or inspect process output (probably in and emacs *Async Shell Command<n>* buffer)

(h "introduction"
   (h "tips"
      (h "auto-re-entering"
	 "When the browser reloads the page, the repl will return to its creation context since the page it’s working on disappears. If you want it to automatically re-enter the page, you can redefine the home function:"
	 (javascsript "repl.home = function() { return this.enter(this._creationContext.content); }")	 
	 "When loading webpages, the repl can return before the page finishes loading. You can use the document.readyState javascript command to ensure the page has finished loading and rendering before performing other actions such as extracting the html source code
readyState"
	 (javascript "repl> content.location.href = 'https://maps.google.com'
repl> repl.enter(content)
repl> document.readyState
\"loading\"
repl> document.readyState # wait a little while then call document.readyState again
\"loading\"
repl> document.readyState # wait a little while then call document.readyState again
\"interactive\"
repl> document.readyState # wait a little while then call document.readyState again
\"complete\"
repl> document.querySelector('html').outerHTML # now fetch the html of the rendered page
\"<html><head>...truncated here</head></html>\"
repl> window.$('html').html() # since we called repl.enter(content) above, global objects defined in the webpage are available inside the repl
repl> $('html').html() # window prefix is not needed
repl> console.log('this came from the repl') # log to the browsers console. Open up the develop tools first and go to the console tab to this output"))))



