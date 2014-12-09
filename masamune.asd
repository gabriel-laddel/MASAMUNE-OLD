(asdf:defsystem #:masamune
  :serial t
  :license "this information is provided \"as is\" without warranty of any kind, express or implied, including, but not limited to, the implied warranties of merchantability, fitness for a particular purpose, and non-infringement. in no event will the author be liable for direct, indirect, special, incidental, or consequential damages resulting from any inaccuracy or error in this document, even if advised of the possibility of such damages."
  :depends-on (#:alexandria
	       #:archive
	       #:arnesi
	       #:anaphora
	       #:asdf
	       #:bordeaux-threads
	       #:cl-arrows
	       #:cl-css
	       #:cl-csv
	       #:cl-html-parse
	       #:cl-json
	       #:cl-ppcre
	       #:cl-who
	       #:chipz
	       #:closer-mop
	       #:clx
	       #:clx-cursor
	       #:do-urlencode
	       #:drakma
	       #:fset
	       #:generic-sequences
	       #:iterate
	       #:let-plus
	       #:local-time
	       #:mcclim
	       #:mcclim-png-bitmaps
	       #:mcclim-gif-bitmaps
	       #:mcclim-jpeg-bitmaps
	       #:mcclim-tiff-bitmaps
	       #:climacs
	       #:lparallel
	       #:opticl
	       #:optima
	       #:parenscript
	       #:stumpwm
	       #:stefil
	       #:uuid
	       #:vecto
	       #:zpng)
  :components ((:file "packages")
	       (:file "init")
	       (:file "util")
	       (:file "monkey-patches")
	       (:file "masamune")
	       (:file "masamune-gui")
	       (:file "dashboard")
	       (:file "repository")
	       (:file "kmap")
	       (:file "default-data")
	       (:file "systems/captains-log")
	       (:file "systems/mathematics-practice")
	       (:file "systems/programming-practice")
	       (:file "systems/summarize-logs")
	       (:file "browser/conkeror")
	       (:file "finalize")))
