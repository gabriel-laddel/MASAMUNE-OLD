(in-package #:mm)

(defmacro m (name arguments &rest body)
  `(defmacro ,name ,arguments ,@body))

(defmacro defalias (new old) `(defun ,new (&rest args) (apply #',old args)))

(defalias i make-instance)
(defalias l list)
(defalias ^ expt)

(defvar *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun scans (regex target-string)
  (let*  ((matches) (current-start-pos 0))
    (awhile (multiple-value-bind (match-start match-end)
		(scan regex target-string :start current-start-pos)
	      (when match-start
		(push (list match-start match-end) matches)
		(setq current-start-pos match-end))))
    (nreverse matches)))

(defun convert-image-file (file-pathname new-format)
  "TODO convert from any image format to another. currently uses imagemagicks's
convert script. new-format can be specifed as a string or keyword due to the
semantics of `format'"
  (uiop:run-program (format nil "convert ~a ~a~a.~a"
			    (namestring file-pathname)
			    (cat "/" (reduce (lambda (s1 s2) (cat s1  "/" s2)) (rest (pathname-directory file-pathname))) "/")
			    (pathname-name file-pathname) 
			    new-format) :output :string))

(defun squash (operator &rest ls)
  (assert (every (lambda (l) (= (length l) (length (car ls)))) ls))
  (iter (for i from 0 to (1- (length (car ls))))
    (collect (apply operator (mapcar (lambda (l) (nth i l)) ls)))))

(defun slurp-stream (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun slurp-file (pathname)
  (with-open-file (stream pathname :direction :input)
    (slurp-stream stream)))

(defun read-file (pathname)
  "File contents as a list of sexps"
  (with-open-file (stream pathname :direction :input)
    (let* ((out))
      (awhile (read stream nil nil)
	(push arnesi::it out))
      (nreverse out))))

(defun write-to-file (filename object &optional (if-exists :append))
  (with-open-file (s filename :direction :output :if-does-not-exist :create :if-exists if-exists)
    (write object :stream s)))

(defun cat (&rest objs)
  (apply #'concatenate 'string
	 (mapcar (lambda (o) (if (equal 'string (type-of o)) (write-to-string o) o)) objs)))

(defun llast (l)
  (etypecase l
    (string (subseq l (1- (length l))))
    (list (car (last l)))))

(defun thread-alive? (thread)
  (when (bt:thread-alive-p thread) thread))

(defun thread-by-name (name)
  "case insensitive"
  (car (remove-if-not (lambda (thread) (equal (string-downcase name)
					 (string-downcase (bt:thread-name thread))))
		      (bt:all-threads))))

(defun threads () (bt:all-threads))

(defun take (n l)
  (subseq l 0 (if (< (length l) n) (if (> n (1- (length l))) (length l) (1- (length l))) n)))

(defun drop (n l)
  (unless (> n (length l)) (subseq l n (length l))))

(defun interleave (l1 l2)
  (loop for i in l1 for j in l2 append (list i j)))

(defun interpose (o l)
  (loop for i in l append (list i o)))

(defun frotate (sequence &optional (n 1))
  "Functional rotate, returns a new sequence. Alexandira sucks and does destructive updates."
  (loop repeat (1+ n) for l = sequence then (append (cdr l) (list(car l))) finally (return l)))

(defun keys (plist)
  (assert (evenp (length plist)))
  (iter (for e in plist)
    (for k initially 0 then (if (= 0 k) 1 0))
    (when (= 0 k) (collect e))))

(defun walk-tree (fun tree)
  (subst-if t (constantly nil) tree :key fun))

(defun ls-clean (pathname)
  "ls, with emacs file recovery files, buffers removed"
  (remove-if (lambda (file) 
	       (let* ((name (pathname-name file))
		      (file-type (pathname-type file)))
		 (or (when name
		       (and (< 1 (length name)) 
			    (or (string= "~" (subseq name (1- (length name))))
				(string= "#"  (subseq name 0 1))
				(string= ".#" (subseq name 0 2)))))
		     (when file-type
		       (string= "~" (subseq file-type  (1- (length file-type))))))))
	     (ls pathname)))

(defun download-url (http-resource pathname)
  (with-open-file (file pathname
			:direction :output
			:if-does-not-exist :create
			:if-exists :supersede
			:element-type '(unsigned-byte 8))
    (let ((input (drakma:http-request http-resource
				      :want-stream t)))
      (awhile (read-byte input nil nil)
	(write-byte arnesi:it file))
      (close input))))

(defun extract-tarball (pathname)
  "Extract a tarball (.tar.gz) file to a directory (*default-pathname-defaults*). from http://8arrow.org/"
  (with-open-file (tarball-stream pathname
				  :direction :input
				  :element-type '(unsigned-byte 8))
    (archive::extract-files-from-archive
     (archive:open-archive 'archive:tar-archive
			   (chipz:make-decompressing-stream 'chipz:gzip tarball-stream)
			   :direction :input))))

(defmacro with-getfs (getfs plist &rest body)
  `(let* ,(loop for getf in getfs 
		collect (list (intern (symbol-name getf))
			      (getf plist getf)))
     ,@body))

(defun object-slots-plist (object)
  (loop with slot-names = (mapcar #'closer-mop::slot-definition-name
				  (closer-mop:class-slots
				   (find-class (type-of object))))
	for slot-name in slot-names
	appending (list (make-keyword slot-name) (slot-value object slot-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JSON, XML, HTML parsers and generators

;;; TODO 2014-09-09T13:34:28-07:00 Gabriel Laddel
;;; - find or make a json parser that returns alists instead of plists.
;;;   see `alist-plist' and `plist-alist'.
(defalias parse-json json:decode-json-from-string)
;;; `parse-xml'
(defalias http drakma:http-request)
(defalias regex-matches cl-ppcre:all-matches-as-strings)
(defalias filter remove-if-not)
(defalias ls cl-fad:list-directory)
(defalias distinct remove-duplicates)
(defmacro html-string (sexp)
   (let* ((out-var (gensym)))
     `(cl-who:with-html-output-to-string (,out-var)
	,sexp
	,out-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add and remove monitors while working with stumpwm

(defun monitors ()
  (let* ((shell-string (run-program "xrandr -q" :output :string)))
    (mapcar (lambda (x) (car (split " " x)))
	    (filter (curry #'scan "[^dis]connected")
		    (split "\\n" shell-string)))))

(defun activate-monitor (monitor-name position)
  "position is one of :up :down :left :right"
  (rp (format nil "xrandr --auto --output ~a --mode 1920x1080 ~a LVDS-1"
	   monitor-name
	   (case position
	     (:up "--above")
	     (:down "--below")
	     (:left "--left-of")
	     (:right "--right-of")
	     (:mirror "--same-as")))))

(defun deactivate-monitor (monitor-name)
  (run-program (format nil "xrandr --output ~a --off" monitor-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; screenshots

(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
	  (,screen (first (xlib:display-roots ,display)))
	  (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defvar screen-width
  (with-display "" (display xlib::screen _) (xlib:screen-width xlib::screen)))
(defvar screen-height
  (with-display "" (display xlib::screen _) (xlib:screen-height xlib::screen)))

(defun take-screenshot (&key (width screen-width) (height screen-height) (host ""))
  (assert (and (<= width screen-width) (<= height screen-height)))
  (with-display host (display screen root-window)
    (xlib:get-image root-window :x 0 :y 0 
				:width width
				:height height
				:result-type 'xlib::image-x)))

(defun save-screenshot-as-png (screenshot file)
  (let+ ((height   (xlib::image-x-height screenshot))
	 (width    (xlib::image-x-width screenshot))
	 (png      (make-instance 'zpng:pixel-streamed-png
				  :color-type :truecolor-alpha
				  :width width
				  :height height))
	 (pixarray (xlib::image-x-data screenshot)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (if (xlib::image-x-bit-lsb-first-p screenshot)
	  ;; lsb first 
	  (do ((i 0 (+ 4 i)))
	      ((>= i (length pixarray)))
	    (zpng:write-pixel (list (aref pixarray (+ 2 i))
				    (aref pixarray (+ 1 i))
				    (aref pixarray i)
				    #xFF)
			      png))
	  ;; msb first
	  (do ((i 0 (+ 4 i)))
	      ((>= i (* height width 4)))
	    (zpng:write-pixel (list (aref pixarray (1+ i))
				    (aref pixarray (+ 2 i))
				    (aref pixarray (+ 3 i))
				    #xFF)
			      png)))
      (zpng:finish-png png))))

(defun screenshot-window-to-png-file
    (drawable file &key (height (xlib:drawable-height drawable))
		     (width (xlib:drawable-width drawable)))
  "usage (screenshot-window (stumpwm::window-x-win (stumpwm::current-window)))"
  (let+ ((png (make-instance 'zpng:pixel-streamed-png
			     :color-type :truecolor-alpha
			     :width width
			     :height height))
	 (pixarray (xlib:get-raw-image drawable :x 0 :y 0 :width width :height height
						:format :Z-PIXMAP)))
    
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (case (xlib:display-byte-order (xlib:drawable-display drawable))
	(:lsbfirst
	 (do ((i 0 (+ 4 i)))
	     ((>= i (length pixarray)))
	   (zpng:write-pixel (list (aref pixarray (+ 2 i))
				   (aref pixarray (+ 1 i))
				   (aref pixarray i)
				   #xFF)
			     png)))
	(:msbfirst 
	 (do ((i 0 (+ 4 i)))
	     ((>= i (* height width 4)))
	   (zpng:write-pixel (list (aref pixarray (1+ i))
				   (aref pixarray (+ 2 i))
				   (aref pixarray (+ 3 i))
				   #xFF)
			     png))))
      (zpng:finish-png png))))

(defun cursor-coordinates ()
  "(x y) in X windows coordinates"
  ;; TODO, Sat Mar 08 2014, Francis Wolke
  ;; This can be done using `xlib'! 
  (let* ((a (uiop:run-program "xdotool getmouselocation" :output :string))
         (b (ppcre:split " " a))
         (number? (lambda (x) (car (member x '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :test #'equal))))
         (c (mapcar (lambda (s) (loop for c across s collect (funcall number? c))) b))
         (d (mapcar (lambda (l) (mapcar #'string (remove-if-not #'characterp l))) c))
         (e (subseq d 0 2))
         (x (apply 'concatenate 'string (first e)))
         (y (apply 'concatenate 'string (second e))))
    (list (parse-integer x) (parse-integer y))))

(in-package #:stumpwm)

(defcommand screenshot () ()
  "Takes a screenshot"
  (mm::save-screenshot-as-png
   (mm::take-screenshot) 
   (format nil "~~/Pictures/screenshots/screenshot-~d.png" (get-universal-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wikipedia

(in-package #:mm)

(defvar wikipedia-search-base-url
  "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=")
(defvar wikipedia-article-base-url
  "http://en.wikipedia.org/wiki/")

(defun search-wikipedia-article-titles (search-string)
  "Tree of wikipedia pages associated with string"
  ;; TODO 2014-08-25T00:22:47-07:00 Gabriel Laddel
  ;; some concepts are actually going to be anchors
  (multiple-value-bind (body)
      (drakma:http-request
       (cat wikipedia-search-base-url
	    (regex-replace-all " " search-string "_")))
    (apply #'append (filter #'listp (json:decode-json-from-string (flexi-streams:octets-to-string body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; google

(defun google (query &optional (number-of-pages 1))
  (iter (with encoded-query = (drakma:url-encode query :latin1))
    (with out = ())
    (for i from 1 to (* 10 number-of-pages) by 10)
    (for page-number = (if (= 1 i) "" (cat "&start="  (write-to-string i))))
    (for query = (cat "http://www.google.com/search?q=" encoded-query page-number))
    (for page = (html-parse:parse-html (drakma:http-request query)))
    (walk-tree (lambda (l) (when (and (equal 'cons (type-of l))
				 (equal '(:a :href) (mm:take 2 l))
				 (equal "/u" (mm:take 2 (nth 2 l))))
			(push (subseq (nth 2 l) 7 (position #\& l)) out)))		     
	       page)
    (finally (return (nreverse (mapcar (lambda (l) (subseq l 0 (position #\& l))) out))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc

(defun browse-url (url)
  (stumpwm::message "implement browse-url"))

(defun class-slot-names (class-or-instance)
  (let* ((class (if (equal 'standard-class (type-of class-or-instance))
		    class-or-instance
		    (find-class (type-of class-or-instance)))))
    (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class))))

(defun url-encode (s) (drakma:url-encode s :latin1))

(defun definitions (english-word)
  (let+ ((req-url (cat "http://dictionary.reference.com/browse/" (url-encode english-word) "?s=t"))
	 (page (drakma:http-request req-url))
	 defns skipped definitions)
    (handler-case
	(if (scan "def-content" page)
	    (progn (walk-tree (lambda (l) (when (and (listp l) (equal '(:div :class "def-content") (car l)))
				       (push (cadr l) defns))) (html-parse:parse-html page))
		   (push english-word definitions)
		   (push defns definitions))
	    (push english-word skipped))
      ('error () (push english-word skipped)))
    (values skipped definitions)))

(defun save-masamune-state (l)
  (with-open-file (stream "~/.masamune/state.lisp" :direction :output
						   :if-exists :append
						   :if-does-not-exist :create)
    (write (append (list :time (get-universal-time)
			 :focus (stumpwm::window-name (stumpwm::current-window))) l)  :stream stream)))

(defun ppath (string)
  "[p]roject [path]name"
  (format nil "~~/quicklisp/local-projects/masamune/~a"
	  (if (string= "/" (subseq string 0 1)) (subseq string 1) string)))

(defun qlpp (&optional string)
  "[q]uicklisp [l]ocal-[p]rojects [p]athname"
  (if string (format nil "~~/quicklisp/local-projects/~a"
		     (if (string= "/" (subseq string 0 1)) (subseq string 1) string))
      "~/quicklisp/local-projects"))

(defun format-escape (control-string) (regex-replace-all "~" control-string "~~"))

(defun object-plist ()
  "maps slots to keywords, recursively. Regenerating the object tree is then a
matter of walking the plist from the bottom upwards and supplying the correct
classname as a argument to `make-instance' applying across the plist for
initargs")

(defun rp (program-string &optional (output-stream :string))
  "shorthand, returns shell program output as string"
  (run-program program-string :output output-stream))

(defun shell-commands-in-dir (commands dir &optional (output-stream :string))
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command) output-stream)))

(defalias rp-in-dir shell-commands-in-dir)
(defalias cm compose)

(defun recursive-contents (pathname)
  (loop for p in (ls pathname)
	collect (if (cl-fad:directory-pathname-p p)  
		    (recursive-contents p)
		    (list p)) into out
	finally (return (remove-if #'null (apply #'append out)))))

(defun start-conkeror () 
  (stumpwm::run-shell-command "~/algol/xulrunner/xulrunner ~/algol/conkeror/application.ini" nil)
  ;; (stumpwm::run-with-timer 1 nil (lambda () (loop for w in (stumpwm::all-windows)
  ;; 					     when (search "Download failed" (stumpwm::window-name w) :test #'string=)
  ;; 					     do (stumpwm::kill-window w))))
  )

(defun open-ports ()
  "I don't know if this implementation is correct, I'm following the 3rd answer down here
http://stackoverflow.com/questions/9609130/quick-way-to-find-if-a-port-is-open-on-linux

I'm currently guessing that /proc/net/tpc6 corresponds to ipv6 and parsing that too.

NOTE: it sometimes happens that a port # occurs twice in the output. why?"
  ;; NOTE 2014-12-17T01:30:39+00:00 Gabriel Laddel
  ;; `slurp-file' doesn't work for whatever reason
  (labels ((parse-ports-from-procfile (procfile)
	     (mapcar (lambda (line) (car (split " " (nth 2 (split ":" line))))) 
		     (rest (split "\\n" (rp (format nil "cat ~a" procfile)))))))
    (let* ((ipv6-hex-ports (parse-ports-from-procfile "/proc/net/tcp6"))
	   (ipv4-hex-ports (parse-ports-from-procfile "/proc/net/tcp")))
      (mapcar (lambda (hex) (parse-integer hex :radix 16)) (append ipv6-hex-ports ipv4-hex-ports)))))

(defun port-in-use-p (port-number)
  (member port-number (open-ports) :test #'=))

(defun trim-dwim (string) (string-trim '(#\space #\newline) string))

(defmacro with-live-swank-connection (&rest body)
  ;; XXX 2014-12-19T09:12:42+00:00 Gabriel Laddel
  ;; this shouldn't have to exist at all
  `(let* ((swank::*emacs-connection* (car swank::*connections*)))
     ,@body))

(defun format-message-for-stumpwm (string)
  (loop with message = (coerce string 'list)
	while message
	collect (if (or (> 80 (length message)) (equal #\space (nth 80 message)))
		    (let* ((o (take 80 message))) 
		      (setf message (drop 80 message))
		      o)
		    (let* ((to-drop (position #\space (reverse (take 80 message))))
			   (to-collect (subseq message 0 (- 80 to-drop))))
		      (setf message (drop (length to-collect) message))
		      to-collect)) into out
	finally (return (coerce (flatten (let* ((clean-out (remove-if #'null (mapcar (lambda (l) (if (equal #\space (car l)) (rest l) l)) out))))
					   (interpose (list #\~ #\%) clean-out))) 'string))))

;;; Debuggering of UNIX
;;; ============================================================================

(defun machine-information ()
  "programmers claim machine independence, but generally fail to delivier. got a
script to derive some information about the machine? contribute it and aid in
the debuggering of linux."
  ;; TODO 2014-11-10T11:55:59-08:00 Gabriel Laddel
  ;; 
  ;; - review man pages for each one of these and give flags to get the
  ;;   information possiblbe - or reverse engineer and unify into something
  ;;   sane.
  ;; 
  ;; - 'tree' /proc & /etc, review the combined ~37832 files and mark all
  ;;   interesting information for export.
  ;;
  ;; - use inxi, sources are available in ~/algol/. apparently it'll give a
  ;;   bunch of interesting information back
  ;;
  ;; - lspci has a bunch of interesting information, see the manpage
  ;;
  ;; check it out: (parse-html (rp "lshw -xml")) << returns all hardware
  (loop for program in (append '("lsb_release -a" "uname --all" "lshw" "lshw-businfo"
				 "lspci" "lspci -v" "lscpu" "lsusb" "lsblk" "df" 
				 "fdisk -l" "mount" "free" "free -m")
			       (mapcar (lambda (a) (format nil "cat ~a" a))
				       '("/proc/buddyinfo"
					 "/proc/cgroups"
					 "/proc/consoles"
					 "/proc/cpuinfo"
					 "/proc/devices"
					 "/proc/diskstats"
					 "/proc/execdomains"
					 "/proc/fbfilesystems"
					 "/proc/interrupts"
					 "/proc/iomem"
					 "/proc/ioports"
					 "/proc/kallsyms"
					 "/proc/loadavg"
					 "/proc/lockso"
					 "/proc/meminfo"
					 "/proc/misc"
					 "/proc/modules"
					 "/proc/mounts"
					 "/proc/mtrr"
					 "/proc/pagetypeinfo"
					 "/proc/partitions"
					 "/proc/slabinfo"
					 "/proc/softirqs"
					 "/proc/stat"
					 "/proc/swaps"
					 "/proc/sysrq-trigger"
					 "/proc/timer_list"
					 "/proc/timer_stats"
					 "/proc/uptime"
					 "/proc/version"
					 "/proc/vmallocinfo"
					 "/proc/vmstat"
					 "/proc/zoneinfo")))
	nconcing (list program (handler-case (rp program) (error nil)))))

(defun environment ()
  (list :machine-information (linux-information)
	:emacs (uiop:run-program "emacs --version" :output :string)
	:machine-type (machine-type)
	:machine-instance (machine-instance)
	:machine-version (machine-version)
	:software-type (software-type)
	:software-version (software-version)
	:lisp-implementation-type (lisp-implementation-type)
	:lisp-implementation-version (lisp-implementation-version)))

(defun levenshtein (a b)
  (let* ((la  (length a))
	 (lb  (length b))
	 (rec (make-array (list (1+ la) (1+ lb)) :initial-element nil)))
    (labels ((leven (x y)
	       (cond
		 ((zerop x) y)
		 ((zerop y) x)
		 ((aref rec x y) (aref rec x y))
		 (t (setf (aref rec x y)
			  (+ (if (char= (char a (- la x)) (char b (- lb y))) 0 1)
			     (min (leven (1- x) y)
				  (leven x (1- y))
				  (leven (1- x) (1- y)))))))))
      (leven la lb))))

(defun read-from-whole-string (string)
  (loop with start = 0 
	with out = nil
	with string  = (trim-dwim string)
	while (/= start (length string))
	do (multiple-value-bind (j k) 
	       (read-from-string string nil nil :start start)
	     (setf start k)
	     (push j out))
	finally (return (nreverse out))))
