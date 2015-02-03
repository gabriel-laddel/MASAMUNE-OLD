(in-package #:mm)

(defun message (string) 
  (stumpwm::message-no-timeout (format-message-for-stumpwm string)))

(defun start-image-recognition ()
  "needs to be converted to CL"
  (mmb::open-uri "http://cs.stanford.edu/people/karpathy/deepimagesent/" t))

(defun start-maxima-introduction ()
  (message "Maxima is a system for the manipulation of symbolic and numerical expressions, including differentiation, integration, Taylor series, Laplace transforms, ordinary differential equations, systems of linear equations, polynomials, and sets, lists, vectors,matrices, and tensors. Maxima yields high precision numeric results by using exact fractions, arbitrary precision integers, and variable precision floating point numbertos. Maxima can plot functions and data in two and three dimensions. M-x imaxima from within Emacs will launch the imaxima program. Documentation is avalible via Emacs info system. Maxima is written in CL. Switching between CL and the Maxima syntax can be done via to_lisp(); and (to-maxima). Maxima currently doesn't integrate into the rest of the Common Lisp world (ie, use quicklisp) this will change in due time."))

(defun start-masamune-tutorial ()
  "https://www.madboa.com/geek/gpg-quickstart/"
  (message 
   (apply #'cat 
	  (interpose "~%~%"
		     ' ("Greetings. you're now running Masamune. Presumably you know why, so let's take a look around."
			"you're connected to conkeror and can do XYZ - but first you need to enable some if its commands, specifically: enable the mozrepl extension."
			"press C-t m or run the command `kgraph' through stumpwm's M-x style interface to display the knowledge graph. You can access this interface via C-t ;."
			"wifi settings can be accessed via the \"network\" command"
			"note the node \"Masamune Tutorial\" is highlighted, this your current location. The knowdlege graph signifies everything that Masamune can currently teach you. right click on one of the nodes to see more options. one can join the irc channel for a node, complete it's tutorial, view a description of its contents or create a dependent tutorial.  If you run into an issue, the concept's channel is a good place to ask for help. Currently all node's channels redirect to #masamune and will continue to do so until the need for more channels occurs."
			"A lesson is a program specifying interactions between Emacs, Common Lisp and the web browser. There currently are no security mechanisms in place to prevent anyone from doing something nasty. Run only programs written by those you trust. The code for all lessons is open source. See a node's details to inspect the integrity of code or author."
			"Everything about Masamune is modifiable by a user fluent in Common Lisp. Lessons on Common Lisp are avalible by default in all Masamune distributions."
			"Masamune also has facilities to help you manage your time. C-t d or the `dashboard' command through stumpwm's interface will bring up your daily dashbaord. Masamune will optionally open this interface when it detects you wake up to give you an overview of your day."
			"Masamune's concept of `habits' is essentially the english defintion, activities that one wants to repeat over some timeframe. Habits can be installed though the masamune code repository avalible through the stumpw command `repository'. The test habit can be removed by selecting the option 'delete habit' avalible on right click. Masamune's habits automatically gather data about your usage habits and informs you of this. In general masamune collects a great deal of data - this information is stored locally for your convenience and not sent to any parties (read code to verify this yourself). Some analysis is done by default for the dashboard."
			"systems, (you should see that masamune is the only system currently inspectable) refer to codebases one is in charge of maintaing etc. only common lisp systems are currently 
recognized. if you begin hacking on masamune, stats on your hacking will begin to show up."))))
  ;; 
  ;; Until I have a chance to work quality image recognition and OCR into the
  ;; masamune toolchain you must manually install adblock and mozrepl
  ;; 
  ;; Installing Adblock
  ;; ===========================================================================
  ;; 
  ;; http://cowboyprogrammer.org/getting-adblock-to-work-in-conkeror/
  ;; 
  ;; I followed this guide with the modification of installing adblock 2.6.5
  ;; after installing versions 1.3.10 and 2.0.1 (which didn't let me subscribe to
  ;; filters).
  ;; 
  ;; Version info: binary 33.1 of XULrunner to launch Conkeror commit
  ;; 48d3ef4369f267faf42451a580b1ac6bcb6a5a18, master branch of the repository
  ;; git://repo.or.cz/conkeror.git
  ;; 
  ;; to run: ~/path/to/xulrunner/xulrunner ~/path/to/conkeror/application.ini
  ;; 
  ;; make sure to subscribe to easylist in 2.6.5 or it won't work
  ;;
  ;; Installing mozrepl
  ;; ===========================================================================
  ;; In Conkeror: M-x extensions, click "Install from file" from the wrench and
  ;; screwdriver icon in the top right and select:
  ;; 
  ;; ~/quicklisp/local-projects/masamune/browser/mozrepl/mozrepl@hyperstruct.net.xpi
  ;;
  ;; restart Conkeror. The parenscript repl should connect automatically. You
  ;; can kill it by removing the call from stumpwm::*timer-list*. If you 
  ;; restart Conkeror via `mm::start-conkeror' the repl will connect
  ;; automatically.
  ;;
  ;; Misc.
  ;; ===========================================================================
  ;; use "e" while in dired to open the file under point in the browser
  ;; 
  ;; C-i to edit a text field in emacs, C-x # from emacs to switch back to
  ;; the browser
  ;; 
  ;; parenscript support
  ;; ===========================================================================
  ;; I have limited time and unfortunately cannot hone parenscript to the razors
  ;; edge I'd like it to be. A lot of code for things such as debugger
  ;; integration, conditions, event systems, general utilities, macros, PSCLOS
  ;; etc. have already been written.
  ;;
  ;; I took some time to identify all the valuable parenscript codebases and
  ;; have included them in .../masamune/research/parenscript/.
  )

(defun start-common-lisp ()
  (stumpwm::select-emacs)
  (listener) 
  (sleep 3)
  (stumpwm::hsplit)
  (stumpwm::select-emacs)
  (with-live-swank-connection
      (ignore-errors
       (swank::eval-in-emacs '(progn 
			       (find-file "~/quicklisp/local-projects/masamune/mathematics-scratch.lisp")
			       (end-of-buffer)
			       (delete-other-windows) nil) t))))

(defun start-non-von-neumann-research-module ()
  (climacs:edit-file (ppath "/systems/non-von-neumann-computing.txt")))

(defun start-analytical-combinatorics () 
  (mmb::open-uri "http://algo.inria.fr/flajolet/Publications/books.html" t))

;;; Haskell
;;; ============================================================================
;;;
;;; I'm reading the book "Learn you a haskell for the great good" and it is quite
;;; clear that the ideas within it could be expressed far more clearly, though
;;; there is certainly a type of person who will find it enjoyable. I propose
;;; that these should be two paths.
;;;
;;; 1. haskell for those who don't know how to program.
;;; 2. haskell for those who do.
;;;
;;; 1. should consist of a lesson that follows directly.
;;; 
;;; 2. should consist of teaching the user how to use the interpreter, all the
;;;    goodies for Emacs, a quick introduction to the language concepts and
;;;    pointers to comprehensive references. I'm not sure what would qualify
;;;    as a "comprehensive reference" that you cna use in your day to day
;;;    so I'm listing options here
;;;
;;;    cheatsheet: git clone git://github.com/m4dc4p/cheatsheet.git
;;;    haskell98 report: https://www.haskell.org/onlinereport/
;;; 
;;; haskell cheat sheet git://github.com/m4dc4p/cheatsheet.git
;;;
;;; 1. starting out
;;; 
;;;    haskell has both prefix and postfix functions. mathematical operators are
;;;    postfix, but other types of functions are prefix. parens work as you would
;;;    expect.
;;;    
;;;    function names cannot begin with uppercase letters, but can include the
;;;    "'" char.
;;;
;;;    [,] list of elements seperated by ","
;;;    ++  list concatenation
;;;    :   cons
;;;    
;;; 

(defun start-haskell ()
  ;; TODO 2014-12-02T18:08:54-08:00 Gabriel Laddel
  ;; (unless (haskell-platform-installed-p) (install-haskell-platform))
  ;; (unless (haskell-emacs-packages-installed-p) (install-haskell-emacs-packages))
  ;; (mm:position-frames :1/3 :right)
  (mmb::open-uri "http://learnyouahaskell.com/introduction")
  (message "The haskell lesson works as follows. You read \"learn you a Haskell\" in the browser (on the right) and input code forms on the left. while in Emacs you can scroll the browser with the stumpwm command XYZ. When you input the last form of the page masamune will automatically move you onto the next page. You progress is transparently saved, and when resuming the lesson you'll be placed at the section of the book matching the last code form you entered. Yes, this is a hack, but nevertheless.."))

;;; Language.C
;;; ============================================================================

(defun start-language-c ()
  "start lesson for the haskell package https://hackage.haskell.org/package/language-c"
  (message "First, I reviewed the first 5 pages of google results on a few searches that approximate to \"I'd like the C AST please\" this turned up several schemes - the best of which is the haskell package Language.C.AST. It is a fully compliant C99 parser that returns an AST object you can deal with programatically. Writing a program that round trips C source -> C AST -> C source is absolutely trivial. There isn't an interface to get sexps, so you've got to deal with the AST as a haskell data structure *shrugs* perhaps you'll find this acceptable. One fellow parsed the entire LINUX kernel with it. There are several lisps that have been written as abstractions over C (Bigloo scheme and ECL being the major players) none have what you want though their offerings are quite well designed and documented. Unfortunately they all use GC and though they've interfaces to C, they don't allow for C source -> AST sexp -> C source transformations. They're all designed so that you don't have to deal with C in your day to day. Outside of this there are perl scripts and python scripts that do various degrees of the above, in particular, there is a full C parser for python. I read over their documentation said \"Eww...\" and moved on."))


;;; Common Lisp introduction
;;; ============================================================================

;; 7 These names are accidents of history. They stand for ``Contents of the Address part of Register'' and ``Contents of the Decrement part of Register'' of the IBM 704 computer, which was used for the first implementation of Lisp in the late 1950s. Scheme is a dialect of Lisp. -- SICM, appendix A

;; |symbol| means that you've not correctly cased the symbol

;;; There isn't any reason to use `setq'

;;; see api.lisp in cl-ppcre to understand how to use compiler macros

;;; M-x slime-export-class, struct symbol at point

;;; Metric System
;;; ============================================================================

(defvar metric-prefixes-table
  '((:text :symbol :factor)
    ("tera" "T" 1000000000000)
    ("giga" "G" 1000000000)
    ("mega" "M" 1000000)
    ("kilo" "k" 1000)
    ("hecto" "h" 100)
    ("deca" "da" 10)
    ("centi" "c" 0.01)
    ("milli" "m" 0.001)
    ("micro" "*u" 0.000001)
    ("nano" "n" 0.0000000001)
    ("pico" "p" 0.000000000001)))

(defun start-metric-system-lesson ()
  (message (format nil "implement metric system quizzes based on ~%~{~%~a~}" metric-prefixes-table)))

(defvar calculus-rules
  ;; The inverse function rule - I'm not entirely sure how to express this
  ;; (g (f x)) = x
  ;; (f (g y)) = y
  ;; then 
  ;; (df g) = (/ 1 (comp (df f) g))
  '(((df (+ f g))                        (+ (df f) (df g)))
    ((df (- f g))                        (- (df f) (df g)))
    ((df (lambda h (x) (* (f x) (g x))))      (+ (* (df f x) (g x)) (* (df g x) (f x))))
    ((df (lambda h (x) (f (g x))))            (* (df (f (g x))) (df (g x))))
    ((df (log x))                        (/ 1 x))
    ((df (exp x n ))                     (df (* n (exp x (1- n)))))
    ((df (sin x))                        (cos x))
    ((df (cos x))                        (- (sin x)))
    ((df (tan x))                        (exp (sec x) 2)) ; = (/ 1 (exp (cos x) 2)) = (1+ (exp (tan x) 2))
    ((df (sec x))                        (* (sec x ) (tan x)))
    ((df (csc x))                        (- (* (csc x) (cot x))))
    ((df (cot x))                        (- (exp (csc x) 2))) ; = (/ -1 (exp (sin x) 2)) = (1+ (exp (cot x) 2))
    ((df (arcsin x))                     (/ 1 (sqrt (1- (exp x 2)))))
    ((df (arccos))                       (- (/ (sqrt (1- (exp x 2))))))
    ((df (arctan))                       (/ 1 (1+ (exp x 2))))
    ((df (arcsec))                       (/ 1 (* (abs x) (sqrt (- (exp x 2) 1)))))
    ((df (arccsc))                       (- (/ 1 (* (abs x) (sqrt (- (exp x 2) 1)))))) 
    ((df (arccot))                       (- (/ 1 (+ 1 (exp x 2)))))))

(defun start-calculus ()
  (message (format nil "implement calculus w/quiz on these rules ~%~{~%~a~}" calculus-rules)))

;;; CL
;;; ============================================================================
;;; - `restart-case'
;;; - how to pass an error onto a restart after declining to handle it

(defun start-cl-testing ()
  "see http://aperiodic.net/phil/archives/Geekery/notes-on-lisp-testing-frameworks.html for more information on the common lisp test frameworks. Fiasco, is the successor of stefil (see https://github.com/luismbo/stefil/issues/9 for more information) and is the best common lisp testing framework by a long shot. It currently isn't in quicklisp" 
  (unless (probe-file "~/quicklisp/local-projects/fiasco")
    (rp "cd ~/quicklisp/local-projects/ && git clone https://github.com/capitaomorte/fiasco.git"))
  (message "implement cl-testing lessons"))

(defun start-feynman-lectures ()
  (mmb::open-uri "http://www.feynmanlectures.caltech.edu/I_01.html"))

(defun start-chemistry ()
  ;; http://www.iza-structure.org/databases/  
  ;; (ql:quickload 'cl-glut-examples)
  ;; followed by
  ;; (cl-glut-examples::molview)
  (message "As it stands there are a bunch of databases (some of them on paper) detailing properties of different compounds etc. there are a few programs which understand molecular formulas and some of the diagrams we use to depict them. In organic chemistry courses worldwide students are memorize reaction charts. This is amazingly primitive. An intelligently designed system would have all of these charts computerized - instantaneous access to any database you might desire, taking into account intellectual property in some fashion (not according to the currently laws, this much is certain) and also allow one to change between any given representation of molecular structure in question on a whim, modify it on a whim. Also, one should be able to specify \"I want to go from here to here\" in the reaction charts and either get a 'recipe' for their lab (ie, program 4 robotz) or run up against the current human understanding of the subject - or the limits of your own personal fork of the \"chemistry\" program that takes into account your lab data and bent. Mathematica wants to be this, but isn't and never will be for a number of reasons I'll address another day. The generalization of this is the ability for all programs in some context to seamlessly interoperate with each other without any loss of expressivity. The studious will note that this is a completely solved problem that no machine learning algorithm will be able to approximate anytime soon. That we don't have this right now today is simply because Silicon Valley (and the rest of the world) are terrible at computing."))

(defun start-practical-foundations-of-mathematics ()
  (mmb::open-uri "http://www.cs.man.ac.uk/~pt/Practical_Foundations/html/summary.html"))

(defun start-backups ()
  "I've made the mistake of having a bad backup, don't let it happen to you, here is the canonical way and why

rsync instead of cp -a or dd, which can fail

also used to copy .iso files onto this

dd -i /dev/null >> -o /dev/sda -bs 1M ;; or something similar to write 0s to the hard drive

the 'real way' is via clonezilla, which will verify they both have the same bits

http://clonezilla.org/")

(defun start-usocket ()
  ;; writing to xterm, from: http://symbo1ics.com/blog/?p=1991

  (let* ((port 8003)
         (_ (stumpwm::run-commands (format nil "exec xterm -e nc -l -p ~d" port)))
         (socket (progn (sleep 2) (usocket:socket-connect "127.0.0.1" port :element-type 'character
  									 :protocol :stream)))
         (stream (usocket:socket-stream socket)))
    (format stream "testing console output~%don't forget to `finish-output' or `force-output'")
    (force-output stream))
  (mmb::open-uri "http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp"))

(progn (setq *nodes* nil)
       (make-node "Masamune Tutorial" () 'start-masamune-tutorial)
       (make-node "Metric System" '("Masamune Tutorial") 'start-metric-system-lesson)
       (make-node "Mathematics" '("Masamune Tutorial"))
       (make-node "Physics" '("Masamune Tutorial"))
       (make-node "Chemistry" '("Masamune Tutorial") 'start-chemistry)
       (make-node "Feynman Lectures" '("Physics") 'start-feynman-lectures)
       ;; emerge app-doc/blas-docs
       ;; emerge sci-libs/blas-reference
       ;; emerge app-doc/lapack-docs
       ;; emerge sci-libs/lapack-reference
       (make-node "Linear Algebra" '("Mathematics"))
       ;; http://cap-lore.com/MathPhys/Vector/
       (make-node "Calculus" '("Mathematics") 'start-calculus)
       ;; "http://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/"
       ;; (make-node "Algebraic Topology")
       ;; "http://www.femlisp.org/"
       (make-node "Partial Differential Equations" '("Calculus" "Linear Algebra"))
       (make-node "Neural Networks" '("Calculus" "Linear Algebra")) ;; see Christopher Olah's blog for more information
       (make-node "Analytical Combinatorics" '("Calculus" "Linear Algebra") 'start-analytical-combinatorics)
       (make-node "Facial Recognition" '("Neural Networks"))
       (make-node "Image Recognition" '("Neural Networks") 'start-image-recognition)
       (make-node "Hacking Masamune" '("Masamune Tutorial"))
       (make-node "Backups" '("Masamune Tutorial") 'start-backups)
       ;; all current bug report formats suck. ideally, one would like to
       ;; make use of `save-sbcl-and-die' instead of a bug report. this would
       ;; allow one to quickly diagnose if the problem lies in masamune, the
       ;; hardware, or compiler. also, any quicklisp packages installed, any
       ;; information that can be grokked about the current hardware etc.
       ;;
       ;; start from showing how to debug https://github.com/stumpwm/stumpwm/wiki/Freezes
       (make-node "Reporting Bugs"  '("Hacking Masamune"))
       (make-node "CLIM, Stumpwm and Emacs" '("Hacking Masamune"))
       (make-node "The browser" '("Hacking Masamune"))
       (make-node "The dashboard" '("Hacking Masamune"))
       (make-node "Maxima" '("Masamune Tutorial") 'start-maxima-introduction)
       ;; "https://github.com/waterhouse/emiya"
       (make-node "Garbage Collection [research]" '("Masamune Tutorial"))
       (make-node "Non-Von Neumann Computing [research]" '("Masamune Tutorial") 'start-non-von-neumann-research-module)
       (make-node "Common Lisp" '("Masamune Tutorial") 'start-common-lisp)
       (make-node "CL format" '("Common Lisp"))
       (make-node "Pathnames" '("Common Lisp"))
       (make-node "CL regular expressions" '("Common Lisp"))
       (make-node "CL Introduction " '("Common Lisp"))
       ;; Macroexpansion trick in emacs - press C-c C-m to macroexpand any arbitary sub expression
       ;; 
       ;; showcase tracing facilities
       ;; 
       ;; quick navigation + all my editing facilities 
       ;; 
       ;; where to put documentation? some people append it to their packages,
       ;; such as in the case of named readtables. this works pretty well IMO.
       (make-node "Inline assembler " '("Common Lisp")
		  (lambda () 
		    (mmb::open-uri "http://www.pvk.ca/Blog/2014/03/15/sbcl-the-ultimate-assembly-code-breadboard/")
		    (mmb::open-uri "http://www.tldp.org/HOWTO/Assembly-HOWTO/" t)))       
       (make-node "CLOS" '("CL Introduction " "Common Lisp"))
       (make-node "CLIM" '("CLOS"))
       (make-node "testing CL code" '("Common Lisp") 'start-cl-testing)
       (make-node "Async CL" '("Common Lisp"))
       ;; https://glyph.twistedmatrix.com/2012/01/concurrency-spectrum-from-callbacks-to.html
       (make-node "CL-CONT" '("Async CL"))       
       (make-node "CL-ASYNC" '("Async CL"))
       (make-node "usocket" '("Common Lisp") 'start-usocket)
       (make-node "Interfacing with C code" '("Common Lisp"))
       (make-node "Manipulating the C AST" '("Interfacing with C code"))
       (make-node "Introduction to Maxima" '("Maxima"))
       (make-node "Hacking Maxima" '("Common Lisp" "Maxima"))
       (make-node "AGDA" '("Masamune Tutorial"))
       (make-node "Haskell" '("Masamune Tutorial") 'start-haskell)
       ;; see haskell-src-exts for manipulating haskell sources programmatically
       (make-node "Practical Foundations of Mathematics" '("Haskell") 'start-practical-foundations-of-mathematics)       
       (make-node "Language.C" '("Haskell") 'start-language-c)       
       (make-node "hcsw.org, Nuff" '("Masamune Tutorial"))
       (make-node "SICP" '("Masamune Tutorial"))
       (make-node "SICM" '("Physics"))
       (make-node "forth" '("Masamune Tutorial"))
       ;; two useful links on the matter
       ;; http://www.hcsw.org/frugal/
       ;; http://hcsw.org/reading/forth.txt
       (setq mmg::*root-node* (node-by-name "Masamune Tutorial"))
       (setq *focused-node* (node-by-name "Masamune Tutorial"))
       (setf (description (node-by-name "Common Lisp")) '("http://en.wikipedia.org/wiki/Common_Lisp"
							  "http://common-lisp.net/tutorials/")))

(in-package #:stumpwm)

(defcommand repository () ()
  "dashboard"
  (mmg::run-or-focus-repository))

(defcommand dashboard () ()
  "dashboard"
  (mmg::run-or-focus-dashboard))

(defcommand kmap () ()
  "knowledge graph"
  (mmg::run-or-focus-kmap))

(define-key *root-map* (kbd "d") "dashboard")
(define-key *root-map* (kbd "m") "kmap")

(in-package #:stumpwm)

(setq *input-window-gravity* :center
      *message-window-gravity* :center
      *normal-border-width* 0
      *window-border-style* :none
      *transient-border-width* 0
      *top-level-error-action* :break)


