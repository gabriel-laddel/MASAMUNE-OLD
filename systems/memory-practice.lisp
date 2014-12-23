;;; To review
;;; ============================================================================
;;; there are a few individuals who have had abnormally large working memory -
;;; how?
;;; 
;;; - http://vserver1.cscs.lsa.umich.edu/~crshalizi/reviews/mathematical-logic/
;;;   has page on memory somewhere.
;;; 
;;; When I look in a book’s index for a word and see the corresponding page
;;; number I quickly close my eyes, even as I turn to the main body of the book,
;;; and transform the image of the numeral into a mental image of the English
;;; sounds that name the number. Even with my eyes closed, especially with my
;;; eyes closed, the image of the numeral is vivid. Then I open my eyes and look
;;; at page numbers within the body of the book and compare those with the
;;; sounds of the English name of the number that I found in the index. I find
;;; that decoding an Arabic numeral is quick, but obliterates the short term
;;; memory of the image of the numeral from the index. The memory of the sound,
;;; however, is more robust and is not displaced by images of new numerals.
;;; This observation is consonant with Julian Jaynes idea that language was
;;; adaptive because it let people remember complex ideas, sort of like a disk
;;; drive can store more stuff than RAM. In particular they could convey and
;;; remember a plan to capture an animal.
;;; 
;;; I have an electric heater in my room. It automatically turns on sometimes
;;; and when it does it begins to draw enough more power to just noticeably dim
;;; the lights. A few seconds later it begins to emit a low hum. I become
;;; consciously aware of the hum at which time I recall the slight dimming. I
;;; think the dimming alone would not have caught my attention. Each time the
;;; hum surprises me, even after very many repetitions of this pattern. I
;;; suppose that if there were unfavorable conditions associated with the hum,
;;; the dimming would also come to my attention, or at least provoke a
;;; conditioned response. This suggests an unconscious portion, or mode, of
;;; short term memory. I think that this is also the mechanism that allows me to
;;; decide to count the hourly chimes of a clock after several chimes have
;;; already occurred. I think I can retroactively count about 5 chimes. This may
;;; well be an illusion.
;;;
;;; TODO
;;; ============================================================================
;;; - history of the world
;;; - history of mathematics
;;; - poetry
;;; - the correct way to go about these quizzes is to have a pane where I can
;;;   just drag and drop in images, text etc. from anywhere else online and
;;;   then save that out as a problem.
;;; "1569246297","Your Memory : How It Works and How to Improve It"
;;; "0596517785","Your Brain: The Missing Manual"

;; A chronlogy of mathematical events, Adrian Rice

;; 18,000 bce the ishango bone, Zaire (possibly the earliest known evidence of counting)

;; 4000 clay accounting tokens used in the middle east 

;; 3400-3200 development of numerical notation, Sumer (souther Iraq)

;; 2050 first attestation of place-value sexagestimal system 

;; 1850-1650 old babylonian mathematics

;; 1650 rhind papyrus (copy of papyrus from around 1850; largest and best preserved mathematical papyrus from ancient Egypt)

;; 1400-1300 decimal numeration, China found on oracle bones of the Shang Dynasty

;; 580 Thales of Miletus ("Father of geometry")

;; 450 Zeno's paradoxes of motion

;; 370 Eudoxus (theory of proportion, astronomy, method of exaustion)

;; 350 Aristole (logic)

;; 320 Eudemu's History of Geometry (important evidence about knowledge of geometry at the time) decimal numeration, India 

;; 300 Euclid's Elements

;; 250 Archimedes (solid geometry, quadrature, statics, hydrostatics, approximation of pi)

;; 230 Eratosthenes (measurement of Earth's circumference, algorithm for finding prime numbers)

;; 200 Apolloniu's Conics (extensive and influential work on comics)

;; 150 Hipparchus (computer first chord table)

;; 100 Jiu Zhang Suan Shu ("Nine Chapter on Mathematical Procedures"; the most important ancient Chinese mathematical text)

;; 60 C.E Heron of Alexandria (optics, geodesy)

;; 100 Menelau's Spherics (spherical trigonometry)

;; 150 Ptolemy's Almagest 

;; 250 Diophantu's Arithmetica (solutions of determinant and indeteminant equations, early algebraic symbolism)

;; 300-400 Sun Zi (Chinese remainder therom)

;; 320 Pappus's Collection (summarized and extended most important mathematics known at the time)

;; 370 Theon of Alexandira (commented on Ptolemy's Almagest, revision of Euclid)

;; 400 Hypatia of Alexandria (commentaries on Diophantus, Apollonius and Ptolemy)

;; 450 Proclus (commentary on Euclid Book I, summary of Eudemus's History)

;; 500-1200 THe Aryabhatya of Aryabhata (Indian astronomical treatise that included close approximations to pi, (srt 2), and the sines of many angles)

;; 510 Boethius translates Greek works into latin

;; 625 Wang Xiaotong (numeric solutions of cubic equations, expressed geometrically)

;; 628 Brahmagupta's Brahmasphutasiddhanta (astromical tteatise, first treatment of so-called Pell's equation)

;; 710 Venerable Bede (calendar reckoning, stronomy, tides)

;; 830 Al-Khwarzimi's Alegbra (theory of equations)

;; 900 Abu Kamil (irrational solutions to quadratics)

;; 970-990 Gerbert d'Aurillac introduces Arabic mathematical techniques to Europe

;; 980990 Abu al-Wafa (regarded as first ot have calculated the modern trigonomtric functions; firts to sue and publish spherical law of sines)

;; 1000 lbn al-Haytham (aptics, Alhazen's problem)

;; 1100 Omar Khayyam (cubic equations, parallel postulate)

;; 1150 Many Bhaskara's Luavant and Btjaganita (standard arithamtic and algebra textbooks of the Sanskirt tradition, the latter includes a detailed treatment of Pell's equation)

;; 1202 Fibonacci's Liber Abacci (introduces Hindu-Arabic numerals into Europe)

(in-package #:mmg)

;; (defvar *quiz* nil "placeholder for application frame for runtime inspection")
;; (defvar *current-question-draw-function* nil
;;   "funcall'd by `render-question' on the question pane")
;; (defvar *quiz-validation-function* nil)

;; (define-application-frame quiz ()
;;   ((question-pane) (interaction-pane))
;;   (:pointer-documentation nil)
;;   (:menu-bar nil)
;;   (:panes (interaction-pane :interactor)
;; 	  (question-pane :application
;; 			 :display-function 'render-question))
;;   (:layouts (default (vertically ()
;; 		       (3/4 question-pane)
;; 		       (1/4 interaction-pane)))))

;; (defun run-or-raise-quiz ()
;;   (labels ((run-quiz () 
;; 	     (when mm::*habits* (setq *focused-habit* (or *focused-habit* (car mm::*habits*))))
;; 	     (setf *quiz* (make-application-frame 'quiz))
;; 	     (run-frame-top-level *quiz* :name "Quiz")))
;;     (aif (stumpwm::window-by-name "quiz")
;; 	 (stumpwm::select-window (stumpwm::window-name it))
;; 	 (bt:make-thread #'run-quiz :name "quiz"))))

;; (defun render-question (frame pane)
;;   (if *current-question-draw-function*
;;       (funcall *current-question-draw-function* frame pane)
;;       (format pane "bind `*current-question-draw-function*' to a dyadic function, with the quiz frame and question pane as its arguments")))

(in-package #:mm)

;; (setf *quiz-string* (val (take (+ 10 (random 10)) "3.14159265358979323846264338327950288419716939937511")))

;; (setf *quiz-validation-function*
;;       (lambda (string) (string= *quiz-string* string)))

;; (defun random-english-keyboard-string (&optional string-length)
;;   (let* ((english-keyboard-symbols (cat *alphabet* "`~!@#$%^&*()[]{}|\/><,.0123456789")))
;;     (coerce (loop repeat (or string-length 10) collect (random-elt english-keyboard-symbols))
;; 	    'string)))

;; (define-quiz-command (com-next-state :name t) ()
;;   (if *current-question-draw-function*
;;       (if (funcall *quiz-validation-function* (accept 'string :prompt "your answer"))
;; 	  ())))

;; (defun test-question (frame pane)
;;   (declare (ignore pane))
;;   (let* ()
;;     (format pane "~A~%~%press RET when ready" val)))

(defun start-memory-practice (habit)
  (with-live-swank-connection
      (record-event habit (event :started))
    (handler-case
	'(progn 
	  (find-file "~/quicklisp/local-projects/masamune/systems/memory-practice.lisp") nil)
      (error nil))
    (stumpwm::run-with-timer (* 10 60) nil			     
			     (lambda ()			       
			       (stumpwm::message-no-timeout "Time is almost up")
			       (loop for i from 15 downto 0
				     finally (progn (record-event habit (event :finished))
						    (stumpwm::message-no-timeout "finished")
						    (mmg::run-or-focus-dashboard)))))))

(defun visualize-memory-practice (habit sheet)
  (format sheet "digits of Pi recalled~%")
  (format sheet "digits of Tau recalled~%")
  (format sheet "digits of e recalled~%")
  (format sheet "longest alphanumeric string recalled~%")
  (format sheet "longest auditory alphanumeric string recalled~%")
  (format sheet "recall-times, pages of text recalled perfectly, quotes etc."))

(defun memory-practice-install ()
  (push (i 'habit
	   :name "Memory Practice"
	   :initialization-function 'start-memory-practice
	   :visualization-function 'visualize-memory-practice
	   :occurrence :daily)
	*habits*))
