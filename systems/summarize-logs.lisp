(in-package #:mm)

(c log-summary () (end-date start-date dialogues))
(c dialogue () (date title comments messages))

(defun start-summarize-logs (habit)
  (record-event habit (event :started))
  (mmb::open-uri "http://log.bitcoin-assets.com"))

(defun visualize-summarize-logs (habit sheet)
  (let* ((y 1084) (x 1460))
    (format sheet " ~%    notes word count: ~d~%    log-length: ~d lines~%    session length: ~d minutes"
	    (random 200) (random 200) (random 200))))

;; (defmethod human-readable-string ((log-summary log-summary))
;;   "Returns a string suitable for viewing as plaintext"
;;   (let* ((header "================================================================================"))
;;     (with-slots (start-date end-date dialogues) log-summary
;;       (apply #'cat (cons (format nil "#bitcoin-assets log summary for ~a through ~a~%~%" start-date end-date)
;; 			 (loop for dialog in dialogues
;; 			       collect (with-slots (comments date title messages) dialog
;; 					 (format nil "~a - ~a~%~a~%~{~%~{~a~%~}~}~%"
;; 						 title date header messages))))))))

;; (defmethod publish-new-summary ((log-summary log-summary))
;;   (let* ((publishing-dir "~/quicklisp/local-projects/")
;; 	 (text-filename (format nil "~a~a-through-~a" 
;; 				publishing-dir (start-date log-summary) (end-date log-summary)))
;; 	 (index-file "~/quicklisp/local-projects/gabriel-laddel.github.io/index.lisp"))
;;     ;; write human readable version
;;     (write-to-file (format nil
;; 			   "~/quicklisp/local-projects/gabriel-laddel.github.io/"
;; 			   text-filename) 
;; 		   (human-readable-string log-summary))
;;     ;; update index
;;     (write-to-file index-file (push (list :a :href (cat "/" text-filename) text-filename)
;; 				    (read-file index-file))
;; 		   :supersede)
;;     ;; TODO 2014-11-30T05:22:21-08:00 Gabriel Laddel
;;     ;; generate html file
;;     ))

;; (defmethod print-object ((log-summary log-summary) stream)
;;   (with-slots (start-date end-date dialogues) log-summary
;;     (format stream "#<~a thru ~a, ~d dialogs>" start-date end-date (length dialogues))))

;; (defun log-summary-cleanup ()
;;   "Emacs will call this function when I've pushed the logs to master"
;;   (record-event (mmg::habit-by-name "Summarize Logs") (event :finished))
;;   (mmg::run-or-focus-dashboard))

;; (defun log-message-plists (log-html-parse)
;;   (let* ((dirty-messages) (clean-messages)) 
;;     (walk-tree (lambda (l) (when (and (listp l) (eq :body (car l)))
;; 			(setq dirty-messages l)))
;; 	       log-html-parse)
;;     (mapcar (lambda (l) (let* ((ugh (rest (car (cdr (car (cdr l)))))))
;; 		     (list :nick (cadadr ugh) :message (car (cdaddr ugh))
;; 			   :link (llast (caadar ugh)) :time (llast (cadar ugh)))))
;; 	    (butlast (drop 5 dirty-messages)))))

;; (defun logs (start-timestamp &optional end-timestamp)
;;   "if END-TIMESTAMP is not supplied, returns a plist of logs to the current date"
;;   (loop for (start-date end-date)
;; 	appending (list date (log-message-plists (parse-html (http url))))))

;; (if (timestamp= (zero-timestamp (now)) current-timestamp)
;;     (format-timestring nil (now) :format  '(:day "-" :month "-" :year)))

;; (defvar my-log-summary
;;   (make-instance 'log-summary
;; 		 :start-date "21-11-2014"
;; 		 :end-date "23-11-2014"
;; 		 :dialogues (list (make-instance 'dialogue
;; 						 :title "More DDoS and hacks. Watch your step."
;; 						 :date "22-11-2014"
;; 						 :messages '(("16:29:08mats_cd03: \"Linux full disk encryption does not really work, for that matter.\" << anyone know what mp is referring to here?"
;; 							      "16:31:21mats_cd03:i suspect he means keys need to be stored in RAM for decryption"
;; 							      "16:35:07asciilifeform:mats_cd03: this plus the fact of the decryptor routine itself normally stored on disk"
;; 							      "16:35:44asciilifeform:mats_cd03: where it can be overwritten by pwner with a version that saves key"
;; 							      "16:42:44mats_cd03:a"
;; 							      "17:26:42asciilifeform:'the time has come, the walrus said, to talk of many things: of shoes, and ships, and sealing-wax, of cabbages and kings; and why the sea is boiling hot, and whether pigs have wings...'"
;; 							      "17:27:11asciilifeform:specifically, there is a vandal lurking in #b-a who 'metasploits' uncloaked folks"
;; 							      "17:27:59asciilifeform:about 2 in 3 of extant consumer-grade routers have published remote anal orifices."
;; 							      "17:28:00asciilifeform:cheers."
;; 							      "17:33:40kanzure:haha metasploit"
;; 							      "17:33:44kanzure:v. professional"
;; 							      "17:33:49kanzure:such leetness"
;; 							      "17:34:03asciilifeform:not necessarily 'metasploit' but perhaps a similar autodiddler."
;; 							      "17:34:09kanzure:don't they know it only counts if you come up with a novel zero day? otherwise all the points go to charity."
;; 							      "17:34:17asciilifeform:lol"
;; 							      "17:34:19kanzure:(and who wants that?)"
;; 							      "17:35:04kanzure:asciilifeform: okay to pm?"
;; 							      "17:35:12asciilifeform:sure, though i'm about to eat"
;; 							      "17:38:22mats_cd03:msf isn't necessarily an amateur tool, though it is used by amateurs"
;; 							      "17:44:43assbot:[HAVELOCK] [AM1] 67 @ 0.1 = 6.7 BTC [-]"
;; 							      "17:45:07kanzure:.title https://bitcointalk.org/index.php?topic=437926.0"
;; 							      "17:45:08assbot:If I was chairman of the Fed ... ( http://bit.ly/1xDre0T )"
;; 							      "17:59:10punkman:so deeds server was maybe hacked"
;; 							      "17:59:45punkman:bitcents still at address though"
;; 							      "18:01:01asciilifeform:punkman: what suggested that it was hacked ?"
;; 							      "18:01:33punkman:kakobrekla got a mail that an attack was originating from the deed server"
;; 							      "18:04:28kakobrekla:all i can say (as i dont have or had access to the machine) that there is something that looks like ddos + complaint from outside of do")
;; 							     ("21:46:29asciilifeform:to briefly continue this morning's thread - don't rely on freenode's 'cloaks', if you have a consumer garbage-grade router on your premises, throw it out, replace with actual router.")
;; 							     ("20:01:41asciilifeform:so it appears that i've merited own, personal ddos."
;; 							      "20:03:58assbot:[MPEX] [S.MPOE] 70800 @ 0.00041465 = 29.3572 BTC [-] {3}"
;; 							      "20:05:11asciilifeform:anybody else?"
;; 							      "20:05:16asciilifeform:or did the gods smile on me alone."))
;; 						 :comments "If you've not been following #b-a up until this point, note that trilema.com, Loper-os.org, log.bitcoin-assets.com and qntra.net have all been getting DDoS'd for some time now. The \"Reddit police\" or some other such nonsense showed up at one point to claim responsibility.")

;; 				  (make-instance 'dialogue
;; 						 :title "The Real Bitcoin"
;; 						 :date "23-11-2014"
;; 						 :messages '(("04:37:12asciilifeform:kakobrekla: should we even be doing bitcoinwhatever on x86 << what do you think."
;; 							      "04:38:20Vexual:yes"
;; 							      "04:38:41asciilifeform:back to that thread: can't speak for others, but i don't intend to use the trimmed 0.5.3 in anger."
;; 							      "04:38:48asciilifeform:beyond verifying that it indeed functions.")
;; 							     ("04:44:06asciilifeform:well it doesn't, at present"
;; 							      "04:44:17asciilifeform:plan was, someone wanted to fix it..."
;; 							      "04:44:32kakobrekla:no that wasnt the question"
;; 							      "04:45:08asciilifeform:lol, question was re: stealth hardfork ?"
;; 							      "04:45:46kakobrekla:yea i guess"
;; 							      "04:45:59asciilifeform:somebody here, iirc, suggested exactly this.")
;; 							     ("04:53:31asciilifeform:quite possibly this is ideal scenario. a hardfork that sane folks on the whole planet would immediately flee to."))
;; 						 :comments "see therealbitcoin.org for the who what when and why of this bitcoind implementation.")

;; 				  (make-instance 'dialogue
;; 						 :title "WoTnet"
;; 						 :date "23-11-2014"
;; 						 :messages '(("05:04:09asciilifeform:mircea_popescu: this is why, in my unofficial wonderland, you can't even open a socket without transmitting an rsa-signed a 'this is me, and my wot' breath of life packet."
;; 							      "05:04:56mircea_popescu:asciilifeform in any case, my original change to github and generally the mangement of open source codebase of \"add a read by X\" field is not actually enough.")
;; 							     ("05:09:30asciilifeform:back to syn packets, signed breath-of-life neatly licks ddos."
;; 							      "05:09:41asciilifeform:all comms answered in priority of wot rank.")
;; 							     ("05:11:53asciilifeform:a 4096-bit rsa signature and key fp fit handily in a udp minimal packet."
;; 							      "05:12:46asciilifeform:i'm going to confess now that i implemented this..."
;; 							      "05:12:48asciilifeform:and am sitting on it"
;; 							      "05:13:10asciilifeform:because not ready for battlefield yet, and other work")
;; 							     ("05:17:13asciilifeform:decimation: in point of fact, you can get by without either tcp or the proverbial 'bad reimplementation' of it")		    
;; 							     ("05:17:39asciilifeform:decimation: one can use error-coding to get around lost and re-ordered packets."
;; 							      "05:17:44decimation:exactly"
;; 							      "05:17:57decimation:tcp really is pretty retarded if you think about"
;; 							      "05:18:03asciilifeform:this also neatly dovetails into 'apocalyptic shortwave radio' net."
;; 							      "05:18:17decimation:it violates the principle of \"the highest protocol layer ought to control\" (end-to-end argument)"
;; 							      "05:18:24mircea_popescu:decimation it's not retarded if you think of how fucking cheap it is."
;; 							      "05:18:25decimation:exactly"
;; 							      "05:18:37mircea_popescu:think about that. think about the early days of bbs, which is how routing fucking got invented in the first placer"
;; 							      "05:18:43mircea_popescu:deliver all the mail locally, cheaper calls"
;; 							      "05:18:47asciilifeform:mircea_popescu: the excess packets - cheap. the ddosyness - no."
;; 							      "05:18:57decimation:well, in elder days when the internet wasn't full of orcs it kinda seemed ok"
;; 							      "05:19:03decimation:now the costs are different"
;; 							      "05:19:06mircea_popescu:asciilifeform it was five cents a minute yo! at the time a minute meant one page, or a gallon of gasoline."
;; 							      "05:19:30asciilifeform:everything in future will work - or not - depending on how readily it cuts through swaths of orcs.")
;; 							     ("05:22:21asciilifeform:wotnet."
;; 							      "05:22:27asciilifeform:'you read it here phirst (tm)'"))))))

(in-package #:mm)

(defun summarize-logs-install () 
  (push (i 'habit
	   :name "Summarize Logs"
	   :description "Read the #b-a logs, take notes, summarize and publish findings online"
	   :initialization-function 'start-summarize-logs
	   :visualization-function 'visualize-summarize-logs
	   :occurrence :weekly)
	*habits*))
