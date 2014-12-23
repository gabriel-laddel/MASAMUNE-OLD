;;; https://www.youtube.com/watch?v=pvt_XIHHSPQ
;;; https://www.youtube.com/watch?v=JBVpQzvrJ4w
;;; http://irc.netsplit.de/channels/?chat=israel

(in-package #:mm)

(defun start-hebrew (habit)
  (record-event habit (event :started))
  (record-event habit (event :finished)))

(defun hebrew-install ()
  (push (i 'habit
	   :name "Study Hebrew"
	   :initialization-function 'start-hebrew
	   :visualization-function 'mmg::visualize-captains-log
	   :occurrence :daily)
	*habits*))

;; '("Alef"
;;   "Bet"
;;   "Gimel"
;;   "Dalet"
;;   "He"
;;   "Vav"
;;   "Zayin"
;;   "Het"
;;   "Tet"
;;   "Yod"
;;   "Kaf"
;;   "Lamed"
;;   "Mem"
;;   "Nun"
;;   "Samekh"
;;   "Ayin"
;;   "Pe"
;;   "Tsadi"
;;   "Qof"
;;   "Resh"
;;   "Shin"
;;   "Tav")
