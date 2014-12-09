;;; TODO 2014-12-08T17:07:12+00:00 Gabriel Laddel
;;; 
;;; This currently doesn't work, and it isn't going to until I have the time to
;;; sit down and change out the contents of systemrescueCD.

(rp "curl http://github.com/gabriel-laddel/masamune-rescue-cd/masamune-rescue-cd.iso > /tmp/masamune-rescue-cd.iso")

(defun make-bootable-hard-drive ()
  "http://www.sysresccd.org/Sysresccd-manual-en_How_to_install_SystemRescueCd_on_an_USB-stick

TODO
System Rescue CD has a guide for customizing it to your needs. Masamune's design
isn't quite yet concrete enough to make this a worthwhile task,
but it will be sometime soon.
   
http://www.sysresccd.org/Sysresccd-manual-en_How_to_personalize_SystemRescueCd"
  (labels ((usb-inserted? () ...)
	   (user-input () (format t "~{~%~a~}" '("" "" "What type of bootable USB/hard drive would you like to create?"
						 ""
						 "0. Bootstrap: network connection required for install"
						 "1. Robust:    download all Masamune dependencies and write them to the USB. (unimplemented)"				    
						 "2. Fork:      writes the current Masamune distribution to the USB,  (unimplemented)"
						 "3. Backup:    writes the current Masamune distribution to the USB, including all of your personal files (unimplemented)"
						 ""
						 "Choose using numeric id: "))
	     (let* ((input (read *query-io*)))
	       (if (member input '(0 1 2 3) :test #'=) input (user-input))))
	   (make-bootable-usb (mkdir -p /tmp/system-rescue-cd 
			       "mount -o loop,exec /tmp/systemrescuecd.iso" "/tmp/cdrom")))
    (if (usb-inserted?)
	(ecase (user-input)
	  (0 (make-bootable-usb))
	  (1 (format t "~%~%Not yet implemented"))
	  (2 (format t "~%~%Not yet implemented"))
	  (3 (format t "~%~%Not yet implemented")))
	(format "~&No USB detected, please insert one and try again or debug `make-bootable-harddrive'"))))

(defun build-masamune ()
  "This gets run, no locally"
  (if (probe-file "/sys/firmware/efi")
      (make-bootable-hard-drive)
      ( "efi-boot is not yet supported")))
