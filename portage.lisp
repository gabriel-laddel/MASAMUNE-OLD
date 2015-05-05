(in-package #:mm)

;;; Portage reimplementation 
;;; ============================================================================
;;; 
;;; build your own liveCD, usb etc.
;;; https://forums.gentoo.org/viewtopic-t-57754.html
;;; 
;;; https://www.gentoo.org/support/documentation/
;;; https://packages.gentoo.org/
;;; https://wiki.gentoo.org/wiki/Gentoolkit
;;;
;;; read the forums
;;;
;;; /var/lib/portage/world
;;; /etc/portage/package.keywords
;;; /etc/portage/package.use
;;; /etc/make.conf
;;; 
;;; equery y maxima
;;; emerge --info
;;; emerge -s maxima
;;; equery files imaxima
;;;
;;; list all installed packages
;;; - equery list "*"
;;; - cd /var/db/pkg/ && ls -d */*
;;; - list all packages the user has installed cat /var/lib/portage/world
;;;
;;; Any documentation that a program might have (other than man pages) is stored in /usr/share/doc/gentoolkit-[version]/[program-name]/. 
;;; 
;;; equery depgraph imaxima << checkout equery --helpe
;;; 
;;; Equery reimplementaiton
;;; 
;;; __init__.py is uesless
;;; 
;;; how to configure the kernel?
;;;
;;; USE flags are portage's way of organizing compile time options

;;; Kernel 
;;; ============================================================================
;;; 
;;; lspci
;;; lsusb
;;; lsmod
;;; uname -a
;;; 
;;; ls /dev/snd/con* << lists sound cards
;;; 
;; http://unix.stackexchange.com/questions/115620/configuring-compiling-and-installing-a-custom-linux-kernel
;; ^ start here - I read through 5 pages of google results - these links are all that is needed.
;; http://edoceo.com/howto/kernel-modules
;; https://www.kernel.org/
;; https://wiki.gentoo.org/wiki/Kernel/Gentoo_Kernel_Configuration_Guide
;; http://0xax.gitbooks.io/linux-insides/content/Booting/index.html
;; 
;; /usr/src/linux/.config << nothing there 

;;; some kernel configuration files
;;; http://kernel.ubuntu.com/~kernel-ppa/configs/lucid/

;;; /usr/src/linux-debian-sources-3.16.2/Documentation:

;;; /usr/lib64/systemd <<< ! what the fuck, how do I get rid of this

;;; cd /usr/src/linux..../ && make menuconfig

;; make localmodconfig - apparently this configures the kernel based on the hardware you've got available
