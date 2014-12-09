(in-package #:mm)

(defvar *hack* nil "Occasionally I need somewhere to put an intermediate value.")
(defvar *project-location* #P"~/quicklisp/local-projects/masamune/")

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

