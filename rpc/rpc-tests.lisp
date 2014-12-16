(fiasco:define-test-package #:rpc-tests
  (:use #:masamune))

(in-package #:rpc-tests)

(in-package #:mm)

(let* ((host "192.168.0.22")
       (port 6103)
       (symbols-to-export '(generate-client-library))
       (output-dir "/tmp/")
       (client-library-name 'masamune-client)
       (input-library-path "/root/quicklisp/local-projects/masamune/"))

  (mm::generate-client-library output-dir
			       input-library-path
			       client-library-name
			       host port
			       symbols-to-export)

  (mm::generate-client-package output-dir client-library-name '(:mmc) symbols-to-export)
  (generate-client-asd-file output-dir client-library-name)
  (mm::generate-server input-library-path host port symbols-to-export))


