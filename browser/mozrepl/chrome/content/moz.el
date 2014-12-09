(require 'comint)

;; Maybe fix-me: C-c control-char are reserved for major modes. But
;; this minor mode is used in only one major mode (or one family of
;; major modes) so it complies I think ...
(defvar moz-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" 'run-mozilla)
    (define-key map "\C-\M-x"  'moz-send-defun)
    (define-key map "\C-c\C-c" 'moz-send-defun-and-go)
    (define-key map "\C-c\C-r" 'moz-send-region)
    (define-key map "\C-c\C-l" 'moz-save-buffer-and-send)
    map))

;;;###autoload
(define-minor-mode moz-minor-mode
  "MozRepl minor mode for interaction with Firefox.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this minor mode is enabled, some commands become available
to send current code area \(as understood by c-mark-function) or
region or buffer to an inferior MozRepl process (which will be
started as needed).

The following keys are bound in this minor mode:

\\{moz-minor-mode-map}"
  nil
  " Moz"
  :keymap moz-minor-mode-map
  :group 'moz)

(defalias 'run-mozilla 'inferior-moz-switch-to-mozilla)

(defvar moz-repl-name "repl"
  "The current name of the repl.")

(defvar moz-input-separator "\n--end-remote-input\n")

(defvar moz-repl-host "localhost")

(defvar moz-repl-port 4242)

(defun moz-temporary-file ()
  (if (and moz-temporary-file
           (file-readable-p moz-temporary-file))
      moz-temporary-file
    (setq moz-temporary-file (make-temp-file "emacs-mozrepl"))))

(defun moz-send-region (start end)
  "Send the region to Firefox via MozRepl."
  (interactive "r")
  (my-send-string (inferior-moz-process)
                      (concat moz-repl-name ".pushenv('printPrompt', 'inputMode'); "
                              moz-repl-name ".setenv('printPrompt', false); "
                              moz-repl-name ".setenv('inputMode', 'multiline'); "
                              "undefined; \n"))
  ;; Give the previous line a chance to be evaluated on its own.  If
  ;; it gets concatenated to the following ones, we are doomed.
  (sleep-for 0 1)
  (comint-send-region (inferior-moz-process)
                      start end)
  (my-send-string (inferior-moz-process)
                      "\n--end-remote-input\n")
  (my-send-string (inferior-moz-process)
                      (concat moz-repl-name ".popenv('inputMode', 'printPrompt'); "
                              "undefined; \n"))
  (my-send-string (inferior-moz-process)
                      "\n--end-remote-input\n")
  (display-buffer (process-buffer (inferior-moz-process))))

(defun moz-send-defun ()
  "Send the current function to Firefox via MozRepl.
Curent function is the one recognized by c-mark-function."
  (interactive)
  (save-excursion
    (mark-defun)
    (moz-send-region (point) (mark))))

(defun moz-send-defun-and-go ()
  "Send the current function to Firefox via MozRepl.
Also switch to the interaction buffer."
  (interactive)
  (moz-send-defun)
  (inferior-moz-switch-to-mozilla nil))

(defun moz-save-buffer-and-send ()
  "Save the current buffer and load it in Firefox via MozRepl."
  (interactive)
  (save-buffer)
  (comint-send-string (inferior-moz-process)
                      (concat moz-repl-name ".pushenv('printPrompt', 'inputMode'); "
                              moz-repl-name ".setenv('inputMode', 'line'); "
                              moz-repl-name ".setenv('printPrompt', false); undefined; "))
  (comint-send-string (inferior-moz-process)
                      (concat moz-repl-name ".load('file://localhost/" (buffer-file-name) "');\n"
                              moz-repl-name ".popenv('inputMode', 'printPrompt'); undefined;\n"))
  (display-buffer (process-buffer (inferior-moz-process))))

;;; Inferior Mode

(defvar inferior-moz-buffer nil
  "The buffer in which the inferior process is running.")

(defun inferior-moz-insert-moz-repl ()
  "Insert value of `moz-repl-name' and a dot (.)."
  (interactive) (insert moz-repl-name "."))

(defvar inferior-moz-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cc" 'inferior-moz-insert-moz-repl)
    map))

;;;###autoload
(define-derived-mode inferior-moz-mode comint-mode "Inf-MozRepl"
  "Major mode for interacting with Firefox via MozRepl."
  (setq comint-input-sender 'inferior-moz-input-sender)
  (add-hook 'comint-output-filter-functions 'inferior-moz-track-repl-name nil t))

(defun inferior-moz-track-repl-name (comint-output)
  (when (string-match "\\(\\w+\\)> $" comint-output)
    (setq moz-repl-name (match-string 1 comint-output))))

(defun inferior-moz-self-insert-or-repl-name ()
  (interactive)
  (if (looking-back "\\(\\w+\\)> $")
      (insert moz-repl-name ".")
    (insert last-command-char)))

(defun inferior-moz-input-sender (proc string)
  "Custom function to send input with comint-send-input.
Instead of sending input and newline separately like in
comint-simple-send, here we *first* concatenate input and
newline, then send it all together.  This prevents newline to be
interpreted on its own."
  (comint-send-string proc (concat string "\n")))

(defun inferior-moz-switch-to-mozilla (arg)
  "Switch to the inferior MozRepl buffer.
Create the buffer and start the MozRepl process and connect to
Firefox if needed.

See also `inferior-moz-start-process'."
  (interactive "P")
  (when arg
    (setq moz-repl-host (read-string "Host: " "localhost"))
    (setq moz-repl-port (read-number "Port: " 4242)))
  (pop-to-buffer (process-buffer (inferior-moz-process)))
  (goto-char (process-mark (inferior-moz-process))))

(defun inferior-moz-process ()
  "Return inferior MozRepl process.  Start it if necessary.
See also `inferior-moz-start-process'."
  (or (if (buffer-live-p inferior-moz-buffer)
          (get-buffer-process inferior-moz-buffer))
      (progn
        (inferior-moz-start-process)
        (inferior-moz-process))))

(defun inferior-moz-start-process ()
  "Start an inferior Mozrepl process and connect to Firefox.
It runs the hook `inferior-moz-hook' after starting the process
and setting up the inferior Firefox buffer.

Note that you have to start the MozRepl server from Firefox."
  (interactive)
  (condition-case err
      (progn
        (setq inferior-moz-buffer
              (apply 'make-comint "MozRepl" (cons moz-repl-host moz-repl-port) nil nil))
        (sleep-for 0 100)
        (with-current-buffer inferior-moz-buffer
          (inferior-moz-mode)
          (run-hooks 'inferior-moz-hook)))
    (file-error
     (with-output-to-temp-buffer "*MozRepl Error*"
       (with-current-buffer (get-buffer "*MozRepl Error*")
         (insert "Can't start MozRepl, the error message was:\n\n     "
                 (error-message-string err)
                 "\n"
                 "\nA possible reason is that you have not installed"
                 "\nthe MozRepl add-on to Firefox or that you have not"
                 "\nstarted it.  You start it from the menus in Firefox:"
                 "\n\n     Tools / MozRepl / Start"
                 "\n"
                 "\nSee ")
         (insert-text-button
          "MozRepl home page"
          'action (lambda (button)
                    (browse-url
                     "http://hyperstruct.net/projects/mozrepl")
                    )
          'face 'button)
         (insert
          " for more information."
          "\n"
          "\nMozRepl is also available directly from Firefox add-on"
          "\npages, but is updated less frequently there.")
         ))
     (error "Can't start MozRepl"))))

(provide 'moz)

;;; moz.el ends here
