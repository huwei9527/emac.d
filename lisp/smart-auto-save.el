(require 'config-custom)

(defgroup smart-auto-save nil
  "Auto save files smartly."
  :group 'convenience
  :prefix "smart-auto-save-")

(defcustom smart-auto-save-idle-time 1
  "The idle seconds to auto save file."
  :type 'integer
  :group 'smart-auto-save)

(defcustom smart-auto-save-interval auto-save-interval
  "The interval between smart-auto-save."
  :type 'integer
  :group 'smart-auto-save)

(defcustom smart-auto-save-mute t
  "Don't show message when save buffer."
  :type 'boolean
  :group 'smart-auto-save)

(defcustom smart-auto-save-filter config-file-filter
  "Suffix of the file to be filtered."
  :type 'string
  :group 'smart-auto-save)

(defcustom smart-auto-save-triggers
  '("switch-to-buffer" "select-window" "suspend-frame")
  "Command to trigger smart auto save current buffer."
  :type '(repeat string)
  :group 'smart-auto-save)

(defvar smart-auto-save-filter-regexp config-file-filter-regexp)

(defvar smart-auto-save-last-time (current-time)
  "The time of last idle auto save.")

(defvar smart-auto-save-idle-timer nil
  "The idle timer for idle auto save.")

(defun smart-auto-save-buffer ()
  "Smart save the current buffer."
  (when (and buffer-file-name
             (buffer-modified-p)
             (not (string-match-p smart-auto-save-filter-regexp buffer-file-name))
             (file-writable-p buffer-file-name))
    (if smart-auto-save-mute (with-temp-message "" (save-buffer)) (save-buffer))
    t))

(defun smart-auto-save-all-buffers ()
  "Smart save all the buffers."
  (let ((cnt 0))
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when (smart-auto-save-buffer)
          (setq cnt (+ cnt 1)))))
    cnt))

(defun smart-auto-save-idle ()
  "Smart save file when Emacs is idle."
  ;(message "%f %d" (time-to-seconds (time-since smart-auto-save-last-time)) smart-auto-save-interval)
  (when (> (time-to-seconds (time-since smart-auto-save-last-time)) smart-auto-save-interval)
    (let ((cnt (smart-auto-save-all-buffers)))
      (unless (= cnt 0)
        (setq smart-auto-save-last-time (current-time))
        (unless smart-auto-save-mute
          (message "Idle save %d files." cnt))))))

(defun smart-auto-save-buffer-trigger (&rest args)
  "Smart save current file when triggered. (Portable to any triggers.)"
  (smart-auto-save-buffer))

(defun smart-auto-save-idle-on ()
  "Enable idle auto save."
  (interactive)
  (unless smart-auto-save-idle-timer
    (setq smart-auto-save-idle-timer
          (run-with-idle-timer smart-auto-save-idle-time t #'smart-auto-save-idle))))

(defun smart-auto-save-idle-off ()
  "Disable idle auto save."
  (interactive)
  (when smart-auto-save-idle-timer
    (cancel-timer smart-auto-save-idle-timer)
    (setq smart-auto-save-idle-timer nil)))

(defun smart-auto-save-lost-focus-on ()
  "Enable auto save when lost focus."
  (interactive)
  (add-hook 'focus-out-hook #'smart-auto-save-buffer))

(defun smart-auto-save-lost-focus-off ()
  "Disable auto save when lost focus."
  (interactive)
  (remove-hook 'focus-out-hook #'smart-auto-save-buffer))

(defun smart-auto-save-trigger-on ()
  "Enable auto save trigger listed in smart-auto-save-triggers."
  (interactive)
  (mapc (lambda (cmd)
          (advice-add (intern cmd) :before #'smart-auto-save-buffer-trigger))
        smart-auto-save-triggers))

(defun smart-auto-save-trigger-off ()
  "Disable auto save trigger listed in smart-auto-save-trigger."
  (interactive)
  (mapc (lambda (cmd)
          (advice-remove (intern cmd) #'smart-auto-save-buffer-trigger))
        smart-auto-save-triggers))

(defun smart-auto-save-on ()
  "Enable smart auto save."
  (interactive)
  (smart-auto-save-idle-on)
  (smart-auto-save-lost-focus-on)
  (smart-auto-save-trigger-on))

(defun smart-auto-save-off ()
  "Disable smart auto save."
  (interactive)
  (smart-auto-sve-idle-off)
  (smart-auto-save-lost-focus-off)
  (smart-auto-save-trigger-off))

(smart-auto-save-on)

(provide 'smart-auto-save)
;;; smart-auto-save.el ends here