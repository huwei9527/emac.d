;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'file-custom)

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

(defcustom smart-auto-save-mute nil
  "Don't show message when save buffer."
  :type 'boolean
  :group 'smart-auto-save)

(defvar smart-auto-save-filter-regexp file-custom-file-tail-filter-regexp)

(defun smart-auto-save-filtered-p (fn)
  "Return t if the suffix of FN (a file name)
matches 'smart-auto-save-fileter-regexp'"
  (string-match-p smart-auto-save-filter-regexp fn))

(defvar smart-auto-save-last-time (current-time)
  "The time of last idle auto save.")

(defvar smart-auto-save-idle-timer nil
  "The idle timer for idle auto save.")

(defsubst smart-auto-save-buffer ()
  "Smart save the current buffer."
  (when (and buffer-file-name
             (buffer-modified-p)
             (not (smart-auto-save-filtered-p buffer-file-name))
             (file-writable-p buffer-file-name))
    (if smart-auto-save-mute (with-temp-message "" (save-buffer)) (save-buffer))
    t))

(defun smart-auto-save-all-buffers ()
  "Smart save all the buffers."
  (let* ((cnt 0))
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when (smart-auto-save-buffer)
          (setq cnt (1+ cnt)))))
    cnt))

(defun smart-auto-save-idle ()
  "Smart save file when Emacs is idle."
  (when (> (time-to-seconds (time-since smart-auto-save-last-time))
           smart-auto-save-interval)
    (let* ((cnt (smart-auto-save-all-buffers)))
      (unless (eq cnt 0)
        (setq smart-auto-save-last-time (current-time))
        (unless smart-auto-save-mute
          (message "Idle save %d files." cnt))))))

(defsubst smart-auto-save-buffer-advice (&rest args)
  "Smart save current file when triggered. (Portable to any triggers.)"
  (smart-auto-save-buffer))

(defsubst smart-auto-save-idle-on ()
  "Enable idle auto save."
  (interactive)
  (or smart-auto-save-idle-timer
    (setq smart-auto-save-idle-timer
          (run-with-idle-timer smart-auto-save-idle-time
                               t #'smart-auto-save-idle))))

(defsubst smart-auto-save-idle-off ()
  "Disable idle auto save."
  (interactive)
  (when smart-auto-save-idle-timer
    (cancel-timer smart-auto-save-idle-timer)
    (setq smart-auto-save-idle-timer nil)))

(eval-when-compile (require 'code))

(defun smart-auto-save-on ()
  "Enable smart-auto-save."
  (interactive)
  (code-add-advice-for-buffer-change-command
   :before smart-auto-save-buffer-advice)
  (code-add-advice-for-window-switch-command
   :before smart-auto-save-buffer-advice)
  (code-add-hook-for-emacs-out smart-auto-save-buffer)
  (code-add-hook (evil-normal-state-entry-hook)
                 smart-auto-save-buffer)
  (smart-auto-save-idle-on))

(defun smart-auto-save-off ()
  "Disable smart-auto-save."
  (interactive)
  (code-remove-advice-for-buffer-change-command
   smart-auto-save-buffer-advice)
  (code-remove-advice-for-window-switch-command
   smart-auto-save-buffer-advice)
  (code-remove-hook-for-emacs-out smart-auto-save-buffer)
  (code-remove-hook (evil-normal-state-entry-hook)
                    smart-auto-save-buffer)
  (smart-auto-save-idle-off))

(provide 'smart-auto-save)
;;; smart-auto-save.el ends here
