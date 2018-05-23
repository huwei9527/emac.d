;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (eval-when-compile
;;   (/require-meta core))

(/require-lib core)
(/require-lib file)
(/require-custom auto-save)

(defvar /--auto-save-last-time (current-time)
  "The timestamp of last auto save.")

(defvar /--auto-save-idle-timer nil
  "The timer used to auto save in idle time.")

(defun /auto-save-idle-function ()
  "Auto save function executed when idle."
  ;; (message "time %s %s"
  ;; 	   (/time-since /--auto-save-last-time) /custom-auto-save-interval)
  (when (> (/time-since /--auto-save-last-time)
	   /custom-auto-save-interval)
    (let* ((cnt (/save-buffer-all /custom-auto-save-silently)))
      (setq /--auto-save-last-time (current-time))
      (or /custom-auto-save-silently (message "Idle save %s files." cnt)))))

(defun /auto-save-buffer-advice (&rest args)
  "Portable auto save advice."
  (/save-buffer /custom-auto-save-silently))

(define-minor-mode /idle-save-mode
  "Toggle idle-saving in the current buffer (Idle Save mode).
With a prefix argument ARG, enable Auto Save mode if ARG is
positive, and disable it otherwise.

If called from Lisp, enable the mode if ARG is omitted or nil.

When Idle Save mode is on, all the modified and editable buffer will
  be saved."
  :init-value nil
  :lighter nil
  :keymap nil
  :global t
  (if /idle-save-mode
      (or /--auto-save-idle-timer
	  (setq /--auto-save-idle-timer
		(run-with-idle-timer /custom-auto-save-idle-time
				     t #'/auto-save-idle-function)))
    (when /--auto-save-idle-timer
      (cancel-timer /--auto-save-idle-timer)
      (setq /--auto-save-idle-timer nil))))

(eval-when-compile (/require-meta hook))

(define-minor-mode /focus-save-mode
  "Toggle focus-saving in the current buffer (Focus Save mode).
With a prefix argument ARG, enable Auto Save mode if ARG is
positive, and disable it otherwise.

If called from Lisp, enable the mode if ARG is omitted or nil.

When Focus Save mode is on, the current modified buffer will be saved
before losing focus."
  :init-value nil
  :lighter nil
  :keymap nil
  :global t
  (if /focus-save-mode
      (progn
	(/advice-add-buffer-change :before /auto-save-buffer-advice)
	(/advice-add-window-switch :before /auto-save-buffer-advice)
	(/add-hook-focus-loss /auto-save-buffer-advice))
    (/advice-remove-buffer-change /auto-save-buffer-advice)
    (/advice-remove-windwo-switch /auto-save-buffer-advice)
    (/remove-hook-focus-loss /auto-save-buffer-advice)))



(/provide)
;;; lib/auto-save.el ends here
