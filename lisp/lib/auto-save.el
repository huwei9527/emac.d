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
  "Toggle auto-saving in the current buffer (Auto Save mode).
With a prefix argument ARG, enable Auto Save mode if ARG is
positive, and disable it otherwise.

If called from Lisp, enable the mode if ARG is omitted or nil."
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

(/provide)
;;; lib/auto-save.el ends here
