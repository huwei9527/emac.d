;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core keymap))

(/require-lib buffer)

(defun /evil-restart ()
  "Restart evil."
  (interactive)
  (call-interactively #'evil-mode)
  (call-interactively #'evil-mode))

(/def-transient-minor-mode scroll-other-window
  "In evil normal state, scroll other window."
  `(([?k] . /scroll-other-window-line-down)
    ([?j] . /scroll-other-window-line-up)
    ([?l] . scroll-other-window-down)
    ([?\s] . /scroll-other-window)
    ([?n] . beginning-of-buffer-other-window)
    ([?m] . end-of-buffer-other-window)
    )
  "scroll-other-window")

(/def-double-keys-event-command scroll-other-window
  ((scroll-other-window))
  ((/put-back-events [?\C-g]))
  "Scroll other window.
Normal: scroll other window.
Double: Stop `/scroll-other-window-transient-mode'.")

(/provide)
;;; lib/evil.el ends here
