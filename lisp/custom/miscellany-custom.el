;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(code-defdir-config "abbrev"
  "The directory to store abbrev_defs file.")

(defvar gnu/linux-x-selection-shell-command "xsel")

(defvar x-set-primary-selection-shell-command
  (cond
   ((eq system-type 'gnu/linux)
    (format "%s -i -p" gnu/linux-x-selection-shell-command))
   (t nil))
  "The shell command sets the primary of X selection.")

(defvar x-set-clipboard-selection-shell-command
  (cond
   ((eq system-type 'gnu/linux)
    (format "%s -i -b" gnu/linux-x-selection-shell-command))
   (t nil))
  "The shell command sets the clipboard of X selection.")

(defvar x-primary-selection-value-shell-command
  (cond
   ((eq system-type 'gnu/linux)
    (format "%s -o -p" gnu/linux-x-selection-shell-command))
   (t nil))
  "The shell command get the value from primary of X selection.")

(defvar x-clipboard-selection-value-shell-command
  (cond
   ((eq system-type 'gnu/linux)
    (format "%s -o -b" gnu/linux-x-selection-shell-command))
   (t nil))
  "The shell command get the value from clipboard of X selection.")

(provide 'miscellany-custom)
; miscellany-custom.el ends here
