;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; Toggle evil-emacs-state with 'ctrl-alt-z', leaving
;; 'ctrl-z' to suspend emacs.
;; This must place before loading evil to make the keymap take effect.
(setq evil-toggle-key "C-M-z")

(eval-when-compile (/require-meta evil))

(require 'evil)
(/require-custom evil)

;; Set the custom evil initial state for major mode.
(dolist (cus /custom-evil-initial-state-alist)
  (evil-set-initial-state (car cus) (cdr cus)))

;; Don't show state in the echo area.
(setq evil-echo-state nil)

;;; Evil mode line tag
;; Configure the tag string and the place in the mode line.
(/setup-evil-state-tags)
(setq evil-mode-line-format '(before . /mode-line-overwrite-mode))

(evil-mode 1)

;;; {{ Text object
;; select Tex mathmode 
(/def-evil-text-object "$" "\\$" "\\$")
;; select Latex mathmode
(/def-evil-text-object "m" "\\\\(" "\\\\)")
(/def-evil-text-object "M" "\\\\\\[" "\\\\\\]")
(/def-evil-text-object "=" "=" "=")
(/def-evil-text-object "|" "|" "|")
;; select a line with space at begin and end trimed.
(/def-evil-text-object "l" "^ *" " *$")
;;; }}

(/provide)
;;; config/evil.el ends here
