;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; Toggle evil-emacs-state with 'ctrl-alt-z', leaving
;; 'ctrl-z' to suspend emacs.
;; This must place before loading evil to make the keymap take effect.
(setq evil-toggle-key "C-M-z")

(require 'evil)
(/require-custom evil)

;; Set the custom evil initial state for major mode.
(dolist (cus /custom-evil-initial-state-alist)
  (evil-set-initial-state (car cus) (cdr cus)))

;; Don't show state in the echo area.
(setq evil-echo-state nil)


(evil-mode)

(/provide)
;;; config/evil.el ends here
