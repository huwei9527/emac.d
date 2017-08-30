;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'evil)
(require 'evil-custom)
(require 'evil-lib)

;;; Set the custom evil initial state for major mode.
(dolist (cus evil-custom-initial-state-alist)
  (evil-set-initial-state (car cus) (cdr cus)))

;; Don't show state in the echo area.
(setq evil-echo-state nil)

;;; Evil mode line tag
;; Configure the tag string and the place in the mode line.
(setq evil-insert-state-tag (propertize "I" 'face 'evil-custom-insert-tag)
      evil-normal-state-tag (propertize "N" 'face 'evil-custom-normal-tag)
      evil-visual-state-tag (propertize "V" 'face 'evil-custom-visual-tag)
      evil-emacs-state-tag (propertize "E" 'face 'evil-custom-emacs-tag)
      evil-motion-state-tag (propertize "M" 'face 'evil-custom-motion-tag)
      evil-operator-state-tag (propertize "O" 'face 'evil-custom-operator-tag)
      evil-replace-state-tag (propertize "R" 'face 'evil-custom-replace-tag)
      evil-mode-line-format '(before . mode-line-overwrite-mode))

(evil-mode 1)

(eval-when-compile (require 'evil-code))

;;; New text object
(code-define-text-object "$" "$" "$")
(code-define-text-object "=" "=" "=")
(code-define-text-object "|" "|" "|")
(code-define-text-object "l" "^ *" " *$")

;;; Custom keymap

(provide 'config-evil)
; config.el ends here