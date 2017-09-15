;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'ui-lib)
(require 'ui-custom)
(eval-when-compile
  (require 'evil-code))

(defconst evil-custom-state-string-list
  `(insert normal visual emacs motion replace operator)
  "The list of all the evil state names.")

(defvar evil-custom-initial-state-alist
  `((messages-buffer-mode . motion)
    ;; (help-mode . motion)
    (package-menu-mode . motion)
    (finder-mode . motion)
    (TeX-error-overview-mode . motion)
    )
  "The alist of (mode . state) for evil to set the initial state.")

(code-defface-evil-state-tags)
(code-defkey-evil-prefix-key)

(provide 'evil-custom)
; evil-custom.el ends here
