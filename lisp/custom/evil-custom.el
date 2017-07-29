;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'ui-lib)
(eval-when-compile (require 'code-gen))

(defvar config-evil-initial-state-alist
  `(
    (messages-buffer-mode . motion)
    ;(completion-list-mode . motion)
    )
  "The alist of (mode . state) for evil to set the initial state.")

(defvar evil-state-mode-line-alist
  '(("insert" . ("#e80000" . "#ffffff"))
    ("replace" . ("#880000" . "#ffffff"))
    ("operator" . ("#480000" . "#ffffff"))
    ("normal" . (nil . nil))
    ("visual" . ("#00e800" . "#ffffff"))
    ("motion" . (nil . nil))
    ("emacs" . ("#444488" . "#ffffff")))
  )

(define-evil-mode-line-setters)

(provide 'evil-custom)
; evil-custom.el ends here