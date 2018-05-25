;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:


(eval-when-compile (/require-meta core evil))

(/def-custom-var evil-initial-state-alist
  `((messages-buffer-mode . motion)
    (finder-mode . motion)
    )
  "The alist of (mode . state) for evil to set the initial state.")

(/defface-evil-mode-line-tag)
; (code-defkey-evil-prefix-key)


(/provide)
;;; custom/evil.el ends here
