;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:


(eval-when-compile (/require-meta core evil))

(/def-custom-var evil-initial-state-alist
  `((messages-buffer-mode . motion)
    (finder-mode . motion)
    )
  "The alist of (mode . state) for evil to set the initial state.")

;; Define faces used in the mode line for evil state tag.
(/defface-evil-mode-line-tag)

;; Define leader key maps.
(/def-evil-leader-keys)


(/provide)
;;; custom/evil.el ends here
