;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'ui-lib)
(require 'ui-custom)

(defconst evil-custom-state-string-list
  `(insert normal visual emacs motion replace operator)
  "The list of all the evil state names.")

(defvar evil-custom-initial-state-alist
  `((messages-buffer-mode . motion)
    (help-mode . motion)
    (finder-mode . motion)
    (minibuffer-inactive-mode . insert))
  "The alist of (mode . state) for evil to set the initial state.")

(require 'test-lib)
(code-defface-evil-state-tags)

(defface evil-custom-insert-tag
  `((t (:inherit (red-black bold))))
  "'evil-insert-state-tag' face show in mode line.")

(defface evil-custom-normal-tag
  `((t (:inherit (white-black bold))))
  "'evil-normal-state-tag' face show in mode line.")

(defface evil-custom-motion-tag
  `((t (:inherit (blue-black bold))))
  "'evil-motion-state-tag' face show in mode line")

(defface evil-custom-visual-tag
  `((t (:inherit (green-black bold))))
  "'evil-visual-state-tag' face show in mode line")

(defface evil-custom-operator-tag
  `((t (:inherit (yellow-black bold))))
  "'evil-operator-state-tag' face show in mode line")

(defface evil-custom-replace-tag
  `((t (:inherit (magenta-black bold))))
  "'evil-replace-state-tag' face show in mode line")

(defface evil-custom-emacs-tag
  `((t (:inherit (cyan-black bold))))
  "'evil-emacs-state-tag' face show in mode line")

(provide 'evil-custom)
; evil-custom.el ends here