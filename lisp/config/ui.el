;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta hook))

(/require-custom ui)

;; Set default font size in gui frame
(set-face-attribute 'default nil :height /custom-font-height)

;; Hide GUI.
(and (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message nil)

;;; Scroll up & down line continuously, not by half page.
(setq-default scroll-up-aggressively 0.0
              scroll-down-aggressively 0.0)

; maximize GUI window (frame for Emacs).
(add-to-list 'default-frame-alist `(fullscreen . maximized))
(add-to-list 'initial-frame-alist `(background-mode . dark))
(add-to-list 'default-frame-alist `(background-mode . dark))

;; Line number.
(global-linum-mode)
(column-number-mode 1)
(setq linum-delay t)
; Don't show line number in some certain major mode.
(/advice-add (linum-on) :around
  (lambda (fun &rest args)
    "Don't show line number for certain buffer."
    (or (memq major-mode /custom-inhibit-linum-mode-list)
	(apply fun args))))

(/require-config theme)
(/require-config mode-line)

(/provide)
;;; config/ui.el ends here
