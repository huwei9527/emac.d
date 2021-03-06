;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (/require-meta core)
  (/require-meta ui))

(/def-custom-var font-height 120 "The height of font")
(/def-custom-var inhibit-linum-mode-list
  (list 'help-mode
        'Info-mode
        'completion-list-mode
        'compilation-mode
	'inferior-python-mode
        )
  "Major mode list which disable line number.")

;; construct simple color face.
(/defface-simple)

(/provide)
;;; custom/ui.el ends here
