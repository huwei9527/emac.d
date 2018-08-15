;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core ui))

(/defcustom font-height 120 "The height of font")
(/defcustom inhibit-linum-mode-list
  (list 'help-mode
        'Info-mode
        'completion-list-mode
        'compilation-mode
	'inferior-python-mode
        )
  "Major mode list which disable line number.")

;; construct simple color face.
(/--defface-simple)

(/provide)
;;; custom/ui.el ends here
