;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(defun /evil-restart ()
  "Restart evil."
  (interactive)
  (call-interactively #'evil-mode)
  (call-interactively #'evil-mode))

(/provide)
;;; lib/evil.el ends here
