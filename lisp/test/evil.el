;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-meta evil)

(defun /evil-show-state (&rest args)
  "Show current evil state"
  (message "%s" evil-state))

(/message-test-start)
(when nil
  (message "%s %s" (/--intern-evil-hook 'normal)
	   (/--intern-evil-hook 'insert 'exit))
  (message "%s" /--evil-entry-hook-list)
  (message "%s" /--evil-exit-hook-list)
  (/add-hook--evil-state /hello-world nil)
  (/add-hook-evil-entry-state /evil-show-state)
  (/add-hook-evil-exit-state /evil-show-state)
  (/defface-evil-mode-line-tag)
  (/ppmacroexpand (/setup-evil-state-tags)))


(/message-test-end)

(/provide)
;;; test/evil.el ends here
