;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (require '/init/global
;; 	 (expand-file-name "lisp/init/global.el" user-emacs-directory))

(defvar /--pre-create-directory-list nil)

; (/require-meta file)
(/require-config
  elpa
  ui
  auto-save
  spell
  evil
  ace-link
  ace-jump-mode
  ivy
  ffip
  rainbow-delimiters
  which-key)
(/require-test test)

;(pp /--pre-create-directory-list)

(switch-to-buffer "*Messages*")

(/provide)
;;; init/terminal.el ends here
