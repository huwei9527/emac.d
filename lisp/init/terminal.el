;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (require '/init/global
;; 	 (expand-file-name "lisp/init/global.el" user-emacs-directory))

(/require-custom core)
(/require-config
  core
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
  which-key
  paredit
  hideshow
  company
  yasnippet
  magit
  select
  file
  buffer
  miscellany
  emacs-lisp
  python
  shell
  )
(/require-test test)

(switch-to-buffer "*Messages*")
;; (switch-to-buffer "*AAA")
;; (switch-to-buffer "*BBB")
;(split-window-right)
;; (other-window 1)
;; (switch-to-buffer "*Messages*")


(/provide)
;;; init/terminal.el ends here
