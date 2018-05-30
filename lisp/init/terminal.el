;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (require '/init/global
;; 	 (expand-file-name "lisp/init/global.el" user-emacs-directory))

(defvar /--pre-create-directory-list nil)

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
  )
(/require-lib highlight-sexp)
; (/require-test test)

;(pp /--pre-create-directory-list)

(switch-to-buffer "*Messages*")
;; (switch-to-buffer "*AAA")
;; (switch-to-buffer "*BBB")
;(split-window-right)
;; (other-window 1)
;; (switch-to-buffer "*Messages*")


(/provide)
;;; init/terminal.el ends here
