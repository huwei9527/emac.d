;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta file))

;; (defconst elpa-custom-packages-list
;;   '(
;;     ;; import vim in emacs
;;     evil
;;     ;; color-theme-solarized
;;     ;; zenburn-theme
;;     ;; labburn-theme
;;     ;; molokai-theme
;;     ;; ivy-hydra
;;     ;; fuzzy search sortting
;;     flx
;;     smex
;;     ;; ivy minibuffer completion and counsel utils
;;     counsel
;;     ;; show keymap menu
;;     which-key
;;     ;; find file in project like ctrl-p
;;     find-file-in-project
;;     ;; latex environment
;;     auctex
;;     company-auctex
;;     ;; paredit
;;     paredit
;;     paredit-everywhere
;;     rainbow-delimiters
;;     ;; hl-sexp
;;     ;; hideshow-org
;;     ;; hideshowvis
;;     company
;;     company-statistics
;;     company-ycmd
;;     browse-kill-ring
;;     workgroups
;;     ;; workgroups2
;;     ace-jump-mode
;;     ace-link
;;     flyspell-lazy
;;     popwin
;;     ;; python
;;     elpy
;;     magit
;;     )
;;   "Package to be install.")

(setq package-selected-packages
      `(evil                 ; vim
	;; color-theme-solarized
	;; zenburn-theme
	;; labburn-theme
	;; molokai-theme
	flx                  ; fuzzy search sortting
	smex                 ; M-x-mru
	counsel              ; minibuffer completion - ivy
	;; ivy-hydra
	which-key            ; show keymap interactively
	find-file-in-project ; find in project
	paredit              ; auto pair sexp
	paredit-everywhere   ; auto pair sexp-like object
	rainbow-delimiters   ; colored parenthesis
	;; hideshow-org
	;; hideshowvis
	company              ; completion engine
	company-statistics   ; complete-mru
	company-ycmd         ; completion server
	browse-kill-ring     ; kill ring
	;; workgroups
	;; workgroups2
	ace-jump-mode        ; move cursor
	ace-link             ; open link fast
	flyspell-lazy        ; spell check in idle time
	;; popwin
	elpy                 ; python major mode
	auctex               ; TeX major mode
	company-auctex       ; completion for TeX
	magit                ; git tool
	))

(/def-user-directory packages
  "The directory to store ELPA packages. This will be set to
package-user-dir automatically.")

(/def-config-directory package-describes
  "The directory to save ELPA describe file. When you describe a
package (C-m or <RET> in the package mode) which is no installed,
ELPA will download a package-name-readme.txt file to describe the
package to you. The file is store in the package-user-dir directory
by default which would dirty the directory. So I hack it to this
directory by advice describe-package-1.")

(/provide)
;;; custom/elpa.el ends here
