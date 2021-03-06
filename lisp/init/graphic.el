;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (require '/init/global
;; 	 (expand-file-name "lisp/init/global.el" user-emacs-directory))

(let* ((file-name-handler-alist nil)) ; Accelerate loading.
  (/require-init terminal)
  ;; (require 'config-elpa)
  ;; (require 'config-ui)
  ;; (require 'config-auto-save)
  ;; (require 'config-spell)
  ;; (require 'config-evil)
  ;; (require 'config-ace-link)
  ;; (require 'config-ace-jump-mode)
  ;; (require 'config-ivy)
  ;; (require 'config-ffip)
  ;; (require 'config-rainbow-delimiters)
  ;; (require 'config-which-key)
  ;; ;(require 'config-paredit)
  ;; ;(require 'config-hideshow)
  ;; (require 'config-company)
  ;; (require 'config-yasnippet)
  ;; (require 'config-magit)
  ;; (require 'config-miscellany)
  ;; ;; (require 'config-desktop)
  ;; (require 'config-emacs-lisp)
  ;; (require 'config-tex)
  ;; (require 'config-python)
  ;; ; (require 'test)
  )

;; (require 'evil-surround)
;; (require 'evil-visualstar)
;; (require 'evil-mark-replace)
;; (require 'evil-escape)
;; (require 'etags-select)
;; (require 'evil-matchit)
;; (require 'expand-region)
;; (require 'magit)
;; (require 'hippie-expand)
;; (require 'flymake-find-file-hook)

(/provide)
;;; init/graphic.el ends here
