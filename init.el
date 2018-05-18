;;; init.el --- emacs init file -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq emacs-load-start-time (current-time))

;; Must before any configuration package.

;;; Commentary:
;; 

(require 'global-custom
         ;; Specify the file because load-path is set after it is loaded.
         (expand-file-name "lisp/custom/global-custom.el" user-emacs-directory))

(require 'test-custom)
(require 'test-lib)

(let* ((file-name-handler-alist nil)) ; Accelerate loading.
  (require 'config-elpa)
  (require 'config-ui)
  (require 'config-auto-save)
  (require 'config-spell)
  (require 'config-evil)
  (require 'config-ace-link)
  (require 'config-ace-jump-mode)
  (require 'config-ivy)
  (require 'config-ffip)
  (require 'config-rainbow-delimiters)
  (require 'config-which-key)
  (require 'config-paredit)
  (require 'config-hideshow)
  (require 'config-company)
  (require 'config-yasnippet)
  (require 'config-magit)
  (require 'config-miscellany)
  ;; (require 'config-desktop)
  (require 'config-emacs-lisp)
  (require 'config-tex)
  (require 'config-python)
  (require 'test)
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


(defvar emacs-load-time (time-to-seconds (time-since emacs-load-start-time)))
(message "%f" emacs-load-time)
(setq initial-scratch-message (format ";; %f\n" emacs-load-time))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (workgroups which-key smex rainbow-delimiters popwin paredit-everywhere magit flyspell-lazy flx evil elpy counsel company-ycmd company-statistics company-auctex browse-kill-ring ace-link ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
