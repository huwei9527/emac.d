;;; init.el --- emacs init file -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

;; (package-initialize)

(setq emacs-load-start-time (current-time))

;; Must before any configuration package.
(require '/init/global
	 (expand-file-name "lisp/init/global.el" user-emacs-directory))

(/require-custom core)

(if (display-graphic-p)
    (/require-init graphic)
  (/require-init terminal)
  )

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
