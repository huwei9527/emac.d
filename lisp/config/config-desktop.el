;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(require 'desktop-custom)
(eval-when-compile
  (require 'hook-code)
  (require 'code)
  (require 'silence-code))

(code-eval-after-load
 desktop
 ;; (code-add-advice
 ;;  (desktop-read)
 ;;  :around
 ;;  (lambda (orig-fun &rest args)
 ;;    (code-silence-function
 ;;     (delay-warning)
 ;;     (apply orig-fun args))))
 (setq desktop-path `(,config-desktop-directory)
       desktop-base-file-name "emacs-desktop"
       desktop-base-lock-name "emacs-desktop-lock"
       desktop-save 'if-exists
       desktop-load-locked-desktop t
       ;; Don't use desktop-save-mode to reload frame
       ;; There is a bug when opening gui emacs after saving
       ;; desktop in tty emacs. Use workgroups2 to reload frames
       desktop-restore-frames nil
       ;; desktop-restore-in-current-display t
       ;; desktop-restore-forces-onscreen t
       desktop-globals-to-save
       `(file-name-history
	 minibuffer-history
	 ivy-history
	 swiper-history
	 counsel-M-x-history
	 counsel-describe-symbol-history
	 counsel-git-grep-history)
       desktop-modes-not-to-save
       `(help-mode
	 dired-mode
	 Info-mode
	 package-menu-mode)))

(code-eval-after-load
 workgroups2
 (setq wg-session-file
       (expand-file-name "emacs-workgroups" config-desktop-directory)
       wg-mode-line-display-on nil))

(when (display-graphic-p)
  (desktop-save-mode 1)
  (workgroups-mode 1)
  )

(provide 'config-desktop)
;;; config-desktop.el ends here
