;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(setq emacs-load-start-time (current-time))

;; Debug flag.
;(setq debug-on-error t)
(setq edebug-all-defs t)
(setq edebug-all-forms t)
(switch-to-buffer "*Messages*")

;; Must before any configuration package.
(require 'global-custom
         ;; Specify the file because load-path is set after it is loaded.
         (expand-file-name "lisp/custom/global-custom.el" user-emacs-directory))

(let* ((file-name-handler-alist nil)) ; Accelerate loading.
  (require 'config-elpa)
  (require 'config-miscellany)
  (require 'config-ui)
  ;(require 'config-auto-save)
  (require 'config-evil)
  ;(require 'test)
  )

;(mdft)

;(add-hook 'post-command-hook (lambda () (message "POST COMMAND HOOK")))

(defvar emacs-load-time (time-to-seconds (time-since emacs-load-start-time)))
(message "%f" emacs-load-time)
(setq initial-scratch-message (format ";; %f\n" emacs-load-time))

(provide 'init)
; init.el ends here
