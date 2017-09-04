;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(setq emacs-load-start-time (current-time))

;; Must before any configuration package.
(require 'global-custom
         ;; Specify the file because load-path is set after it is loaded.
         (expand-file-name "lisp/custom/global-custom.el" user-emacs-directory))

(require 'test-custom)

(let* ((file-name-handler-alist nil)) ; Accelerate loading.
  (require 'config-elpa)
  (require 'config-miscellany)
  (require 'config-ui)
  (require 'config-auto-save)
  (require 'config-evil)
  (require 'config-ivy)
  (require 'code)
  (require 'test-lib)
  ;; (require 'test)
  )

;; (require 'evil-surround)
;; (require 'evil-visualstar)
;; (require 'find-file-in-project)
;; (require 'evil-mark-replace)
;; (require 'evil-escape)
;; (require 'etags-select)
;; (require 'evil-matchit)
;; (require 'expand-region)
;; (require 'magit)


(defvar emacs-load-time (time-to-seconds (time-since emacs-load-start-time)))
(message "%f" emacs-load-time)
(setq initial-scratch-message (format ";; %f\n" emacs-load-time))

(provide 'init)
; init.el ends here
