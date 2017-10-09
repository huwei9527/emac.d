;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'keymap-code)
  (require 'hook-code)
  (require 'evil-code))

(code-defkey-ctl-x
  "g" magit-status)

(code-eval-after-load
 magit-repos
 (setq magit-repository-directories '(("~/Projects" . 3)))
 (code-set-evil-initial-state magit-repolist-mode motion))

(code-eval-after-load
 magit-log
 (setq magit-log-auto-more t
       ;; magit-log-show-refname-after-summary t
       ))


(provide 'config-magit)
;;; config-magit.el ends here
