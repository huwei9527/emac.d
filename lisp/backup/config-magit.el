;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

(eval-when-compile
  (require 'keymap-code)
  (require 'hook-code)
  (require 'evil-code))

(code-defkey-ctl-x
  "g" magit-status)

(setq magit-log-margin '(nil age magit-log-margin-width t 18))

(code-eval-after-load
 magit-repos
 (setq magit-repository-directories '(("~/Projects" . 3)))
 (code-set-evil-initial-state magit-repolist-mode motion))

(code-eval-after-load
 magit-blame
 (evil-make-overriding-map magit-blame-mode-map))

(code-eval-after-load
 git-commit
 (code-add-hook
  (git-commit-setup-hook)
  evil-force-normal-state))

(code-eval-after-load
 magit-log
 (setq magit-log-auto-more t
       ;; magit-log-show-refname-after-summary t
       ))

(code-eval-after-load
 magit-branch
 (magit-remove-popup-key 'magit-branch-popup :action ?b)
 (magit-define-popup-action 'magit-branch-popup
   ?b "Checkout" 'magit-branch-or-checkout 'magit-brach t))

(code-eval-after-load
 magit
 (code-set-evil-initial-state magit-merge-preview-mode motion))

(code-eval-after-load
 magit-submodule
 (code-set-evil-initial-state magit-submodule-list-mode motion))


(provide 'config-magit)
;;; config-magit.el ends here
