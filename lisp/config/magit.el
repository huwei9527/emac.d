;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(eval-when-compile (/require-meta core keymap hook evil))

(require 'evil)

(/def-keys-ctl-x g magit-status)

(setq magit-log-margin '(nil age margit-log-margin-width t 18))

(/eval-after-load magit-repos
  (setq magit-repository-directories '(("~/Projects" . 3)))
  (/evil-set-initial-state-and-make-override
    magit-repolist-mode motion))

(/eval-after-load magit-blame
  (evil-make-overriding-map magit-blame-mode-map))

(/eval-after-load git-commit
  (/add-hook (git-commit-setup-hook) evil-force-normal-state))

(/eval-after-load git-log
  (setq magit-log-auto-more t
	; magit-log-show-refname-after-summary t
	))

(/eval-after-load magit-branch
  (magit-remove-popup-key 'magit-branch-popup :action ?b)
  (magit-define-popup-action 'magit-branch-popup ?b
			     "Checkout" magit-branch-or-checkout
			     'magit-branch t))

(/eval-after-load magit
  (/evil-set-initial-state-and-make-override
    magit-merge-preview-mode motion))

(/eval-after-load magit-submodule
  (/evil-set-initial-state-and-make-override
    magit-submodule-list-mode motion))


(/provide)
;;; config/magit.el ends here
